;;;; $Id: lispworks.lisp 476 2008-11-12 14:18:15Z ctian $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/backend/lispworks.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

;;; ---------------------------------------------------------------------------
;;;  Warn if multiprocessing is not running on Lispworks

#-win32
(defun check-for-multiprocessing-started (&optional errorp)
  (unless mp:*current-process*
    (funcall (if errorp 'error 'warn)
             "You must start multiprocessing on Lispworks by calling~
              ~%~3t(~s)~
              ~%for ~s function properly."
             'mp:initialize-multiprocessing
             'wait-for-input)))

#-win32
(check-for-multiprocessing-started)

#+win32
(fli:register-module "ws2_32")

(fli:define-foreign-function (get-host-name-internal "gethostname" :source)
      ((return-string (:reference-return (:ef-mb-string :limit 257)))
       (namelen :int))
      :lambda-list (&aux (namelen 256) return-string)
      :result-type :int
      #+win32 :module
      #+win32 "ws2_32")

(defun get-host-name ()
  (multiple-value-bind (retcode name)
      (get-host-name-internal)
    (when (= 0 retcode)
      name)))

#+win32
(defun remap-maybe-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +lispworks-error-map+
  #+win32
  (append (remap-maybe-for-win32 +unix-errno-condition-map+)
          (remap-maybe-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))

(defun raise-or-signal-socket-error (errno socket)
  (let ((usock-err
         (cdr (assoc errno +lispworks-error-map+ :test #'member))))
    (if usock-err
        (if (subtypep usock-err 'error)
            (error usock-err :socket socket)
          (signal usock-err :socket))
      (error 'unknown-error
             :socket socket
             :real-condition nil))))

(defun raise-usock-err (errno socket &optional condition)
  (let* ((usock-err
          (cdr (assoc errno +lispworks-error-map+
                      :test #'member))))
    (if usock-err
        (if (subtypep usock-err 'error)
            (error usock-err :socket socket)
          (signal usock-err :socket))
      (error 'unknown-error
             :socket socket
             :real-error condition))))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (simple-error (destructuring-bind (&optional host port err-msg errno)
                      (simple-condition-format-arguments condition)
                    (declare (ignore host port err-msg))
                    (raise-usock-err errno socket condition)))))

(defun socket-connect (host port &key (element-type 'base-char)
                       timeout deadline (nodelay t nodelay-specified)
                       local-host local-port)
  (declare (ignorable nodelay))

  ;; What's the meaning of this keyword?
  (when deadline
    (unimplemented 'deadline 'socket-connect))

  #+(and lispworks4 (not lispworks4.4)) ; < 4.4.5
  (when timeout
    (unsupported 'timeout 'socket-connect :minimum "LispWorks 4.4.5"))
  
  #+(or lispworks4 lispworks5.0) ; < 5.1
  (when nodelay-specified
    (unsupported 'nodelay 'socket-connect :minimum "LispWorks 5.1"))

  #+lispworks4 #+lispworks4
  (when local-host
     (unsupported 'local-host 'socket-connect :minimum "LispWorks 5.0"))
  (when local-port
     (unsupported 'local-port 'socket-connect :minimum "LispWorks 5.0"))

  (let ((hostname (host-to-hostname host))
        (stream))
    (setf stream
          (with-mapped-conditions ()
             (comm:open-tcp-stream hostname port
                                   :element-type element-type
				   #-(and lispworks4 (not lispworks4.4)) ; >= 4.4.5
				   #-(and lispworks4 (not lispworks4.4))
				   :timeout timeout
                                   #-lispworks4 #-lispworks4
                                   #-lispworks4 #-lispworks4
                                   :local-address (when local-host (host-to-hostname local-host))
                                   :local-port local-port
                                   #-(or lispworks4 lispworks5.0) ; >= 5.1
                                   #-(or lispworks4 lispworks5.0)
                                   :nodelay nodelay)))
    (if stream
        (make-stream-socket :socket (comm:socket-stream-socket stream)
                            :stream stream)
      (error 'unknown-error))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'base-char))
  #+lispworks4.1
  (unsupported 'host 'socket-listen :minimum "LispWorks 4.0 or newer than 4.1")
  #+lispworks4.1
  (unsupported 'backlog 'socket-listen :minimum "LispWorks 4.0 or newer than 4.1")

  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (comm::*use_so_reuseaddr* reuseaddress)
         (hostname (host-to-hostname host))
         (sock (with-mapped-conditions ()
                  #-lispworks4.1 (comm::create-tcp-socket-for-service
                                  port :address hostname :backlog backlog)
                  #+lispworks4.1 (comm::create-tcp-socket-for-service port))))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (let* ((sock (with-mapped-conditions (usocket)
                 (comm::get-fd-from-socket (socket usocket))))
         (stream (make-instance 'comm:socket-stream
                                :socket sock
                                :direction :io
                                :element-type (or element-type
                                                  (element-type usocket)))))
    #+win32
    (when sock
      (setf (%ready-p usocket) nil))
    (make-stream-socket :socket sock :stream stream)))

;; Sockets and their streams are different objects
;; close the stream in order to make sure buffers
;; are correctly flushed and the socket closed.
(defmethod socket-close ((usocket stream-usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (close (socket-stream usocket)))

(defmethod socket-close ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
     (comm::close-socket (socket usocket))))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-peer-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (mapcar #'hbo-to-vector-quad
             (comm:get-host-entry name :fields '(:addresses)))))

(defun os-socket-handle (usocket)
  (socket usocket))

(defun usocket-listen (usocket)
  (if (stream-usocket-p usocket)
      (when (listen (socket-stream usocket))
        usocket)
    (when (comm::socket-listen (socket usocket))
      usocket)))

;;;
;;; Non Windows implementation
;;;   The Windows implementation needs to resort to the Windows API in order
;;;   to achieve what we want (what we want is waiting without busy-looping)
;;;

#-win32
(progn

  (defun %setup-wait-list (wait-list)
    (declare (ignore wait-list)))

  (defun %add-waiter (wait-list waiter)
    (declare (ignore wait-list waiter)))

  (defun %remove-waiter (wait-list waiter)
    (declare (ignore wait-list waiter)))

  (defun wait-for-input-internal (wait-list &key timeout)
    (with-mapped-conditions ()
      ;; unfortunately, it's impossible to share code between
      ;; non-win32 and win32 platforms...
      ;; Can we have a sane -pref. complete [UDP!?]- API next time, please?
      (dolist (x (wait-list-waiters wait-list))
        (mp:notice-fd (os-socket-handle x)))
      (labels ((wait-function (socks)
		 (let (rv)
		   (dolist (x socks rv)
		     (when (usocket-listen x)
		       (setf (state x) :READ
			     rv t))))))
	(if timeout
	    (mp:process-wait-with-timeout "Waiting for a socket to become active"
					(truncate timeout)
					#'wait-function
					(wait-list-waiters wait-list))
	    (mp:process-wait "Waiting for a socket to become active"
			     #'wait-function
			     (wait-list-waiters wait-list))))
      (dolist (x (wait-list-waiters wait-list))
        (mp:unnotice-fd (os-socket-handle x)))
      wait-list)))


;;;
;;;  The Windows side of the story
;;;    We want to wait without busy looping
;;;    This code only works in threads which don't have (hidden)
;;;    windows which need to receive messages. There are workarounds in the Windows API
;;;    but are those available to 'us'.
;;;


#+win32
(progn

  ;; LispWorks doesn't provide an interface to wait for a socket
  ;; to become ready (under Win32, that is) meaning that we need
  ;; to resort to system calls to achieve the same thing.
  ;; Luckily, it provides us access to the raw socket handles (as we 
  ;; wrote the code above.
  (defconstant fd-read 1)
  (defconstant fd-read-bit 0)
  (defconstant fd-write 2)
  (defconstant fd-write-bit 1)
  (defconstant fd-oob 4)
  (defconstant fd-oob-bit 2)
  (defconstant fd-accept 8)
  (defconstant fd-accept-bit 3)
  (defconstant fd-connect 16)
  (defconstant fd-connect-bit 4)
  (defconstant fd-close 32)
  (defconstant fd-close-bit 5)
  (defconstant fd-qos 64)
  (defconstant fd-qos-bit 6)
  (defconstant fd-group-qos 128)
  (defconstant fd-group-qos-bit 7)
  (defconstant fd-routing-interface 256)
  (defconstant fd-routing-interface-bit 8)
  (defconstant fd-address-list-change 512)
  (defconstant fd-address-list-change-bit 9)
  
  (defconstant fd-max-events 10)

  (defconstant fionread 1074030207)


  ;; Note:
  ;;
  ;;  If special finalization has to occur for a given
  ;;  system resource (handle), an associated object should
  ;;  be created.  A special cleanup action should be added
  ;;  to the system and a special cleanup action should
  ;;  be flagged on all objects created for resources like it
  ;;
  ;;  We have 2 functions to do so:
  ;;   * hcl:add-special-free-action (function-symbol)
  ;;   * hcl:flag-special-free-action (object)
  ;;
  ;;  Note that the special free action will be called on all
  ;;  objects which have been flagged for special free, so be
  ;;  sure to check for the right argument type!
  
  (fli:define-foreign-type ws-socket () '(:unsigned :int))
  (fli:define-foreign-type win32-handle () '(:unsigned :int))
  (fli:define-c-struct wsa-network-events (network-events :long)
    (error-code (:c-array :int 10)))

  (fli:define-foreign-function (wsa-event-create "WSACreateEvent" :source)
      ()
      :lambda-list nil
    :result-type :int
    :module "ws2_32")
  (fli:define-foreign-function (wsa-event-close "WSACloseEvent" :source)
      ((event-object win32-handle))
    :result-type :int
    :module "ws2_32")
  (fli:define-foreign-function (wsa-enum-network-events "WSAEnumNetworkEvents" :source)
      ((socket ws-socket)
       (event-object win32-handle)
       (network-events (:reference-return wsa-network-events)))
    :result-type :int
    :module "ws2_32")
  
  (fli:define-foreign-function (wsa-event-select "WSAEventSelect" :source)
      ((socket ws-socket)
       (event-object win32-handle)
       (network-events :long))
    :result-type :int
    :module "ws2_32")

  (fli:define-foreign-function (wsa-get-last-error "WSAGetLastError" :source)
      ()
    :result-type :int
    :module "ws2_32")

  (fli:define-foreign-function (wsa-ioctlsocket "ioctlsocket" :source)
      ((socket :long) (cmd :long) (argp (:ptr :long)))
    :result-type :int
    :module "ws2_32")


  ;; The Windows system 


  ;; Now that we have access to the system calls, this is the plan:

  ;; 1. Receive a wait-list with associated sockets to wait for
  ;; 2. Add all those sockets to an event handle
  ;; 3. Listen for an event on that handle (we have a LispWorks system:: internal for that)
  ;; 4. After listening, detect if there are errors
  ;;    (this step is different from Unix, where we can have only one error)
  ;; 5. If so, raise one of them
  ;; 6. If not so, return the sockets which have input waiting for them


  (defun maybe-wsa-error (rv &optional socket)
    (unless (zerop rv)
      (raise-usock-err (wsa-get-last-error) socket)))

  (defun bytes-available-for-read (socket)
    (fli:with-dynamic-foreign-objects ((int-ptr :long))
      (let ((rv (wsa-ioctlsocket (os-socket-handle socket) fionread int-ptr)))
        (if (= 0 rv)
            (fli:dereference int-ptr)
          0))))

  (defun socket-ready-p (socket)
     (if (typep socket 'stream-usocket)
       (< 0 (bytes-available-for-read socket))
       (%ready-p socket)))

  (defun waiting-required (sockets)
    (notany #'socket-ready-p sockets))

  (defun wait-for-input-internal (wait-list &key timeout)
    (when (waiting-required (wait-list-waiters wait-list))
      (system:wait-for-single-object (wait-list-%wait wait-list)
                                     "Waiting for socket activity" timeout))
    (update-ready-and-state-slots (wait-list-waiters wait-list)))

  
  (defun map-network-events (func network-events)
    (let ((event-map (fli:foreign-slot-value network-events 'network-events))
          (error-array (fli:foreign-slot-pointer network-events 'error-code)))
      (unless (zerop event-map)
	  (dotimes (i fd-max-events)
	    (unless (zerop (ldb (byte 1 i) event-map)) ;;### could be faster with ash and logand?
	      (funcall func (fli:foreign-aref error-array i)))))))

  (defun update-ready-and-state-slots (sockets)
     (dolist (socket sockets)
        (if (or (and (stream-usocket-p socket)
                     (listen (socket-stream socket)))
                (%ready-p socket))
            (setf (state socket) :READ)
          (multiple-value-bind
                (rv network-events)
                (wsa-enum-network-events (os-socket-handle socket) 0 t)
             (if (zerop rv)
                 (map-network-events #'(lambda (err-code)
                                          (if (zerop err-code)
                                             (setf (%ready-p socket) t
                                                   (state socket) :READ)
                                             (raise-usock-err err-code socket)))
                                     network-events)
                 (maybe-wsa-error rv socket))))))



  ;; The wait-list part

  (defun free-wait-list (wl)
    (when (wait-list-p wl)
      (unless (null (wait-list-%wait wl))
        (wsa-event-close (wait-list-%wait wl)))))
  
  (hcl:add-special-free-action 'free-wait-list)
  
  (defun %setup-wait-list (wait-list)
    (hcl:flag-special-free-action wait-list)
    (setf (wait-list-%wait wait-list) (wsa-event-create)))

  (defun %add-waiter (wait-list waiter)
    (let ((events (etypecase waiter
                    (stream-server-usocket (logior fd-connect fd-accept fd-close))
                    (stream-usocket (logior fd-connect fd-read fd-oob fd-close)))))
      (maybe-wsa-error
       (wsa-event-select (os-socket-handle waiter) (wait-list-%wait wait-list) events)
       waiter)))

  (defun %remove-waiter (wait-list waiter)
    (maybe-wsa-error
     (wsa-event-select (os-socket-handle waiter) (wait-list-%wait wait-list) 0)
     waiter))
  
  );; end of WIN32-block
