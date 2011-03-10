;;;; $Id: cmucl.lisp 470 2008-10-28 12:02:01Z ctian $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/backend/cmucl.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)

#+win32
(defun remap-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +cmucl-error-map+
  #+win32
  (append (remap-for-win32 +unix-errno-condition-map+)
          (remap-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))

(defun cmucl-map-socket-error (err &key condition socket)
  (let ((usock-err
         (cdr (assoc err +cmucl-error-map+ :test #'member))))
    (if usock-err
        (if (subtypep usock-err 'error)
            (error usock-err :socket socket)
          (signal usock-err :socket socket))
      (error 'unknown-error
             :socket socket
             :real-error condition))))

;; CMUCL error handling is brain-dead: it doesn't preserve any
;; information other than the OS error string from which the
;; error can be determined. The OS error string isn't good enough
;; given that it may have been localized (l10n).
;;
;; The above applies to versions pre 19b; 19d and newer are expected to
;; contain even better error reporting.
;;
;;
;; Just catch the errors and encapsulate them in an unknown-error
(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (ext::socket-error (cmucl-map-socket-error (ext::socket-errno condition)
                                               :socket socket
                                               :condition condition))))

(defun socket-connect (host port &key (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
		       (local-host nil local-host-p)
		       (local-port nil local-port-p)
		       &aux
		       (local-bind-p (fboundp 'ext::bind-inet-socket)))
  (declare (ignore nodelay))
  (when timeout (unsupported 'timeout 'socket-connect))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when nodelay-specified (unsupported 'nodelay 'socket-connect))
  (when (and local-host-p (not local-bind-p))
     (unsupported 'local-host 'socket-connect :minimum "Snapshot 2008-08 (19E)"))
  (when (and local-port-p (not local-bind-p))
     (unsupported 'local-port 'socket-connect :minimum "Snapshot 2008-08 (19E)"))

  (let* ((socket))
    (setf socket
	  (let ((args (list (host-to-hbo host) port :stream)))
	    (when (and local-bind-p (or local-host-p local-port-p))
	      (nconc args (list :local-host (when local-host
					      (host-to-hbo local-host))
				:local-port local-port)))
	    (with-mapped-conditions (socket)
	      (apply #'ext:connect-to-inet-socket args))))
    (if socket
        (let* ((stream (sys:make-fd-stream socket :input t :output t
                                           :element-type element-type
                                           :buffering :full))
               ;;###FIXME the above line probably needs an :external-format
               (usocket (make-stream-socket :socket socket
                                            :stream stream)))
          usocket)
      (let ((err (unix:unix-errno)))
        (when err (cmucl-map-socket-error err))))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
 (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
        (server-sock
         (with-mapped-conditions ()
           (apply #'ext:create-inet-listener
                  (nconc  (list port :stream
                                :backlog backlog
                                :reuse-address reuseaddress)
                          (when (ip/= host *wildcard-host*)
                            (list :host
                                  (host-to-hbo host))))))))
   (make-stream-server-socket server-sock :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (with-mapped-conditions (usocket)
    (let* ((sock (ext:accept-tcp-connection (socket usocket)))
           (stream (sys:make-fd-stream sock :input t :output t
                                       :element-type (or element-type
                                                         (element-type usocket))
                                       :buffering :full)))
      (make-stream-socket :socket sock :stream stream))))

;; Sockets and socket streams are represented
;; by different objects. Be sure to close the
;; socket stream when closing a stream socket.
(defmethod socket-close ((usocket stream-usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod socket-close ((usocket usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (ext:close-socket (socket usocket))))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (ext:get-socket-host-and-port (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (ext:get-peer-host-and-port (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))


(defun lookup-host-entry (host)
  (multiple-value-bind
      (entry errno)
      (ext:lookup-host-entry host)
    (if entry
        entry
      ;;###The constants below work on *most* OSes, but are defined as the
      ;; constants mentioned in C
      (let ((exception
             (second (assoc errno
                            '((1 ns-host-not-found-error) ;; HOST_NOT_FOUND
                              (2 ns-no-recovery-error)    ;; NO_DATA
                              (3 ns-no-recovery-error)    ;; NO_RECOVERY
                              (4 ns-try-again))))))       ;; TRY_AGAIN
        (when exception
          (error exception))))))


(defun get-host-by-address (address)
  (handler-case (ext:host-entry-name
                 (lookup-host-entry (host-byte-order address)))
    (condition (condition) (handle-condition condition))))

(defun get-hosts-by-name (name)
  (handler-case (mapcar #'hbo-to-vector-quad
                        (ext:host-entry-addr-list
                         (lookup-host-entry name)))
    (condition (condition) (handle-condition condition))))

(defun get-host-name ()
  (unix:unix-gethostname))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (push (socket waiter) (wait-list-%wait wait-list)))

(defun %remove-waiter (wait-list waiter)
  (setf (wait-list-%wait wait-list)
        (remove (socket waiter) (wait-list-%wait wait-list))))

(defun wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (alien:with-alien ((rfds (alien:struct unix:fd-set)))
       (unix:fd-zero rfds)
       (dolist (socket (wait-list-%wait wait-list))
         (unix:fd-set socket rfds))
       (multiple-value-bind
           (secs musecs)
           (split-timeout (or timeout 1))
         (multiple-value-bind
             (count err)
             (unix:unix-fast-select (1+ (reduce #'max
                                                (wait-list-%wait wait-list)))
                                    (alien:addr rfds) nil nil
                                    (when timeout secs) musecs)
           (if (<= 0 count)
               ;; process the result...
               (dolist (x (wait-list-waiters wait-list))
                 (when (unix:fd-isset (socket x) rfds)
                   (setf (state x) :READ)))
             (progn
               ;;###FIXME generate an error, except for EINTR
               )))))))
