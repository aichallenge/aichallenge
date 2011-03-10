;;;; $Id: sbcl.lisp 485 2008-12-26 14:31:49Z ctian $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/backend/sbcl.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)

;; There's no way to preload the sockets library other than by requiring it
;;
;; ECL sockets has been forked off sb-bsd-sockets and implements the
;; same interface. We use the same file for now.
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sockets))

#+sbcl
(progn
  #-win32
  (defun get-host-name ()
    (sb-unix:unix-gethostname))

  ;; we assume winsock has already been loaded, after all,
  ;; we already loaded sb-bsd-sockets and sb-alien
  #+win32
  (defun get-host-name ()
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
       (let ((result (sb-alien:alien-funcall
                      (sb-alien:extern-alien "gethostname"
                                             (sb-alien:function sb-alien:int
                                                                (* sb-alien:char)
                                                                sb-alien:int))
                      (sb-alien:cast buf (* sb-alien:char))
                      256)))
         (when (= result 0)
           (sb-alien:cast buf sb-alien:c-string))))))


#+ecl
(progn

  #-:wsock
  (ffi:clines
   "#include <errno.h>"
   "#include <sys/socket.h>")
  #+:wsock
  (ffi:clines
   "#ifndef FD_SETSIZE"
   "#define FD_SETSIZE 1024"
   "#endif"
   "#include <winsock2.h>")

  (ffi:clines
   "#include <sys/time.h>"
   "#include <ecl/ecl-inl.h>")

  #+:prefixed-api
  (ffi:clines
   "#define CONS(x, y) ecl_cons((x), (y))"
   "#define MAKE_INTEGER(x) ecl_make_integer((x))")
  #-:prefixed-api
  (ffi:clines
   "#define CONS(x, y) make_cons((x), (y))"
   "#define MAKE_INTEGER(x) make_integer((x))")

  (defun fd-setsize ()
    (ffi:c-inline () () :fixnum
     "FD_SETSIZE" :one-liner t))

  (defun fdset-alloc ()
    (ffi:c-inline () () :pointer-void
     "cl_alloc_atomic(sizeof(fd_set))" :one-liner t))

  (defun fdset-zero (fdset)
    (ffi:c-inline (fdset) (:pointer-void) :void
     "FD_ZERO((fd_set*)#0)" :one-liner t))

  (defun fdset-set (fdset fd)
    (ffi:c-inline (fdset fd) (:pointer-void :fixnum) :void
     "FD_SET(#1,(fd_set*)#0)" :one-liner t))

  (defun fdset-clr (fdset fd)
    (ffi:c-inline (fdset fd) (:pointer-void :fixnum) :void
     "FD_CLR(#1,(fd_set*)#0)" :one-liner t))

  (defun fdset-fd-isset (fdset fd)
    (ffi:c-inline (fdset fd) (:pointer-void :fixnum) :bool
     "FD_ISSET(#1,(fd_set*)#0)" :one-liner t))

  (declaim (inline fd-setsize
                   fdset-alloc
                   fdset-zero
                   fdset-set
                   fdset-clr
                   fdset-fd-isset))

  (defun get-host-name ()
    (ffi:c-inline
     () () :object
     "{ char *buf = cl_alloc_atomic(257);

        if (gethostname(buf,256) == 0)
          @(return) = make_simple_base_string(buf);
        else
          @(return) = Cnil;
      }" :one-liner nil :side-effects nil))

  (defun read-select (wl to-secs &optional (to-musecs 0))
    (let* ((sockets (wait-list-waiters wl))
           (rfds (wait-list-%wait wl))
           (max-fd (reduce #'(lambda (x y)
                               (let ((sy (sb-bsd-sockets:socket-file-descriptor
                                          (socket y))))
                                 (if (< x sy) sy x)))
                           (cdr sockets)
                           :initial-value (sb-bsd-sockets:socket-file-descriptor
                                           (socket (car sockets))))))
      (fdset-zero rfds)
      (dolist (sock sockets)
        (fdset-set rfds (sb-bsd-sockets:socket-file-descriptor
                         (socket sock))))
      (let ((count
             (ffi:c-inline (to-secs to-musecs rfds max-fd)
                           (t :unsigned-int :pointer-void :int)
                           :int
      "
          int count;
          struct timeval tv;

          if (#0 != Cnil) {
            tv.tv_sec = fixnnint(#0);
            tv.tv_usec = #1;
          }
        @(return) = select(#3 + 1, (fd_set*)#2, NULL, NULL,
                           (#0 != Cnil) ? &tv : NULL);
" :one-liner nil)))
        (cond
          ((= 0 count)
           (values nil nil))
          ((< count 0)
           ;; check for EINTR and EAGAIN; these should not err
           (values nil (ffi:c-inline () () :int "errno" :one-liner t)))
          (t
           (dolist (sock sockets)
             (when (fdset-fd-isset rfds (sb-bsd-sockets:socket-file-descriptor
                                         (socket sock)))
               (setf (state sock) :READ))))))))


)

(defun map-socket-error (sock-err)
  (map-errno-error (sb-bsd-sockets::socket-error-errno sock-err)))

(defparameter +sbcl-condition-map+
  '((interrupted-error . interrupted-condition)))

(defparameter +sbcl-error-map+
  `((sb-bsd-sockets:address-in-use-error . address-in-use-error)
    (sb-bsd-sockets::no-address-error . address-not-available-error)
    (sb-bsd-sockets:bad-file-descriptor-error . bad-file-descriptor-error)
    (sb-bsd-sockets:connection-refused-error . connection-refused-error)
    (sb-bsd-sockets:invalid-argument-error . invalid-argument-error)
    (sb-bsd-sockets:no-buffers-error . no-buffers-error)
    (sb-bsd-sockets:operation-not-supported-error
     . operation-not-supported-error)
    (sb-bsd-sockets:operation-not-permitted-error
     . operation-not-permitted-error)
    (sb-bsd-sockets:protocol-not-supported-error
     . protocol-not-supported-error)
    #-ecl
    (sb-bsd-sockets:unknown-protocol
     . protocol-not-supported-error)
    (sb-bsd-sockets:socket-type-not-supported-error
     . socket-type-not-supported-error)
    (sb-bsd-sockets:network-unreachable-error . network-unreachable-error)
    (sb-bsd-sockets:operation-timeout-error . timeout-error)
    (sb-bsd-sockets:socket-error . ,#'map-socket-error)

    ;; Nameservice errors: mapped to unknown-error
    #-ecl #-ecl #-ecl
    (sb-bsd-sockets:no-recovery-error . ns-no-recovery-error)
    (sb-bsd-sockets:try-again-error . ns-try-again-condition)
    (sb-bsd-sockets:host-not-found-error . ns-host-not-found-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (serious-condition (let* ((usock-error (cdr (assoc (type-of condition)
                                           +sbcl-error-map+)))
                  (usock-error (if (functionp usock-error)
                                   (funcall usock-error condition)
                                 usock-error)))
             (when usock-error
                 (error usock-error :socket socket))))
    (condition (let* ((usock-cond (cdr (assoc (type-of condition)
                                              +sbcl-condition-map+)))
                      (usock-cond (if (functionp usock-cond)
                                      (funcall usock-cond condition)
                                    usock-cond)))
                 (if usock-cond
                     (signal usock-cond :socket socket))))))


(defun socket-connect (host port &key (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
                       local-host local-port
		       &aux
		       (sockopt-tcp-nodelay-p
			(fboundp 'sb-bsd-sockets::sockopt-tcp-nodelay)))
  (when deadline (unsupported 'deadline 'socket-connect))
  (when timeout (unsupported 'timeout 'socket-connect))
  (when (and nodelay-specified
             ;; 20080802: ECL added this function to its sockets
             ;; package today. There's no guarantee the functions
             ;; we need are available, but we can make sure not to
             ;; call them if they aren't
             (not sockopt-tcp-nodelay-p))
    (unsupported 'nodelay 'socket-connect))

  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (handler-case
        (let* ((stream
                (sb-bsd-sockets:socket-make-stream socket
                                                   :input t
                                                   :output t
                                                   :buffering :full
                                                   :element-type element-type))
               ;;###FIXME: The above line probably needs an :external-format
               (usocket (make-stream-socket :stream stream :socket socket))
               (ip (host-to-vector-quad host)))
	  ;; binghe: use SOCKOPT-TCP-NODELAY as internal symbol
	  ;;         to pass compilation on ECL without it.
          (when (and nodelay-specified sockopt-tcp-nodelay-p)
            (setf (sb-bsd-sockets::sockopt-tcp-nodelay socket) nodelay))
          (when (or local-host local-port)
            (sb-bsd-sockets:socket-bind socket
                                        (host-to-vector-quad
                                         (or local-host *wildcard-host*))
                                        (or local-port *auto-port*)))
          (with-mapped-conditions (usocket)
            (sb-bsd-sockets:socket-connect socket ip port))
          usocket)
      (t (c)
        ;; Make sure we don't leak filedescriptors
        (sb-bsd-sockets:socket-close socket)
        (error c)))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (ip (host-to-vector-quad host))
         (sock (make-instance 'sb-bsd-sockets:inet-socket
                              :type :stream :protocol :tcp)))
    (handler-case
        (with-mapped-conditions ()
          (setf (sb-bsd-sockets:sockopt-reuse-address sock) reuseaddress)
          (sb-bsd-sockets:socket-bind sock ip port)
          (sb-bsd-sockets:socket-listen sock backlog)
          (make-stream-server-socket sock :element-type element-type))
      (t (c)
        ;; Make sure we don't leak filedescriptors
        (sb-bsd-sockets:socket-close sock)
        (error c)))))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (with-mapped-conditions (socket)
     (let ((sock (sb-bsd-sockets:socket-accept (socket socket))))
       (make-stream-socket
        :socket sock
        :stream (sb-bsd-sockets:socket-make-stream
                 sock
                 :input t :output t :buffering :full
                 :element-type (or element-type
                                   (element-type socket)))))))

;; Sockets and their associated streams are modelled as
;; different objects. Be sure to close the stream (which
;; closes the socket too) when closing a stream-socket.
(defmethod socket-close ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (sb-bsd-sockets:socket-close (socket usocket))))

(defmethod socket-close ((usocket stream-usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod get-local-name ((usocket usocket))
  (sb-bsd-sockets:socket-name (socket usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (sb-bsd-sockets:socket-peername (socket usocket)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))


(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (sb-bsd-sockets::host-ent-name
        (sb-bsd-sockets:get-host-by-address address))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (sb-bsd-sockets::host-ent-addresses
         (sb-bsd-sockets:get-host-by-name name))))

#+(and sbcl (not win32))
(progn

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (push (socket waiter) (wait-list-%wait wait-list)))

(defun %remove-waiter (wait-list waiter)
  (setf (wait-list-%wait wait-list)
        (remove (socket waiter) (wait-list-%wait wait-list))))



  (defun wait-for-input-internal (sockets &key timeout)
    (with-mapped-conditions ()
      (sb-alien:with-alien ((rfds (sb-alien:struct sb-unix:fd-set)))
         (sb-unix:fd-zero rfds)
         (dolist (socket (wait-list-%wait sockets))
           (sb-unix:fd-set
            (sb-bsd-sockets:socket-file-descriptor socket)
            rfds))
         (multiple-value-bind
             (secs musecs)
             (split-timeout (or timeout 1))
           (multiple-value-bind
               (count err)
               (sb-unix:unix-fast-select
                (1+ (reduce #'max (wait-list-%wait sockets)
                            :key #'sb-bsd-sockets:socket-file-descriptor))
                (sb-alien:addr rfds) nil nil
                (when timeout secs) (when timeout musecs))
	     (if (null count)
		 (unless (= err sb-unix:EINTR)
		   (error (map-errno-error err)))
		 (when (< 0 count)
		   ;; process the result...
                   (dolist (x (wait-list-waiters sockets))
                     (when (sb-unix:fd-isset
                            (sb-bsd-sockets:socket-file-descriptor
                             (socket x))
                            rfds)
                       (setf (state x) :READ))))))))))
) ; progn

#+(and sbcl win32)
  (warn "wait-for-input not (yet!) supported...")

#+ecl
(progn
  (defun wait-for-input-internal (wl &key timeout)
    (with-mapped-conditions ()
      (multiple-value-bind
            (secs usecs)
          (split-timeout (or timeout 1))
        (multiple-value-bind
              (result-fds err)
            (read-select wl (when timeout secs) usecs)
          (unless (null err)
            (error (map-errno-error err)))))))

  (defun %setup-wait-list (wl)
    (setf (wait-list-%wait wl)
          (fdset-alloc)))

  (defun %add-waiter (wl w)
    (declare (ignore wl w)))

  (defun %remove-waiter (wl w)
    (declare (ignore wl w)))

  )
