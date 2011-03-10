;;;; $Id: allegro.lisp 456 2008-10-22 13:51:19Z ctian $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/backend/allegro.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock)
  ;; for wait-for-input:
  (require :process)
  ;; note: the line below requires ACL 6.2+
  (require :osi))

(defun get-host-name ()
  ;; note: the line below requires ACL 7.0+ to actually *work* on windows
  (excl.osi:gethostname))

(defparameter +allegro-identifier-error-map+
  '((:address-in-use . address-in-use-error)
    (:address-not-available . address-not-available-error)
    (:network-down . network-down-error)
    (:network-reset . network-reset-error)
    (:network-unreachable . network-unreachable-error)
    (:connection-aborted . connection-aborted-error)
    (:connection-reset . connection-reset-error)
    (:no-buffer-space . no-buffers-error)
    (:shutdown . shutdown-error)
    (:connection-timed-out . timeout-error)
    (:connection-refused . connection-refused-error)
    (:host-down . host-down-error)
    (:host-unreachable . host-unreachable-error)))

(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (excl:socket-error
     (let ((usock-err
            (cdr (assoc (excl:stream-error-identifier condition)
                        +allegro-identifier-error-map+))))
       (if usock-err
           (error usock-err :socket socket)
         (error 'unknown-error
                :real-error condition
                :socket socket))))))

(defun to-format (element-type)
  (if (subtypep element-type 'character)
      :text
    :binary))

(defun socket-connect (host port &key (element-type 'character)
                       timeout deadline
                       (nodelay t) ;; nodelay == t is the ACL default
                       local-host local-port)
  (when timeout (unsupported 'timeout 'socket-connect))
  (when deadline (unsupported 'deadline 'socket-connect))

  (let ((socket))
    (setf socket
	  (labels ((make-socket ()
		     (socket:make-socket :remote-host (host-to-hostname host)
					 :remote-port port
					 :local-host (when local-host
						       (host-to-hostname local-host))
					 :local-port local-port
					 :format (to-format element-type)
					 :nodelay nodelay)))
	    (with-mapped-conditions (socket)
	      (if timeout
		  (mp:with-timeout (timeout nil)
		    (make-socket))
		  (make-socket)))))
    (make-stream-socket :socket socket :stream socket)))

;; One socket close method is sufficient,
;; because socket-streams are also sockets.
(defmethod socket-close ((usocket usocket))
  "Close socket."
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket usocket))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  ;; Allegro and OpenMCL socket interfaces bear very strong resemblence
  ;; whatever you change here, change it also for OpenMCL
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (sock (with-mapped-conditions ()
                 (apply #'socket:make-socket
                        (append (list :connect :passive
                                      :reuse-address reuseaddress
                                      :local-port port
                                      :backlog backlog
                                      :format (to-format element-type)
                                      ;; allegro now ignores :format
                                      )
                                (when (ip/= host *wildcard-host*)
                                  (list :local-host host)))))))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (declare (ignore element-type)) ;; allegro streams are multivalent
  (let ((stream-sock
         (with-mapped-conditions (socket)
            (socket:accept-connection (socket socket)))))
    (make-stream-socket :socket stream-sock :stream stream-sock)))

(defmethod get-local-address ((usocket usocket))
  (hbo-to-vector-quad (socket:local-host (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (hbo-to-vector-quad (socket:remote-host (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (socket:local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (socket:remote-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))


(defun get-host-by-address (address)
  (with-mapped-conditions ()
    (socket:ipaddr-to-hostname (host-to-hbo address))))

(defun get-hosts-by-name (name)
  ;;###FIXME: ACL has the acldns module which returns all A records
  ;; only problem: it doesn't fall back to tcp (from udp) if the returned
  ;; structure is too long.
  (with-mapped-conditions ()
    (list (hbo-to-vector-quad (socket:lookup-hostname
                               (host-to-hostname name))))))

(defun %setup-wait-list (wait-list)
  (declare (ignore wait-list)))

(defun %add-waiter (wait-list waiter)
  (push (socket waiter) (wait-list-%wait wait-list)))

(defun %remove-waiter (wait-list waiter)
  (setf (wait-list-%wait wait-list)
        (remove (socket waiter) (wait-list-%wait wait-list))))

(defun wait-for-input-internal (wait-list &key timeout)
  (with-mapped-conditions ()
    (let ((active-internal-sockets
           (if timeout
               (mp:wait-for-input-available (wait-list-%wait wait-list)
                                            :timeout timeout)
             (mp:wait-for-input-available (wait-list-%wait wait-list)))))
      ;; this is quadratic, but hey, the active-internal-sockets
      ;; list is very short and it's only quadratic in the length of that one.
      ;; When I have more time I could recode it to something of linear
      ;; complexity.
      ;; [Same code is also used in openmcl.lisp]
      (dolist (x active-internal-sockets)
        (setf (state (gethash x (wait-list-map wait-list)))
              :READ))
      wait-list)))
