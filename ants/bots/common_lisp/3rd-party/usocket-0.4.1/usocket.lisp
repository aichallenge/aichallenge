;;;; $Id: usocket.lisp 452 2008-10-22 07:18:07Z ctian $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/usocket.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)

(defparameter *wildcard-host* #(0 0 0 0)
  "Hostname to pass when all interfaces in the current system are to be bound.")

(defparameter *auto-port* 0
  "Port number to pass when an auto-assigned port number is wanted.")

(defclass usocket ()
  ((socket
    :initarg :socket
    :accessor socket
    :documentation "Implementation specific socket object instance.'")
   (wait-list
    :initform nil
    :accessor wait-list
    :documentation "WAIT-LIST the object is associated with.")
   (state
    :initform nil
    :accessor state
    :documentation "Per-socket return value for the `wait-for-input' function.

The value stored in this slot can be any of
 NIL          - not ready
 :READ        - ready to read
 :READ-WRITE  - ready to read and write
 :WRITE       - ready to write

The last two remain unused in the current version.
")
   #+(and lispworks win32)
   (%ready-p
    :initform nil
    :accessor %ready-p
    :documentation "Indicates whether the socket has been signalled
as ready for reading a new connection.

The value will be set to T by `wait-for-input-internal' (given the
right conditions) and reset to NIL by `socket-accept'.

Don't modify this slot or depend on it as it is really intended
to be internal only.

Note: Accessed, but not used for 'stream-usocket'.
"
   ))
  (:documentation
"The main socket class.

Sockets should be closed using the `socket-close' method."))

(defclass stream-usocket (usocket)
   ((stream
     :initarg :stream
     :accessor socket-stream
     :documentation "Stream instance associated with the socket."
;;
;;Iff an external-format was passed to `socket-connect' or `socket-listen'
;;the stream is a flexi-stream. Otherwise the stream is implementation
;;specific."
))
   (:documentation
"Stream socket class.
'
Contrary to other sockets, these sockets may be closed either
with the `socket-close' method or by closing the associated stream
(which can be retrieved with the `socket-stream' accessor)."))

(defclass stream-server-usocket (usocket)
  ((element-type
    :initarg :element-type
    :initform #-lispworks 'character
              #+lispworks 'base-char
    :reader element-type
    :documentation "Default element type for streams created by
`socket-accept'."))
  (:documentation "Socket which listens for stream connections to
be initiated from remote sockets."))

(defun usocket-p (socket)
  (typep socket 'usocket))

(defun stream-usocket-p (socket)
  (typep socket 'stream-usocket))

(defun stream-server-usocket-p (socket)
  (typep socket 'stream-server-usocket))

(defun make-socket (&key socket)
  "Create a usocket socket type from implementation specific socket."
  (unless socket
    (error 'invalid-socket))
  (make-stream-socket :socket socket))

(defun make-stream-socket (&key socket stream)
  "Create a usocket socket type from implementation specific socket
and stream objects.

Sockets returned should be closed using the `socket-close' method or
by closing the stream associated with the socket.
"
  (unless socket
    (error 'invalid-socket-error))
  (unless stream
    (error 'invalid-socket-stream-error))
  (make-instance 'stream-usocket
                 :socket socket
                 :stream stream))

(defun make-stream-server-socket (socket &key (element-type
                                               #-lispworks 'character
                                               #+lispworks 'base-char))
  "Create a usocket-server socket type from an
implementation-specific socket object.

The returned value is a subtype of `stream-server-usocket'.
"
  (unless socket
    (error 'invalid-socket-error))
  (make-instance 'stream-server-usocket
                 :socket socket
                 :element-type element-type))

(defgeneric socket-accept (socket &key element-type)
  (:documentation
      "Accepts a connection from `socket', returning a `stream-socket'.

The stream associated with the socket returned has `element-type' when
explicitly specified, or the element-type passed to `socket-listen' otherwise."))

(defgeneric socket-close (usocket)
  (:documentation "Close a previously opened `usocket'."))

(defgeneric get-local-address (socket)
  (:documentation "Returns the IP address of the socket."))

(defgeneric get-peer-address (socket)
  (:documentation
   "Returns the IP address of the peer the socket is connected to."))

(defgeneric get-local-port (socket)
  (:documentation "Returns the IP port of the socket.

This function applies to both `stream-usocket' and `server-stream-usocket'
type objects."))

(defgeneric get-peer-port (socket)
  (:documentation "Returns the IP port of the peer the socket to."))

(defgeneric get-local-name (socket)
  (:documentation "Returns the IP address and port of the socket as values.

This function applies to both `stream-usocket' and `server-stream-usocket'
type objects."))

(defgeneric get-peer-name (socket)
  (:documentation
   "Returns the IP address and port of the peer
the socket is connected to as values."))

(defmacro with-connected-socket ((var socket) &body body)
  "Bind `socket' to `var', ensuring socket destruction on exit.

`body' is only evaluated when `var' is bound to a non-null value.

The `body' is an implied progn form."
  `(let ((,var ,socket))
     (unwind-protect
         (when ,var
           (with-mapped-conditions (,var)
             ,@body))
       (when ,var
         (socket-close ,var)))))

(defmacro with-client-socket ((socket-var stream-var &rest socket-connect-args)
                              &body body)
  "Bind the socket resulting from a call to `socket-connect' with
the arguments `socket-connect-args' to `socket-var' and if `stream-var' is
non-nil, bind the associated socket stream to it."
  `(with-connected-socket (,socket-var (socket-connect ,@socket-connect-args))
       ,(if (null stream-var)
           `(progn ,@body)
          `(let ((,stream-var (socket-stream ,socket-var)))
             ,@body))))

(defmacro with-server-socket ((var server-socket) &body body)
  "Bind `server-socket' to `var', ensuring socket destruction on exit.

`body' is only evaluated when `var' is bound to a non-null value.

The `body' is an implied progn form."
  `(with-connected-socket (,var ,server-socket)
      ,@body))

(defmacro with-socket-listener ((socket-var &rest socket-listen-args)
                                &body body)
  "Bind the socket resulting from a call to `socket-listen' with arguments
`socket-listen-args' to `socket-var'."
  `(with-server-socket (,socket-var (socket-listen ,@socket-listen-args))
      ,@body))


(defstruct (wait-list (:constructor %make-wait-list))
  %wait     ;; implementation specific
  waiters ;; the list of all usockets
  map  ;; maps implementation sockets to usockets
  )

;; Implementation specific:
;;
;;  %setup-wait-list
;;  %add-waiter
;;  %remove-waiter

(defun make-wait-list (waiters)
  (let ((wl (%make-wait-list)))
    (setf (wait-list-map wl) (make-hash-table))
    (%setup-wait-list wl)
    (dolist (x waiters)
      (add-waiter wl x))
    wl))

(defun add-waiter (wait-list input)
  (setf (gethash (socket input) (wait-list-map wait-list)) input
        (wait-list input) wait-list)
  (pushnew input (wait-list-waiters wait-list))
  (%add-waiter wait-list input))

(defun remove-waiter (wait-list input)
  (%remove-waiter wait-list input)
  (setf (wait-list-waiters wait-list)
        (remove input (wait-list-waiters wait-list))
        (wait-list input) nil)
  (remhash (socket input) (wait-list-map wait-list)))

(defun remove-all-waiters (wait-list)
  (dolist (waiter (wait-list-waiters wait-list))
    (%remove-waiter wait-list waiter))
  (setf (wait-list-waiters wait-list) nil)
  (clrhash (wait-list-map wait-list)))


(defun wait-for-input (socket-or-sockets &key timeout ready-only)
  "Waits for one or more streams to become ready for reading from
the socket.  When `timeout' (a non-negative real number) is
specified, wait `timeout' seconds, or wait indefinitely when
it isn't specified.  A `timeout' value of 0 (zero) means polling.

Returns two values: the first value is the list of streams which
are readable (or in case of server streams acceptable).  NIL may
be returned for this value either when waiting timed out or when
it was interrupted (EINTR).  The second value is a real number
indicating the time remaining within the timeout period or NIL if
none."
  (unless (wait-list-p socket-or-sockets)
    (let ((wl (make-wait-list (if (listp socket-or-sockets)
                                  socket-or-sockets (list socket-or-sockets)))))
      (multiple-value-bind
            (socks to)
          (wait-for-input wl :timeout timeout :ready-only ready-only)
        (return-from wait-for-input
          (values (if ready-only socks socket-or-sockets) to)))))
  (let* ((start (get-internal-real-time))
         (sockets-ready 0))
    (dolist (x (wait-list-waiters socket-or-sockets))
      (when (setf (state x)
                  (if (and (stream-usocket-p x)
                           (listen (socket-stream x)))
                      :READ NIL))
        (incf sockets-ready)))
         ;; the internal routine is responsibe for
         ;; making sure the wait doesn't block on socket-streams of
         ;; which theready- socket isn't ready, but there's space left in the
         ;; buffer
    (wait-for-input-internal socket-or-sockets
                             :timeout (if (zerop sockets-ready) timeout 0))
    (let ((to-result (when timeout
                       (let ((elapsed (/ (- (get-internal-real-time) start)
                                         internal-time-units-per-second)))
                         (when (< elapsed timeout)
                           (- timeout elapsed))))))
      (values (if ready-only
                  (remove-if #'null (wait-list-waiters socket-or-sockets) :key #'state)
                  socket-or-sockets)
              to-result))))

;;
;; Data utility functions
;;

(defun integer-to-octet-buffer (integer buffer octets &key (start 0))
  (do ((b start (1+ b))
       (i (ash (1- octets) 3) ;; * 8
          (- i 8)))
      ((> 0 i) buffer)
    (setf (aref buffer b)
          (ldb (byte 8 i) integer))))

(defun octet-buffer-to-integer (buffer octets &key (start 0))
  (let ((integer 0))
    (do ((b start (1+ b))
         (i (ash (1- octets) 3) ;; * 8
            (- i 8)))
        ((> 0 i)
         integer)
      (setf (ldb (byte 8 i) integer)
            (aref buffer b)))))


(defmacro port-to-octet-buffer (port buffer &key (start 0))
  `(integer-to-octet-buffer ,port ,buffer 2 ,start))

(defmacro ip-to-octet-buffer (ip buffer &key (start 0))
  `(integer-to-octet-buffer (host-byte-order ,ip) ,buffer 4 ,start))

(defmacro port-from-octet-buffer (buffer &key (start 0))
  `(octet-buffer-to-integer ,buffer 2 ,start))

(defmacro ip-from-octet-buffer (buffer &key (start 0))
  `(octet-buffer-to-integer ,buffer 4 ,start))

;;
;; IP(v4) utility functions
;;

(defun list-of-strings-to-integers (list)
  "Take a list of strings and return a new list of integers (from
parse-integer) on each of the string elements."
  (let ((new-list nil))
    (dolist (element (reverse list))
      (push (parse-integer element) new-list))
    new-list))

(defun hbo-to-dotted-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (format nil "~A.~A.~A.~A" first second third fourth)))

(defun hbo-to-vector-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (vector first second third fourth)))

(defun vector-quad-to-dotted-quad (vector)
  (format nil "~A.~A.~A.~A"
          (aref vector 0)
          (aref vector 1)
          (aref vector 2)
          (aref vector 3)))

(defun dotted-quad-to-vector-quad (string)
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (vector (first list) (second list) (third list) (fourth list))))

(defgeneric host-byte-order (address))
(defmethod host-byte-order ((string string))
  "Convert a string, such as 192.168.1.1, to host-byte-order,
such as 3232235777."
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (+ (* (first list) 256 256 256) (* (second list) 256 256)
       (* (third list) 256) (fourth list))))

(defmethod host-byte-order ((vector vector))
  "Convert a vector, such as #(192 168 1 1), to host-byte-order, such as
3232235777."
  (+ (* (aref vector 0) 256 256 256) (* (aref vector 1) 256 256)
     (* (aref vector 2) 256) (aref vector 3)))

(defmethod host-byte-order ((int integer))
  int)

(defun host-to-hostname (host)
  "Translate a string or vector quad to a stringified hostname."
  (etypecase host
    (string host)
    ((or (vector t 4)
         (array (unsigned-byte 8) (4)))
     (vector-quad-to-dotted-quad host))
    (integer (hbo-to-dotted-quad host))))

(defun ip= (ip1 ip2)
  (etypecase ip1
    (string (string= ip1 (host-to-hostname ip2)))
    ((or (vector t 4)
         (array (unsigned-byte 8) (4)))
     (or (eq ip1 ip2)
         (and (= (aref ip1 0) (aref ip2 0))
              (= (aref ip1 1) (aref ip2 1))
              (= (aref ip1 2) (aref ip2 2))
              (= (aref ip1 3) (aref ip2 3)))))
    (integer (= ip1 (host-byte-order ip2)))))

(defun ip/= (ip1 ip2)
  (not (ip= ip1 ip2)))

;;
;; DNS helper functions
;;

#-(or clisp armedbear)
(progn
  (defun get-host-by-name (name)
    (let ((hosts (get-hosts-by-name name)))
      (car hosts)))

  (defun get-random-host-by-name (name)
    (let ((hosts (get-hosts-by-name name)))
      (when hosts
        (elt hosts (random (length hosts))))))

  (defun host-to-vector-quad (host)
    "Translate a host specification (vector quad, dotted quad or domain name)
to a vector quad."
    (etypecase host
      (string (let* ((ip (ignore-errors
                           (dotted-quad-to-vector-quad host))))
                (if (and ip (= 4 (length ip)))
                    ;; valid IP dotted quad?
                    ip
                  (get-random-host-by-name host))))
      ((or (vector t 4)
           (array (unsigned-byte 8) (4)))
       host)
      (integer (hbo-to-vector-quad host))))

  (defun host-to-hbo (host)
    (etypecase host
      (string (let ((ip (ignore-errors
                          (dotted-quad-to-vector-quad host))))
                (if (and ip (= 4 (length ip)))
                    (host-byte-order ip)
            (host-to-hbo (get-host-by-name host)))))
      ((or (vector t 4)
           (array (unsigned-byte 8) (4)))
       (host-byte-order host))
      (integer host))))

;;
;; Other utility functions
;;

(defun split-timeout (timeout &optional (fractional 1000000))
  "Split real value timeout into seconds and microseconds.
Optionally, a different fractional part can be specified."
  (multiple-value-bind
      (secs sec-frac)
      (truncate timeout 1)
    (values secs
            (truncate (* fractional sec-frac) 1))))




;;
;; Setting of documentation for backend defined functions
;;

;; Documentation for the function
;;
;; (defun SOCKET-CONNECT (host port &key element-type) ..)
;;
(setf (documentation 'socket-connect 'function)
      "Connect to `host' on `port'.  `host' is assumed to be a string or
an IP address represented in vector notation, such as #(192 168 1 1).
`port' is assumed to be an integer.

`element-type' specifies the element type to use when constructing the
stream associated with the socket.  The default is 'character.

Returns a usocket object.")

;; Documentation for the function
;;
;; (defun SOCKET-LISTEN (host port &key reuseaddress backlog element-type) ..)
;;###FIXME: extend with default-element-type
(setf (documentation 'socket-listen 'function)
      "Bind to interface `host' on `port'. `host' should be the
representation of an ready-interface address.  The implementation is not
required to do an address lookup, making no guarantees that hostnames
will be correctly resolved.  If `*wildcard-host*' is passed for `host',
the socket will be bound to all available interfaces for the IPv4
protocol in the system.  `port' can be selected by the IP stack by
passing `*auto-port*'.

Returns an object of type `stream-server-usocket'.

`reuse-address' and `backlog' are advisory parameters for setting socket
options at creation time. `element-type' is the element type of the
streams to be created by `socket-accept'.  `reuseaddress' is supported for
backward compatibility (but deprecated); when both `reuseaddress' and
`reuse-address' have been specified, the latter takes precedence.
")
