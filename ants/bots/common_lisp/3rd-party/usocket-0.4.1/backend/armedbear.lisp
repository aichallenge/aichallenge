;;;; $Id: armedbear.lisp 453 2008-10-22 07:21:47Z ctian $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/backend/armedbear.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)


;;;;; Proposed contribution to the JAVA package

(defpackage :jdi
  (:use :cl)
  (:export #:jcoerce
           #:jop-deref
           #:do-jmethod-call
           #:do-jmethod
           #:do-jstatic-call
           #:do-jstatic
           #:do-jnew-call
           #:do-jfield
           #:jequals))
;; but still requires the :java package.

(in-package :jdi)

(defstruct (java-object-proxy (:conc-name :jop-)
                              :copier)
  value
  class)

(defvar *jm-get-return-type*
  (java:jmethod "java.lang.reflect.Method" "getReturnType"))

(defvar *jf-get-type*
  (java:jmethod "java.lang.reflect.Field" "getType"))

(defvar *jc-get-declaring-class*
  (java:jmethod "java.lang.reflect.Constructor" "getDeclaringClass"))

(declaim (inline make-return-type-proxy))
(defun make-return-type-proxy (jmethod jreturned-value)
  (if (java:java-object-p jreturned-value)
      (let ((rt (java:jcall *jm-get-return-type* jmethod)))
        (make-java-object-proxy :value jreturned-value
                                :class rt))
    jreturned-value))

(defun make-field-type-proxy (jfield jreturned-value)
  (if (java:java-object-p jreturned-value)
      (let ((rt (java:jcall *jf-get-type* jfield)))
        (make-java-object-proxy :value jreturned-value
                                :class rt))
    jreturned-value))

(defun make-constructor-type-proxy (jconstructor jreturned-value)
  (if (java:java-object-p jreturned-value)
      (let ((rt (java:jcall *jc-get-declaring-class* jconstructor)))
        (make-java-object-proxy :value jreturned-value
                                :class rt))
    jreturned-value))

(defun jcoerce (instance &optional output-type-spec)
  (cond
   ((java-object-proxy-p instance)
    (let ((new-instance (copy-structure (the java-object-proxy instance))))
      (setf (jop-class new-instance)
            (java:jclass output-type-spec))
      new-instance))
   ((java:java-object-p instance)
    (make-java-object-proxy :class (java:jclass output-type-spec)
                            :value instance))
   ((stringp instance)
    (make-java-object-proxy :class "java.lang.String"
                            :value instance))
   ((keywordp output-type-spec)
    ;; all that remains is creating an immediate type...
    (let ((jval (java:make-immediate-object instance output-type-spec)))
      (make-java-object-proxy :class output-type-spec
                              :value jval)))
   ))

(defun jtype-of (instance) ;;instance must be a jop
  (cond
   ((stringp instance)
    "java.lang.String")
   ((keywordp (jop-class instance))
    (string-downcase (symbol-name (jop-class instance))))
   (t
    (java:jclass-name (jop-class instance)))))

(declaim (inline jop-deref))
(defun jop-deref (instance)
  (if (java-object-proxy-p instance)
      (jop-value instance)
    instance))

(defun java-value-and-class (object)
  (values (jop-deref object)
          (jtype-of object)))

(defun do-jmethod-call (object method-name &rest arguments)
  (multiple-value-bind
      (instance class-name)
      (java-value-and-class object)
    (let* ((argument-types (mapcar #'jtype-of arguments))
           (jm (apply #'java:jmethod class-name method-name argument-types))
           (rv (apply #'java:jcall jm instance
                      (mapcar #'jop-deref arguments))))
      (make-return-type-proxy jm rv))))

(defun do-jstatic-call (class-name method-name &rest arguments)
  (let* ((argument-types (mapcar #'jtype-of arguments))
         (jm (apply #'java:jmethod class-name method-name argument-types))
         (rv (apply #'java:jstatic jm (java:jclass class-name)
                    (mapcar #'jop-deref arguments))))
    (make-return-type-proxy jm rv)))

(defun do-jnew-call (class-name &rest arguments)
  (let* ((argument-types (mapcar #'jtype-of arguments))
         (jm (apply #'java:jconstructor class-name argument-types))
         (rv (apply #'java:jnew jm (mapcar #'jop-deref arguments))))
    (make-constructor-type-proxy jm rv)))

(defun do-jfield (class-or-instance-or-name field-name)
  (let* ((class (cond
                 ((stringp class-or-instance-or-name)
                  (java:jclass class-or-instance-or-name))
                 ((java:java-object-p class-or-instance-or-name)
                  (java:jclass-of class-or-instance-or-name))
                 ((java-object-proxy-p class-or-instance-or-name)
                  (java:jclass (jtype-of class-or-instance-or-name)))))
         (jf (java:jcall (java:jmethod "java.lang.Class" "getField"
                                       "java.lang.String")
                         class field-name)))
    (make-field-type-proxy jf
                           (java:jfield class field-name)))) ;;class))))

(defmacro do-jstatic (&rest arguments)
  `(do-jstatic-call ,@arguments))

(defmacro do-jmethod (&rest arguments)
  `(do-jmethod-call ,@arguments))

;;

(defmacro jstatic-call (class-name (method-name &rest arg-spec)
                                   &rest args)
  (let ((class-sym (gensym)))
    `(let ((,class-sym ,class-name))
       (java:jstatic
        (java:jmethod ,class-sym ,method-name ,@arg-spec)
        (java:jclass ,class-sym) ,@args))))

(defmacro jmethod-call (instance-and-class (method &rest arg-spec) &rest args)
  (let ((isym (gensym)))
    (multiple-value-bind
        (instance class-name)
        (if (listp instance-and-class)
            (values (first instance-and-class)
                    (second instance-and-class))
          (values instance-and-class))
      (when (null class-name)
        (setf class-name `(java:jclass-name (java:jclass-of ,isym))))
      `(let* ((,isym ,instance))
         (java:jcall (java:jmethod ,class-name ,method ,@arg-spec)
                     ,isym ,@args)))))

(defun jequals (x y)
  (do-jmethod-call (jcoerce x "java.lang.Object") "equals"
                   (jcoerce y "java.lang.Object")))

(defmacro jnew-call ((class &rest arg-spec) &rest args)
  `(java:jnew (java:jconstructor ,class ,@arg-spec)
         ,@args))



(in-package :usocket)

(defun get-host-name ()
  (jdi:do-jmethod-call (jdi:do-jstatic-call "java.net.InetAddress"
                                            "getLocalHost")
                       "getHostName"))

(defun handle-condition (condition &optional socket)
  (typecase condition
    (error (error 'unknown-error :socket socket :real-error condition))))

(defun socket-connect (host port &key (element-type 'character)
                       timeout deadline (nodelay nil nodelay-specified)
                       local-host local-port)
  (when deadline (unsupported 'deadline 'socket-connect))
  (when local-host (unimplemented 'local-host 'socket-connect))
  (when local-port (unimplemented 'local-port 'socket-connect))

  (let ((usock))
    (with-mapped-conditions (usock)
      (let* ((sock-addr (jdi:jcoerce
                         (jdi:do-jnew-call "java.net.InetSocketAddress"
                           (host-to-hostname host)
                           (jdi:jcoerce port :int))
                         "java.net.SocketAddress"))
             (jchan (jdi:do-jstatic-call "java.nio.channels.SocketChannel"
                      "open" sock-addr))
             (sock (jdi:do-jmethod-call jchan "socket")))
        (when nodelay-specified
          (jdi:do-jmethod-call sock "setTcpNoDelay"
                               (if nodelay
                                   (java:make-immediate-object t :boolean)
                                   (java:make-immediate-object nil :boolean))))
        (when timeout
          (jdi:do-jmethod-call sock "setSoTimeout"
                                    (truncate (* 1000 timeout))))
        (setf usock
              (make-stream-socket
               :socket jchan
               :stream (ext:get-socket-stream (jdi:jop-deref sock)
                                              :element-type element-type)))))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (sock-addr (jdi:do-jnew-call "java.net.InetSocketAddress"
                                      (host-to-hostname host)
                                      (jdi:jcoerce port :int)))
         (chan (jdi:do-jstatic-call "java.nio.channels.ServerSocketChannel"
                                    "open"))
         (sock (jdi:do-jmethod-call chan "socket")))
    (when reuseaddress
      (with-mapped-conditions ()
        (jdi:do-jmethod-call sock
                             "setReuseAddress"
                             (jdi:jcoerce reuseaddress :boolean))))
    (with-mapped-conditions ()
      (jdi:do-jmethod-call sock
                           "bind"
                           (jdi:jcoerce sock-addr
                                        "java.net.SocketAddress")
                           (jdi:jcoerce backlog :int)))
    (make-stream-server-socket chan :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (let* ((jsock (socket socket))
         (jacc-chan (with-mapped-conditions (socket)
                       (jdi:do-jmethod-call jsock "accept")))
         (jacc-stream
          (ext:get-socket-stream (jdi:jop-deref
                                  (jdi:do-jmethod-call jacc-chan "socket"))
                                 :element-type (or element-type
                                                   (element-type socket)))))
    (make-stream-socket :socket jacc-chan
                        :stream jacc-stream)))

;;(defun print-java-exception (e)
;;  (let* ((native-exception (java-exception-cause e)))
;;    (print (jcall (jmethod "java.net.BindException" "getMessage") native-exception))))

(defmethod socket-close ((usocket usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (jdi:do-jmethod (socket usocket) "close")))

;; Socket streams are different objects than
;; socket streams. Closing the stream flushes
;; its buffers *and* closes the socket.
(defmethod socket-close ((usocket stream-usocket))
  (when (wait-list usocket)
     (remove-waiter (wait-list usocket) usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod get-local-address ((usocket usocket))
  (dotted-quad-to-vector-quad (ext:socket-local-address
                               (jdi:jop-deref
                                (jdi:do-jmethod-call (socket usocket)
                                  "socket")))))

(defmethod get-peer-address ((usocket stream-usocket))
  (dotted-quad-to-vector-quad (ext:socket-peer-address
                               (jdi:jop-deref
                                (jdi:do-jmethod-call (socket usocket)
                                  "socket")))))

(defmethod get-local-port ((usocket usocket))
  (ext:socket-local-port (jdi:jop-deref
                          (jdi:do-jmethod-call (socket usocket) "socket"))))

(defmethod get-peer-port ((usocket stream-usocket))
  (ext:socket-peer-port (jdi:jop-deref
                         (jdi:do-jmethod-call (socket usocket) "socket"))))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))


#|
Pseudo code version of what we're trying to do:

We're being called with 2 args:

  - sockets (list)
  - timeout (non-negative real)

Selector := java.nio.channels.Selector.open()

For all usockets
  get the java socket
  get its channel
  register the channel with the selector
    with ops (operations) OP_READ and OP_ACCEPT

make the selector wait trunc(timeout*1000) miliseconds,
  unless (null timeout), because then:
  selectNow()

retrieve the selectedKeys() set from the selector
  unless select() returned 0 selected keys.

for set-iterator.hasNextKey()
  with that key
    retrieve the channel
    retrieve the channel's socket
    add the retrieved socket to the list of ready sockets

for all usockets
  check if the associated java object
    is in the list of ready sockets
  it is? add it to the function result list

close() the selector

return the function result list.

|#

(defun op-read ()
  (jdi:do-jfield "java.nio.channels.SelectionKey"
                 "OP_READ"))

(defun op-accept ()
  (jdi:do-jfield "java.nio.channels.SelectionKey"
                 "OP_ACCEPT"))

(defun op-connect ()
  (jdi:do-jfield "java.nio.channels.SelectionKey"
                 "OP_CONNECT"))

(defun valid-ops (jchannel)
  (jdi:do-jmethod-call jchannel "validOps"))

(defun channel-class (jchannel)
  (let ((valid-ops (valid-ops jchannel)))
    (cond ((/= 0 (logand valid-ops (op-connect)))
           "java.nio.channels.SocketChannel")
          ((/= 0 (logand valid-ops (op-accept)))
           "java.nio.channels.ServerSocketChannel")
          (t
           "java.nio.channels.DatagramChannel"))))

(defun socket-channel-class (socket)
  (cond
   ((stream-usocket-p socket)
    "java.nio.channels.SocketChannel")
   ((stream-server-usocket-p socket)
    "java.nio.channels.ServerSocketChannel")
   (t
    "java.nio.channels.DatagramChannel")))

(defun wait-for-input-internal (wait-list &key timeout)
  (let* ((sockets (wait-list-waiters wait-list))
         (ops (logior (op-read) (op-accept)))
         (selector (jdi:do-jstatic "java.nio.channels.Selector" "open"))
         (channels (mapcar #'socket sockets)))
    (unwind-protect
        (with-mapped-conditions ()
          (let ((sel (jdi:jop-deref selector)))
            (dolist (channel channels)
              (let ((chan (jdi:jop-deref channel)))
                (java:jcall (java:jmethod "java.nio.channels.SelectableChannel"
                                          "configureBlocking"
                                          "boolean")
                            chan (java:make-immediate-object nil :boolean))
                (java:jcall (java:jmethod "java.nio.channels.SelectableChannel"
                                          "register"
                                          "java.nio.channels.Selector" "int")
                            chan sel (logand ops (valid-ops channel)))))
            (let ((ready-count
                   (java:jcall (java:jmethod "java.nio.channels.Selector"
                                             "select"
                                             "long")
                               sel (truncate (* timeout 1000)))))
              (when (< 0 ready-count)
                ;; we actually have work to do
                (let* ((selkeys (jdi:do-jmethod selector "selectedKeys"))
                       (selkey-iterator (jdi:do-jmethod selkeys "iterator"))
                       (%wait (wait-list-%wait wait-list)))
                  (loop while (java:jcall
                               (java:jmethod "java.util.Iterator" "hasNext")
                               (jdi:jop-deref selkey-iterator))
                        do (let* ((key (jdi:jcoerce
                                        (jdi:do-jmethod selkey-iterator "next")
                                        "java.nio.channels.SelectionKey"))
                                  (chan (jdi:jop-deref
                                         (jdi:do-jmethod key "channel"))))
                             (setf (state (gethash chan %wait))
                                   :READ))))))))
      ;; close the selector: all keys will be deregistered
      (java:jcall (java:jmethod "java.nio.channels.Selector" "close")
                  (jdi:jop-deref selector))
      ;; make all sockets blocking again.
     (dolist (channel channels)
       (java:jcall (java:jmethod "java.nio.channels.SelectableChannel"
                                 "configureBlocking"
                                 "boolean")
                      (jdi:jop-deref channel)
                      (java:make-immediate-object t :boolean))))))


;;
;;
;;
;; The WAIT-LIST part
;;

;;
;; Note that even though Java has the concept of the Selector class, which
;; remotely looks like a wait-list, it requires the sockets to be non-blocking.
;; usocket however doesn't make any such guarantees and is therefore unable to
;; use the concept outside of the waiting routine itself (blergh!).
;;

(defun %setup-wait-list (wl)
  (setf (wait-list-%wait wl)
        (make-hash-table :test #'equal :rehash-size 1.3d0)))

(defun %add-waiter (wl w)
  (setf (gethash (jdi:jop-deref (socket w)) (wait-list-%wait wl))
        w))

(defun %remove-waiter (wl w)
  (remhash (socket w) (wait-list-%wait wl)))