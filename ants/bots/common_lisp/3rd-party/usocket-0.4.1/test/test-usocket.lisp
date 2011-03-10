;;;; $Id: test-usocket.lisp 228 2007-04-08 21:56:25Z ehuelsmann $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/test/test-usocket.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket-test)

;; The parameters below may need adjustments to match the system
;; the tests are run on.
(defparameter +non-existing-host+ "192.168.1.1")
(defparameter +unused-local-port+ 15213)
(defparameter *soc1* (usocket::make-stream-socket :socket :my-socket
                                                  :stream :my-stream))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +common-lisp-net+ #(80 68 86 115))) ;; common-lisp.net IP

(defmacro with-caught-conditions ((expect throw) &body body)
  `(catch 'caught-error
     (handler-case
      (progn ,@body)
      (usocket:unknown-error (c) (if (typep c ,expect)
                                     (throw 'caught-error ,throw)
                                   (progn
                                     (describe c)
                                     (describe
                                      (usocket::usocket-real-error c))
                                     c)))
      (error (c) (if (typep c ,expect)
                     (throw 'caught-error ,throw)
                   (progn
                     (describe c)
                     c)))
      (usocket:unknown-condition (c) (if (typep c ,expect)
                                         (throw 'caught-error ,throw)
                                       (progn
                                         (describe c)
                                         (describe
                                          (usocket::usocket-real-condition c))
                                         c)))
      (condition (c) (if (typep c ,expect)
                         (throw 'caught-error ,throw)
                       (progn
                         (describe c)
                         c))))))

(deftest make-socket.1 (usocket:socket *soc1*) :my-socket)
(deftest make-socket.2 (usocket:socket-stream *soc1*) :my-stream)

(deftest socket-no-connect.1
  (with-caught-conditions ('usocket:socket-error nil)
      (usocket:socket-connect "127.0.0.0" +unused-local-port+)
      t)
  nil)
(deftest socket-no-connect.2
  (with-caught-conditions ('usocket:socket-error nil)
    (usocket:socket-connect #(127 0 0 0) +unused-local-port+)
    t)
  nil)
(deftest socket-no-connect.3
  (with-caught-conditions ('usocket:socket-error nil)
    (usocket:socket-connect 2130706432 +unused-local-port+) ;; == #(127 0 0 0)
    t)
  nil)

(deftest socket-failure.1
  (with-caught-conditions (#-(or cmu lispworks armedbear openmcl)
                             'usocket:network-unreachable-error
                           #+(or cmu lispworks armedbear)
                             'usocket:unknown-error
                           #+openmcl
                             'usocket:timeout-error
                           nil)
    (usocket:socket-connect 2130706432 +unused-local-port+) ;; == #(127 0 0 0)
    :unreach)
  nil)
(deftest socket-failure.2
  (with-caught-conditions (#+(or lispworks armedbear)
                             'usocket:unknown-error
                           #+cmu
                             'usocket:network-unreachable-error
                           #+openmcl
                             'usocket:timeout-error
                           #-(or lispworks armedbear cmu openmcl)
                             'usocket:host-unreachable-error
                           nil)
      (usocket:socket-connect +non-existing-host+ 80) ;; 80 = just a port
      :unreach)
  nil)


;; let's hope c-l.net doesn't move soon, or that people start to
;; test usocket like crazy..
(deftest socket-connect.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
      (unwind-protect
          (typep sock 'usocket:usocket)
        (usocket:socket-close sock))))
  t)
(deftest socket-connect.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (typep sock 'usocket:usocket)
        (usocket:socket-close sock))))
  t)
(deftest socket-connect.3
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect (usocket::host-byte-order +common-lisp-net+) 80)))
      (unwind-protect
          (typep sock 'usocket:usocket)
        (usocket:socket-close sock))))
  t)

;; let's hope c-l.net doesn't change its software any time soon
(deftest socket-stream.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect "common-lisp.net" 80)))
      (unwind-protect
          (progn
            (format (usocket:socket-stream sock)
                    "GET / HTTP/1.0~A~A~A~A"
                    #\Return #\Newline #\Return #\Newline)
            (force-output (usocket:socket-stream sock))
            (read-line (usocket:socket-stream sock)))
        (usocket:socket-close sock))))
  #+clisp "HTTP/1.1 200 OK"
  #-clisp #.(format nil "HTTP/1.1 200 OK~A" #\Return) nil)

(deftest socket-name.1
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-peer-address sock)
        (usocket:socket-close sock))))
  #.+common-lisp-net+)
(deftest socket-name.2
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-peer-port sock)
        (usocket:socket-close sock))))
  80)
(deftest socket-name.3
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-peer-name sock)
        (usocket:socket-close sock))))
  #.+common-lisp-net+ 80)
(deftest socket-name.4
  (with-caught-conditions (nil nil)
    (let ((sock (usocket:socket-connect +common-lisp-net+ 80)))
      (unwind-protect
          (usocket::get-local-address sock)
        (usocket:socket-close sock))))
  #(192 168 1 65))


(defun run-usocket-tests ()
  (do-tests))
