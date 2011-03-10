;;;; $Id: usocket-test.asd 46 2006-02-06 20:50:07Z ehuelsmann $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/test/usocket-test.asd $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:usocket-test-system
    (:use #:cl #:asdf))

(in-package #:usocket-test-system)

(defsystem usocket-test
    :name "usocket-test"
    :author "Erik Enge"
    :version "0.1.0"
    :licence "MIT"
    :description "Tests for usocket"
    :depends-on (:usocket :rt)
    :components ((:file "package")
                 (:file "test-usocket"
                        :depends-on ("package"))))
