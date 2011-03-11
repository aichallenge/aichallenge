;;; -*- Lisp -*- mode
(defpackage #:split-sequence-system (:use #:cl #:asdf))
(in-package :split-sequence-system)

(defsystem :split-sequence
    :version "20011114.1"
    :components ((:file "split-sequence")))
