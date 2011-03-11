;;; com.dvlsoft.clon.asd --- ASDF system definition

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  #+sbcl  (require :sb-grovel)
  #+clisp (handler-case (asdf:operate 'asdf:load-op :cffi-grovel)
	    (asdf:missing-component ()
	      (format *error-output* "~
*********************************************************************
* WARNING: ASDF component CFFI-GROVEL not found.                    *
* Clon will be loaded without support for terminal autodetection.   *
* See section A.1 of the user manual for more information.          *
*********************************************************************"))))

(defpackage :com.dvlsoft.clon.asdf
  (:documentation "The Command-Line Options Nuker package for ASDF.")
  (:use :cl)
  (:export :define-constant
	   :+release-major-level+
	   :+release-minor-level+
	   :+release-status+ :+release-status-level+
	   :+release-name+
	   :version))


(in-package :com.dvlsoft.clon.asdf)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

(defconstant +release-major-level+ 1
  "The major level of this release.")

(defconstant +release-minor-level+ 0
  "The minor level of this release.")

(defconstant +release-status+ :beta
  "The status of this release.")

(defconstant +release-status-level+ 19
  "The status level of this release.")

(define-constant +release-name+ "Michael Brecker"
  "The name of this release.")

;; #### TODO: I'm sure the format strings can be improved
(defun %version (type major minor status level name)
  (ecase type
    (:number
     (apply #'+
       (* major 10000)
       (* minor 100)
       (when (eq status :patchlevel)
	 (list level))))
    (:short
     (format nil "~S.~S~
		 ~[~
		   a~*~S~;~
		   b~*~S~;~
		   rc~*~S~;~
		   ~:[.~S~;~*~]~
		 ~]"
       major
       minor
       (ecase status
	 (:alpha 0)
	 (:beta 1)
	 (:rc 2)
	 (:patchlevel 3))
       (zerop level)
       level))
    (:long
     (format nil "~S.~S ~
		 ~[~
		   alpha ~*~S ~;~
		   beta ~*~S ~;~
		   release candidate ~*~S ~;~
		   ~:[patchlevel ~S ~;~*~]~
		 ~]~
		 ~S"
       major
       minor
       (ecase status
	 (:alpha 0)
	 (:beta 1)
	 (:rc 2)
	 (:patchlevel 3))
       (zerop level)
       level
       name))))

(defun version (&optional (type :number))
  "Return the current version of Clon.
TYPE can be one of :number, :short or :long.

A version number is computed as major*10000 + minor*100 + patchlevel, leaving
two digits for each level. Alpha, beta and rc status are ignored in version
numbers.

A short version is something like 1.3{a,b,rc}4, or 1.3.4 for patchlevel.
Alpha, beta or rc levels start at 1. Patchlevels start at 0 but are ignored
in the output, so that 1.3.0 appears as just 1.3.

A long version is something like
1.3 {alpha,beta,release candidate,patchlevel} 4 \"Michael Brecker\". As for
the short version, a patchlevel of 0 is ignored in the output."
  (%version type +release-major-level+ +release-minor-level+
	    +release-status+ +release-status-level+
	    +release-name+))

(asdf:defsystem :com.dvlsoft.clon
  :description "The Command-Line Options Nuker."
  :long-description "Clon is a library for command-line option management.
It is intended to ease the creation of standalone Common Lisp applications by
providing a powerful and uniform command-line option interface.
The most important features of Clon are:
- [from the programmer's point of view] Centralized command-line options
  specification and management, including automatic generation of help
  strings, conversion from command-line / environment strings to
  application-level option values, global or on-demand option retrieval, and
  extensibility (the programmer can define his own option types).
- [from the end-user's point of view] Uniform command-line option syntax
  across Clonified applications, including customization of the help strings
  layout (with optional ISO6429 coloring on terminals that support it),
  possibly abbreviated option calls and short/long syntax."
  :author "Didier Verna <didier@lrde.epita.fr>"
  :maintainer "Didier Verna <didier@lrde.epita.fr>"
  :license "BSD style"
  :version #.(version :long)
  :depends-on (#+sbcl             :sb-posix
	       #+(and clisp cffi) :cffi)
  :components ((:file "package")
	       #+sbcl
	       (:module "sbcl"
		 :depends-on ("package")
		 :serial t
		 :components ((sb-grovel:grovel-constants-file
			       "constants" :package :com.dvlsoft.clon)
			      (:file "util")))

	       #+(and clisp cffi)
	       (:module "clisp"
		 :depends-on ("package")
		 :serial t
		 :components ((cffi-grovel:grovel-file "constants")
			      (:file "util")))
	       (module "src"
		 :depends-on (#+sbcl "sbcl"
			      #+(and clisp cffi) "clisp"
			      "package")
		 :components ((:file "util")
			      (:file "item" :depends-on ("util"))
			      (:file "text" :depends-on ("item"))
			      (:module "options"
				:depends-on ("text")
				:components ((:file "option")
					     (:file "flag"
						    :depends-on ("option"))
					     (:file "valued"
						    :depends-on ("option"))
					     (:file "negatable"
						    :depends-on ("valued"))
					     (:file "switch-base"
						    :depends-on ("negatable"))
					     (:file "switch"
						    :depends-on
						    ("switch-base"))
					     (:file "stropt"
						    :depends-on ("valued"))
					     (:file "lispobj"
						    :depends-on ("valued"))
					     (:file "path"
						    :depends-on ("valued"))
					     (:file "enum-base")
					     (:file "enum"
						    :depends-on
						    ("valued" "enum-base"))
					     (:file
					      "xswitch"
					      :depends-on ("valued"
							   "switch-base"
							   "enum-base"))))
			      (:file "container" :depends-on ("options"))
			      (:file "group" :depends-on ("container"))
			      (:module "retrieval"
				:depends-on ("options")
				:components ((:file "cmdline")
					     (:file "environ")))
			      (:file "synopsis" :depends-on ("group"))
			      (:module "output"
				:depends-on ("synopsis" "retrieval")
				:components ((:file "face")
					     (:file "sheet"
						    :depends-on ("face"))))
			      (:file "context" :depends-on ("output"))))))


;;; com.dvlsoft.clon.asd ends here
