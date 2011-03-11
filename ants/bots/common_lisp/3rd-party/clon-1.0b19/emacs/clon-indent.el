;;; clon-indent.el --- cl-indent additions

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Keywords:      extensions, lisp, data

;; This file is part of Clon.

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

(dolist (symbol '(select-keys remove-keys replace-keys
		  add-to
		  convert-value convert-environment
		  push-cmdline-option push-unknown-option
		  make-face))
  (put symbol 'common-lisp-indent-function 1))

(dolist (symbol '(with-winsize
		  stream-file-stream
		  make-internal-flag
		  make-internal-switch
		  make-internal-stropt
		  make-internal-lispobj
		  make-internal-path
		  make-internal-enum
		  make-internal-xswitch))
  (put symbol 'common-lisp-indent-function 2))

(dolist (symbol '(push-retrieved-option replace-in-keys))
  (put symbol 'common-lisp-indent-function 3))


;;; clon-indent.el ends here
