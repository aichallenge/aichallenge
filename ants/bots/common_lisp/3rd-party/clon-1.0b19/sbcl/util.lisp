;;; util.lisp --- SBCL specific utilities

;; Copyright (C) 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

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


;;; Code:

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


(defun sbcl/stream-line-width (stream)
  "SBCL specific version of STREAM-LINE-WIDTH.
This function relies on SB-GROVEL."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (handler-case
      (with-winsize winsize ()
	(sb-posix:ioctl (stream-file-stream stream :output)
			+tiocgwinsz+
			winsize)
	(winsize-ws-col winsize))
    (sb-posix:syscall-error (error)
      (unless (= (sb-posix:syscall-errno error) sb-posix:enotty)
	(values nil error)))))


;;; util.lisp ends here
