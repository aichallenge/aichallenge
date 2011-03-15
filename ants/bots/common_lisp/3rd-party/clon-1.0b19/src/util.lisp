;;; util.lisp --- General utilities

;; Copyright (C) 2010, 2011 Didier Verna

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; Preamble C code needed for ECL's FD-LINE-WIDTH function.
#+ecl (ffi:clines "
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>")



;; ==========================================================================
;; Miscellaneous Auxiliary Routines
;; ==========================================================================

(defmacro econd (&body clauses)
  "Like COND, but signal an error if no clause evaluates to t."
  `(cond ,@(append clauses
		   '((t (error "Fell out of ECOND clauses."))))))

(defmacro endpush (object place)
  "Like push, but at the end."
  `(setf ,place (nconc ,place (list ,object))))

(defmacro maybe-push (object place)
  "Like push, but only if OBJECT is non-nil."
  (let ((the-object (gensym "object")))
    `(let ((,the-object ,object))
      (when ,the-object (push ,the-object ,place)))))

(defmacro accumulate ((initial-value) &body body)
  "Accumulate BODY forms in a list beginning with INITIAL-VALUE.
INITIAL-VALUE is not evaluated. BODY forms are accumulated only when their
value is non-nil.
If nothing to accumulate, then return nil instead of the list of
INITIAL-VALUE."
  (let ((place (gensym "place"))
	(initial-place (gensym "initial-place")))
    `(let* ((,place (list ',initial-value))
	    (,initial-place ,place))
      ,@(mapcar (lambda (body-form)
		  `(maybe-push ,body-form ,place))
		body)
      (when (not (eq ,initial-place ,place))
	(nreverse ,place)))))

(defun beginning-of-string-p (beginning string &optional ignore-case)
  "Check that STRING starts with BEGINNING.
If IGNORE-CASE, well, ignore case."
  (let ((length (length beginning)))
    (and (>= (length string) length)
	 (funcall (if ignore-case #'string-equal #'string=)
		  beginning string :end2 length))))

(defun closest-match (match list &key ignore-case (key #'identity))
  "Return the LIST element closest to MATCH, or nil.
If IGNORE-CASE, well, ignore case.
KEY should provide a way to get a string from each LIST element."
  (let ((match-length (length match))
	(shortest-distance most-positive-fixnum)
	closest-match)
    (dolist (elt list)
      (let ((elt-string (funcall key elt))
	    distance)
	(when (and (beginning-of-string-p match elt-string ignore-case)
		   (< (setq distance (- (length elt-string) match-length))
		      shortest-distance))
	  (setq shortest-distance distance)
	  (setq closest-match elt))))
    closest-match))

(defun complete-string (beginning complete)
  "Complete BEGINNING with the rest of COMPLETE in parentheses.
For instance, completing 'he' with 'help' will produce 'he(lp)'."
  (assert (beginning-of-string-p beginning complete))
  (assert (not (string= beginning complete)))
  (concatenate 'string beginning "(" (subseq complete (length beginning)) ")"))

(defun list-to-string (list &key (key #'identity) (separator ", "))
  "Return a SEPARATOR-separated string of all LIST elements.
- KEY should provide a way to get a string from each LIST element.
- SEPARATOR is the string to insert between elements."
  (reduce (lambda (str1 str2) (concatenate 'string str1 separator str2))
	  list
	  :key key))



;; ==========================================================================
;; Key-Value Pairs Manipulation
;; ==========================================================================

(defun select-keys (keys &rest selected)
  "Return a new property list from KEYS with only SELECTED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:when (member key selected)
	:nconc (list key val)))

(defun remove-keys (keys &rest removed)
  "Return a new property list from KEYS without REMOVED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:unless (member key removed)
	:nconc (list key val)))

(defmacro replace-in-keys ((key val) keys the-key form)
  "Replace every occurrence of THE-KEY in KEYS with FORM.
At every KEYS round, KEY and VAL are bound to the current key-value pair.
FORM is evaluated each time and should return a key-value list."
  `(loop :for ,key :in ,keys :by #'cddr
    :for ,val :in (cdr ,keys) :by #'cddr
    :if (eql ,key ,the-key)
    :append ,form
    :else
    :nconc (list ,key ,val)))

;; #### NOTE: that's the typical situation where I would like a
;; destructuring-cond, but it seems difficult to do so because of the
;; standard imprecision of the reported error in case of a pattern matching
;; failure.
;; #### NOTE: I could extend this utility by supporting a global :test, or
;; even a per-replacement local one.
(defun replace-key (replacement keys)
  "Return a new property list from KEYS with REPLACEMENT.
REPLACEMENT can take the following forms:
- :KEY
  The effect is to remove :KEY from KEYS, as per REMOVE-KEYS.
- (:KEY :NEW-KEY)
  The effect is to replace :KEY with :NEW-KEY, leaving the values unchanged.
- (:KEY :NEW-KEY (VAL-OR-VALS NEW-VAL)*), with VAL-OR-VALS being
  either a value or a list of values. The effect is to replace :KEY with
  :NEW-KEY and a value matching one of the VAL-OR-VALS with the
  corresponding NEW-VAL. Values not matching any VAL-OR-VALS remain unchanged.
- (:KEY (VAL-OR-VALS :NEW-KEY NEW-VAL...)*), with VAL-OR-VALS as above. The
  effect is the same as above, but :NEW-KEY additionally depends on the
  matched value. If multiple :NEW-KEY NEW-VAL couples are provided, that many
  new keys are inserted along with their values. For values not matching any
  VAL-OR-VALS, :KEY and its value remain unchanged."
  (econd ((symbolp replacement)
	  (remove-keys keys replacement))
	 ((and (consp replacement)
	       (= (length replacement) 2)
	       (symbolp (car replacement))
	       (symbolp (cadr replacement)))
	  (destructuring-bind (old-key new-key) replacement
	    (replace-in-keys (key val) keys old-key
	      (list new-key val))))
	 ((and (consp replacement)
	       (> (length replacement) 2)
	       (symbolp (car replacement))
	       (symbolp (cadr replacement)))
	  (destructuring-bind (old-key new-key &rest replacements) replacement
	    (replace-in-keys (key val) keys old-key
	      (list new-key
		    (let ((match
			   (assoc val replacements
				  :test (lambda (val val-or-vals)
					  (if (consp val-or-vals)
					      (member val val-or-vals)
					      (eql val val-or-vals))))))
		      (if match (cadr match) val))))))
	 ((and (consp replacement)
	       (> (length replacement) 1)
	       (symbolp (car replacement)))
	  (destructuring-bind (old-key &rest replacements) replacement
	    (replace-in-keys (key val) keys old-key
	      (let ((match (assoc val replacements
				  :test (lambda (val val-or-vals)
					  (if (consp val-or-vals)
					      (member val val-or-vals)
					      (eql val val-or-vals))))))
		(if match
		    (cdr match)
		    (list key val))))))))

(defun replace-keys (keys &rest replacements)
  "Return a new property list from KEYS with REPLACEMENTS.
See REPLACE-KEY for more information on the replacement syntax."
  (let ((new-keys keys))
    (dolist (replacement replacements)
      (setq new-keys (replace-key replacement new-keys)))
    new-keys))



;; ==========================================================================
;; CLOS Utility Routines
;; ==========================================================================

;; --------------------
;; Portability wrappers
;; --------------------

(defmacro validate-superclass (class superclass)
  "Validate SUPERCLASS classes for CLASS classes."
  ;; #### PORTME.
  #+abcl (declare (ignore class superclass))
  #+abcl '(progn)
  #-abcl
  `(defmethod #+sbcl  sb-mop:validate-superclass
	      #+cmu   mop:validate-superclass
	      #+ccl   ccl:validate-superclass
	      #+ecl   clos:validate-superclass
	      #+clisp clos:validate-superclass
    ((class ,class) (superclass ,superclass))
    #+ecl (declare (ignore class superclass))
    t))

(defun class-slots (class)
  "Return CLASS slots."
  ;; #### PORTME.
  (#+sbcl  sb-mop:class-slots
   #+cmu   mop:class-slots
   #+ccl   ccl:class-slots
   #+ecl   clos:class-slots
   #+clisp clos:class-slots
   #+abcl  mop:class-slots
   class))

(defun slot-definition-name (slot)
  "Return SLOT's definition name."
  ;; #### PORTME.
  (#+sbcl  sb-mop:slot-definition-name
   #+cmu   mop:slot-definition-name
   #+ccl   ccl:slot-definition-name
   #+ecl   clos:slot-definition-name
   #+clisp clos:slot-definition-name
   #+abcl  mop:slot-definition-name
   slot))


;; ----------------
;; Abstract classes
;; ----------------

(defclass abstract-class (standard-class)
  ()
  (:documentation "The ABSTRACT-CLASS class.
This is the meta-class for abstract classes."))

(defmacro defabstract (class super-classes slots &rest options)
  "Like DEFCLASS, but define an abstract class."
  (when (assoc :metaclass options)
    (error "Defining abstract class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
    (:metaclass abstract-class)))

(defmethod make-instance ((class abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "Instanciating class ~S: is abstract." (class-name class)))

(validate-superclass abstract-class standard-class)
(validate-superclass standard-class abstract-class)


;; ----------------
;; Instance copying
;; ----------------

(defgeneric copy-instance (instance &optional subclass)
  (:documentation "Return a copy of INSTANCE.
Copy is either an object of INSTANCE's class, or INSTANCE's SUBCLASS if given.")
  (:method (instance &optional subclass)
    "Return a copy of INSTANCE.
Both instances share the same slot values."
    (let* ((class (class-of instance))
	   (slots (class-slots class))
	   (new-instance (make-instance (or subclass class))))
      (loop :for slot :in slots
	    :when (slot-boundp instance (slot-definition-name slot))
	    :do (setf (slot-value new-instance (slot-definition-name slot))
		      (slot-value instance (slot-definition-name slot))))
      new-instance)))



;; ==========================================================================
;; System-related utilities
;; ==========================================================================

(defun home-directory ()
  "Return user's home directory in canonical form."
  (truename (user-homedir-pathname)))

(defun macosp ()
  "Return t if running on Mac OS."
  (string= (software-type) "Darwin"))



;; ==========================================================================
;; Wrappers around non ANSI features
;; ==========================================================================

;; Thanks Nikodemus!
(defgeneric stream-file-stream (stream &optional direction)
  (:documentation "Convert STREAM to a file-stream.")
  (:method ((stream file-stream) &optional direction)
    (declare (ignore direction))
    stream)
  (:method ((stream synonym-stream) &optional direction)
    (declare (ignore direction))
    (stream-file-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream) &optional direction)
    (stream-file-stream
	(case direction
	  (:input (two-way-stream-input-stream stream))
	  (:output (two-way-stream-output-stream stream))
	  (otherwise
	   (error "Cannot extract file-stream from TWO-WAY-STREAM ~A:
invalid direction: ~S"
		  stream direction)))
	direction)))

#+ecl
(defun fd-line-width (fd)
  "Get the line width for FD (file descriptor).
Return two values:
- the line width, or -1 if it can't be computed
  (typically when FD does not denote a tty),
- an error message if the operation failed."
  (ffi:c-inline (fd) (:int) (values :int :cstring) "{
    int fd = #0;

    int cols = -1;
    char *msg = NULL;

    struct winsize window;
    if (ioctl (fd, TIOCGWINSZ, &window) == -1)
      {
	if (errno != ENOTTY)
	  msg = strerror (errno);
      }
    else
      cols = (int) window.ws_col;

    @(return 0) = cols;
    @(return 1) = msg;
}"))

;; #### NOTE: SBCL and CLISP have their specific, not "inlined" version of
;; this function elsewhere because they both use a separate ASDF module. The
;; SBCL one depends on SB-GROVEL and the CLISP one depends on CFFI.
(defun stream-line-width (stream)
  "Get STREAM's line width.
Return two values:
- the stream's line width, or nil if it can't be computed
  (typically when the stream does not denote a tty),
- an error message if the operation failed."
  ;; #### NOTE: doing a TIOCGWINSZ ioctl here is a convenient way to both know
  ;; whether we're connected to a tty, and getting the terminal width at the
  ;; same time. In case the ioctl fails, we need to distinguish between and
  ;; ENOTTY error, which simply means that we're not connected to a terminal,
  ;; and the other which are real errors and need to be reported.
  ;; #### PORTME.
  #+abcl (declare (ignore stream))
  #+(and sbcl (not win32)) (sbcl/stream-line-width stream)
  #+(and sbcl win32) nil
  #+cmu
  (locally (declare (optimize (ext:inhibit-warnings 3)))
    (alien:with-alien ((winsize (alien:struct unix:winsize)))
      (multiple-value-bind (success error-number)
	  (unix:unix-ioctl
	   (system:fd-stream-fd (stream-file-stream stream :output))
	   unix:tiocgwinsz
	   winsize)
	(if success
	    (alien:slot winsize 'unix:ws-col)
	  (unless (= error-number unix:enotty)
	    (values nil (unix:get-unix-error-msg error-number)))))))
  #+ccl
  (ccl:rlet ((winsize :winsize))
    (let ((result
	   (ccl::int-errno-call
	    (#_ioctl (ccl::stream-device stream :output)
		     #$TIOCGWINSZ
		     :address winsize))))
      (if (zerop result)
	  (ccl:pref winsize :winsize.ws_col)
	(unless (= result (- #$ENOTTY))
	  (values nil (ccl::%strerror (- result)))))))
  #+ecl
  (multiple-value-bind (cols msg)
      (fd-line-width (ext:file-stream-fd stream))
    (values (unless (= cols -1) cols) msg))
  #+clisp
  (when (fboundp 'clisp/stream-line-width)
    (clisp/stream-line-width stream))
  #+abcl
  nil)

(defun exit (&optional (status 0))
  "Quit the current application with STATUS."
  ;; #### PORTME.
  #+sbcl  (sb-ext:quit :unix-status status)
  #+cmu   (unix:unix-exit status)
  #+ccl   (ccl:quit status)
  #+ecl   (ext:quit status)
  #+clisp (ext:exit status)
  #+abcl  (extensions:exit :status status))

(defun cmdline ()
  "Get the current application's command-line."
  ;; #### PORTME.
  #+sbcl  sb-ext:*posix-argv*
  #+cmu   lisp::lisp-command-line-list
  #+ccl   ccl::*command-line-argument-list*
  #+ecl   (ext:command-args)
  #+clisp (cons (aref (ext:argv) 0) ext:*args*)
  #+abcl  (cons "abcl" extensions:*command-line-argument-list*))

(defun getenv (variable)
  "Get environment VARIABLE's value. VARIABLE may be null."
  ;; #### PORTME.
  (when variable
    (#+sbcl  sb-posix:getenv
     #+cmu   unix:unix-getenv
     #+ccl   ccl:getenv
     #+ecl   ext:getenv
     #+clisp ext:getenv
     #+abcl  extensions:getenv
     variable)))

;; #### NOTE: JAVA doesn't provide a way to set an environment variable. I've
;; seen tricks around to modify the startup environment memory mapping instead
;; of doing a real putenv, but I'll just disable the "modify-environment"
;; restart in environ.lisp for now.
#-abcl
(defun putenv (variable value)
  "Set environment VARIABLE to VALUE."
  ;; #### PORTME.
  #+sbcl  (sb-posix:putenv  (concatenate 'string variable "=" value))
  #+cmu   (unix:unix-putenv (concatenate 'string variable "=" value))
  #+ccl   (ccl:setenv variable value)
  #+ecl   (ext:setenv variable value)
  #+clisp (setf (ext:getenv variable) value))

#+abcl
(defconstant +abcl-main-class-template+
  "import org.armedbear.lisp.*;

public class ~A
{
    public static void main (final String[] argv)
    {
	Runnable r = new Runnable ()
	    {
		public void run()
		{
		    try
			{
			    LispObject cmdline = Lisp.NIL;
			    for (String arg : argv)
				cmdline = new Cons (arg, cmdline);
			    cmdline.nreverse ();
			    Lisp._COMMAND_LINE_ARGUMENT_LIST_.setSymbolValue
				(cmdline);

			    Interpreter.createInstance ();
			    Load.loadSystemFile (\"/~A\", false, false, false);
			}
		    catch (ProcessingTerminated e)
			{
			    System.exit (e.getStatus ());
			}
		}
	    };

	new Thread (null, r, \"interpreter\", 4194304L).start();
    }
}~%"
  "Main class template for ABCL.")

(defmacro dump (name function)
  "Dump a standalone executable named NAME starting with FUNCTION.

Since executable dumping is not available in all supported implementations,
this function behaves differently in some cases, as described below.

- ECL doesn't create executables by dumping a Lisp image, but relies on having
  toplevel code to execute instead, so this macro simply expands to a call to
  FUNCTION.
- ABCL can't dump executables at all because of the underlying Java
  implementation, so this macro expands to just (PROGN) but creates a Java
  class file with a main function that creates an interpreter, loads
  the file in which this macro call appears and calls FUNCTION."
  ;; #### PORTME.
  #+ecl (declare (ignore name))
  #+sbcl  `(sb-ext:save-lisp-and-die ,name
	    :toplevel #',function
	    :executable t
	    :save-runtime-options t)
  #+cmu   `(ext:save-lisp ,name
	    :init-function #',function
	    :executable t
	    :load-init-file nil
	    :site-init nil
	    :print-herald nil
	    :process-command-line nil)
  #+ccl   `(ccl:save-application ,name
	    :toplevel-function #',function
	    :init-file nil
	    :error-handler :quit
	    :prepend-kernel t)
  ;; #### NOTE: ECL works differently: it needs an entry point (i.e. actual
  ;; code to execute) instead of a main function. So we expand DUMP to just
  ;; call that function.
  #+ecl   (list function)
  ;; CLISP's saveinitmem function doesn't quit, so we need to do so here.
  #+clisp `(progn
	    (ext:saveinitmem ,name
	     :init-function #',function
	     :executable 0
	     :quiet t
	     :norc t)
	    (exit))
  #+abcl (if (boundp 'cl-user::com.dvlsoft.clon.dump)
	     (let ((source-pathname (or *compile-file-pathname*
					*load-pathname*))
		   (class-name (copy-seq name)))
	       (setf (aref class-name 0) (char-upcase (aref class-name 0)))
	       (with-open-file
		   (*standard-output*
		    (merge-pathnames
		     (make-pathname :name class-name :type "java")
		     source-pathname)
		    :direction :output :if-exists :supersede)
		 (format t +abcl-main-class-template+
		   class-name (namestring source-pathname)))
	       '(progn))
	   (list function)))


;;; util.lisp ends here
