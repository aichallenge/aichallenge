;;;; asdf-init.lisp

(let ((dpd *default-pathname-defaults*))
  (setf asdf:*central-registry*
        (list dpd
              ;; This is how you add third party libraries:
              ;(merge-pathnames "3rd-party/split-sequence-20011114.1/" dpd)
              ;(merge-pathnames "3rd-party/usocket-0.4.1/" dpd)
              )))
