(in-package :mounit)

(defvar *original-readtable* nil)

(defun |#?-reader|  (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character
     #\#
     (lambda (stream char)
       (declare (ignore char))
       `(get-element-by-id ,(read stream :recursive-p t))))
    (set-macro-character
     #\.
     (lambda (stream char)
       (declare (ignore char))
       `(get-element-by-class ,(read stream :recursive-p t))))
    (read stream :recursive-p t)))

(defun enable-selector-reader ()
  (sunless *original-readtable*
    (setf it *readtable*
          *readtable* (copy-readtable nil)))
  (set-dispatch-macro-character #\# #\? '|#?-reader|))

(defun disable-selector-reader ()
  (when *original-readtable*
    (setf *readtable* *original-readtable*
          *original-readtable* nil)))

(enable-selector-reader)
