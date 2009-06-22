(in-package :mounit)

(defvar *client* nil)

(defclass client ()
  ())

(defmacro do-test ((&rest opts) &body body)
  (declare (ignore opts))
  `(progn
     ,@body))

(defmacro $ (&rest args)
  (declare (ignore args))
  nil)

(defun request (url &optional (client *client*))
  (values url client))

(defun submit (&key (client *client*)
                    form)
  (values client form))

(do-test ()
  (request "http://www.google.co.jp/")
  (is "ja" ($ "#f input[name='name']" val))
  ($ "#f.#p" val= "lisp")
  (submit)
  (is "letter" ($ "div[name=list] a :first" :text))
  )

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
