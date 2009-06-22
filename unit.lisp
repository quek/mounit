(in-package :mounit)

(defvar *unit-ok-fun* (lambda (x-form x-result y-form y-result)
                        (declare (ignore x-form x-result y-form y-result))
                        (princ ".")
                        'ok))
(defvar *unit-ok-p* t)
(defvar *unit-failed-fun*
  (lambda (x-form x-result y-form y-result)
    (setf *unit-ok-p* nil)
    (princ (format nil "~%FAILED!!~%~s~%=> ~s~%~s~%=> ~s~%"
                   x-form x-result y-form y-result))))

(q:defmacro! is (x y &key (test 'equal))
  `(let* ((,g!x-result ,x)
          (,g!y-result ,y))
     (if (,test ,g!x-result ,g!y-result)
         (funcall *unit-ok-fun* ',x ,g!x-result ',y ,g!y-result)
         (funcall *unit-failed-fun* ',x ,g!x-result ',y ,g!y-result))))

(defmacro do-test (&body body)
  `(let ((*unit-ok-p* t))
     ,@body
     (if *unit-ok-p*
         (print 'ok)
         (print 'failed!!))))

#+nil
(do-test (is 3 (+ 1 2)) (is 3 (+ 1 2)) (is 3 (+ 1 2))
         (is 3 (+ 1 23)) (is 3 (+ 1 2)) (is 3 (+ 1 2)))