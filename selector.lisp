(in-package :mounit)

(defun select (x doc)
  (cond ((endp x)
         doc)
        (t
         (select (cdr x)
                 (caddr (find-if (lambda (y)
                                   (and (consp y)
                                        (equal (symbol-name (car x))
                                               (car y))))
                                 doc))))))

#|
(select '#?(html body)
        (parse "<html><head><meta http-equiv=\"Content-Type\" content='text/html; charset=EUC-JP'><meta lang='ja'></head><body>hello</body></html>"))

(identity '#?(html body))
|#