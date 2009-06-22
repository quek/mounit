(in-package :mounit)

(defun select (path doc)
  (cond ((endp path)
         doc)
        (t
         (select (cdr path)
                 (children (find-if (lambda (x)
                                      (equal (symbol-name (car path))
                                             (name x)))
                                    doc))))))

#|
(select '#?(html body)
        (parse "<html><head><meta http-equiv=\"Content-Type\" content='text/html; charset=EUC-JP'><meta lang='ja'></head><body>hello</body></html>"))

(identity '#?(html body))
|#