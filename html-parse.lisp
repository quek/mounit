;;(require :mounit)
#|
TODO
<!-- -->
|#
(in-package :mounit)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)
                   (compilation-speed 0)))

(defun whitespacep (char)
  (find char '(#\Space #\Tab #\Return #\Newline)))

(defun skip-whitespace (in)
  (peek-char t in nil))

(defun text-end-p (c)
  (char= #\< c))

(defun tag-name-end-p (c)
  (or (whitespacep c)
      (char= #\/ c)
      (char= #\> c)))

(defun attribute-name-end-p (c)
  (or (whitespacep c)
      (char= #\= c)
      (char= #\/ c)
      (char= #\> c)))

(defun no-quete-attribute-value-end-p (c)
  (or (whitespacep c)
      (char= #\/ c)
      (char= #\> c)))

(defgeneric scan-until (in pred))

(defmethod scan-until :around (in pred)
  (let ((result (call-next-method)))
    (if (equal "" result)
        nil
        result)))

(defmethod scan-until ((in stream) (pred character))
  (with-output-to-string (out)
    (loop for c = (read-char in nil)
          while (and c (char/= c pred))
          do (write-char c out))))

(defmethod scan-until ((in stream) (pred function))
  (with-output-to-string (out)
    (loop for c = (peek-char nil in nil)
          while (and c (not (funcall pred c)))
          do (write-char (read-char in) out))))

(defun scan-tag-name (in)
  (skip-whitespace in)
  (scan-until in #'tag-name-end-p))

(defun scan-attribute-name (in)
  (skip-whitespace in)
  (scan-until in #'attribute-name-end-p))

(defun scan-attribute-value (in)
  (let ((c (skip-whitespace in)))
    (when (equal c #\=)
      (read-char in)
      (let ((c (skip-whitespace in)))
        (cond ((char= #\' c)
               (read-char in)
               (scan-until in #\'))
              ((char= #\" c)
               (read-char in)
               (scan-until in #\"))
              (t
               (scan-until in #'no-quete-attribute-value-end-p)))))))

(defvar *end-tag-name* nil)

(defgeneric parse (object))

(defmethod parse ((string string))
  (with-input-from-string (in string)
    (parse in)))

(defmethod parse ((in stream))
  (let ((*end-tag-name* nil))
    (loop for char = (peek-char t in nil)
          while char
          if (char= #\< char)
            append (parse-tag in)
          else
            collect (parse-text in))))

(defun parse-text (in)
  (scan-until in #'text-end-p))

(defun parse-tag (in)
  (read-char in)                        ; skip #\<
  (if (equal #\/ (skip-whitespace in))
      (progn
        (read-char in)     ; skip #\/
        (list (parse-end-tag in)))
      (let* ((tag-name (scan-tag-name in))
             (attributes (parse-attributes in)))
        (if (prog1 (equal #\/ (skip-whitespace in))
              (scan-until in #\>))
            (list (list tag-name attributes)
                  (list (q:string+ "/" tag-name)))
            (list (list tag-name attributes))))))

(defun parse-end-tag (in)
  (prog1 (list (q:string+ "/" (scan-tag-name in)))
    (scan-until in #\>)))

(defun parse-attributes (in)
  (loop for attribute-name = (scan-attribute-name in)
        while attribute-name
        collect (cons attribute-name (scan-attribute-value in))))

(defun make-html-tree (doc)
  (cond ((endp doc)
         nil)
        ((atom (car doc))
         (append (list (car doc)) (make-html-tree (cdr doc))))
        (t
         (let* ((end-tag-name (q:string+ "/" (caar doc)))
                (end-pos (position-if (lambda (x)
                                        (and (consp x)
                                             (equal end-tag-name (car x))))
                                      (cdr doc))))
           (if end-pos
               (let ((children (subseq doc 1 (1+ end-pos))))
                 (cons
                  (append (car doc)
                          (list (make-html-tree children)))
                  (make-html-tree (subseq doc (+ 2 end-pos)))))
               (cons (car doc) (make-html-tree (cdr doc))))))))

#+nil
(do-test
  (is '(("div" nil) ("/div")) (parse " <div></div>"))
  (is '(("div" nil) ("/div")) (parse " <div/>"))
  (is '(("div" nil) "aa" ("/div")) (parse " <div>aa</div>"))
  (is '(("div" (("id" . "d1"))) "aa" ("/div"))
      (parse " <div id='d1'>aa</div>"))
  (is '(("div" (("id" . "d1") ("class" . "c1")))
        "aa" ("span" (("id" . "s1") ("class" . "s1"))) "bb" ("/span")
        ("/div"))
      (parse " <div id='d1' class='c1'>aa<span id='s1' class='s1'>bb</span></div>"))
  (is '(("html" nil)
        ("head" nil)
        ("meta" (("http-equiv" . "Content-Type")
                 ("content" . "text/html; charset=EUC-JP")))
        ("meta" (("lang" . "ja")))
        ("/head")
        ("body" nil)
        "hello"
        ("/body")
        ("/html"))
      (parse "<html><head><meta http-equiv=\"Content-Type\" content='text/html; charset=EUC-JP'><meta lang='ja'></head><body>hello</body></html>"))
  (is '(("html" nil
         (("head" nil
                  (("meta"
                    (("http-equiv" . "Content-Type")
                     ("content" . "text/html; charset=EUC-JP")))
                   ("meta" (("lang" . "ja")))))
          ("body" nil ("hello")))))
      (make-html-tree (parse "<html><head><meta http-equiv=\"Content-Type\" content='text/html; charset=EUC-JP'><meta lang='ja'></head><body>hello</body></html>")))
  )