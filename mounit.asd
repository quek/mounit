;;;; -*- Mode: LISP; -*-
(asdf:defsystem :mounit
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "unit")
               (:file "html-parse")
               (:file "mounit"))
  :depends-on (series
               anaphora
               cl-ppcre
               drakma
               quek))

