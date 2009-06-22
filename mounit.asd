;;;; -*- Mode: LISP; -*-
(asdf:defsystem :mounit
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "unit")
               (:file "html-tree")
               (:file "html-parse")
               (:file "selector-reader")
               (:file "selector")
               (:file "mounit"))
  :depends-on (series
               anaphora
               cl-ppcre
               drakma
               quek))

