;;;; cl-extempore.asd

(asdf:defsystem #:cl-extempore
  :description "Common Lisp adaption of parts of extempore"
  :author "Orm Finnendahl"
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:incudine #:cl-ansi-text)
  :components ((:file "package")
               (:file "utils")
               (:file "midi")
               (:file "perform")
               (:file "pc_ivl")
               (:file "globals")
               (:file "metro")
               (:file "patterns")
               (:file "instruments")
               (:file "playp")
               (:file "cl-extempore")))
