;;;; cl-extempore.asd

(asdf:defsystem #:cl-extempore
  :description "Common Lisp adaption of parts of extempore"
  :author "Orm Finnendahl"
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:cm #:cm-all #:incudine)
  :components ((:file "package")
               (:file "utils")
               (:file "pc_ivl")
               (:file "metro")
               (:file "patterns")
               (:file "cl-extempore")))
