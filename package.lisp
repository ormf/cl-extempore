;;;; package.lisp

(defpackage #:cl-extempore
  (:use #:cl #:cm)
  (:nicknames "ET" "EXTEMPORE")
  (:shadowing-import-from #:incudine
   :at :now))
