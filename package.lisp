;;;; package.lisp

(defpackage #:cl-extempore
  (:use #:cl)
  (:nicknames "ET" "EXTEMPORE")
  (:shadowing-import-from #:incudine
   :at :now :+TWOPI+)
  (:export #:midi-panic))
