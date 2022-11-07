;;;; package.lisp

(defpackage #:cl-extempore
  (:use #:cl #:cm)
  (:nicknames "ET" "EXTEMPORE")
  (:shadowing-import-from #:orm-utils
   :rotate)
  (:shadowing-import-from #:incudine
   :at :now :+TWOPI+))
