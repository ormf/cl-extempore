;;;; package.lisp

(defpackage #:cl-extempore
  (:use #:cl #:cm)
  (:nicknames "ET" "EXTEMPORE")
  (:shadowing-import-from #:orm-utils
   :rotate :flatten)
  (:shadowing-import-from #:cm
   :shuffle)
  (:shadowing-import-from #:incudine
   :at :now :+TWOPI+))
