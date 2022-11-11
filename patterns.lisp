;;; 
;;; patterns.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-extempore)

(defun construct-var-list (num)
  (loop
    for n from 1 to num
    collect (intern (string-upcase (format nil "@~d" n)) 'cl-extempore)))

(defmacro :> (name len offs expr &rest seqs)
  (let* ((sqs (mapcar #'eval seqs))
         (dur (/ (rationalize len) (apply #'max (mapcar #'length sqs)))))
    `(progn
       (defun ,name (beat dur seqs)
         (apply (lambda ,(construct-var-list (length seqs)) ,expr) (mapcar #'first seqs))
         (at (*metro* (+ beat (* 0.5 dur))) #',name
             (+ beat dur) dur
             (mapcar #'rotate seqs)))
       (,name (*metro* 'get-beat ,offs) ,dur ',sqs))))

(defmacro :< (name len offs expr &rest seqs)
  (declare (ignore len offs expr seqs))
  `(progn
     (defun ,name (beat dur seqs)
       (declare (ignore beat dur seqs)))))

#|
;;; (:> model4 8 1 (print @1) '(48 52 55 60 64 55 60 64))
(:> model4 8 4 (play beat :piano @1 80 dur) '(48 52 55 60 64 55 60 64))
(:> model4 8 4 (play beat :piano @1 @2 dur) '(48 52 55 60 64 55 60 64) '(80 60 40))
(:< model4 8 4 (play beat :piano @1 @2 dur) '(48 52 55 60 64 55 60 64) '(80 60 40))
|#

(setf (fdefinition 'rnd) #'r-elt)
(setf (fdefinition '%) #'mod)
(setf (fdefinition 'orb) #'orbit)
(defun rot (x lst) (rotate lst x))
;;; (define rnd random)
(setf (fdefinition 'rng) #'range)
(setf (fdefinition 'rev) #'reverse)
(setf (fdefinition 'vec) #'vector)
;;; (define zip (lambda (&rest seqs) (apply #'append (apply #'mapcar #'list seqs))))
(setf (fdefinition 'ped) #'pedal)

(setf (fdefinition 'id) #'identity)
;;; (define union union)
;;; (define intersection intersection)
(setf (fdefinition 'difference) #'set-difference)
;; remove all duplicates from lst
(setf (fdefinition 'unique) #'remove-duplicates)
