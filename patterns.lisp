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

(defparameter *root* 0)
(defparameter *chord* '(36 60 63 67))
(defparameter *scale* (pc-scale 0 'aeolian))

(defun construct-var-list (num)
  (loop
    for n from 1 to num
    collect (intern (string-upcase (format nil "@~d" n)) 'cl-extempore)))

(defmacro :> (name len offs expr &rest seqs)
  (let* ((sqs (mapcar #'eval seqs))
         (len (rationalize len))
         (dur (/ len (apply #'max (mapcar #'length sqs)))))
    `(let ((LC -1)
           (LL len)
           (LP 0)
           startbeat)
       (defun ,name (beat dur seqs)
         (unless startbeat
           (setf startbeat beat))
         (multiple-value-setf (LC LP) (floor (- beat startbeat) len))
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

#|

(defun filter (pred seq)
  (reverse (reduce (lambda (acc x) (if (funcall pred x) (cons x acc) acc)) seq :initial-value '())))

|#

(defun filter (pred lst)
  (remove-if-not pred lst))

;;; (filter (lambda (x) (< x 3)) '(1 2 3 2 1 2 3 2))

(defun get-scale-stepsize (args)
  (cond ((null args) 1)
        ((numberp (first args)) (first args))
        ((null (rest args)) 1)
        ((numberp (second args)) (second args))
        (:else 1)))

(defun get-scale-scale (args)
  (mapcar #'pc
          (cond ((null args) *scale*)
                ((listp (first args)) (first args))
                ((null (rest args)) *scale*)
                ((listp (rest args)) (rest args))
                (:else *scale*))))

(defun scale (octave num &rest args)
  (let ((stepsize (get-scale-stepsize args)) 
        (scale (get-scale-scale args)))
    (mapcar (lambda (x) (pc-relative (+ 12 (car scale) (* 12 octave)) (* x stepsize) scale))
         (range 0 num))))

;;; (scale 3 8) -> (48 50 51 53 55 56 58 60)
;;; (scale 3 8 (pc-scale 2 'dorian)) -> (50 52 53 55 57 59 62 64)

(defun qnt (pitch &rest args)
    (pc-quantize pitch (or (mapcar #'pc (car args)) *scale*)))

(defun rel (pitch i &rest args)
    (pc-relative pitch i (or (mapcar #'pc (car args)) *scale*)))

(defun chrd (type &rest args)
  (pc-chord (or (pc (car args)) *root*) type))

;; orbit will usually use LC - an internal (to the looper macro) symbol defined
;; for loop count

;; if 3rd argument is present, that's the "default" case (when loop count
;; *isn't* congruent to 0 mod m)
(defun orbit (loopcnt m val &rest args)
  (if (zerop (mod loopcnt m))
      val ;; return the value; this is the 'orbit' case
      (or (car args) '_)))  ;; return '_ (non-orbit case)

;; cycle will usually use LC - an internal (to the looper macro) symbol defined
;; for loop count

;; cycles elongates the time between changes
;; i.e. (cycle 0 1 '(60 63 67) '(58 62 65)) -> (60 63 67)
;; i.e. (cycle 1 1 '(60 63 67) '(58 62 65)) -> (58 62 65)

(defun cycle (cnt cycles &rest args)
  "return nth arg depending on count. cycles determines how often the
nth arg is repeated before advancing to the next."
  (elt args (mod (floor (/ cnt cycles)) (length args))))

;; (cycle 0 1 '(60 63 67) '(58 62 65)) -> (60 63 67)
;; (cycle 1 1 '(60 63 67) '(58 62 65)) -> (58 62 65)

;; (cycle 0 2 '(60 63 67) '(58 62 65)) -> (60 63 67)
;; (cycle 1 2 '(60 63 67) '(58 62 65)) -> (60 63 67)
;; (cycle 2 2 '(60 63 67) '(58 62 65)) -> (58 62 65)
;; (cycle 3 2 '(60 63 67) '(58 62 65)) -> (58 62 65)

(defun pedal (a b)
  (flatten
   (mapcar (lambda (x)
             (if (listp a) (list x b) (list a x)))
           (if (listp a) a b))))

(defun take-while (n pred lst)
  "return first n elems of list for which predicate is true"
  (loop
    while (> n 0)
    for elt in lst
    for curr = (funcall pred elt)
    do (when curr (decf n))
    if curr collect elt into result
      finally (return result)))

;;; (take-while 4 (lambda (x) (> x 12)) (range 0 20)) -> (13 14 15 16)

#|
;; scale, qnt and rel take an optional *scale* argument
(define get-scale-stepsize
  (lambda (args)
    (cond ((null? args) 1)
          ((number? (car args)) (car args))
          ((null? (cdr args)) 1)
          ((number? (cadr args)) (cadr args))
          (else 1))))

(define get-scale-scale
  (lambda (args)
    (map (lambda (pitch) (modulo pitch 12))
         (cond ((null? args) *scale*)
               ((list? (car args)) (car args))
               ((null? (cdr args)) *scale*)
               ((list? (cadr args)) (cadr args))
               (else *scale*)))))

(define scale
  (lambda (octave num . args)
    (let ((stepsize (get-scale-stepsize args)) 
          (scale (get-scale-scale args)))
      (map (lambda (x)
             (pc:relative (+ 12 (car scale) (* 12 octave)) (* x stepsize) scale))
           (range 0 num)))))

(define qnt
  (lambda (pitch . args)
    (pc:quantize pitch (if (null? args) *scale*
                       (map (lambda (pitch)
                              (modulo pitch 12))
                            (car args))))))

(define rel
  (lambda (pitch i . args)
    (pc:relative pitch i (if (null? args) *scale*
                         (map (lambda (pitch)
                                (modulo pitch 12))
                              (car args))))))

;; always requires type, will default to *root* if second arg not provided
(define chrd
  (lambda (type . args)
    (pc:chord (if (null? args) *root*
                  (modulo (car args) 12))
              type)))

;; orbit will usually use LC - an internal (to the looper macro) symbol defined
;; for loop count

;; if 3rd argument is present, that's the "default" case (when loop count
;; *isn't* congruent to 0 mod m)
(define orbit
  (lambda (loopcnt m val . args)
    (if (= 0 (modulo loopcnt m))
        val ;; return the value; this is the 'orbit' case
        (if (null? args) '_ (car args))) ;; return '_ (non-orbit case)
    ))

;; cycle will usually use LC - an internal (to the looper macro) symbol defined
;; for loop count

;; cycles elongates the time between changes
;; i.e. (cycle LC 1 '(60 63 67) '(58 62 65))
(define (cycle cnt cycles . args)
  (list-ref args (modulo (floor (/ cnt cycles)) (length args))))

;; add pedal value a to list of notes b
;; basically zip value a into list b
(define pedal
  (lambda (a b)
    (flatten (map (lambda (x)
                    (if (list? a)
                        (list x b)
                        (list a x)))
                  (if (list? a) a b)))))

;; euclidean rhythms
;; written by Ben in recursive Scheme; Kernighan's aphorism probably applies

(define euclid-distribute-zeros
  (lambda (dst-undst)
    (let loop ((out '())
               (dst (car dst-undst))
               (undst (cadr dst-undst)))
      (if (or (null? dst) (null? undst))
          (append (list (reverse out))
                  (if (null? dst) '() (list dst))
                  (if (null? undst) '() (list undst)))
          (loop (cons (append (car dst) (car undst)) out)
                (cdr dst)
                (cdr undst))))))

(define euclid
  (lambda (k n . args)
    (let ((one-val (if (null? args) 1 (car args)))
          (zero-val (if (< (length args) 2) '_ (cadr args))))
      (cond ((<= k 0) (repeat n zero-val))
            ((>= k n) (repeat n one-val))
            (else
             (let loop ((groups (euclid-distribute-zeros
                                 (list (repeat k (list one-val))
                                       (repeat (- n k) (list zero-val))))))
               (if (or (= (length groups) 1) (member (cadr groups) (list zero-val)))
                   (flatten groups)
                   (loop (euclid-distribute-zeros groups)))))))))

;; number of (numof -> nof)
(define-macro (nof num body)
  `(make-list-with-proc ,num (lambda (idx) ,body)))

;; item as position
(define nth (lambda (at lst) (list-ref lst at)))
;; last num of lst
(define last (lambda (num lst) (cl:last lst num)))
;; take first num of lst
(define take (lambda (num lst) (cl:butnthcdr num lst)))
;; sublist: take 'num' elements starting at position 'at'
(define sub (lambda (num at lst) (cl:butnthcdr num (cl:nthcdr at lst))))




;; return first N of list for which predicate is truthy
(define take-while
  (lambda (pred lst)
    (let loop ((ret '())
               (remaining lst))
      (cond
       ((null? remaining) (reverse ret))
       ((pred (car remaining)) (loop (cons (car remaining) ret) (cdr remaining)))
       (else (reverse ret))))))

;; skip by m with a lst rotation of r
;; for example: (skip 2 -1 (scale 3 8))
(define skip
  (lambda (m r lst)
    (map cadr
         (filter (lambda (y) (= 0 (modulo (car y) m)))
                 (map (lambda (x y) (list x y))
                      (range 0 (length lst)) (rotate lst r))))))

(define holder
  (lambda ()
    (let ((cache '()))
      (lambda (lc expr LC LP LL)
        (if (null? cache) (set! cache expr))
        (if (and (= (modulo LP LL) 0)
                 (= (modulo LC lc) 0))
            (set! cache expr))
        cache))))

(define-macro (hold h pos expr)
  (let* ((localpos (modulo pos 1))
         (num (- pos localpos)))
    `(,h ,num ,expr LC LP LL)))

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
