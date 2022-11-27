;;; 
;;; pc_ivl.lisp
;;;
;;; pitch class/interval related code. translation/adaption of parts
;;; of Andrew Sorensen's code from extempore
;;; (https://github.com/digego/extempore) to Common Lisp
;;;
;;; The original scheme Code is Copyright (c) 2011-2020, Andrew
;;; Sorensen
;;;
;;; **********************************************************************
;;; The Common Lisp code is Copyright (c) 2022, Orm Finnendahl
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

;;; pc_ivl.xtm -- pitch class and interval sets

;; Author: Andrew Sorensen
;; Keywords: extempore

;;; Commentary:

;; A pitch class in this library is taken to be a
;; list of MIDI note values from the first octave (0-11)
;; from which other pitches are compared using modulo 12.
;; Therefore, 0 = C, 1 = C#, etc..

;;; Code:


; Define basic diatonic major
(defparameter *diatonic-major*
   '((i . (0 . ^))
     (i6 . (0 . ^6))
     (i64 . (0 . ^64))
     (i7 . (0 . ^7))
     (i- . (0 . -))
     (i-7 . (0 . -7))
     (n . (1 . ^)) ; neapolitan
     (n6 . (1 . ^6)) ; neapolitan
     (ii . (2 . -))
     (ii6 . (2 . -6))
     (ii7 . (2 . -7))
     (ii9 . (2 . -9))
     (ii^ . (2 . ^))
     (ii^7 . (2 . ^7))
     (iii . (4 . -))
     (iii6 . (4 . -6))
     (iii7 . (4 . -7))
     (iii^ . (4 . ^))
     (iii^7 . (4 . ^7))
     (iv . (5 . ^))
     (iv6 . (5 . ^6))
     (iv7 . (5 . ^7))
     (iv- . (5 . -))
     (iv-7 . (5 . -7))
     (v . (7 . ^))
     (v6 . (7 . ^6))
     (v7 . (7 . 7))
     (v- . (7 . -))
     (v-7 . (7 . -7))
     (b6 . (8 . ^))
     (vi . (9 . -))
     (vi6 . (9 . -6))
     (vi7 . (9 . -7))
     (vi^ . (9 . ^))
     (vi^7 . (9 . ^7))
     (b7 . (10 . ^))
     (viio . (11 . o))
     (viio7 . (11 . o7))
     (vii . (11 . o))
     (vii7 . (11 . -7b5))
     ))

(defparameter *kdiatonic-major*
  (let ((hash (make-hash-table)))
    (loop for (key value) on
          '(:i (0 . ^)
            :i6 (0 . ^6)
            :i64 (0 . ^64)
            :i7 (0 . ^7)
            :i- (0 . -)
            :i-7 (0 . -7)
            :n (1 . ^)     ; neapolitan
            :n6 (1 . ^6)   ; neapolitan
            :ii (2 . -)
            :ii6 (2 . -6)
            :ii7 (2 . -7)
            :ii9 (2 . -9)
            :ii^ (2 . ^)
            :ii^7 (2 . ^7)
            :iii (4 . -)
            :iii6 (4 . -6)
            :iii7 (4 . -7)
            :iii^ (4 . ^)
            :iii^7 (4 . ^7)
            :iv (5 . ^)
            :iv6 (5 . ^6)
            :iv7 (5 . ^7)
            :iv- (5 . -)
            :iv-7 (5 . -7)
            :v (7 . ^)
            :v6 (7 . ^6)
            :v7 (7 . 7)
            :v- (7 . -)
            :v-7 (7 . -7)
            :b6 (8 . ^)
            :vi (9 . -)
            :vi6 (9 . -6)
            :vi7 (9 . -7)
            :vi^ (9 . ^)
            :vi^7 (9 . ^7)
            :b7 (10 . ^)
            :viio (11 . o)
            :viio7 (11 . o7)
            :vii (11 . o)
            :vii7 (11 . -7b5)
            )
          by #'cddr
          do (setf (gethash key hash) value))
    hash))

;; Define basic diatonic minor
(defparameter *diatonic-minor*
   '((i . (0 . -))
     (i6 . (0 . -6))
     (i64 . (0 . -64))
     (i7 . (0 . -7))
     (i^ . (0 . ^))
     (i^6 . (0 . ^6))
     (i^64 . (0 . ^64))
     (i^7 . (0 . ^7))
     (n . (1 . ^)) ; neapolitan
     (n6 . (1 . ^6)) ; neapolitan
     (ii . (2 . o))
     (ii6 . (2 . o6))
     (ii7 . (2 . o7))
     (ii- . (2 . -))
     (ii-6 . (2 . -6))
     (ii-7 . (2 . -7))
     (ii^ . (2 . ^))
     (ii^7 . (2 . ^7))
     (iii . (3 . ^))
     (iii6 . (3 . ^6))
     (iii7 . (3 . ^7))
     (iii- . (3 . -))
     (iii-6 . (3 . -6))
     (iii-7 . (3 . -7))
     (iv . (5 . -))
     (iv6 . (5 . -6))
     (iv7 . (5 . -7))
     (iv^ . (5 . ^))
     (iv^6 . (5 . ^6))
     (iv^7 . (5 . ^7))
     (v . (7 . ^))
     (v^ . (7 . ^))
     (v6 . (7 . ^6))
     (v7 . (7 . 7))
     (v- . (7 . -))
     (v-6 . (7 . -6))
     (v-6 . (7 . -6))
     (v-7 . (7 . -))
     (vi . (8 . ^))
     (vi6 . (8 . ^6))
     (vi7 . (8 . ^7))
     (vi- . (8 . -))
     (vi-6 . (8 . -6))
     (vi-7 . (8 . -7))
     (vii . (10 . ^))
     (vii6 . (10 . ^6))
     (vii7 . (10 . ^7))
     (viio . (11 . o)) ;raised 7 (dim)
     (viio6 . (11 . o6)) ;raised 7 (dim)
     (viio7 . (11 . o7)) ; raised 7 (dim)
     ))

(defparameter *kdiatonic-minor*
  (let ((hash (make-hash-table)))
    (loop for (key value) on
          '(:i (0 -)
            :i6 (0 -6)
            :i64 (0 -64)
            :i7 (0 -7)
            :i^ (0 ^)
            :i^6 (0 ^6)
            :i^64 (0 ^64)
            :i^7 (0 ^7)
            :n (1 ^)                    ; neapolitan
            :n6 (1 ^6)                  ; neapolitan
            :ii (2 o)
            :ii6 (2 o6)
            :ii7 (2 o7)
            :ii- (2 -)
            :ii-6 (2 -6)
            :ii-7 (2 -7)
            :ii^ (2 ^)
            :ii^7 (2 ^7)
            :iii (3 ^)
            :iii6 (3 ^6)
            :iii7 (3 ^7)
            :iii- (3 -)
            :iii-6 (3 -6)
            :iii-7 (3 -7)
            :iv (5 -)
            :iv6 (5 -6)
            :iv7 (5 -7)
            :iv^ (5 ^)
            :iv^6 (5 ^6)
            :iv^7 (5 ^7)
            :v (7 ^)
            :v^ (7 ^)
            :v6 (7 ^6)
            :v7 (7 7)
            :v- (7 -)
            :v-6 (7 -6)
            :v-6 (7 -6)
            :v-7 (7 -)
            :vi (8 ^)
            :vi6 (8 ^6)
            :vi7 (8 ^7)
            :vi- (8 -)
            :vi-6 (8 -6)
            :vi-7 (8 -7)
            :vii (10 ^)
            :vii6 (10 ^6)
            :vii7 (10 ^7)
            :viio (11 o)                ;raised 7 (dim)
            :viio6 (11 o6)              ;raised 7 (dim)
            :viio7 (11 o7)              ; raised 7 (dim)
            )
          by #'cddr
          do (setf (gethash key hash) value))
    hash))

;; various scales defined as pc sets
(defparameter *scales*
   '((blues . (2 1 1 3 2))
     (blue-note . (3 2 1 1 3))
     (diminished . (2 1 2 1 2 1 2))
     (half-diminished . (2 1 2 1 2 2))
     (dominant-diminished . (1 2 1 2 1 2 1))
     (acoustic . (2 2 2 1 2 1))
     (algerian . (2 1 3 1 1 3))
     (altered . (1 2 1 2 2 2))
     (augmented . (3 1 3 1 3))
     (bebop . (2 2 1 2 1 1 2))
     (bebop-dominant . (2 2 1 2 2 1 1))
     (enigmentic . (1 3 2 2 2 1))
     (flamenco . (1 3 1 2 1 3))
     (gypsy . (2 1 3 1 1 2))
     (istrian . (1 2 1 2 1))
     (iwato . (1 4 1 4))
     (melodic . (2 1 2 2 2 2))
     (neapolitan . (1 2 2 2 2))
     (persian . (1 3 1 1 2 1))
     (prometheus . (2 2 2 3 1))
     (tritone . (1 3 2 1 3))
     (ukrainian . (2 1 3 1 2 1))
     (yo . (3 2 2 3))
     (pentatonic . (2 2 3 2))
     (wholetone . (2 2 2 2 2))
     (chromatic . (1 1 1 1 1 1 1 1 1 1 1))
     (octatonic . (2 1 2 1 2 1 2))
     (messiaen1 . (2 2 2 2 2))
     (messiaen2 . (2 1 2 1 2 1 2))
     (messiaen3 . (2 1 1 2 1 1 2 1))
     (messiaen4 . (1 1 3 1 1 1 3))
     (messiaen5 . (1 4 1 1 4))
     (messiaen6 . (2 2 1 1 2 2 1))
     (messiaen7 . (1 1 1 2 1 1 1 1 2))
     (harmonic . (2 1 2 2 1 3))
     (double-harmonic . (1 3 1 2 1 3))
     (ionian . (2 2 1 2 2 2))
     (dorian . (2 1 2 2 2 1))
     (phrygian . (1 2 2 2 1 2))
     (phrygian-dominant . (1 3 1 2 1 2))
     (lydian . (2 2 2 1 2 2))
     (lydian-dominant . (2 2 2 1 2 1))
     (lydian-mixolydian . (2 1 2 1 2 1 2))
     (lydian-augmented . (2 2 2 2 1 2))
     (mixolydian . (2 2 1 2 2 1))
     (aeolian . (2 1 2 2 1 2))
     (locrian . (1 2 2 1 2 2))))


(defparameter *kscales*
  (let ((hash (make-hash-table)))
    (loop for (key value) on
          '(:blues (2 1 1 3 2)
            :blue-note (3 2 1 1 3)
            :diminished (2 1 2 1 2 1 2)
            :half-diminished (2 1 2 1 2 2)
            :dominant-diminished (1 2 1 2 1 2 1)
            :acoustic (2 2 2 1 2 1)
            :algerian (2 1 3 1 1 3)
            :altered (1 2 1 2 2 2)
            :augmented (3 1 3 1 3)
            :bebop (2 2 1 2 1 1 2)
            :bebop-dominant (2 2 1 2 2 1 1)
            :enigmentic (1 3 2 2 2 1)
            :flamenco (1 3 1 2 1 3)
            :gypsy (2 1 3 1 1 2)
            :istrian (1 2 1 2 1)
            :iwato (1 4 1 4)
            :melodic (2 1 2 2 2 2)
            :neapolitan (1 2 2 2 2)
            :persian (1 3 1 1 2 1)
            :prometheus (2 2 2 3 1)
            :tritone (1 3 2 1 3)
            :ukrainian (2 1 3 1 2 1)
            :yo (3 2 2 3)
            :pentatonic (2 2 3 2)
            :wholetone (2 2 2 2 2)
            :chromatic (1 1 1 1 1 1 1 1 1 1 1)
            :octatonic (2 1 2 1 2 1 2)
            :messiaen1 (2 2 2 2 2)
            :messiaen2 (2 1 2 1 2 1 2)
            :messiaen3 (2 1 1 2 1 1 2 1)
            :messiaen4 (1 1 3 1 1 1 3)
            :messiaen5 (1 4 1 1 4)
            :messiaen6 (2 2 1 1 2 2 1)
            :messiaen7 (1 1 1 2 1 1 1 1 2)
            :harmonic (2 1 2 2 1 3)
            :double-harmonic (1 3 1 2 1 3)
            :ionian (2 2 1 2 2 2)
            :dorian (2 1 2 2 2 1)
            :phrygian (1 2 2 2 1 2)
            :phrygian-dominant (1 3 1 2 1 2)
            :lydian (2 2 2 1 2 2)
            :lydian-dominant (2 2 2 1 2 1)
            :lydian-mixolydian (2 1 2 1 2 1 2)
            :lydian-augmented (2 2 2 2 1 2)
            :mixolydian (2 2 1 2 2 1)
            :aeolian (2 1 2 2 1 2)
            :locrian (1 2 2 1 2 2))
          by #'cddr
          do (setf (gethash key hash) value))
    hash))

; Define basic chord symbols
(defparameter *chord-syms*
   '((^ . (0 4 7))
     (^sus . (0 5 7))
     (^6 . (4 7 0))
     (^64 . (7 0 4))
     (^7 . (0 4 7 11))
     (^65 . (4 7 11 0))
     (^43 . (7 11 0 4))
     (^42 . (11 0 4 7))
     (^2 . (11 0 4 7))
     (^7#4 . (0 4 7 11 6))
     (^9 . (0 4 7 11 2))
     (7 . (0 4 7 10))
     (9 . (0 4 7 10 2))
     (65 . (4 7 10 0))
     (43 . (7 10 0 4))
     (2 . (10 0 4 7))
     (42 . (10 0 4 7))
     (- . (0 3 7))
     (-sus . (0 5 7))
     (-6 . (3 7 0))
     (-64 . (7 0 3))
     (-7 . (0 3 7 10))
     (-65 . (3 7 10 0))
     (-43 . (7 10 0 3))
     (-42 . (10 0 3 7))
     (-2 . (10 0 3 7))
     (-9 . (0 3 7 10 2))
     (o . (0 3 6))
     (o6 . (3 6 0))
     (o64 . (6 0 3))
     (o7 . (0 3 6 8))
     (o65 . (3 6 8 0))
     (o43 . (6 8 0 3))
     (o42 . (8 0 3 6))
     (o2 . (8 0 3 6))
     (-7b5 . (0 3 6 9))))


(defparameter *kchord-syms*
  (let ((hash (make-hash-table)))
    (loop for (key value) on
          '(:^ (0 4 7)
            :^sus (0 5 7)
            :^6 (4 7 0)
            :^64 (7 0 4)
            :^7 (0 4 7 11)
            :^65 (4 7 11 0)
            :^43 (7 11 0 4)
            :^42 (11 0 4 7)
            :^2 (11 0 4 7)
            :^7#4 (0 4 7 11 6)
            :^9 (0 4 7 11 2)
            :7 (0 4 7 10)
            :9 (0 4 7 10 2)
            :65 (4 7 10 0)
            :43 (7 10 0 4)
            :2 (10 0 4 7)
            :42 (10 0 4 7)
            :- (0 3 7)
            :-sus (0 5 7)
            :-6 (3 7 0)
            :-64 (7 0 3)
            :-7 (0 3 7 10)
            :-65 (3 7 10 0)
            :-43 (7 10 0 3)
            :-42 (10 0 3 7)
            :-2 (10 0 3 7)
            :-9 (0 3 7 10 2)
            :o (0 3 6)
            :o6 (3 6 0)
            :o64 (6 0 3)
            :o7 (0 3 6 8)
            :o65 (3 6 8 0)
            :o43 (6 8 0 3)
            :o42 (8 0 3 6)
            :o2 (8 0 3 6)
            :-7b5 (0 3 6 9))
          by #'cddr
          do (setf (gethash key hash) value))
    hash))

;;; (gethash :-2 *kchord-syms*)



(defparameter *chord-syms-scales*
   '((^ . 'ionian)
     (^sus . 'mixolydian)
     (^6 . 'ionian)
     (^64 . 'ionian)
     (^7 . 'ionian)
     (^65 . 'ionian)
     (^43 . 'ionian)
     (^42 . 'ionian)
     (^2 . 'ionian)
     (^7#4 . 'ionian)
     (^9 . 'ionian)
     (7 . 'mixolydian)
     (9 . 'mixolydian)
     (65 . 'mixolydian)
     (43 . 'mixolydian)
     (2 . 'mixolydian)
     (42 . 'mixolydian)
     (- . 'dorian)
     (-sus . 'mixolydian)
     (-6 . 'dorian)
     (-64 . 'dorian)
     (-7 . 'dorian)
     (-65 . 'dorian)
     (-43 . 'dorian)
     (-42 . 'dorian)
     (-2 . 'dorian)
     (-9 . 'dorian)
     (o . 'locrian)
     (o6 . 'locrian)
     (o64 . 'locrian)
     (o7 . 'locrian)
     (o65 . 'locrian)
     (o43 . 'locrian)
     (o42 . 'locrian)
     (o2 . 'locrian)
     (-7b5 . 'locrian)))

(defparameter *kchord-syms-scales*
  (let ((hash (make-hash-table)))
    (loop for (key value) on
          '(:^ 'ionian
            :^sus 'mixolydian
            :^6 'ionian
            :^64 'ionian
            :^7 'ionian
            :^65 'ionian
            :^43 'ionian
            :^42 'ionian
            :^2 'ionian
            :^7#4 'ionian
            :^9 'ionian
            :7 'mixolydian
            :9 'mixolydian
            :65 'mixolydian
            :43 'mixolydian
            :2 'mixolydian
            :42 'mixolydian
            :- 'dorian
            :-sus 'mixolydian
            :-6 'dorian
            :-64 'dorian
            :-7 'dorian
            :-65 'dorian
            :-43 'dorian
            :-42 'dorian
            :-2 'dorian
            :-9 'dorian
            :o 'locrian
            :o6 'locrian
            :o64 'locrian
            :o7 'locrian
            :o65 'locrian
            :o43 'locrian
            :o42 'locrian
            :o2 'locrian
            :-7b5 'locrian)
          by #'cddr
          do (setf (gethash key hash) value))
    hash))


;; returns a scale based on a chord (standard jazz translations)
(defparameter *chord->scale*
   '((i . (0 . ionian))
     (i7 . (0 . ionian))
     (ii . (2 . dorian))
     (ii7 . (2 . dorian))
     (ii9 . (2 . dorian))
     (iii . (4 . phrygian))
     (iii7 . (4 . phrygian))
     (iv . (5 . lydian))
     (iv7 . (5 . lydian))
     (v . (7 . mixolydian))
     (v7 . (7 . mixolydian))
     (vi . (9 . aeolian))
     (vi7 . (9 . aeolian))
     (vii . (11 . locrian))
     (vii7 . (11 . locrian))))

(defparameter *kchord->scale*
  (let ((hash (make-hash-table)))
    (loop
      for (key value) on
      '(:i (0 ionixan)
        :i7 (0 ionian)
        :ii (2 dorian)
        :ii7 (2 dorian)
        :ii9 (2 dorian)
        :iii (4 phrygian)
        :iii7 (4 phrygian)
        :iv (5 lydian)
        :iv7 (5 lydian)
        :v (7 mixolydian)
        :v7 (7 mixolydian)
        :vi (9 aeolian)
        :vi7 (9 aeolian)
        :vii (11 locrian)
        :vii7 (11 locrian))
      by #'cddr
      do (setf (gethash key hash) value))
    hash))


;; return the pitch class of pitch

(defun pc (pitch)
  "return pitch-class of pitch or nil if pitch isn't a number."
  (and (numberp pitch) (mod pitch 12)))

;; A predicate for calculating if pitch is in pc-set
;;
;; arg 1: pitch to check against pc-set
;; arg 2: pc-set to check pitch against
;;
;; retuns true or false
;;

(defun ? (pitch pc-set)
  "is pitch-class of pitch contained in pc-set?"
  (consp (member (pc pitch) pc-set)))

#|
(defun quantize (keynum pc-set)
  (multiple-value-bind (oct pc-in) (floor (round keynum) 12)
    (loop
      for (low high) on (cons (- (first (last pc-set)) 12) pc-set)
      while high until (<= low pc-in high)
      finally (if high
                  (return
                    (+ (* oct 12)
                       (if (<= (- high pc-in) (- pc-in low))
                           high low)))
                  (warn "no pc-set value to quantize to ~d ~d" keynum pc-set)))))
|#

(defun pc-quantize (keynum pc-set)
  "quantize keynum to the closest keynum acording to pc-set."
  (let ((pc-set (sort pc-set #'<)))
    (multiple-value-bind (oct pc-in) (floor (round keynum) 12)
      (multiple-value-bind (low high)
          (if (< pc-in (first pc-set))
              (values (- (first (last pc-set)) 12) (first pc-set))
              (loop
                for (low high) on pc-set
                while high until (<= low pc-in high)
                finally (return
                          (if high
                              (values low high)
                              (values low (+ (first pc-set) 12))))))
        (+ (* oct 12) (if (< (- pc-in low) (- high pc-in))
                          low high))))))

;;; (pc-quantize 60 '(2 4 5 7 8 11)) -> 59

;;; (pc-quantize 61 '(2 4 5 7 9 11)) -> 62
;;; (pc-quantize 59 '(2 4 5 7 9 11))

;; quantize-high pc
;; Always slelects a higher value before a lower value where distance is equal.
;;
;; arg 1: pitch to quantize to pc
;; arg 2: pc to quantize pitch against
;;
;; returns quantized pitch or #f if non available
;;

;;; the original extempore routine (here for compatibility
;;; purposes). Inefficient and not always returning a value...

(defun quantize-high (pitch-in pc-set)
  (loop
    with pitch = (round pitch-in)
    for offset below 8
    do (cond
         ((? (+ pitch offset) pc-set) (return (+ pitch offset)))
         ((? (- pitch offset) pc-set) (return (- pitch offset))))
    finally (warn "no pc-set value to quantize to ~d ~d" pitch pc-set)))

;; quantize pc
;; Always slelects a lower value before a higher value where distance is equal.
;;
;; arg 1: pitch to quantize to pc
;; arg 2: pc to quantize pitch against
;;
;; returns quntized pitch or #f if non available
;;

(defun quantize-low (pitch-in pc-set)
  (loop
    with pitch = (round pitch-in)
    for offset below 8
    do (cond
         ((? (- pitch offset) pc-set) (return (- pitch offset)))
         ((? (+ pitch offset) pc-set) (return (+ pitch offset))))
    finally (warn "no pc-set value to quantize to ~d ~d" pitch pc-set)))



;; select random pitch from pitch class
;; bounded by lower and upper (inclusive lower exclusive upper)
;;
;; arg 1: lower bound (inclusive)
;; arg 2: upper bound (exclusive)
;; arg 3: pitch class
;;
;; returns #f if no valid pitch is possible
;;

#|

(defun get-all-keynums-in-range (pc-set lower upper)
  "return all keynums in [lower..upper["
  (labels ((inner (p)
             (multiple-value-bind (oct lower-p) (floor lower 12)
               (let* ((startoct (if (> p lower-p) oct (1+ oct)))
                      (startpitch (+ p (* startoct 12) )))
                 (loop for keynum from startpitch below upper by 12 collect keynum)))))
    (sort (apply #'append (mapc-setar #'inner pc-set)) #'<)))
|#

(defun get-all-keynums-in-range (lower upper pc-set)
  "return all keynums in range [lower..upper[ according to pc-set"
  (let* ((oct (floor lower 12)))
    (loop for startkey from (* oct 12) below upper by 12
          append (loop
                   for offs in pc-set
                   for keynum = (+ startkey offs)
                   while (< keynum upper)
                   if (<= lower keynum)
                     collect keynum))))

;;; (get-all-keynums-in-range 60 66 '(0 4 7)) -> (60 64) 

;;; (get-all-keynums-in-range 56 69 '(4)) ->  64

(defun r-elt (seq)
  "return a random element from seq (same es pc:random in extempore)"
  (elt seq (random (length seq))))

(defun r-pc (lower upper pc-set)
  "return a random keynum contained in pc-set in the range
[lower..upper]."
  (unless (null pc-set)
      (let ((choices (get-all-keynums-in-range lower upper pc-set)))
        (unless (null choices) (r-elt choices)))))

;;; (r-pc 60 72 '(0 4 7))

(setf (fdefinition 'pc-random) #'r-pc)

;; select pitch from pitch class relative to a given pitch
;;
;; 1st: bass pitch
;; 2nd: pc relationship to bass pitch (max is abs 7)
;; 3rd: pitch class
;;
;; example:
;; (pc:relative 64 -2 '(0 2 4 5 7 9 11)) => 60
;; (pc:relative 69 3 '(0 2 4 5 7 9 11)) => 74
;;

(defun pc-relative (keynum num-steps pc-set)
  "return the keynum which is num-steps apart from supplied keynum
according to the pc-set. The pc-set is replicating in the octave, so
num-steps can extend several octaves up or down."
  (let ((q-key (pc-quantize keynum pc-set))
        (len (length pc-set)))
    (multiple-value-bind (q-key-oct q-key-pc) (floor q-key 12)
      (let* ((q-key-pos (position q-key-pc pc-set))
             (new-pos (+ q-key-pos (round num-steps))))
        (multiple-value-bind (oct new-idx) (floor new-pos len)
          (+ (* 12 (+ oct q-key-oct)) (elt pc-set new-idx)))))))

;;; (pc-relative 62 2 '(0 2 4 5 7 9 11)) -> 65

;; make-chord creates a list of keynums in the range [lower..upper[ in
;; accordance to the given pc-set. The keynums are spread out over the
;; range, but it is not guaranteed, that all pcs of the pc-set are
;; present and pcs might occur multiple times unless :unique t ist
;; provided.

;; A division of the bounds by the number of elements requested breaks
;; down the selection into equal ranges from which each pitch is
;; selected.  make-chord attempts to select pitches of all degrees of
;; the pc.  it is possible for elements of the returned chord to be -1
;; if no possible pc is available for the given range.
;; non-deterministic (i.e. result can vary each time)
;;
;; arg1: lower bound (inclusive)
;; arg2: upper bound (exclusive)
;; arg3: number of pitches in chord
;; arg4: pitch class
;;
;; example: c7
;; (make-chord 60 85 4 '(0 4 7) :unique nil) => (60 70 76 79)
;;

(defun pc-make-chord (lower upper num pc-set)
  (let* ((gap (round (/ (- upper lower) num))))
    (sort
     (loop repeat num
           with remaining-pcs = pc-set
           while remaining-pcs
           for l from (round lower) by gap
           for keynum = (or (r-pc l (+ l gap) remaining-pcs) (r-pc lower upper remaining-pcs)(r-pc lower upper pc-set))
           if keynum collect keynum
           do (setf remaining-pcs (remove (pc keynum) remaining-pcs)))
     #'<)))

;;; (sort (mapcar #'pc (make-chord 30 70 4 '(0 4 7 10))) #'<)

;; Returns a scale degree (zero-based) of a given value (pitch) based on a pc
(defun degree (value pc-set)
  (or (position (pc value) pc-set)
      (warn "value ~a not in pc ~a" value pc-set)))

;; quantize the values of seq to pc-set
(defun quantize-list (seq pc-set)
  (mapcar (lambda (p) (pc-quantize p pc-set)) seq))

;; retrograde list
(defun retrograde (seq) (reverse seq))

;; invert list paying no attention to key
(defun invert (seq &rest args)
  (let ((pivot (if (null args) (car seq) (car args))))
    (cons (first seq) (mapcar (lambda (i) (- pivot (- i pivot))) (rest seq)))))

;; transpose list paying no attention to key
(defun transpose (val seq)
  (map 'list (lambda (i) (+ i val)) seq))

(defun integrate (seq &key (modifier #'+) (start (first seq)))
  "return a running sum (or any other modifier fn) of a seq."
  (loop
     for i in seq
     for j = start then (funcall modifier j i)
     collect j))

(defun differentiate (seq &key (modifier #'-) (start (first seq)))
  "return differences between subsequent elements of seq."
  (cons start
	(mapcar (lambda (x y) (funcall modifier x y)) (cdr seq) seq)))

;; expand/contract list by factor paying no attention to key
(defun expand/contract (seq factor)
  (mapcar #'round
          (integrate (cons (first seq)
                           (mapcar #'(lambda (x) (* x factor))
                                   (cdr (differentiate seq)))))))

;; invert the values of lst quantizing to pcs
(defun pc-invert (seq pcs &rest args)
  (if (null args)
      (quantize-list (invert seq) pcs)
      (quantize-list (invert seq (car args)) pcs)))

;; transpose the values of lst quantizing to pcs
(defun pc-transpose (val lst pcs)
  (quantize-list (transpose val lst) pcs))

;; expand/contract lst by factor quantizing to pc
(defun pc-expand/contract (lst factor pc)
  (quantize-list (expand/contract lst factor) pc))

;; returns the pc-set of a scale type based on a given root
(defun pc-scale (root type &key (sort nil))
  (if (stringp type) (setf type (intern (string-upcase type))))
  (let ((scale-ivs (cdr (assoc type *scales*))))
    (if scale-ivs
        (let ((pc-set (mapcar #'pc (integrate (cons root scale-ivs)))))
          (if sort (sort pc-set #'<) pc-set))
        (warn "Scale type ~a not found." type))))

;; returns a scale pc-set based on a chord type (basic jazz modal theory)
;; chord type is a symbol indicating diatonic position and 
;; 
;; e.g. (chord->scale 2 'i7) (1 2 4 6 7 9 11)

(defun chord->scale (root type &key (sort t))
   (pc-scale (pc (+ (cadr (assoc type *chord->scale*)) root))
          (cddr (assoc type *chord->scale*))
          :sort sort))

;; returns a chord given a root and type
;; see *chord-syms* for currently available types
;;
;; e.g. (pc-chord 0 '^7)  => '(0 4 7 11)

(defun pc-chord (root type &key (sort nil))
  "return the pc-set of a chord with given root and type, optionally
sorted."
  (let* ((chord (cdr (assoc type *chord-syms*)))
         (pc-set
           (if chord
               (loop for pc in chord collect (pc (+ root pc)))          
               (warn "Chord type ~a not found." type))))
    (if sort (sort pc-set #'<) pc-set)))


(defun diatonic (root maj-min degree &key (sort nil))
  "return the pc-set of a chord following basic diatonic harmony rules
 based on root (0 for C etc.) maj/min ('^ or '-) and degree
 [0..11] or ['i..'vii]."
  (if (numberp degree)
      (setf degree (cdr (assoc degree '((0 . i) (1 . ii) (2 . ii)
                                        (3 . iii) (4 . iii) (5 . iv)
                                        (6 . iv) (7 . v) (8 . vi) (9 . vi)
                                        (10 . vii) (11 . vii))))))
  (let* ((val (assoc degree
                     (if (equal '^ maj-min)
                         *diatonic-major*
                         *diatonic-minor*))))
    (pc-chord (pc (+ root (cadr val))) (cddr val) :sort sort)))

;;; (diatonic 0 '^ 'vii)

(defun chord-options (root maj-min pc-set)
  "returns chord options for root in maj-min key of pc-set.

Example: (chord-options 2 '^ (pc-scale 0 'ionian)) => ((0 4 7) (0 4 7 11) (0 5 7) (0 4 7 11 2) (0 4 7 11 6))
"
  (let ((major7 '(^ ^7 ^sus ^9 ^7#4))
        (dom7 '(^ 7 ^sus 9))
        (minor7 '(- -7 -sus -9))
        (dim7 '(o -7b5 o7))
        (degree (degree root pc-set)))
    (mapcar (lambda (sym) (pc-chord root sym))
            (if degree
                (if (equal maj-min '^)
                    (case degree
;;;                      (-1 '())
                      ((0 3) major7)
                      ((4) dom7)
                      ((1 2 5) minor7)
                      ((6) dim7))
                    (case degree
;;;                      ((-1) '())
                      ((0 3 5) minor7)
                      ((2) major7)
                      ((4) (append minor7 dom7))
                      ((1) dim7)
                      ((6) (append dom7 dim7))))))))

;;; (chord-options 0 '- (pc-scale 0 'dorian))

;;; (chord-options 0 '^ (pc-scale 0 'ionian))
;;; (chord-options 3 '^ (pc-scale 0 'ionian))

;; make a chord that is fixed at either the 'top or the 'bottom
;; where fixed is as close as the chord allows to fix-point
;; defaults to bottom
;;
;; (make-chord-fixed 60 3 '(0 3 7))      => (60 63 67)
;; (make-chord-fixed 60 3 '(0 3 7) 'top) => (51 55 60)
;; (make-chord-fixed 60 3 '(11 2 4))


(defun make-chord-fixed (fix-point num pc-set &rest args)
  "return a chord of n keynums corresponding to the pc-set starting or
ending at a pitch closest to fix-point. if arg is not supplied or
'bottom the lowest keynum of the returned chord will be closest to
fix-point, otherwise the highest keynum will be closest."
  (unless (< num 1)
    (loop
      repeat num
      with dir = (if (null args) 1 (if (eq 'bottom (car args)) 1 -1))
      for keynum = (pc-quantize fix-point pc-set) then (pc-relative keynum dir pc-set)
      collect keynum into result
      finally (return (sort result #'<)))))

;; distance between pitch and a pc
(defun distance (keynum pc)
  "return the abs distance between pc of keynum and supplied pc."
  (if (numberp pc) (setf pc (list pc)))
  (let ((p (pc keynum)))
    (car (sort (mapcar (lambda (class)
                         (let ((val (abs (- p class))))
                           (abs (if (< val (- 12 val)) val (- 12 val)))))
                       pc)
               #'<))))

;;; (distance 73 0)

(defun distance-of-chord (chd pc)
  (apply #'+ (mapcar (lambda (p) (distance p pc)) chd)))

;;; (distance-of-chord '(72 76 79) 4)

;; find the pc that is closest to given pitch
;; useful for finding next step for pitch given a chord
(defun closest-pc (pitch pc-set)
  "return the pc from pc-set that is closest to given pitch"
  (cdar (sort (mapcar (lambda (class) (cons (distance pitch class) class)) pc-set)
              #'< :key #'first)))

;;; (closest-pc 75 '(0 4 7)) -> 4

(defun closest-pitch (pc keynums)
  "return the keynum in keynums closest to pc."
  (cdar (sort (mapcar (lambda (p) (cons (distance p pc) p)) keynums)
                 #'< :key #'first)))

;;; (closest-pitch 3 '(70 72 75 79))

(defun closest-keynum (pc keynums)
  "return the keynum in keynums closest to pc."
  (cdar (sort (mapcar (lambda (p) (cons (distance p pc) p)) keynums)
                 #'< :key #'first)))

;; returns the pitch in plst that is closest to the pc set
;; if multiple pitches in plst are the closest return the first
(defun find-closest (plst pc)
  "return the keynum in plst closest to pc"
  (cdar (sort (mapcar (lambda (p) (cons (distance p pc) p)) plst)
              #'< :key #'first)))

(defun move-chord (chord pc)
 "find shortest part movement from chord to pc"
  (loop with new-pitch with match with chdb = '()
        for pci = pc then (remove (pc new-pitch) pci :count 1)
        for chda = chord then (remove match chda :count 1)
        while chda
        do (progn
             (if (null pci) (setf pci pc))
             (setf match (find-closest chda pci))
             (setf new-pitch (if (> (random 1.0) .5)
                                 (quantize-low match pci)
                                 (pc-quantize match pci)))
             (cons new-pitch chdb))
        finally (return (sort chdb #'<))))

(defun scale-from-pc (pc-set)
"return the type of scale for a given pc-set or the pc-set itself if
no known scale is found."
  (let ((scale (mapcar #'pc (rest (differentiate pc-set)))))
    (loop for (name . ivs) in *scales*
          for result = (equal ivs scale)
          until result
          finally (return (or result pc-set)))))

(defun from-intervals (plst intervals)
"return a pc-set based on a list of intervals
plst is the seed for the progression
usually this will be a list with one element
"  (if (null intervals) (reverse plst)
      (from-intervals (cons (pc (+ (car plst) (car intervals))) plst)
                      (cdr intervals))))

(defun from-steps (keynum steps pc-set)
  "return a seq of keynums based on a list of steps, a beginning
keynum and a pc-set."
  (loop for step in steps
        for curr = (pc-relative keynum step pc-set) then (pc-relative curr step pc-set)
        collect curr))

;;; (from-steps 60 '(1 2 -1 -2) '(0 2 4 5 7 9 11)) -> (62 65 64 60)

;; attempts to return a reasonable scale based on the chord and root provided
(defun scale-from-chord (root chord)
  "attempts to return a reasonable scale based on the chord and root provided"
  (let ((res (find-if (lambda (v) (equal (car v) chord))
                      (mapcar (lambda (scale) (cons (intersection chord scale) scale))
                              (mapcar (lambda (type) (pc-scale root type))
                                      '(ionian aeolian mixolydian lydian phrygian locrian
                                        dorian lydian-mixolydian wholetone chromatic))))))
    (if (consp res) (cdr res) chord)))

#|
(defun melody-by-step (starting-pitch steps pc-set &rest args)
  "generate a melody from a list of steps in a pc-set"
  (if (null steps)
      (reverse (car args))
      (if (null args)
          (melody-by-step starting-pitch steps pc-set (list starting-pitch))
          (melody-by-step (pc-relative starting-pitch (car steps) pc-set)
                          (cdr steps)
                          pc-set
                          (cons (pc-relative starting-pitch (car steps) pc-set) (car args))))))
|#

(defun melody-by-step (starting-pitch steps pc-set)
  "generate a melody from a list of steps in a pc-set"
  (loop
    for step in steps
    with curr = starting-pitch
    collect (setf curr (pc-relative curr step pc-set))))

;;; (melody-by-step 60 '(1 2 -1 -2) '(0 2 4 5 7 9 11))

(defun melody-by-ivl (starting-pitch ivls)
  "generate a melody from a list of intervals in a pc-set"
  (loop
    for iv in ivls
    with curr = starting-pitch
    collect (setf curr (+ curr iv))))

#|

;; base drums      20-29 - bd
;; snare drums     30-39 - sd
;; hats closed     40-44 - hc
;; hats open       45-49 - ho
;; crash cymbols   50-54 - cc
;; ride cymbols    55-59 - rc
;; high toms       60-64 - ht
;; mid toms        65-70 - m5
;; low toms        70-74 - lt
;; rim shots       75-79 - rs
;; cow bell        80-84 - cb
;; clap            85-89 - cp
;; maracca         90-93 - ma

;; base drums 20-29
(define bd0 20)
(define bd1 21)
(define bd2 22)
(define bd3 23)
(define bd4 24)
(define bd5 25)
(define bd6 26)
(define bd7 27)
(define bd8 28)
(define bd9 29)
;; snare drums 30-39
(define sd0 30)
(define sd1 31)
(define sd2 32)
(define sd3 33)
(define sd4 34)
(define sd5 35)
(define sd6 36)
(define sd7 37)
(define sd8 38)
(define sd9 39)
;; hats closed 40-44
(define hc0 40)
(define hc1 41)
(define hc2 42)
(define hc3 43)
(define hc4 44)
;; hats open 45-49
(define ho0 45)
(define ho1 46)
(define ho2 47)
(define ho3 48)
(define ho4 49)
;; crash cymbols 50-54
(define cc0 50)
(define cc1 51)
(define cc2 52)
(define cc3 53)
(define cc4 54)
;; ride cymbols 55-59
(define rc0 55)
(define rc1 56)
(define rc2 57)
(define rc3 58)
(define rc4 59)
;; high toms 60-64
(define ht0 60)
(define ht1 61)
(define ht2 62)
(define ht3 63)
(define ht4 64)
;; mid toms 65-69
(define mt0 65)
(define mt1 66)
(define mt2 67)
(define mt3 68)
(define mt4 69)
;; low toms 70-74
(define lt0 70)
(define lt1 71)
(define lt2 72)
(define lt3 73)
(define lt4 74)
;; rim shots 75-79
(define rs0 75)
(define rs1 76)
(define rs2 77)
(define rs3 78)
(define rs4 79)
;; cow bell 80-84
(define cb0 80)
(define cb1 81)
(define cb2 82)
(define cb3 83)
(define cb4 84)
;; clap 85-89
(define cp0 85)
(define cp1 86)
(define cp2 87)
(define cp3 88)
(define cp4 89)
;; maracca 90-92
(define ma0 90)
(define ma1 91)
(define ma2 92)
;; tambourine 93-95
(define tb0 93)
(define tb1 94)
(define tb2 95)



(define __ -1) ;; rest

(define c0 (- 60 48))
(define c#0 (- 61 48))
(define db0 (- 61 48))
(define d0 (- 62 48))
(define d#0 (- 63 48))
(define eb0 (- 63 48))
(define e0 (- 64 48))
(define f0 (- 65 48))
(define f#0 (- 66 48))
(define gb0 (- 66 48))
(define g0 (- 67 48))
(define g#0 (- 68 48))
(define ab0 (- 68 48))
(define a0 (- 69 48))
(define a#0 (- 70 48))
(define bb0 (- 70 48))
(define b0 (- 71 48))
(define cb0 (- 71 48))

(define c1 (- 60 36))
(define c#1 (- 61 36))
(define db1 (- 61 36))
(define d1 (- 62 36))
(define d#1 (- 63 36))
(define eb1 (- 63 36))
(define e1 (- 64 36))
(define f1 (- 65 36))
(define f#1 (- 66 36))
(define gb1 (- 66 36))
(define g1 (- 67 36))
(define g#1 (- 68 36))
(define ab1 (- 68 36))
(define a1 (- 69 36))
(define a#1 (- 70 36))
(define bb1 (- 70 36))
(define b1 (- 71 36))
(define cb1 (- 71 36))

(define c2 (- 60 24))
(define c#2 (- 61 24))
(define db2 (- 61 24))
(define d2 (- 62 24))
(define d#2 (- 63 24))
(define eb2 (- 63 24))
(define e2 (- 64 24))
(define f2 (- 65 24))
(define f#2 (- 66 24))
(define gb2 (- 66 24))
(define g2 (- 67 24))
(define g#2 (- 68 24))
(define ab2 (- 68 24))
(define a2 (- 69 24))
(define a#2 (- 70 24))
(define bb2 (- 70 24))
(define b2 (- 71 24))
(define cb2 (- 71 24))

(define c3 (- 60 12))
(define c#3 (- 61 12))
(define db3 (- 61 12))
(define d3 (- 62 12))
(define d#3 (- 63 12))
(define eb3 (- 63 12))
(define e3 (- 64 12))
(define f3 (- 65 12))
(define f#3 (- 66 12))
(define gb3 (- 66 12))
(define g3 (- 67 12))
(define g#3 (- 68 12))
(define ab3 (- 68 12))
(define a3 (- 69 12))
(define a#3 (- 70 12))
(define bb3 (- 70 12))
(define b3 (- 71 12))
(define cb3 (- 71 12))

(define c4 60)
(define c#4 61)
(define db4 61)
(define d4 62)
(define d#4 63)
(define eb4 63)
(define e4 64)
(define f4 65)
(define f#4 66)
(define gb4 66)
(define g4 67)
(define g#4 68)
(define ab4 68)
(define a4 69)
(define a#4 70)
(define bb4 70)
(define b4 71)
(define cb4 71)

(define c5 (+ 60 12))
(define c#5 (+ 61 12))
(define db5 (+ 61 12))
(define d5 (+ 62 12))
(define d#5 (+ 63 12))
(define eb5 (+ 63 12))
(define e5 (+ 64 12))
(define f5 (+ 65 12))
(define f#5 (+ 66 12))
(define gb5 (+ 66 12))
(define g5 (+ 67 12))
(define g#5 (+ 68 12))
(define ab5 (+ 68 12))
(define a5 (+ 69 12))
(define a#5 (+ 70 12))
(define bb5 (+ 70 12))
(define b5 (+ 71 12))
(define cb5 (+ 71 12))

(define c6 (+ 60 24))
(define c#6 (+ 61 24))
(define db6 (+ 61 24))
(define d6 (+ 62 24))
(define d#6 (+ 63 24))
(define eb6 (+ 63 24))
(define e6 (+ 64 24))
(define f6 (+ 65 24))
(define f#6 (+ 66 24))
(define gb6 (+ 66 24))
(define g6 (+ 67 24))
(define g#6 (+ 68 24))
(define ab6 (+ 68 24))
(define a6 (+ 69 24))
(define a#6 (+ 70 24))
(define bb6 (+ 70 24))
(define b6 (+ 71 24))
(define cb6 (+ 71 24))

(define c7 (+ 60 36))
(define c#7 (+ 61 36))
(define db7 (+ 61 36))
(define d7 (+ 62 36))
(define d#7 (+ 63 36))
(define eb7 (+ 63 36))
(define e7 (+ 64 36))
(define f7 (+ 65 36))
(define f#7 (+ 66 36))
(define gb7 (+ 66 36))
(define g7 (+ 67 36))
(define g#7 (+ 68 36))
(define ab7 (+ 68 36))
(define a7 (+ 69 36))
(define a#7 (+ 70 36))
(define bb7 (+ 70 36))
(define b7 (+ 71 36))
(define cb7 (+ 71 36))

(define c8 (+ 60 48))
(define c#8 (+ 61 48))
(define db8 (+ 61 48))
(define d8 (+ 62 48))
(define d#8 (+ 63 48))
(define eb8 (+ 63 48))
(define e8 (+ 64 48))
(define f8 (+ 65 48))
(define f#8 (+ 66 48))
(define gb8 (+ 66 48))
(define g8 (+ 67 48))
(define g#8 (+ 68 48))
(define ab8 (+ 68 48))
(define a8 (+ 69 48))
(define a#8 (+ 70 48))
(define bb8 (+ 70 48))
(define b8 (+ 71 48))
(define cb8 (+ 71 48))
|#

;;
;; some midi helpers
;;

(defparameter *tuning-base* 440.0)

(defun midi2frq (keynum)
  (if (<= keynum 0) 0.0
      (* *tuning-base* (expt 2 (/ (- keynum 69) 12)))))

(defun frq2midi (freq)
  (if (<= freq 0) 0
      (+ (* 12 (log (/ freq *tuning-base*) 2)) 69)))
