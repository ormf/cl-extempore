;;; 
;;; carol-of-the-bells.lisp
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

(defparameter h1 (holder))

(:> s1 4 0 (play beat :cello @1 (cosr 60 10 7/4) dur :channel 1)
    `(,(scale 3 8 (r-elt '(-1 0 1 2)))
       ,(reverse (scale 3 8 (r-elt '(-1 0 1 2))))))

(:> p1 2 0 (play beat :Pizzicato-Strings @1 50 dur :channel 2) `(48 60))

(:> w1 3 0 (play beat :flute (if (numberp @1) (qnt (+ 12 @1)) '_) (cosr 60 10 7/4) dur :channel 3)
    `(,(hold h1 1 (r-elt '(((55 56) (58 60) (61 63) (65 67) 65 63)
                           (63 (62 63) 60 63 (62 63) 60)
                           (72 (72 72) (70 68) 67 (67 67) (65 63))
                           (65 (65 65) (67 65) 63 (62 63) 60)
                           (_) (_) (_) (_))))))

(:> w2 3 0 (play beat :flute (if (numberp @1) (qnt (+ 7 @1)) '_) (cosr 60 10 7/4) dur :channel 4)
    `(,(hold h1 1 (r-elt '(((55 56) (58 60) (61 63) (65 67) 65 63)
                           (63 (62 63) 60 63 (62 63) 60)
                           (72 (72 72) (70 68) 67 (67 67) (65 63))
                           (65 (65 65) (67 65) 63 (62 63) 60)
                           (_) (_) (_) (_))))))

(:> b1 3/2 0 (play beat :trombone (qnt @1) (cosr 40 20 7/3) dur :channel 5) `(60 (62 63) 60))
(:> b2 3/2 0 (play beat :french-horn (qnt (+ 3 @1)) (cosr 40 20 7/4) dur :channel 6) `(60 (62 63) 60))
(:> b3 3/2 0 (play beat :trumpet (qnt (+ 8 @1)) (cosr 40 20 7/2) dur :channel 7) `(60 (62 63) 60))




(dolist (sym '(p1 s1 b1 b2 b3 w1 w2))
  (fmakunbound sym))

(fmakunbound 'w2)
