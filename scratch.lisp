;;; 
;;; scratch.lisp
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

;;; (*metro* 'get-beat 4)
;;; (sprout (new midi :time 0))

(define-metro *metro* 120)
(*metro* 'set-tempo 120)
(defparameter *phaseshift* nil)

(setf *phaseshift* 0.001)
(setf *phaseshift* 0.0)

(defun model6 (beat riff phase dur)
  (play beat :piano1 (first riff) (cosr 100 20 15/3) (* 4 dur))
  (play (+ beat phase) :piano2 (first riff) (cosr 100 20 17/3) (* 4 dur))
  (at (*metro* (+ beat (* 0.5 dur))) #'model6 (+ beat dur)
      (rotate riff -1)
      (+ phase *phaseshift*)
      dur))

(model6 (*metro* 'get-beat 4) (list 60 62 67 69 70 62 60 70 69 62 70 69) 0.0 1/4)

(define-metro *metro* 120)

(defparameter *riff* '(60 62 67 69 70 62 60 70 69 62 70 69))

(defparameter *phaseshift* 0.001)

(setf *phaseshift* 0)
(setf *phaseshift* 0.001)

(defun model6 (beat riff phase dur)
  (play beat :piano1 (first riff) (cosr 80 50 1/3 :phase 0.5) dur)
  (play beat :piano2 (first riff) (cosr 80 50 1/3 :phase 0) dur)
  (at (*metro* (+ beat (* 0.5 dur))) #'model6
      (+ beat dur)
      (rotate riff -1)
      phase
      dur))

(defun model6 (beat riff phase dur)
  (play beat :piano1 (first riff) 80 dur)
  (at (*metro* (+ beat (* 0.5 dur))) #'model6
      (+ beat dur)
      (rotate riff -1)
      phase
      dur))

(setf *phaseshift* 0)
(setf *phaseshift* 0.001)

(defun model6 (beat riff phase dur)
  (play beat :piano1 (first riff) (cosr 80 0 1/3 :phase 0.5) dur)
  (play (+ beat phase) :piano2 (first riff) (cosr 80 0 1/3 :phase 0) dur)
  (at (*metro* (+ beat (* 0.5 dur))) #'model6
      (+ beat dur)
      (rotate riff -1)
      (+ phase *phaseshift*)
      dur))

(defun model6 (beat riff phase dur)
  (play beat :piano1 (first riff) (cosr 60 20 1/7) (* 0.5 dur))
  (play (+ beat phase) :piano2 (first riff) (cosr 60 20 1/15) (* 0.5 dur))
  (at (*metro* (+ beat (* 0.5 dur))) #'model6
      (+ beat dur)
      (rotate riff -1)
      (+ phase *phaseshift*)
      dur))

(model6 (*metro* 'get-beat 1) *riff* 0 1/4)

(model6 (*metro* 'get-beat 1) (cdr (mapcar (lambda (x) (+ x -7)) (cdr *riff*))) 0 1/4)

(model6 (*metro* 'get-beat 1) (cdr (mapcar (lambda (x) (+ x 7)) (subseq *riff* 2 11))) 0 1/4)

(model6 (*metro* 'get-beat 1) (list 63 65 57 53 58) 0 1/4)

(*metro* 2)

(*metro* 2.5)

(defun model6 (a b c d))




(let ((beat (*metro* 'get-beat)))
  (samples->secs (- (*metro* 'get-time (+ beat 1/4))
                    (*metro* 'get-time beat))))











(defun model6 (beat riff phase dur)
  (play beat :piano1 (first riff) (cosr 100 20 5/3) dur)
  (play (+ beat phase) :piano2 (first riff) (cosr 100 20 7/3) dur)
  (at (*metro* (+ beat (* 0.5 dur))) #'model6 (+ beat dur)
      (rotate riff -1)
      (+ phase 0)
      dur))

(defun model6 (beat riff phase dur)
  (declare (ignore beat riff phase dur)))
