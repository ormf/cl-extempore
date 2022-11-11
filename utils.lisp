;;; 
;;; utils.lisp
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

(setf *print-case* :downcase)
(defparameter *samplerate* incudine.scratch::*sample-rate*)

(defun rt-start ()
  (prog1 (incudine.scratch::rt-start)
    (setf *samplerate* incudine.scratch::*sample-rate*)))

(defun rt-stop ()
  (prog1 (incudine.scratch::rt-stop)))

(defmacro define-metro (name tempo)
  (let ((metro (gensym "METRO")))
    `(progn
       (defparameter ,metro (make-metro ,tempo))
       (defun ,name (&rest args)
         (apply ,metro args)))))

(defun secs->samples (secs)
  (* secs *samplerate*))

(defun samples->secs (samples)
  (float (/ samples *samplerate*) 1.0))

(defparameter *instruments*
  '(:piano (:type midi :channel 0 :pgm 0)))

(defun piano (&rest args)
  (apply #'make-instance 'midi :time (getf args :time) args))

(defun piano1 (&rest args)
  (apply #'make-instance 'midi :time (getf args :time) :channel 0 args))

(defun piano2 (&rest args)
  (apply #'make-instance 'midi :time (getf args :time) :channel 1 args))

(defun play-note (time instr keynum amp dur)
  (sprout
   (funcall (symbol-function (intern (string-upcase (format nil "~a" instr)) 'cl-extempore))
            :time (samples->secs (- time (now))) :keynum keynum :amplitude amp
            :duration (samples->secs dur))))


(defmacro play (time instr keynum amp dur)
  `(play-note (*metro* 'get-time ,time) ,instr ,keynum (max 0 (min 127 ,amp))
                (- (*metro* 'get-time (+ beat ,dur))
                   (*metro* 'get-time beat))))

(defmacro cosr (mid dev beats-per-cycle &key (phase 0))
  `(round (+ ,mid (* ,dev (cos (+ (*,phase +TWOPI+) (* +TWOPI+ beat ,beats-per-cycle)))))))

(defmacro sinr (mid dev beats-per-cycle)
  `(round (+ ,mid (* ,dev (sin (* +TWOPI+ beat ,beats-per-cycle))))))

(defun range (low high &optional (step 1))
  (loop for i from low below high by step collect i))

(defun take (n seq)
  (subseq seq 0 n))

;;; (take 4 (range 2 10)) -> (2 3 4 5)

(defun zip (&rest seqs)
  (apply #'append (apply #'mapcar #'list seqs)))

;;; (zip '(1 2) '(3 4)) -> (1 3 2 4)

(defun pedal (e seq)
  (apply #'append (mapcar (lambda (x) (list e x)) seq)))

(defun jumble (seq)
  (shuffle seq))

(defun orbit (val mod then else)
  (if (zerop (mod val mod)) then else))

(defun orb (val mod then else)
  (if (zerop (mod val mod)) then else))
