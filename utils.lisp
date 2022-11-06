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

(defun samples->seks (samples)
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
   (funcall (symbol-function (intern (string-upcase (format nil "~a" instr))))
            :time (samples->seks (- time (now))) :keynum keynum :amplitude amp
            :duration (samples->seks dur))))
