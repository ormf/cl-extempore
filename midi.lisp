;;; 
;;; midi.lisp
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

(defvar *midi-in1* (jackmidi:open :direction :input))
(defvar *midi-out1* (jackmidi:open :direction :output))

(defun midi-pgm-out (pgm chan &key (stream *midi-out1*))
  (jackmidi:write stream (jackmidi:data (+ #xC0 chan) pgm)))

(defun midi-play-note (&rest args)
  (let ((stream (getf args :stream *midi-out1*))
        (time (getf args :time 0))
        (keynum (getf args :keynum 60))
        (duration (getf args :duration 0.5))
        (chan (1- (getf args :channel 1)))
        (amp (let ((a (getf args :amplitude 0.5)))
               (if (integerp a) a (min 127 (max 0 (round (* a 127)))))) ))
    (at (+ (now) (secs->samples time))
        #'jackmidi:write stream (jackmidi:data (+ #x90 chan) keynum amp))
    (at (+ (now) (secs->samples (+ time duration)))
        #'jackmidi:write stream (jackmidi:data (+ #x80 chan) keynum 0))))

(defun midi-panic (&optional (stream *midi-out1*))
  (dotimes (chan 16) (jackmidi:write stream (jackmidi:data (+ #xB0 chan) 123 127))))

#|
(midi-play-note :time 0 :keynum 60 :channel 1)
|#

