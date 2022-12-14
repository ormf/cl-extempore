;;; 
;;; metro.lisp
;;;
;;; translation/adaption of parts of Andrew Sorensen's code from
;;; extempore (https://github.com/digego/extempore) to Common Lisp
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

;;; (defparameter *samplerate* 48000d0)

;; creates a metronome object
;; metro is basically a linear function that returns
;; a time in absolute samples when given a time in beats.
;;
;; metro is instantiated with a starting tempo.
;; you can call the metro with the following symbols
;;
;; 'get-time ; which is also the default
;; 'get-beat
;; 'get-tempo
;; 'set-tempo
;; 'get-cycle
;; 'set-cycle
;; 'pos
;; 'dur
;

(defmacro rational->real (&rest args)
  `(float ,@args 1.0))

(defmacro real->rational (&rest args)
  `(rationalize ,@args))

(defun make-metro (start-tempo &rest args)
  (let* ((offset (if (null args) (now) (caar args)))
         (cycle 4)
         (mark offset)
         (loffset 0.0)
         (total-beats (if (null args) 0 (cdar args)))
         (cycle-beats total-beats)
         (g-tempo (/ 60 start-tempo))
         (beat-pos (lambda (x1 y1 x2 y2)
                     (let* ((m (if (zerop (- x2 x1)) 0 (/ (- y2 y1) (- x2 x1))))
                            (c (- y1 (* m x1))))
                       (lambda (time)
                         (+ (* time m) c)))))
         (beat-env (funcall beat-pos mark total-beats (+ mark (* g-tempo *samplerate*)) (+ total-beats 1)))
         (samp-env (funcall beat-pos total-beats mark (+ total-beats 1) (+ mark (* g-tempo *samplerate*)))))
    (lambda (sym &rest args)
      (cond ((numberp sym) (+ (funcall samp-env sym) loffset))
            ((eq sym 'get-mark) (cons mark total-beats))
            ((eq sym 'get-time) (+ (funcall samp-env (car args)) loffset)) ;mark))
            ((eq sym 'get-cycle) cycle)
            ((eq sym 'get-cycle-mark) cycle-beats)
            ((eq sym 'set-cycle)
             (setf cycle-beats (cadr args))
             (setf cycle (car args)))
            ((eq sym 'pos) (mod (- (car args) cycle-beats) cycle))
            ((eq sym 'beat-at-time) (float (funcall beat-env (car args)) 1.0))
            ((eq sym 'set-tempo)
             (let ((time (if (rest args) (cadr args) (now))))
               (if (or (null (cdr args))
                       (null (cddr args)))
                   (setf total-beats
                         (+ total-beats (/ (- time mark)
                                           (* *samplerate* g-tempo))))
                   (setf total-beats (caddr args)))
               (setf g-tempo (/ 60 (car args)))
               (setf mark time)
               (setf samp-env (funcall beat-pos total-beats
                                        mark
                                        (+ total-beats 1)
                                        (+ mark (* g-tempo *samplerate*))))
               (setf beat-env (funcall beat-pos mark
                                        total-beats
                                        (+ mark (* g-tempo *samplerate*))
                                        (+ total-beats 1)))
               (car args)))
            ((eq sym 'get-tempo) (* (/ 1 g-tempo) 60))
            ((eq sym 'dur) (* *samplerate* g-tempo (car args)))
            ((eq sym 'push) (setf loffset (+ loffset 256)))
            ((eq sym 'pull) (setf loffset (- loffset 256)))
            ((eq sym 'get-beat)
             (progn
               (let ((val (+ total-beats
                             (/ (- (now) mark)
                                (* *samplerate* g-tempo))))
                     (quantize (if (or (null args) (<= (car args) 0)) 1.0 (car args))))
                 (rationalize (+ val (- quantize (mod val quantize)))))))
            (:else 'bad-method-name)))))
