;;; 
;;; perform.lisp
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

(defun play-note (time instr keynum amp dur &rest args)
;;;  (format t "time:~a, now: ~a~%" (samples->secs time) (samples->secs (now)))
  (apply (play-instrument instr)
         :time (samples->secs (- time (now))) :keynum keynum :amplitude amp
         :duration (samples->secs dur) args))

(defun rplay (beat instr keynum amp dur &rest args)
  "play one note of instr at beat in the future with specified params. Dur is in beat units"
;;;  (format t "beat:~a, now: ~a~%" beat (samples->secs (now)))
  (apply #'play-note (+ (now) (- (*metro* 'get-time beat)(*metro* 'get-time 0))) instr keynum (max 0 (min 127 amp))
         (- (*metro* 'get-time (+ beat dur))
            (*metro* 'get-time beat))
         args))

#|
(defun rplay (beat instr keynum amp dur &rest args)
  "play one note of instr at beat in the future with specified params. Dur is in beat units"
  (format t "beat:~a, time: ~a~%" beat (*metro* 'get-time (*metro* 'get-beat beat)))
  (apply #'play-note (*metro* 'get-time (*metro* 'get-beat beat)) instr keynum (max 0 (min 127 amp))
         (- (*metro* 'get-time (+ beat dur))
            (*metro* 'get-time beat))
         args))

(defun rplay (beat instr keynum amp dur &rest args)
  "play one note of instr at beat in the future with specified params. Dur is in beat units"
  (apply #'play-note (*metro* 'get-time (*metro* 'get-beat beat)) instr keynum (max 0 (min 127 amp))
         (- (*metro* 'get-time (+ beat dur))
            (*metro* 'get-time beat))
         args))
|#

(defun play (beat instr keynum amp dur &rest args)
  "play one note of instr at beat in the future with specified params. Dur is in beat units"
  (unless (member keynum '(_ ~))
    (apply #'play-note (*metro* 'get-time beat) instr keynum (max 0 (min 127 amp))
           (- (*metro* 'get-time (+ beat dur))
              (*metro* 'get-time beat))
           args)))

;;;; (*metro* 'get-time (*metro* 'get-beat 1/4))
;;; (play 0.5 :piano 60 80 1 :channel )


(format t "playing!~%")

#|
(let ((beat (*metro* 'get-beat 1)))
  (play beat :tenor-sax 60 80 1/2 :channel 2))
|#

(defun pplay (beat instr keynum amp dur &rest args)
  (declare (special LC LL LP))
  (let* ((keys (if (listp keynum) (first keynum) keynum))
         (dur (* (getf args :durfactor 1) (get-durfactor keynum) dur)))
    (remf args :durfactor)
    (map nil (lambda (key) (apply #'play-note (*metro* 'get-time beat) instr key (max 0 (min 127 amp))
                                 (- (*metro* 'get-time (+ beat dur))
                                    (*metro* 'get-time beat))
                                 args))
         (cond
           ((vectorp keys) (coerce keys 'list))
           ((listp keys) (let ((len (length keys)))
                           (loop
                             with dur = (/ dur len)
                             for idx below len
                             do (apply #'play (+ beat (* idx dur)) instr (rotate keys idx) amp dur args))))
           ((member keys '(_ ~)) nil)
           (:else (list keys))))))

#|
(defmacro play (time instr keynum amp dur)
  `(play-note (*metro* 'get-time ,time) ,instr ,keynum (max 0 (min 127 ,amp))
                (- (*metro* 'get-time (+ beat ,dur))
                   (*metro* 'get-time beat))))
|#
