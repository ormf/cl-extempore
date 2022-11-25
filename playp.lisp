;;; 
;;; playp.lisp
;;;
;;; translated from instruments-scm.xtm (c) by Andrew Sorensen
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

(defun playp_play_list (beat dur pclas inst vols durs lst mod_diff step offset poffset args)
  (let ((lst_idx 0)
        (duration 0) (volume 0)
        (phase 0))
    (map nil (lambda (x tval)
                (if (listp durs)
                    (if (and (symbolp (car durs))
                             (boundp (car durs))
                             (or (functionp (eval (car durs)))
                                 (macro-function (eval (car durs)))))
                        (setf duration durs)
                        (if (= (length durs) (length lst))
                            (setf duration (elt durs lst_idx))
                            (setf duration step)))
                    (setf duration durs))
                (if (listp vols)
                    (if (and (symbolp (car vols))
                             (boundp (car vols))
                             (or (functionp (eval (car vols)))
                                 (macro-function (eval (car vols)))))
                        (setf volume vols)
                        (if (= (length vols) (length lst))
                            (setf volume (elt vols lst_idx))
                            (setf volume 80)))
                    (setf volume vols))
                (if (listp x)
                    (playp_play_list beat dur pclas inst volume
                                     duration x mod_diff (/ step (length lst)) (+ tval offset) poffset args)
                    (unless (member x '(_ ~))
                      (if (> x 0)
                          (progn
                            (setf phase (+ mod_diff tval offset))
                            (apply #'play phase ;(+ mod_diff tval offset)
                                   inst
                                   (pc-quantize (+ x poffset) pclas)
                                   volume
                                   duration
                                   args)))))
                (incf lst_idx))
              lst
              (range 0 step (/ step (length lst))))))

(defun playp_f (beat dur &rest args)
  (let ((pclas '(0 1 2 3 4 5 6 7 8 9 10 11))
        (offset 0)
        (poffset 0)
        (inst '())
        (data '())
        (vols '())
        (durs '())
        (datal 0)
        (cycle 0)
        (step 0))
    ;; check for quantizer list
    (if (listp (car args)) (setf pclas (pop args)))
    ;; now cycle
    (if (keywordp (car args))
        (setf cycle dur)
        (setf cycle (pop args)))
    ;; if no instrument must be an offset
    (if (not (keywordp (car args)))
        (setf offset (pop args)))
    ;; now instrument (which should be a closure!)
    (setf inst (pop args))
    ;; if not pitch list must be offset
    (if (not (listp (car args)))
        (setf poffset (pop args)))
    ;; now must be pitch list
    (setf data (pop args))
    (setf datal (length data))
    (setf vols (pop args))
    (setf durs (pop args))
    (setf step (/ cycle datal))
    (let ((local_beat (mod beat cycle))
          (mod_diff 0)
          (volume vols)
          (phase 0.0)
          (duration durs)
          (pitch 0))
      (dotimes (i datal)
        (setf mod_diff (+ (* i step) local_beat))
        (setf pitch (elt data (mod i datal)))
;;;        (break "datal: ~a, i: ~a, mod_diff: ~a, pitch: ~a, dur: ~a" datal i mod_diff pitch dur)
        (if (listp durs)
            (if (and (symbolp (car durs))
                     (boundp (car durs))
                     (or (functionp (eval (car durs)))
                         (macro-function (eval (car durs)))))
                (setf duration durs)
                (if (= (length durs) datal)
                    (setf duration (elt durs (mod i datal)))
                    (setf duration step))))
        (if (listp vols)
            (if (and (symbolp (car vols))
                     (boundp (car vols))
                     (or (functionp (eval (car vols)))
                         (macro-function (eval (car vols)))))
                (setf volume vols)
                (if (= (length vols) datal)
                    (setf volume (elt vols (mod i datal)))
                    (setf volume 80))))
        (if (listp pitch)
            (if (and (>= mod_diff 0)
                     (< mod_diff dur)
                     (not (null pitch)))
                (playp_play_list beat dur pclas inst volume duration pitch mod_diff step offset poffset args))
            (progn
              (setf phase (+ mod_diff offset))
              (if (and (not (member pitch '(_ ~)))
                       (>= mod_diff 0)
                       (< mod_diff dur)
                       (> pitch 0))
                  (progn
                    (apply #'play phase ;(+ mod_diff offset)
                           inst
                           (pc-quantize (+ pitch poffset) pclas)
                           volume
                           duration
                           args)))))))))


(defun playp_f (beat dur &rest args)
  (let ((pclas '(0 1 2 3 4 5 6 7 8 9 10 11))
        (offset 0)
        (poffset 0)
        (inst '())
        (data '())
        (vols '())
        (durs '())
        (datal 0)
        (cycle 0)
        (step 0))
    ;; check for quantizer list
    (if (listp (car args)) (setf pclas (pop args)))
    ;; now cycle
    (if (keywordp (car args))
        (setf cycle dur)
        (setf cycle (pop args)))
    ;; if no instrument must be an offset
    (if (not (keywordp (car args)))
        (setf offset (pop args)))
    ;; now instrument (which should be a closure!)
    (setf inst (pop args))
    ;; if not pitch list must be offset
    (if (not (listp (car args)))
        (setf poffset (pop args)))
    ;; now must be pitch list
    (setf data (pop args))
    (setf datal (length data))
    (setf vols (pop args))
    (setf durs (pop args))
    (setf step (/ cycle datal))
    (let ((local_beat (mod beat cycle))
          (mod_diff 0)
          (volume vols)
          (phase 0.0)
          (duration durs)
          (pitch 0))
      ;; (format t "cycle: ~a, offset: ~a, local_beat: ~a, data: ~a, durs: ~a, vols: ~a, step: ~a~%"
      ;;         cycle offset local_beat data durs vols step)
      (dotimes (i (* 1 datal))
        (setf mod_diff (- (* i step) local_beat))
        (setf pitch (elt data (mod i datal)))
        ;; (format t "i: ~a, mod_diff: ~a, pitch: ~a, dur: ~a, " i mod_diff pitch dur)
        (if (listp durs)
            (if (and (symbolp (car durs))
                     (boundp (car durs))
                     (or (functionp (eval (car durs)))
                         (macro-function (eval (car durs)))))
                (setf duration durs)
                (if (= (length durs) datal)
                    (setf duration (elt durs (mod i datal)))
                    (setf duration step))))
        (if (listp vols)
            (if (and (symbolp (car vols))
                     (boundp (car vols))
                     (or (functionp (eval (car vols)))
                         (macro-function (eval (car vols)))))
                (setf volume vols)
                (if (= (length vols) datal)
                    (setf volume (elt vols (mod i datal)))
                    (setf volume 80))))
        (if (listp pitch)
            (progn
             (if (and (>= mod_diff 0)
                      (< mod_diff dur)
                      (not (null pitch)))
                 (playp_play_list beat dur pclas inst volume duration pitch mod_diff step offset poffset args)))
            (progn
             (setf phase (+ mod_diff offset))
             (if (and (not (member pitch '(_ ~)))
                      (>= mod_diff 0)
                      (< mod_diff dur)
                      (> pitch 0))
                 (progn
;;;                   (format t "~&playing: ~a at ~a~%" (pc-quantize (+ pitch poffset) pclas) phase)
                   (apply #'rplay phase ;(+ mod_diff offset)
                               inst
                               (pc-quantize (+ pitch poffset) pclas)
                               volume
                               duration
                               args))
                 (progn))))))))

(defmacro playp (&rest args)
  `(playp_f beat dur ,@args))
