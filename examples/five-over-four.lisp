;;; 
;;; five-over-four.lisp
;;;
;;; Reimplementation of Andrew Sorenson's "Five over Four"
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

(defun f2 (beat notes dur)
  (if (> (random 1.0) 0.5)
      (playp *scale*
             dur 0 :flute (r-elt '(0 0 -12))
             notes (cosr 50 30 0.5)
             (+ 0.1 (/ dur (length notes))) :channel 6))
  (at (*metro* (+ beat (* 0.5 dur))) #'f2 (+ beat dur)
      (take (+ 3 (random 4))
            (rotate `(60 63 67 ,(r-elt '(72 74 75)) 70 58)
                    (* -1 (random 6))))
      (r-elt '(1 2))))

(f2 (*metro* 'get-beat 4) `(60 63 67 ,(r-elt '(72 74 75)) 70 58) 2)

(defun f2 (beat notes dur))

;;; (untrace)

;;;; (play 0 :electric-piano-1 60 60 1 :channel 3)


(progn
  (setf *root* 0)
  (setf *scale* (pc-scale *root* 'aeolian)))

(defun f1 (beat dur)
 (format t "beat: ~4,2f~%" (float beat 1.0))
  (onbeat? 1 20
           (progn
             (setf *root* (r-elt (remove *root* '(-2 0 3 9))))
             (setf *scale* (pc-scale *root* 'aeolian))
                (map nil (lambda (p) (play_delta (random 3) :electric-piano-1 p 60 10 :channel 3))
                     (pc-make-chord 50 80 4 (pc-chord *root* '-7)))))
  (playp *scale* 5/4 0 :fingered-bass -24 `(60 _ 72) 110 (* 3.0 dur) :channel 1)
;;;  (play 0 :piano (+ *root* 48) 70 1/5 :channel 7)
  (playp *scale* 5/4 0 :vibraphone 0 `(60 63 67) (cosr 60 30 1) (* 4.9 dur) :channel 2)
  (format t "~a~%" beat)
  (let ((v 50))
    ;; (playp *scale* 15 0 :tenor-sax 0
    ;;        `(60 63 67 ,(r-elt '(72 74)) 70 58) v (* 10 dur) :channel 6)
    ;; (playp *scale* 15 0 :tenor-sax -8
    ;;        `(60 63 67 ,(r-elt '(68 72 74)) 70 58) v (* 10 dur) :channel 5)
    ;; (playp *scale* 10 0 :tenor-sax -8
    ;;        `(60) v (* 60 dur) :channel 5)
    ;; (playp *scale* 15 3 :trombone 0
    ;;        `(60 63 67 ,(r-elt '(68 72 74)) 70 58) v (* 10 dur) :channel 5)
    )
  (at (*metro* (+ beat (* 0.5 dur))) #'f1 (+ beat dur) dur))

(f1 (*metro* 'get-beat 1) 1/4)

;;; (onbeat?)


#|
(loop for chan below 16
      do (set-channel-pgm chan 0 :force t))
|#
