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

(defun flatten (seq &key (test #'atom) (key #'identity))
  "remove all brackets except the outmost in seq."
  (flatten-fn seq :test test :key key))

(defun nconc-get-size (list)
  (labels ((inner-loop (x n head)
             (if (null (cdr x))
                 (progn
                   (setf (cdr x) head)
                   (incf n))
                 (inner-loop (cdr x) (incf n) head))))
    (inner-loop list 0 list)))

;; (defparameter test '(1 2 3))
;; (nconc-get-size test)
;; (loop for x in test for z from 1 to 5 collect x)

;; new rotate function which only traverses the list one time,
;; determining the length and nconcing it in the same step. It uses
;; nconc-get-size as helper function. Function ist non-destructive: It
;; uses a local copy of list instead of the original.

(defun rotate (list &optional (num 1))
  "rotate a list by n elems. n can be negative. If n is larger than
the list size it will wrap around as if the rotation was called
recursively n times."
  (let* ((new-list (copy-tree list))
         (num-normalized (mod num (nconc-get-size new-list)))) ;; innermost function nconcs new-list to an endless list and gets size in one step
    (if (zerop num-normalized) list
      (let* ((newlast (nthcdr (- num-normalized 1) new-list)) ;; determine the new last element of the list (wraps around for nums > (length list) or < 0) and store into variable
             (newfirst (cdr newlast))) ;; the new listhead is the cdr of the last element; also stored in variable
        (setf (cdr newlast) nil) ;; set the cdr of the new last element to nil
        (values newfirst))))) ;; return the new first element (listhead)

(defun shuffle (seq &key (start 0) (end (length seq))
                (state *random-state*) (copy t) &aux
                (width (- end start)))
  (if (< width 2)
      seq
      (progn (when copy (setf seq (copy-list seq)))
             (loop for i from start to (- end 1)
                   for j = (+ start (random width state))
                   for v = (elt seq i)
                   do (setf (elt seq i) (elt seq j))
                      (setf (elt seq j) v))
                   seq)))

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

(defmacro onbeat? (b of tval &rest f)
  (if (null f)
      `(if (= (mod beat ,of) (- ,b 1))
           ,tval)
      `(if (= (mod beat ,of) (- ,b 1))
           ,tval ,(car f))))

;;; (play-note (*metro* 'get-beat 1) :fingered-bass 36 80 *samplerate* :channel 2)

#|

(defun redefine-keys (sym keys dur)
  (append `((,sym ,(if (listp keys) (first keys) keys)))
          (list (list 'dur (if (and (listp keys) (or (numberp (first keys)) (vectorp (first keys))) (eq (second keys) '~))
                               (* dur (loop for x in (cddr keys) for num from 2 while (eq x '~) finally (return num))) dur)))))

(let ((dur 4)
      (keynum '(64 ~ ~)))
  (with-special-chars (keys keynum)
                      (play-note dur)))

(defmacro with-special-chars ((sym keys dur) &rest body)
  `(let ,(redefine-keys sym keys dur)
     ,@body))

(with-special-chars (keys (60 ~ #(72 75) (60 61 62)) 4)
                    (if keys
                        (map nil (lambda (key) (play-note (*metro* 'get-time beat) instr key (max 0 (min 127 amp))
                                                     (- (*metro* 'get-time (+ beat dur))
                                                        (*metro* 'get-time beat))))
                             (cond
                               ((vectorp keys) (coerce keys 'list))
                               ((listp keys) (let* ((len (length keys))
                                                    (dur (/ dur len)))
                                               (map nil (lambda (key o) (play (+ beat o) instr key amp dur))
                                                    keys
                                                    (mapcar (lambda (x) (* x dur)) (range 0 len)))))
                               ((member keys '(_ ~)) nil)
                               (:else (list keys))))))
|#

#|
(defun play (beat instr keynum amp dur)
  (declare (special LC LL LP))
  (with-special-chars (keys keynum dur)
    (map nil (lambda (key) (play-note (*metro* 'get-time beat) instr key (max 0 (min 127 amp))
                                 (- (*metro* 'get-time (+ beat dur))
                                    (*metro* 'get-time beat))))
         (cond
           ((vectorp keys) (coerce keys 'list))
           ((listp keys) (let* ((len (length keys))
                                (dur (/ dur len)))
                           (map nil (lambda (key o) (play (+ beat o) instr key amp dur))
                                keys
                                (mapcar (lambda (x) (* x dur)) (range 0 len)))))
           ((member keys '(_ ~)) nil)
           (:else (list keys))))))
|#

(defun get-durfactor (keynum)
  (let ((keys (first keynum)))
    (if (and (listp keynum) (or (numberp keys) (vectorp keys)) (eq (second keynum) '~))
        (loop for x in (cddr keynum) with num = 2 while (eq x '~) do (if (eq x '~) (incf num)) finally (return num))
        1)))

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
