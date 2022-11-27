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

(unless (find-package :cl-extempore) (ql:quickload "cl-extempore"))
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

(range 0 3)

(diatonic 0 '- 'ii)

(defun model3 (beat dur degree)
  (let ((chrd (make-chord-fixed 60 3 (diatonic 0 '^ degree))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano p 80 dur))
         chrd
         (range 0 3)))
  (at (*metro* (+ beat (* 0.5 dur))) #'model3
      (+ beat dur)
      3
      (r-elt (cdr (assoc degree '((i v vii ii iv vi)
                                  (iv i)
                                  (vi iv)
                                  (ii v)
                                  (v i vi)
                                  (vii i vi)))))))

(defun model3 (beat dur degree)
  (let ((chrd (make-chord-fixed 60 3 (diatonic 0 '^ degree))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano p 80 dur))
         chrd
         (range 0 3)))
  (at (*metro* (+ beat (* 0.5 dur))) #'model3
      (+ beat dur)
      3
      (r-elt (cdr (assoc degree '((i iv v)
                                  (iv v)
                                  (v i)))))))

(model3 (*metro* 'get-beat 4) 3 'i)

(*metro* 'set-tempo 140)


(defun model4 (beat dur seq)
  (play beat :piano (first seq) 80 dur)
  (at (*metro* (+ beat (* 0.5 dur))) #'model4
      (+ beat dur)
      dur
      (rotate seq)))

(model4 (*metro* 'get-beat 1) 1 '(60 62 63 64))

(defun alberti-fuer-doofe (beat dur degree)
  (let* ((new
             (r-elt (cdr
                     (assoc degree '((i iv v vi viio n6)
                                     (n6 viio v i)
                                     (viio i)
                                     (vi iv)
                                     (iv i)
                                     (v i vi))))))
         (chrd (make-chord-fixed 60 8 (diatonic 0 '^ new))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano (elt chrd p) 80 dur))
         (range 0 7)
         (range 0 7))
    (at (*metro* (+ beat (* 0.5 dur))) #'alberti-fuer-doofe
        (+ beat dur)
        8 new)))

(alberti-fuer-doofe (*metro* 'get-beat 4) 8 'i)

;;; (shuffle (range 0 5))

'((i iv v vi viio n6)
  (n6 viio v i)
  (viio i)
  (vi iv)
  (iv i)
  (v i vi))

'((i iv v vi viio n6)
                                     (n6 viio v i)
                                     (viio i)
                                     (vi iv)
                                     (iv i)
                                     (v i vi))

(diatonic 0 '- 'i)

(make-chord-fixed 60 6 (diatonic 0 '^ 'i))

(alberti-fuer-doofe (*metro* 'get-beat 4) 8 'i)

(defun alberti-fuer-doofe (beat dur degree)
  (let* ((new
             (r-elt (cdr
                     (assoc degree '((i iv v vi viio n6)
                                     (n6 viio v i)
                                     (viio i)
                                     (vi iv)
                                     (iv i)
                                     (v i vi))))))
         (chrd (make-chord-fixed 60 6 (diatonic 0 '^ new))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano (elt chrd p) 80 dur))
         '(0 2 1 2 0 2 1 2)
         (range 0 7))
    (at (*metro* (+ beat (* 0.5 dur))) #'alberti-fuer-doofe
        (+ beat dur)
        8 new)))

(defun alberti-fuer-doofe (beat dur degree)
  (let* ((new
             (r-elt (cdr
                     (assoc degree '((i iv v vi viio n6)
                                     (n6 viio v i)
                                     (viio i)
                                     (vi iv)
                                     (iv i)
                                     (v i vi))))))
         (chrd (make-chord-fixed 60 6 (diatonic -2 '^ new))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano (elt chrd p) 80 dur))
         (append (range 0 4) (reverse (range 1 3)))
         (range 0 7))
    (at (*metro* (+ beat (* 0.5 dur))) #'alberti-fuer-doofe
        (+ beat dur)
        8 new)))

(next (new cycle :of '(i iv v vi viio n6)) 200)



(defun alberti-fuer-doofe (beat dur degree)
  (let* ((new
           (r-elt (cdr
                   (assoc degree '((i iv)
                                   (ii v)
                                   (iii vi)
                                   (iv ii)
                                   (vi ii)
                                   (vii iii)
                                   (v i))))))
         (chrd (make-chord-fixed 60 3 (diatonic 0 '- new))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano (elt chrd p) 80 dur))
         '(0 2 1 2 0 2 1 2)
         (range 0 7))
    (at (*metro* (+ beat (* 0.5 dur))) #'alberti-fuer-doofe
        (+ beat dur)
        8 new)))















(defun alberti-fuer-doofe (beat dur degree)
  (let ((chrd (make-chord-fixed 60 3 (diatonic 0 '- degree))))
    (map nil (lambda (p o)
               (play (+ beat o) :piano (elt chrd p) 80 dur))
         '(0 2 1 2 0 2 1 2)
         (range 0 7)))
  (at (*metro* (+ beat (* 0.5 dur))) #'alberti-fuer-doofe
      (+ beat dur)
      8
      (r-elt (cdr (assoc degree '((i vi)
                                  (iv v)
                                  (ii v)
                                  (vi ii)
                                  (v i)))))))

(alberti-fuer-doofe (*metro* 'get-beat 4) 8 'i)

(defun rrange (low high)
  (+ low (random (- high low))))

(rrange 1 7)

(let* ((*root* (r-elt '(-2 0 3 9)))
       (*scale* (pc-scale *root* 'aeolian)))
  (mapcar (lambda (keynum) (pc-quantize keynum *scale*))
          (take (rrange 1 7)
                (rotate `(60 63 67 ,(r-elt '(72 74 75)) 70 58)
                    (* -1 (random 6))))))

(let* ((*root* (r-elt '(-2 0 3 9)))
       (*scale* (pc-scale *root* 'aeolian)))
  (mapcar (lambda (keynum) (pc-quantize keynum *scale*))
          '(60 63 67)))

(sprout (new midi :time 0))

(play-note 0 :piano 60 100 *samplerate*)

(map 'list #'round #(0.2 0.4 0.6))

(defmacro play (time instr keynum amp dur)
    `(map nil (lambda (key) (play-note (*metro* 'get-time ,time) ,instr key (max 0 (min 127 ,amp))
                                  (- (*metro* 'get-time (+ beat ,dur))
                                     (*metro* 'get-time beat))))

          (cond
            ((vectorp ,keynum) (coerce ,keynum 'list))
            ((listp ,keynum) (let* ((len (length ,keynum))
                                    (dur (/ ,dur len)))
                               (map nil (lambda (key o) (play (+ ,time o) ,instr key ,amp dur))
                                    ,keynum
                                    (mapcar (lambda (x) (* x dur)) (range 0 len)))))
            ((eq ,keynum '_) nil)
            (:else (list ,keynum)))))



(defun model6 (beat instr riff dur &rest args)
  (apply #'play beat instr riff (cosr 80 40 1/6) dur args)
  (at (*metro* (+ beat (* 0.5 dur)))
      #'model6 (+ beat dur)
      instr
      (rotate riff 1)
      dur))

(defun model6 (beat riff phase dur)
  (declare (ignore beat riff phase dur)))

(model6 (*metro* 'get-beat 1) '(60 ~ #(72 75) (60 61 62)) 0 1/2)

(model6 (*metro* 'get-beat 1) '((60 62 ~ ~ 62 ~ ~ ~) (#(72 75) (60 61 62))) 0 1)

(model6 (*metro* 'get-beat 1) :fingered-bass '(36 _ 48) 1/2 :channel 3)

(trace model6)


(let ((beat (*metro* 'get-beat 1))
      (dur 1/4))
  (playp *scale* 5/4 0 :piano -24 `(60 63 72) 120 (* 3.0 dur)))

(let ((beat (*metro* 'get-beat 1))
      (dur 1/4))
  (playp_f beat dur *scale* 5/4 0 :piano -24 `(60 _ 72) 120 (* 3.0 dur)))

(let ((beat (*metro* 'get-beat 0))
      (dur 1/4)
      (notes (take (1+ (random 6))
                   (rotate `(60 63 67 ,(r-elt '(72 74 75)) 70 58)
                           (* -1 (random 6))))))
  (playp *scale*
         dur 0 :tenor-sax 0 notes (cosr 100 40 2)
         (+ 0.1 (/ dur (length notes))) :channel 6))

(defun f2 (beat notes dur)
  (let* ((offs (/ dur (length notes)))
         (curr 0))
    (if (> (random 1.0) 0.7)
        (dolist (keynum notes)
          (play curr :baritone-sax (pc-quantize (- keynum 24) *scale*) 60 (+ offs 0.1) :channel 6)
          (incf curr offs)))) 
  (at (*metro* (+ beat (* 0.5 dur))) #'f2 (+ beat dur)
      (take (1+ (random 6))
            (rotate `(60 63 67 ,(r-elt '(72 74 75)) 70 58)
                    (* -1 (random 6))))
      (r-elt '(1 2))))


(defun f2 (beat notes dur)
  (if (> (random 1.0) -0.1)
      (playp *scale*
             dur 0 :flute (r-elt '(0 0 -12))
             notes (cosr 60 30 2)
             (+ 0.1 (/ dur (length notes))) :channel 6))
  (at (*metro* (+ beat (* 0.5 dur))) #'f2 (+ beat dur)
      (take (1+ (random 6))
            (rotate `(60 63 67 ,(r-elt '(72 74 75)) 70 58)
                    (* -1 (random 6))))
      (r-elt '(1 2))))

(f2 (*metro* 'get-beat 4) `(60 63 67 ,(r-elt '(72 74 75)) 70 58) 2)

(defun f1 (beat dur)
;;;  (format t "beat: ~4,2f~%" (float beat 1.0))
  ;; (onbeat? 1 10
  ;;          (progn
  ;;            (setf *root* (r-elt (remove *root* '(-2 0 3 9))))
  ;;            (setf *scale* (pc-scale *root* 'aeolian))
  ;;               (map nil (lambda (p) (play (random 3) :piano p 85 10 :channel 3))
  ;;                    (pc-make-chord 50 80 4 (pc-chord *root* '-7)))))
  (playp *scale* 5/4 0 :fingered-bass -24 `(60 _ 72) 110 (* 3.0 dur) :channel 1)
;;;   (play 0 :piano 48 30 1/5 :channel 7)
  (playp *scale* 5/4 0 :vibraphone 0 `(60 63 67) (cosr 80 30 1) (* 4.9 dur) :channel 2)
  ;; (let ((v 80))
  ;;   (playp *scale* 15 0 :tenor-sax 0
  ;;          `(60 63 67 ,(r-elt '(72 74)) 70 58) v (* 10 dur) :channel 4)
  ;;   (playp *scale* 15 0 :tenor-sax -15
  ;;          `(60 63 67 ,(r-elt '(68 72 74)) 70 58) v (* 10 dur) :channel 5))
  (at (*metro* (+ beat (* 1.0 dur))) #'f1 (+ beat dur) dur))

(f1 (*metro* 'get-beat 1) 1/4)

(let ((beat (*metro* 'get-beat 1))
      (dur 1/4))
  (playp *scale* 5/4 0 :vibraphone 0 `(60 63 67) (cosr 50 20 1) (* 4.9 dur) :channel 2))

(play 0 :piano 60 60 1/4)
(playp_f )
(let ((beat (*metro* 'get-beat 1))
      (dur 1/4))
  (play beat :piano 60 80 dur))


(defparameter *root* 0)
(defparameter *scale* (pc-scale *root* 'aeolian))

(defun splay (beat instr keynum amp dur &rest args)
  (declare (special LC LL LP))
  (let* ((keys (if (listp keynum) (first keynum) keynum))
         (dur (* (getf args :durfactor 1) (get-durfactor keynum) dur)))
    (remf args :durfactor)
    (map nil (lambda (key) (apply #'play-note (*metro* 'get-time beat) instr (pc-quantize key *scale*) (max 0 (min 127 amp))
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

(defun pat-play (beat instr pat dur &rest args)
  (apply #'splay beat instr pat 120 dur args)
  (at (*metro* (+ beat (* 0.5 dur)))
      (lambda ()
        (apply #'pat-play (+ beat dur)
               instr
               (rotate pat 1)
               dur
               args))))

(defun vib-play (beat instr pat dur &rest args)
  (apply #'splay beat instr pat (cosr 60 5 4/10) dur args)
  (at (*metro* (+ beat (* 0.5 dur)))
      (lambda ()
        (apply #'vib-play (+ beat dur)
               instr
               (rotate pat 1)
               dur
               args))))

(defun vib2-play (beat instr pat dur &rest args)
  (apply #'splay beat instr pat 85 dur args)
  (at (*metro* (+ beat (* 0.5 dur)))
      (lambda ()
        (apply #'vib2-play (+ beat dur)
               instr
               (rotate pat 1)
               dur
               args))))

(*metro* 'set-tempo 144)

(jack-connect-qsynth)

(pat-play (*metro* 'get-beat 3) :fingered-bass '(36 _ 48) 1/2 :durfactor 2.9 :channel 1)
(vib-play (*metro* 'get-beat 3) :vibraphone '(60 63 67) 1/2 :durfactor 5 :channel 2)
(vib2-play (*metro* 'get-beat 6) :vibraphone '(60 _ _ _ _ 67 _ _ _ _ 63 _) 1/2 :durfactor 5 :channel 3)


(progn
  (setf *root* 0)
  (setf *scale* (pc-scale *root* 'aeolian)))


(pc-quantize 60 (pc-scale 0 'aeolian))

(pat-play (*metro* 'get-beat 1) :piano '((60 62 ~ ~ 62 ~ ~ ~) (#(72 75) (60 61 62))) 1 :channel 3)

;;;; pattern-language scratch




;; this function is for managing symbols inside a pattern _ for rest etc..

(defun rmap_helper_lst_sym_transform (lst)
  (if (atom lst) (setf lst (list lst)))
  (if (and (= (length lst) 1) (null (car lst))) (setf lst `(_)))
  (apply #'append '()
         (mapcar (lambda (elt next)
                   (cond ((and (vectorp elt) (symbolp next))
                          (list elt))
                         ((listp elt)
                          (if (= (length elt) 1)
                              elt
                              (list elt)))
                         (:else (list elt))))
              lst
              (append (cdr lst) '(0)))))

(defun rmap_helper
  (lambda (beats offset func beat dur loopcnt looppos &rest args)
    (let* ((lst (rmap_helper_lst_sym_transform (if (list? (car args))
                                                   (car args)
                                                   (if (pair? (car args))
                                                       (caar args)
                                                       '(_ _ _ _)))))
           (pos (modulo (- looppos offset) beats)) ;;(modulo (- beat offset) beats))
           (one_beat (/ beats (length lst)))
           (lst_positions (range 0 (+ pos beats) one_beat))
           (nextpos (+ pos dur))
           (idx 0)
           (f (lambda (old new)
                (set! idx (+ idx 1))
                (if (and (>= new pos) (< new nextpos))
                    (cons (cons new idx) old)
                    old)))
           (newlst (foldl f '() lst_positions)))
      (map (lambda (t)
             (let* ((tmpargs (map (lambda (l)
                                    (if (list? l)
                                        (set! l (rmap_helper_lst_sym_transform l))
                                        (if (pair? l)
                                            (set! l ((eval (cdr l)) (rmap_helper_lst_sym_transform (car l))))
                                            (set! l (list l))))
                                    (if (< (length l) (cdr t))
                                        (list-ref l (modulo (- (cdr t) 1) (length l)))
                                        (list-ref l (- (cdr t) 1))))
                                  args))
                    (targs (map (lambda (l)
                                  (cond ((vector? l) (map (lambda (x) (eval x)) (vector->list l)))
                                        ((and (symbol? l) (not (member l '(_ |)))) (eval l))
                                        (else l)))
                                tmpargs)))
               (cond
                ((or (list? (car tmpargs)) (pair? (car tmpargs)))
                 (apply rmap_helper one_beat offset func (+ beat (- (car t) pos)) one_beat loopcnt (+ looppos (- (car t) pos)) targs))
                ((member (car tmpargs) '(_ |)) #f) ;; skip these symbols
                (else
                 ;; this is a messy expression, but it just counts the number of
                 ;; '| symbols *after* the current value, and adds them to the
                 ;; duration
                 (let ((note-dur (* one_beat
                                   (+ 1 (length (take-while (lambda (x) (equal? x '|))
                                                            (cl:nthcdr (min (cdr t) (length (car args))) (car args))))))))
                   (apply callback
                          (- (*metro* (+ beat (- (car t) pos))) *RMAP_HELPER_CALLBACK_OFFSET*)
                          func (+ beat (- (car t) pos)) note-dur
                          loopcnt targs))))))
           newlst))))

;; rhythm map
;; rmap expects f to take beat + dur + length(lsts) args.
(define-macro (rmap beats offset f loopcnt looppos . lsts)
  `(rmap_helper ,beats ,offset ,f beat dur ,loopcnt ,looppos ,@lsts))

;;
;; example usage
;;

;; (define test
;;   (lambda (beat dur)
;;     (rmap 4 0 (lambda (beat dur p r)
;;                 (play pad p 80 r))
;;               `(60 70 80)
;;               `(1 0.5 0.2))
;;     (callback (*metro* (+ beat (* .5 dur))) 'test (+ beat dur) dur)))
;;
;; (test (*metro* 'get-beat 4) 1/4)

(setf (fdefinition 'real->integer) #'floor)
(setf (fdefinition 'modulo) #'mod)

(real->integer 2.9999)

(defun pc-make-chord (lower upper number pc)
  (loop  with l = (round lower)
         with u = (round upper)
         with n = number
         with p = pc
         with range with gap
         with chord = '()
         until (< n 1)
           initially (progn
                       (setf range (- u l))
                       (setf gap (round (/ range n))))
         do (let* ((pitch (r-pc l (+ l gap) p)))
              (if (not pitch) ; if new pitch is #f, try from whole range
                  (setf chord (cons (r-pc lower upper p) chord))
                  (setf chord (cons pitch chord)))
              (incf l gap)
              (decf n)
              (if (> (length p) 1)
                  (setf pc (remove (modulo (car chord) 12) p))))
         finally (return
                   (mapcar (lambda (x)
                              (real->integer x))
                        (sort (remove -1 chord) #'<))) ; lowest pitch to highest pitch remove -1s
         ))

(loop for x below 10 collect (pc-random 60 66 '(0 4 7)))

(untrace)

(remove-duplicates (loop for x below 100 collect (pc-make-chord 50 80 4 (pc-chord 0 '-7))) :test #'equal)

(loop for x in '((55 60 63 70) (55 63 70 72) (55 58 63 72) (55 58 72 75) (51 58 72 79)
                 (55 60 70 75) (51 60 67 70) (51 58 67 72) (51 60 70 79) (51 58 60 67))
      collect (sort (mapcar #'pc x) #'<))

((0 3 7 10) (0 3 7 10) (0 3 7 10) (0 3 7 10) (0 3 7 10) (0 3 7 10) (0 3 7 10)
            (0 3 7 10) (0 3 7 10) (0 3 7 10))

(format t "~c[31mRed~:*~c[32mGreen~:*~c[34mBlue~:*~c[mPlain~%" (code-char 27))

(play)
(cl-ansi-text:with-color (:yellow) (princ "
Hallo"))

(pc-make-chord 60 85 4 '(0 4 7))


(functionp (lambda () ()))

(functionp #'+)

(functionp 4)

(let ((args '(1 2 3 4)))
  (list (pop args) args)
  )


(keywordp 'test)

(setf (fdefinition 'list-ref) #'elt)
(setf (fdefinition 'symbol?) #'symbolp)
(setf (fdefinition 'list?) #'listp)
(setf (fdefinition 'null?) #'null)
(setf (fdefinition 'defined?) #'boundp)


;;;;;;; playp
(defun make-counter (n)
  (let ((count -1))
    (lambda () (mod (incf count) n))))

(defparameter *tcount* (make-counter 5))

(funcall *tcount*)

(setf (fdefinition 'mycount) *tcount*)

(mycount)






(:> pat-1 2 0 (format t "~a ~a ~a ~a ~a ~a~%" LC LL LP dur beat @1) (list 60 58 60 (cycle 0 1 '(72 67) '(73 72))) '(60 62 65))

(expand-args
         '((list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65)))

(untrace)

(defmacro :> (name len offs expr &rest seqs)
  (let* ((len (rationalize len))
;;;;         (dur (/ len (apply #'max (mapcar #'length seqs))))
         (repeats (if (numberp (first seqs)) (pop seqs)))
         (varlist (construct-var-list (length seqs)))
         )
    `(let* ((lc -1)
            (ll ,len)
            (lp 0)
            (continue t)
            (repeats ,repeats)
            (dur (/ ll (length ,(first seqs))))
;;;;            (dur (/ ,len (max ,@(expand-args seqs)))) ;;; alternativ: maximale Länge aller Listen
            (fn (lambda ,varlist (declare (ignorable ,@varlist)) ,expr))
            startbeat)
       (defun ,name (beat dur depth seqs)
         (multiple-value-setq (LC LP) (floor (- beat startbeat) ll))
         (when (zerop lp)
           (setf seqs (list ,@seqs))
           (when repeats
             (decf repeats)
             (if (< repeats 0) (setf continue nil))))
         (when continue
           (when (first seqs)
             (if (consp (caar seqs))
                 (,name beat (/ dur (length (caar seqs))) (1+ depth) (mapcar #'first-as-list seqs))
                 (apply fn lc ll lp dur beat (mapcar #'first seqs))))
           (when (or (and (zerop depth) continue) (cadar seqs))
             (at (*metro* (+ beat (* 0.5 dur))) #',name
                 (+ beat dur) dur depth
                 (cons (if (zerop depth)
                           (rotate (first seqs))
                           (cdr (first seqs)))
                       (mapcar #'rotate (cdr seqs)))))))
       (setf startbeat (*metro* 'get-beat ,offs))
       (,name (*metro* 'get-beat ,offs) dur 0 (list ,@seqs)))))

(defmacro :> (&rest args)
  (let* ((repeats (if (numberp (first args)) (pop args)))
         (name (pop args))
         (len (pop args))
         (offs (pop args))
         (expr (pop args))
         (seqs args)
         (len (rationalize len))
;;;;         (dur (/ len (apply #'max (mapcar #'length seqs))))
         
         (varlist (construct-var-list (length seqs)))
         )
    `(let* ((lc -1)
            (ll ,len)
            (lp 0)
            (continue t)
            (repeats ,repeats)
            (dur (/ ll (length ,(first seqs))))
;;;;            (dur (/ ,len (max ,@(expand-args seqs)))) ;;; alternativ: maximale Länge aller Listen
            (fn (lambda ,varlist (declare (ignorable ,@varlist)) ,expr))
            startbeat)
       (defun ,name (beat dur depth seqs)
         (multiple-value-setq (LC LP) (floor (- beat startbeat) ll))
         (when (zerop lp)
           (setf seqs (list ,@seqs))
           (when repeats
             (decf repeats)
             (if (< repeats 0) (setf continue nil))))
         (when continue
           (when (first seqs)
             (if (consp (caar seqs))
                 (,name beat (/ dur (length (caar seqs))) (1+ depth) (mapcar #'first-as-list seqs))
                 (apply fn lc ll lp dur beat (mapcar #'first seqs))))
           (when (or (and (zerop depth) continue) (cadar seqs))
             (at (*metro* (+ beat (* 0.5 dur))) #',name
                 (+ beat dur) dur depth
                 (cons (if (zerop depth)
                           (rotate (first seqs))
                           (cdr (first seqs)))
                       (mapcar #'rotate (cdr seqs)))))))
       (setf startbeat (*metro* 'get-beat ,offs))
       (,name (*metro* 'get-beat ,offs) dur 0 (list ,@seqs)))))

(progn
  (:<  pat-1 2 2 (play (float beat 1.0) :piano @1 80 dur) (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65))
  (:> pat-1 2 2 (play beat :piano @1 80 dur) (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65)))



(let* ((lc -1)
       (ll 4)
       (lp 0)
       (dur (/ ll (length (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))))))
       (fn
        (lambda (lc ll lp dur beat @1 @2)
          (declare (ignorable lc ll lp dur beat @1 @2))
          (play (float beat 1.0) :piano @1 80 dur)
;;;          (format t "~a ~a ~a ~a ~a ~a~%" LC LL LP dur beat @1)
          ))
       startbeat)
  (defun pat-1 (beat dur depth seqs)
    (multiple-value-setq (lc lp) (floor (- beat startbeat) ll))
    (if (zerop lp)
        (setf seqs
                (list (list 60 58 60 (cycle lc 1 '(72 67) '(73 72)))
                      '(60 62 65))))
    (if (first seqs)
        (if (consp (caar seqs)) ;;; next pitches are a list
            (progn
              (pat-1 beat (/ dur (length (caar seqs))) (1+ depth) (mapcar #'first-as-list seqs)))
            (apply fn lc ll lp dur beat (mapcar #'first seqs))))
    (if (or (zerop depth) (cadar seqs))
        (progn
          (at (*metro* (+ beat (* 0.5 dur)))
              #'pat-1 (+ beat dur) dur depth
              (cons (if (zerop depth)
                        (rotate (first seqs))
                        (cdr (first seqs)))
                    (mapcar #'rotate (cdr seqs)))))))
  (setf startbeat (*metro* 'get-beat 4))
  (pat-1 startbeat dur 0
         (list (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65))))


(let* ((lc -1)
       (ll 4)
       (lp 0)
       (dur (/ ll (length (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))))))
       (fn
        (lambda (lc ll lp dur beat @1 @2)
          (declare (ignorable lc ll lp dur beat @1 @2))
          (play (float beat 1.0) :piano @1 80 dur)
          (format t "~a ~a ~a ~a ~a ~a~%" LC LL LP dur beat @1)))
       startbeat)
  (defun pat-1 (beat dur depth seqs)
    (format t "seqs: ~a, depth:~a, beat: ~,2f, dur: ~a ~%" seqs depth beat dur)
    (multiple-value-setq (lc lp) (floor (- beat startbeat) ll))
    (if (zerop lp)
        (setf seqs
                (list (list 60 58 60 (cycle lc 1 '(72 67) '(73 72)))
                      '(60 62 65))))
    (if (first seqs)
        (if (consp (caar seqs)) ;;; next pitches are a list
            (progn
              (format t "recurse: ~a, dur: ~a~%" (length (caar seqs)) dur)
              (pat-1 beat (/ dur (length (caar seqs))) (1+ depth) (mapcar #'first-as-list seqs)))
            (apply fn lc ll lp dur beat (mapcar #'first seqs))))
    (if (or (zerop depth) (cadar seqs))
        (progn
          (format t "rescheduling: ~a, depth: ~a, dur: ~a time: ~,2f~%" seqs depth dur (+ beat dur))
          (at (*metro* (+ beat (* 0.5 dur)))
              #'pat-1 (+ beat dur) dur depth
              (cons (if (zerop depth)
                        (rotate (first seqs))
                        (cdr (first seqs)))
                    (mapcar #'rotate (cdr seqs)))))))
  (setf startbeat (*metro* 'get-beat 0))
  (pat-1 startbeat dur 0
         (list (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65))))


(now)
(sprout (new midi :time 0))
(*metro* 'set-tempo 60)

(play)
(listp '())
(float 4.3168342d7 1.0)

(- (*metro* 'get-time 1051/4) (now))
(float 2959/4 )
(cadar '(nil (62 65 60)))

(let* ((lc -1)
       (ll 2)
       (lp 0)
       (dur (/ 2 (length (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))))))
       (fn
        (lambda (lc ll lp dur beat @1 @2)
          (declare (ignorable lc ll lp dur beat @1 @2))
          (format t "~a ~a ~a ~a ~a ~a~%" lc ll lp dur beat @1)))
       startbeat)
  (defun pat-1 (beat dur seqs)
    (unless startbeat (setf startbeat beat))
    (format t "~a ~%" seqs)
    (multiple-value-setq (lc lp) (floor (- beat startbeat) ll))
    (if (zerop lp) (setf seqs (list (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65))))
    (apply fn lc ll lp dur beat (mapcar #'first seqs))
    (at (*metro* (+ beat (* 0.5 dur))) #'pat-1 (+ beat dur) dur
        (mapcar #'rotate seqs)))
  (pat-1 (*metro* 'get-beat 0) dur
         (list (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65))))

(let* ((lc -1)
       (ll 2)
       (lp 0)
       (dur (/ 2 (length (list 60 58 60 (cycle lc 1 '(72 67) '(73 72))))))
       (fn
        (lambda (lc ll lp dur beat @1 @2)
          (declare (ignorable lc ll lp dur beat @1 @2))
          (format t "~a ~a ~a ~a ~a ~a~%" lc ll lp dur beat @1)))
       startbeat)
  (defun pat-1 (beat dur seqs)
    (unless startbeat (setf startbeat beat))
    (format t "~a ~%" seqs)
    (multiple-value-setq (lc lp) (floor (- beat startbeat) 2))
    (apply fn lc ll lp dur beat (mapcar #'first seqs))
    (at (*metro* (+ beat (* 0.5 dur))) #'pat-1 (+ beat dur) dur
        (mapcar #'rotate seqs)))
  (pat-1 (*metro* 'get-beat 0) dur
         '((list 60 58 60 (cycle lc 1 '(72 67) '(73 72))) '(60 62 65))))




(let* ((lc -1)
       (ll 2)
       (lp 0)
       (dur
        (/ 2))
       startbeat)
  dur)


(let* ((lc -1)
       (start? (not (fboundp 's1)))
       (ll 4)
       (lp 0)
       (continue t)
       (repeats nil)
       (dur
        (/ ll
           (length
            `(,(scale 3 8 (r-elt '(-1 0 1 2)))
              ,(reverse (scale 3 8 (r-elt '(-1 0 1 2))))))))
       (fn
        (lambda (lc ll lp dur beat @1)
          (declare (ignorable lc ll lp dur beat @1))
          (play beat :cello @1 (cosr 60 10 7/4) dur :channel 1)))
       startbeat)
  (defun s1 (&optional beat dur depth seqs)
    (multiple-value-setq (lc lp) (floor (- beat startbeat) ll))
    (when (and (zerop depth) (zerop lp))
      (setf seqs
              (list
               `(,(scale 3 8 (r-elt '(-1 0 1 2)))
                 ,(reverse (scale 3 8 (r-elt '(-1 0 1 2)))))))
      (when repeats
        (decf repeats)
        (if (< repeats 0)
            (setf continue nil))))
    (when continue
        (progn
         (when (first seqs)
           (if (consp (caar seqs))
               (when (fboundp 's1)
                 (s1 beat (/ dur (length (caar seqs))) (1+ depth)
                     (mapcar #'first-as-list seqs)))
               (apply fn lc ll lp dur beat (mapcar #'first seqs))))
         (when (or (and (zerop depth) continue (fboundp 's1)) (cadar seqs))
           (at (*metro* (+ beat (* 0.5 dur))) #'s1 (+ beat dur) dur depth
               (cons
                (if (zerop depth)
                    (rotate (first seqs))
                    (cdr (first seqs)))
                (mapcar #'rotate (cdr seqs))))))
;;; (fmakunbound 's1)
        ))
  (setf startbeat (*metro* 'get-beat 0))
  (when start?
    (push 's1 *patterns*)
    (s1 (*metro* 'get-beat 0) dur 0
        (list
         `(,(scale 3 8 (r-elt '(-1 0 1 2)))
           ,(reverse (scale 3 8 (r-elt '(-1 0 1 2)))))))))

(defun abs-pc-transpose (val lst pcs)
  "transpose lst by val num-steps within pcs."
  (quantize-list (transpose val lst) pcs))





(step-transpose 60 -1 (pc-scale 0 'octatonic))

(let* ((val 60)
       (step 3)
       (pcs (pc-scale 0 'octatonic))
       (keynum (pc-quantize 60 pcs)))
  (position (pc keynum) pcs)
  )
