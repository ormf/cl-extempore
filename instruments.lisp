;;; 
;;; instruments.lisp
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

(defparameter *instruments* (make-hash-table))
(defvar *gm-lookup* (make-hash-table))

(loop
  for num from 0
  for name in '(:Acoustic-Grand-Piano :Bright-Acoustic-Piano :Electric-Grand-Piano :Honky-tonk-Piano :Electric-Piano-1 :Electric-Piano-2 :Harpsichord :Clavi
                :Celesta :Glockenspiel :Music-Box :Vibraphone :Marimba :Xylophone :Tubular-Bells :Dulcimer
                :Drawbar-Organ :Percussive-Organ :Rock-Organ :Church-Organ :Reed-Organ :Accordion :Harmonica :Tango-Accordion
                :Acoustic-Guitar-nylon :Acoustic-Guitar-steel :Electric-Guitar-jazz :Electric-Guitar-clean :Electric-Guitar-muted :Overdriven-Guitar :Distortion-Guitar :Guitar-harmonics
                :Acoustic-Bass :Electric-Bass-finger :Electric-Bass-pick :Fretless-Bass :Slap-Bass-1 :Slap-Bass-2 :Synth-Bass-1 :Synth-Bass-2
                :Violin :Viola :Cello :Contrabass :Tremolo-Strings :Pizzicato-Strings :Orchestral-Harp :Timpani
                :String-Ensemble-1 :String-Ensemble-2 :SynthStrings-1 :SynthStrings-2 :Choir-Aahs :Voice-Oohs :Synth-Voice :Orchestra-Hit
                :Trumpet :Trombone :Tuba :Muted-Trumpet :French-Horn :Brass-Section :SynthBrass-1 :SynthBrass-2
                :Soprano-Sax :Alto-Sax :Tenor-Sax :Baritone-Sax :Oboe :English-Horn :Bassoon :Clarinet
                :Piccolo :Flute :Recorder :Pan-Flute :Blown-Bottle :Shakuhachi :Whistle :Ocarina
                :Lead-1-square :Lead-2-sawtooth :Lead-3-calliope :Lead-4-chiff :Lead-5-charang :Lead-6-voice :Lead-7-fifths :Lead-8-bass-+-lead
                :Pad-1-new-age :Pad-2-warm :Pad-3-polysynth :Pad-4-choir :Pad-5-bowed :Pad-6-metallic :Pad-7-halo :Pad-8-sweep :FX-1-rain :FX-2-soundtrack :FX-3-crystal :FX-4-atmosphere :FX-5-brightness :FX-6-goblins :FX-7-echoes :FX-8-sci-fi
                :Sitar :Banjo :Shamisen :Koto :Kalimba :Bag-pipe :Fiddle :Shanai :Tinkle-Bell :Agogo :Steel-Drums :Woodblock :Taiko-Drum :Melodic-Tom :Synth-Drum :Reverse-Cymbal :Guitar-Fret-Noise :Breath-Noise :Seashore :Bird-Tweet :Telephone-Ring :Helicopter :Applause :Gunshot)
  do (setf (gethash name *gm-lookup*) num)
     (setf (gethash num *gm-lookup*) name))

(defun def-gm-alias (alias name)
  (setf (gethash alias *gm-lookup*) (gethash name *gm-lookup*)))

;;; aliases

(loop for (alias name) on '(:piano :acoustic-grand-piano :guitar :acoustic-guitar-nylon :fingered-bass :acoustic-bass)
      by #'cddr do (def-gm-alias alias name))

(defun get-gm-idx (name)
  (gethash name *gm-lookup*))

(eval-when (:compile-toplevel)
  (defconstant +midi-value+ '(unsigned-byte 7)))

(defun define-instrument (name val &key (hash *instruments*))
  (setf (gethash name hash) val))

(defun play-instrument (name)
  (gethash name *instruments*))

(defun list-instruments (&optional (hash *instruments*))
  (loop for name being the hash-keys in hash collect name))

;;; (list-instruments)

(defparameter *midi-channel-pgms* (make-array 256 :initial-element 0 :element-type +midi-value+))

(defun set-channel-pgm (chan pgm &key (force nil))
  (when (or force (/= (aref *midi-channel-pgms* chan) pgm))
    (midi-pgm-out pgm chan)
    (setf (aref *midi-channel-pgms* chan) pgm)))


;;; send default setup for qsynth

#|
(define-instrument :piano
    (lambda (&rest args)
      (let ((chan (getf args :channel 0)))
        (set-channel-pgm chan (get-gm-idx :piano))
        (apply #'make-instance 'midi (append (list :channel chan) args)))))

(define-instrument :fingered-bass
    (lambda (&rest args)
      (let ((chan (getf args :channel 1)))
        (set-channel-pgm chan (get-gm-idx :fingered-bass))
        (apply #'make-instance 'midi (append (list :channel chan) args)))))

(define-instrument :vibraphone
    (lambda (&rest args)
      (let ((chan (getf args :channel 2)))
        (set-channel-pgm chan (get-gm-idx :vibraphone))
        (apply #'make-instance 'midi (append (list :channel chan) args)))))
|#

(loop for instr being the hash-keys of *gm-lookup*
      do (define-instrument instr
             (let ((instr instr))
               (lambda (&rest args)
                 (let ((chan (getf args :channel 1)))
                   (set-channel-pgm (1- chan) (get-gm-idx instr))
                   (apply #'midi-play-note (append (list :channel chan) args)))))))

;;; (jack-connect-qsynth)

;;; (funcall (play-instrument :piano) :time 0 :keynum 60 :duration 1 :channel 1)

