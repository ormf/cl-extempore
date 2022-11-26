;;; 
;;; elisp-setup.lisp
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

(in-package :cl-user)

(progn
  (swank:eval-in-emacs
   '(defun incudine-hush ()
     (interactive)
     (progn
       (slime-interactive-eval "(incudine:flush-pending)")
       (slime-interactive-eval "(cl-extempore:midi-panic)")
;;;    (slime-interactive-eval "(scratch::node-free-unprotected)")
       (slime-interactive-eval "(scratch::node-free-all)")
;;;     (slime-interactive-eval "(heidelberg::osc-stop)")
;;;     (slime-interactive-eval "(heidelberg::osc-notes-off)")
       )))

  (swank:eval-in-emacs
   '(defun incudine-rt-start ()
     (interactive)
     (slime-interactive-eval "(incudine:rt-start)")))

  (swank:eval-in-emacs
   '(defun incudine-rt-stop ()
     (interactive)
     (slime-interactive-eval "(incudine:rt-stop)")))

  (swank:eval-in-emacs
   '(define-key lisp-mode-map (kbd "C-.") 'incudine-hush))
  (swank:eval-in-emacs
   '(define-key lisp-mode-map (kbd "C-c C-.") 'incudine-rt-stop))
  (swank:eval-in-emacs
   '(define-key lisp-mode-map (kbd "C-c M-.") 'incudine-rt-start)))
