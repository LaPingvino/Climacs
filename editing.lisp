;;; -*- Mode: Lisp; Package: CLIMACS-EDITING; -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Climacs editing

;;; See information in motion.lisp
;;;
;;; An editing function is a function named FORWARD-<frob>-<unit> or
;;; BACKWARD-<frob>-<unit>, or just <frob>-<unit> in the case where
;;; discering between forward and backward commands does not make
;;; sense (an example is TRANSPOSE-<unit>).
;;;
;;; A proper unit is a unit for which all the functions required by
;;; the motion protocol has been implemented, this can be trivially
;;; done by using the macro CLIMACS-MOTION:DEFINE-MOTION-COMMANDS.
;;;
;;; Given a proper unit,
;;;
;;;   (DEFINE-EDIT-FNS <unit>)
;;;
;;; defines the editing functions FORWARD-DELETE-<unit>,
;;; BACKWARD-DELETE-<unit>, FORWARD-KILL-<unit>, BACKWARD-KILL-<unit>
;;; and TRANSPOSE-<unit>.
;;;
;;; This file also holds definitions for other miscellaneus
;;; editing-related functions. The definitions in this file should
;;; have to do with immediate editing, understood as insertion,
;;; deletion or movement of buffer contents. Transformation of buffer
;;; contents (such as converting the case of a region) should not be
;;; here (FIXME: It actually is. Moving it to misc-commands.lisp is
;;; tempting, but I don't like putting too much non-command-related
;;; into the command files Perhaps the solution is a
;;; transformation.lisp?).

(in-package :climacs-editing)

(defmacro define-edit-fns (unit &key plural)
  (labels ((symbol (&rest strings)
             (intern (apply #'concat strings)))
           (concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings))))
    (let* ((unit-name (string-downcase unit))
           (plural (or plural (concat unit-name "s")))
           (upper-plural (string-upcase plural))
           (forward-delete (symbol "FORWARD-DELETE-" unit))
           (backward-delete (symbol "BACKWARD-DELETE-" unit))
           (forward-kill (symbol "FORWARD-KILL-" unit))
           (backward-kill (symbol "BACKWARD-KILL-" unit))
           (transpose (symbol "TRANSPOSE-" upper-plural))
           (forward (find-symbol (concat "FORWARD-" (string-upcase unit))))
           (backward (find-symbol (concat "BACKWARD-" (string-upcase unit)))))
      (unless (and forward backward)
        (error "The unit ~A is not known." unit))
      `(progn
         (defgeneric ,forward-delete
             (mark syntax &optional count limit-action)
           (:documentation
            ,(concat "Delete COUNT " plural " beginning from MARK.")))
         (defmethod ,forward-delete
             (mark syntax &optional (count 1) limit-action)
           (let ((mark2 (clone-mark mark)))
             (,forward mark2 syntax count limit-action)
             (delete-region mark mark2)))
         (defmethod ,forward-delete :around
             (mark syntax &optional (count 1) limit-action)
           (cond ((minusp count)
                  (,backward-delete mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,backward-delete
             (mark syntax &optional count limit-action)
           (:documentation
            ,(concat "Delete COUNT " plural " backwards beginning from MARK.")))
         (defmethod ,backward-delete
             (mark syntax &optional (count 1) limit-action)
           (let ((mark2 (clone-mark mark)))
             (,backward mark2 syntax count limit-action)
             (delete-region mark mark2)))
         (defmethod ,backward-delete :around
             (mark syntax &optional (count 1) limit-action)
           (cond ((minusp count)
                  (,forward-delete mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,forward-kill
             (mark syntax &optional count concatenate-p limit-action)
           (:documentation
            ,(concat "Kill COUNT " plural " beginning from MARK.")))
         (defmethod ,forward-kill
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (let ((start (offset mark)))
             (,forward mark syntax count limit-action)
             (unless (mark= mark start)
               (if concatenate-p
                   (if (plusp count)
                       (kill-ring-concatenating-push *kill-ring*
                                                     (region-to-sequence start mark))
                       (kill-ring-reverse-concatenating-push *kill-ring*
                                                             (region-to-sequence start mark)))
                   (kill-ring-standard-push *kill-ring*
                                            (region-to-sequence start mark)))
               (delete-region start mark))))
         (defmethod ,forward-kill :around
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (declare (ignore concatenate-p))
           (cond ((minusp count)
                  (,backward-kill mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,backward-kill
             (mark syntax &optional count concatenate-p limit-action)
           (:documentation
            ,(concat "Kill COUNT " plural " backwards beginning from MARK.")))
         (defmethod ,backward-kill
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (let ((start (offset mark)))
             (,backward mark syntax count limit-action)
             (unless (mark= mark start)
               (if concatenate-p
                   (if (plusp count)
                       (kill-ring-concatenating-push *kill-ring*
                                                     (region-to-sequence start mark))
                       (kill-ring-reverse-concatenating-push *kill-ring*
                                                             (region-to-sequence start mark)))
                   (kill-ring-standard-push *kill-ring*
                                            (region-to-sequence start mark)))
               (delete-region start mark))))
         (defmethod ,backward-kill :around
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (declare (ignore concatenate-p))
           (cond ((minusp count)
                  (,forward-kill mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,transpose
             (mark syntax)
           (:documentation
            ,(concat "Transpose two " plural " at MARK.")))
         (defmethod ,transpose
             (mark syntax)
           (let (start1 end1 start2 end2)
             (,backward mark syntax 1 nil)
             (setf start1 (clone-mark mark))
             (,forward mark syntax 1 #'error-limit-action)
             (setf end1 (clone-mark mark))
             (,forward mark syntax 1 #'error-limit-action)
             (setf end2 (clone-mark mark))
             (,backward mark syntax 1 nil)
             (setf start2 (clone-mark mark))
             (let ((obj1 (buffer-sequence (buffer mark) (offset start1) (offset end1)))
                   (obj2 (buffer-sequence (buffer mark) (offset start2) (offset end2))))
               (,forward-delete mark syntax 1 nil)
               (insert-sequence mark obj1)
               ;; KLUDGE: Having to do this manually is ugly, but it
               ;; is necessary if the motion functions uses syntax
               ;; information.
               (update-syntax (buffer syntax)
                              syntax)
               (,backward mark syntax 2 nil)
               (,forward-delete mark syntax 1 nil)
               (insert-sequence mark obj2)
               (update-syntax (buffer syntax)
                              syntax)
               (,forward mark syntax 1 nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Object editing

(defun transpose-objects (mark)
  (unless (beginning-of-buffer-p mark)
    (when (end-of-line-p mark)
      (backward-object mark))
    (unless (beginning-of-buffer-p mark)
      (let ((object (object-after mark)))
        (delete-range mark)
        (backward-object mark)
        (insert-object mark object)
        (forward-object mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Line editing

(define-edit-fns line)
(define-edit-fns line-start)

;; Autogenerated TRANSPOSE-LINES is not good enough.
(defmethod transpose-lines
    (mark syntax)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (backward-line mark syntax))
  (let* ((bol (offset mark))
	 (eol (progn (end-of-line mark)
		     (offset mark)))
	 (line (buffer-sequence (buffer mark) bol eol)))
    (delete-region bol mark)
    ;; Remove newline at end of line as well.
    (unless (end-of-buffer-p mark)
      (delete-range mark))
    (end-of-line mark)
    (insert-object mark #\Newline)
    (forward-line mark syntax 0)
    (insert-sequence mark line)
    (insert-object mark #\Newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Word editing

(define-edit-fns word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Page editing

(define-edit-fns page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Paragraph editing

(define-edit-fns paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Sentence editing

(define-edit-fns sentence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Expression editing

(define-edit-fns expression)
(define-edit-fns definition)
