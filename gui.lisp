;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

;;; GUI for the Climacs editor. 

(in-package :climacs-gui)

(define-application-frame climacs ()
  ((buffer :initform (make-instance 'abbrev-buffer)
	   :accessor buffer)
   (point :initform nil :reader point))
  (:panes
   (win :application :width 600 :height 400
	:display-function 'display-win)
   (int :interactor :width 600 :height 100))
  (:layouts
   (default
       (vertically () win))
   (with-interactor
       (vertically () win int)))
  (:top-level (climacs-top-level)))

(defmethod initialize-instance :after ((frame climacs) &rest args)
  (declare (ignore args))
  (setf (slot-value frame 'point)
	(make-instance 'standard-right-sticky-mark
	   :buffer (buffer frame))))

(defun climacs ()
  (let ((frame (make-application-frame 'climacs)))
    (run-frame-top-level frame)))

(defun display-win (frame pane)
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium))
	 (width (text-style-width style medium))
	 (buffer (buffer frame))
	 (size (size (buffer frame)))
	 (offset 0)
	 (cursor-x nil)
	 (cursor-y nil))
    (flet ((display-line ()
	     (loop with offset1 = nil
		   when (= offset (offset (point frame)))
		     do (multiple-value-bind (x y) (stream-cursor-position pane)
			  (setf cursor-x (+ x (if (null offset1)
						  0
						  (* width (- offset offset1))))
				cursor-y y))
		   when (= offset size)
		     do (unless (null offset1)
			  (present (buffer-sequence buffer offset1 offset) 'string :stream pane)
			  (setf offset1 nil))
			(return)
		   until (eql (buffer-object buffer offset) #\Newline)
		   do (let ((obj (buffer-object buffer offset)))
			(cond ((eql obj #\Space)
			       (unless (null offset1)
				 (princ (buffer-sequence buffer offset1 offset) pane)
				 (setf offset1 nil))
			       (princ obj pane))
			      ((constituentp obj)
			       (when (null offset1)
				 (setf offset1 offset)))
			      (t
			       (unless (null offset1)
				 (princ (buffer-sequence buffer offset1 offset) pane)
				 (setf offset1 nil))
			       (princ obj pane))))
		      (incf offset)
		   finally (unless (null offset1)
			     (princ (buffer-sequence buffer offset1 offset) pane)
			     (setf offset1 nil))
			   (incf offset)
			   (terpri pane))))
      (loop while (< offset size)
	    do (display-line))
      (when (= offset (offset (point frame)))
	(multiple-value-bind (x y) (stream-cursor-position pane)
	  (setf cursor-x x
		cursor-y y))))
    (draw-line* pane
		cursor-x (- cursor-y (* 0.2 height))
		cursor-x (+ cursor-y (* 0.8 height))
		:ink +red+)))

(defun find-gestures (gestures start-table)
  (loop with table = (find-command-table start-table)
	for (gesture . rest) on gestures
	for item = (find-keystroke-item  gesture table :errorp nil)
	while item
	do (if (eq (command-menu-item-type item) :command)
	       (return (if (null rest) item nil))
	       (setf table (command-menu-item-value item)))
	finally (return item)))

(defparameter *current-gesture* nil)

(defun climacs-top-level (frame &key
			  command-parser command-unparser 
			  partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (let ((*standard-output* (frame-standard-output frame))
	(*standard-input* (frame-standard-input frame))
	(*print-pretty* nil))
    (redisplay-frame-panes frame :force-p t)
    (loop with gestures = '()
	  do (setf *current-gesture* (read-gesture :stream *standard-input*))
	     (when (or (characterp *current-gesture*)
		       (keyboard-event-character *current-gesture*))
	       (setf gestures (nconc gestures (list *current-gesture*)))
	       (let ((item (find-gestures gestures 'global-climacs-table)))
		 (cond ((not item)
			(beep) (setf gestures '()))
		       ((eq (command-menu-item-type item) :command)
			(funcall (command-menu-item-value item))
			(setf gestures '()))
		       (t nil))))
	     (redisplay-frame-panes frame :force-p t))))

(define-command com-quit ()
  (frame-exit *application-frame*))

(define-command com-self-insert ()
  (unless (constituentp *current-gesture*)
    (possibly-expand-abbrev (point *application-frame*)))
  (insert-object (point *application-frame*) *current-gesture*))

(define-command com-backward-object ()
  (decf (offset (point *application-frame*))))

(define-command com-forward-object ()
  (incf (offset (point *application-frame*))))

(define-command com-beginning-of-line ()
  (beginning-of-line (point *application-frame*)))

(define-command com-end-of-line ()
  (end-of-line (point *application-frame*)))

(define-command com-delete-object ()
  (delete-range (point *application-frame*)))

(define-command com-previous-line ()
  (previous-line (point *application-frame*)))

(define-command com-next-line ()
  (next-line (point *application-frame*)))

(define-command com-open-line ()
  (open-line (point *application-frame*)))

(define-command com-kill-line ()
  (kill-line (point *application-frame*)))

(define-command com-forward-word ()
  (forward-word (point *application-frame*)))

(define-command com-backward-word ()
  (backward-word (point *application-frame*)))

(define-command com-toggle-layout ()
  (setf (frame-current-layout *application-frame*)
	(if (eq (frame-current-layout *application-frame*) 'default)
	    'with-interactor
	    'default)))

(defclass weird () ())

(define-command com-insert-weird-stuff ()
  (insert-object (point *application-frame*) (make-instance 'weird)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Global command table

(make-command-table 'global-climacs-table :errorp nil)

(defun global-set-key (gesture command)
  (add-command-to-command-table command 'global-climacs-table
				:keystroke gesture :errorp nil))

(loop for code from (char-code #\space) to (char-code #\~)
      do (global-set-key (code-char code) 'com-self-insert))

(global-set-key #\newline 'com-self-insert)
(global-set-key '(#\f :control) 'com-forward-object)
(global-set-key '(#\b :control) 'com-backward-object)
(global-set-key '(#\a :control) 'com-beginning-of-line)
(global-set-key '(#\e :control) 'com-end-of-line)
(global-set-key '(#\d :control) 'com-delete-object)
(global-set-key '(#\p :control) 'com-previous-line)
(global-set-key '(#\n :control) 'com-next-line)
(global-set-key '(#\o :control) 'com-open-line)
(global-set-key '(#\k :control) 'com-kill-line)
(global-set-key '(#\f :meta) 'com-forward-word)
(global-set-key '(#\b :meta) 'com-backward-word)
(global-set-key '(#\x :meta) 'com-toggle-layout)
(global-set-key '(#\a :meta) 'com-insert-weird-stuff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; C-x command table

(make-command-table 'c-x-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "C-x"
				:menu 'c-x-climacs-table
				:keystroke '(#\x :control))

;;; for some reason, C-c does not seem to arrive as far as CLIM.

(add-command-to-command-table 'com-quit 'c-x-climacs-table
			      :keystroke '(#\q :control))


