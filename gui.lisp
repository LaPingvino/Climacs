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
   (int :interactor :width 600 :height 50))
  (:layouts
   (default
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
	 (offset1 nil)
	 (cursor-x nil)
	 (cursor-y nil))
    (labels ((present-contents ()
	     (unless (null offset1)
	       (present (coerce (buffer-sequence buffer offset1 offset) 'string)
			'string
			:stream pane)
	       (setf offset1 nil)))
	   (display-line ()
	     (loop when (= offset (offset (point frame)))
		     do (multiple-value-bind (x y) (stream-cursor-position pane)
			  (setf cursor-x (+ x (if (null offset1)
						  0
						  (* width (- offset offset1))))
				cursor-y y))
		   when (= offset size)
		     do (present-contents)
			(return)
		   until (eql (buffer-object buffer offset) #\Newline)
		   do (let ((obj (buffer-object buffer offset)))
			(cond ((eql obj #\Space)
			       (present-contents)
			       (princ obj pane))
			      ((constituentp obj)
			       (when (null offset1)
				 (setf offset1 offset)))
			      (t
			       (present-contents)
			       (princ obj pane))))
		      (incf offset)
		   finally (present-contents)
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
			(handler-case 
			    (funcall (command-menu-item-value item))
			  (error (condition)
			    (beep)
			    (format *error-output* "~a~%" condition)))
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

(define-command com-extended-command ()
  (accept 'command :prompt "Extended Command"))

(defclass weird () ())

(define-command com-insert-weird-stuff ()
  (insert-object (point *application-frame*) (make-instance 'weird)))

(define-command com-insert-number ()
  (insert-sequence (point *application-frame*)
		   (format nil "~a" (accept 'number :prompt "Insert Number"))))

(define-presentation-type completable-pathname ()
  :inherit-from 'pathname)

(defun filename-completer (so-far mode)
  (flet ((remove-trail (s)
	   (subseq s 0 (let ((pos (position #\/ s :from-end t)))
			 (if pos (1+ pos) 0)))))
    (let* ((directory-prefix
	    (if (and (plusp (length so-far)) (eql (aref so-far 0) #\/))
		""
		(namestring #+sbcl (car (directory ".")) #+cmu (ext:default-directory))))
	   (full-so-far (concatenate 'string directory-prefix so-far))
	   (pathnames
	    (loop with length = (length full-so-far)
		  for path in (directory (concatenate 'string
						       (remove-trail so-far)
						      "*.*"))
		  when (let ((mismatch (mismatch (namestring path) full-so-far)))
			 (or (null mismatch) (= mismatch length)))
		    collect path))
	   (strings (mapcar #'namestring pathnames))
	   (first-string (car strings))
	   (length-common-prefix nil)
	   (completed-string nil)
	   (full-completed-string nil))
      (unless (null pathnames)
	(setf length-common-prefix
	      (loop with length = (length first-string)
		    for string in (cdr strings)
		    do (setf length (min length (or (mismatch string first-string) length)))
		    finally (return length))))
      (unless (null pathnames)
	(setf completed-string
	      (subseq first-string (length directory-prefix)
		      (if (null (cdr pathnames)) nil length-common-prefix)))
	(setf full-completed-string
	      (concatenate 'string directory-prefix completed-string)))
      (case mode
	((:complete-limited :complete-maximal)
	 (cond ((null pathnames)
		(values so-far nil nil 0 nil))
	       ((null (cdr pathnames))
		(values completed-string t (car pathnames) 1 nil))
	       (t
		(values completed-string nil nil (length pathnames) nil))))
	(:complete
	 (cond ((null pathnames)
		(values so-far nil nil 0 nil))
	       ((null (cdr pathnames))
		(values completed-string t (car pathnames) 1 nil))
	       ((find full-completed-string strings :test #'string-equal)
		(let ((pos (position full-completed-string strings :test #'string-equal)))
		  (values completed-string
			  t (elt pathnames pos) (length pathnames) nil)))
	       (t
		(values completed-string nil nil (length pathnames) nil))))
	(:possibilities
	 (values nil nil nil (length pathnames)
		 (loop with length = (length directory-prefix)
		       for name in pathnames
		       collect (list (subseq (namestring name) length nil)
				     name))))))))

(define-presentation-method accept
    ((type completable-pathname) stream (view textual-view) &key)
  (multiple-value-bind (pathname success string)
      (complete-input stream
		      #'filename-completer
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success))
    (or pathname string)))

(define-command com-find-file ()
  (let ((filename (handler-case (accept 'completable-pathname
					:prompt "Find File")
		    (simple-parse-error () (error 'file-not-found)))))
    (setf (buffer *application-frame*)
	  (make-instance 'abbrev-buffer
	     :contents (cons nil
			     (with-open-file (stream filename :direction :input)
			       (loop for ch = (read-char stream nil nil)
				     while ch
				     collect ch)))))
    (setf (slot-value *application-frame* 'point)
	  (make-instance 'standard-right-sticky-mark
	     :buffer (buffer *application-frame*)))))

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
(global-set-key '(#\x :meta) 'com-extended-command)
(global-set-key '(#\a :meta) 'com-insert-weird-stuff)
(global-set-key '(#\c :meta) 'com-insert-number)

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

(add-command-to-command-table 'com-find-file 'c-x-climacs-table
			      :keystroke '(#\f :control))


