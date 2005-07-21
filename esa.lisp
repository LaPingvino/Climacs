;;; -*- Mode: Lisp; Package: ESA -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)

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

;;; Emacs-Style Appication

(in-package :esa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Info pane, a pane that displays some information about another pane

(defclass info-pane (application-pane)
  ((master-pane :initarg :master-pane))
  (:default-initargs
      :background +gray85+
      :scroll-bars nil
      :borders nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Minibuffer pane

(defclass minibuffer-pane (application-pane)
  ((message :initform nil :accessor message))
  (:default-initargs
      :scroll-bars nil
      :display-function 'display-minibuffer))

(defun display-minibuffer (frame pane)
  (declare (ignore frame))
  (with-slots (message) pane
    (unless (null message)
    (princ message pane)
    (setf message nil))))

(defmethod stream-accept :before ((pane minibuffer-pane) type &rest args)
  (declare (ignore type args))
  (window-clear pane))

(defun display-message (format-string &rest format-args)
  (setf (message *standard-input*)
	(apply #'format nil format-string format-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ESA pane mixin

(defclass esa-pane-mixin ()
  (;; allows a certain number of commands to have some minimal memory
   (previous-command :initform nil :accessor previous-command)))

(defmethod handle-repaint :before ((pane esa-pane-mixin) region)
  (declare (ignore region))
  (redisplay-frame-pane *application-frame* pane))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ESA frame mixin

(defclass esa-frame-mixin ()
  ((windows :accessor windows)
   (recordingp :initform nil :accessor recordingp)
   (executingp :initform nil :accessor executingp)
   (recorded-keys :initform '() :accessor recorded-keys)
   (remaining-keys :initform '() :accessor remaining-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Command processing

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

(defun meta-digit (gesture)
  (position gesture
	    '((#\0 :meta) (#\1 :meta) (#\2 :meta) (#\3 :meta) (#\4 :meta)
	      (#\5 :meta) (#\6 :meta) (#\7 :meta) (#\8 :meta) (#\9 :meta))
	    :test #'event-matches-gesture-name-p))

(defun esa-read-gesture ()
  (unless (null (remaining-keys *application-frame*))
    (return-from esa-read-gesture
      (pop (remaining-keys *application-frame*))))
  (loop for gesture = (read-gesture :stream *standard-input*)
	until (or (characterp gesture)
		  (and (typep gesture 'keyboard-event)
		       (or (keyboard-event-character gesture)
			   (not (member (keyboard-event-key-name
					 gesture)
					'(:control-left :control-right
					  :shift-left :shift-right
					  :meta-left :meta-right
					  :super-left :super-right
					  :hyper-left :hyper-right
					  :shift-lock :caps-lock
					  :alt-left :alt-right))))))
	finally (progn (when (recordingp *application-frame*)
			 (push gesture (recorded-keys *application-frame*)))
		       (return gesture))))

(defun esa-unread-gesture (gesture stream)
  (cond ((recordingp *application-frame*)
	 (pop (recorded-keys *application-frame*))
	 (unread-gesture gesture :stream stream))
	((executingp *application-frame*)
	 (push gesture (remaining-keys *application-frame*)))
	(t 
	 (unread-gesture gesture :stream stream))))

(defun read-numeric-argument (&key (stream *standard-input*))
  (let ((gesture (esa-read-gesture)))
    (cond ((event-matches-gesture-name-p gesture '(:keyboard #\u 512)) ; FIXME
	   (let ((numarg 4))
	     (loop for gesture = (esa-read-gesture)
		   while (event-matches-gesture-name-p gesture '(:keyboard #\u 512)) ; FIXME
		   do (setf numarg (* 4 numarg))
		   finally (esa-unread-gesture gesture stream))
	     (let ((gesture (esa-read-gesture)))
	       (cond ((and (characterp gesture)
			   (digit-char-p gesture 10))
		      (setf numarg (- (char-code gesture) (char-code #\0)))
		      (loop for gesture = (esa-read-gesture)
			    while (and (characterp gesture)
				       (digit-char-p gesture 10))
			    do (setf numarg (+ (* 10 numarg)
					       (- (char-code gesture) (char-code #\0))))
			    finally (esa-unread-gesture gesture stream)
				    (return (values numarg t))))
		     (t
		      (esa-unread-gesture gesture stream)
		      (values numarg t))))))
	  ((meta-digit gesture)
	   (let ((numarg (meta-digit gesture)))
	     (loop for gesture = (esa-read-gesture)
		   while (meta-digit gesture)
		   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
		   finally (esa-unread-gesture gesture stream)
			   (return (values numarg t)))))
	  (t (esa-unread-gesture gesture stream)
	     (values 1 nil)))))

(defvar *numeric-argument-p* (list nil))

(defun substitute-numeric-argument-p (command numargp)
  (substitute numargp *numeric-argument-p* command :test #'eq))

(defun process-gestures (frame command-table)
  (loop
   for gestures = '()
   do (multiple-value-bind (numarg numargp)
	  (read-numeric-argument :stream *standard-input*)
	(loop 
	 (setf *current-gesture* (esa-read-gesture))
	 (setf gestures 
	       (nconc gestures (list *current-gesture*)))
	 (let ((item (find-gestures gestures command-table)))
	   (cond 
	     ((not item)
	      (beep) (return))
	     ((eq (command-menu-item-type item) :command)
	      (let ((command (command-menu-item-value item)))
		(unless (consp command)
		  (setf command (list command)))
		(setf command (substitute-numeric-argument-marker command numarg))
		(setf command (substitute-numeric-argument-p command numargp))
		(execute-frame-command frame command)
		(return)))
	     (t nil)))))
   do (redisplay-frame-panes frame)))

(defmethod redisplay-frame-panes :around ((frame esa-frame-mixin) &key force-p)
  (declare (ignore force-p))
  (when (null (remaining-keys *application-frame*))
    (setf (executingp *application-frame*) nil)
    (call-next-method)))

(defmethod execute-frame-command :after ((frame esa-frame-mixin) command)
  (setf (previous-command *standard-output*)
	(if (consp command)
	    (car command)
	    command)))

(defun find-real-pane (vbox)
  (first (sheet-children
	  (find-if-not (lambda (pane) (typep pane 'scroll-bar-pane))
		       (sheet-children
			(find-if (lambda (pane) (typep pane 'scroller-pane))
				 (sheet-children vbox)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Top level

(defun esa-top-level (frame &key
			    command-parser command-unparser
			    partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (with-slots (windows) frame
    (setf windows (list (find-real-pane (find-pane-named frame 'win))))
    (let ((*standard-output* (car windows))
	  (*standard-input* (frame-standard-input frame))
	  (*print-pretty* nil)
	  (*abort-gestures* '((:keyboard #\g 512))))
      (redisplay-frame-panes frame :force-p t)
      (loop
       for maybe-error = t
       do (restart-case
	   (progn
	     (handler-case
	      (with-input-context 
		  ('(command :command-table global-example-table))
		  (object)
		  (process-gestures frame 'global-example-table)
		(t
		 (execute-frame-command frame object)
		 (setq maybe-error nil)))
	      (abort-gesture () (display-message "Quit")))
	     (when maybe-error
	       (beep))
	     (redisplay-frame-panes frame))
	   (return-to-climacs () nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; example application

(defclass example-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (with-slots (master-pane) pane
    (format pane "Pane name: ~s" (pane-name master-pane))))

(defclass example-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20))

(defclass example-pane (esa-pane-mixin application-pane)
  ((contents :initform "hello" :accessor contents)))

(define-application-frame example (standard-application-frame
				   esa-frame-mixin)
  ()
  (:panes
   (win (let* ((my-pane 
		(make-pane 'example-pane
			   :width 900 :height 400
			   :name 'my-pane
			   :display-function 'display-my-pane))
	       (my-info-pane
		(make-pane 'example-info-pane
			   :master-pane my-pane
			   :width 900)))
	  (vertically ()
	    (scrolling ()
	      my-pane)
	    my-info-pane)))
   (int (make-pane 'example-minibuffer-pane :width 900)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 win
	 int)))
  (:top-level (esa-top-level)))

(defun display-my-pane (frame pane)
  (declare (ignore frame))
  (princ (contents pane) *standard-output*))

(defun example (&key (width 900) (height 400))
  "Starts up the example application"
  (let ((frame (make-application-frame 'example :width width :height height)))
    (run-frame-top-level frame)))

(define-command-table global-example-table)

(define-command (com-quit :name t :command-table global-example-table) ()
  (frame-exit *application-frame*))

(defun set-key (command table gesture)
  (add-command-to-command-table
    command table :keystroke gesture :errorp nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; C-x command table

(make-command-table 'global-c-x-example-table :errorp nil)

(add-menu-item-to-command-table 'global-example-table "C-x"
				:menu 'global-c-x-example-table
				:keystroke '(#\x :control))

(set-key 'com-quit 'global-c-x-example-table
	 '(#\c :control))





