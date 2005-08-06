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
  ((master-pane :initarg :master-pane :reader master-pane))
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
   (previous-command :initform nil :accessor previous-command)
   (command-table :initarg :command-table :accessor command-table)))

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

(defun find-gestures-with-inheritance (gestures start-table)
  (or (find-gestures gestures start-table)
      (some (lambda (table)
	      (find-gestures-with-inheritance gestures table))
	    (command-table-inherit-from
	     (find-command-table start-table)))))

(defparameter *current-gesture* nil)

(defparameter *meta-digit-table*
  (loop for i from 0 to 9
       collect (list :keyboard (digit-char i) (make-modifier-state :meta))))

(defun meta-digit (gesture)
  (position gesture *meta-digit-table*
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

(define-gesture-name universal-argument :keyboard (#\u :control))

(define-gesture-name meta-minus :keyboard (#\- :meta))

(defun read-numeric-argument (&key (stream *standard-input*))
  "Reads gestures returning two values: prefix-arg and whether prefix given.
Accepts: EITHER C-u, optionally followed by other C-u's, optionally followed
by a minus sign, optionally followed by decimal digits;
OR An optional M-minus, optionally followed by M-decimal-digits.
You cannot mix C-u and M-digits.
C-u gives a numarg of 4. Additional C-u's multiply by 4 (e.g. C-u C-u C-u = 64).
After C-u you can enter decimal digits, possibly preceded by a minus (but not
a plus) sign. C-u 3 4 = 34, C-u - 3 4 = -34. Note that C-u 3 - prints 3 '-'s.
M-1 M-2 = 12. M-- M-1 M-2 = -12. As a special case, C-u - and M-- = -1.
In the absence of a prefix arg returns 1 (and nil)."
  (let ((gesture (esa-read-gesture)))
    (cond ((event-matches-gesture-name-p
	    gesture 'universal-argument)
	   (let ((numarg 4))
	     (loop for gesture = (esa-read-gesture)
		   while (event-matches-gesture-name-p
			  gesture 'universal-argument)
		   do (setf numarg (* 4 numarg))
		   finally (esa-unread-gesture gesture stream))
	     (let ((gesture (esa-read-gesture))
		   (sign +1))
	       (when (and (characterp gesture)
			  (char= gesture #\-))
		 (setf gesture (esa-read-gesture)
		       sign -1))
	       (cond ((and (characterp gesture)
			   (digit-char-p gesture 10))
		      (setf numarg (digit-char-p gesture 10))
		      (loop for gesture = (esa-read-gesture)
			    while (and (characterp gesture)
				       (digit-char-p gesture 10))
			    do (setf numarg (+ (* 10 numarg)
					       (digit-char-p gesture 10)))
			    finally (esa-unread-gesture gesture stream)
				    (return (values (* numarg sign) t))))
		     (t
		      (esa-unread-gesture gesture stream)
		      (values (if (minusp sign) -1 numarg) t))))))
	  ((or (meta-digit gesture)
	       (event-matches-gesture-name-p
		gesture 'meta-minus))
	   (let ((numarg 0)
		 (sign +1))
	     (cond ((meta-digit gesture)
		    (setf numarg (meta-digit gesture)))
		   (t (setf sign -1)))
	     (loop for gesture = (esa-read-gesture)
		   while (meta-digit gesture)
		   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
		   finally (esa-unread-gesture gesture stream)
			   (return (values (if (and (= sign -1) (= numarg 0))
					       -1
					       (* sign numarg))
					   t)))))
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
	 (let ((item (find-gestures-with-inheritance gestures command-table)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Top level

(defun esa-top-level (frame &key
			    command-parser command-unparser
			    partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (with-slots (windows) frame
    (let ((*standard-output* (car windows))
	  (*standard-input* (frame-standard-input frame))
	  (*print-pretty* nil)
	  (*abort-gestures* `((:keyboard #\g ,(make-modifier-state :control)))))
      (redisplay-frame-panes frame :force-p t)
      (loop
       for maybe-error = t
       do (restart-case
	   (progn
	     (handler-case
	      (with-input-context 
		  (`(command :command-table ,(command-table (car (windows frame)))))
		  (object)
		  (process-gestures frame (command-table (car (windows frame))))
		(t
		 (execute-frame-command frame object)
		 (setq maybe-error nil)))
	      (abort-gesture () (display-message "Quit")))
	     (when maybe-error
	       (beep))
	     (redisplay-frame-panes frame))
	   (return-to-climacs () nil))))))

(defmacro simple-command-loop (command-table loop-condition end-clauses)
  (let ((gesture (gensym))
        (item (gensym))
        (command (gensym)))
    `(progn 
       (redisplay-frame-panes *application-frame*)
       (loop while ,loop-condition
             as ,gesture = (esa-read-gesture)
             as ,item = (find-gestures-with-inheritance (list ,gesture) ,command-table)
             do (cond ((and ,item (eq (command-menu-item-type ,item) :command))
                       (setf *current-gesture* ,gesture)
                       (let ((,command (command-menu-item-value ,item)))
                         (unless (consp ,command)
                           (setf ,command (list ,command)))
			 (execute-frame-command *application-frame*
						,command)))
                      (t
                       (unread-gesture ,gesture)
                       ,@end-clauses))
             (redisplay-frame-panes *application-frame*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; comand table manipulation

(defun ensure-subtable (table gesture)
  (let* ((event (make-instance
		'key-press-event
		:key-name nil
		:key-character (car gesture)
		:modifier-state (apply #'make-modifier-state (cdr gesture))))
	 (item (find-keystroke-item event table :errorp nil)))
    (when (or (null item) (not (eq (command-menu-item-type item) :menu)))
      (let ((name (gensym)))
	(make-command-table name :errorp nil)
	(add-menu-item-to-command-table table (symbol-name name)
					:menu name
					:keystroke gesture)))
    (command-menu-item-value
     (find-keystroke-item event table :errorp nil))))
    
      
(defun set-key (command table gestures)
  (if (null (cdr gestures))
      (add-command-to-command-table
       command table :keystroke (car gestures) :errorp nil)
      (set-key command
	       (ensure-subtable table (car gestures))
	       (cdr gestures))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; standard key bindings 

;;; global

(define-command-table global-esa-table)

(define-command (com-quit :name t :command-table global-esa-table) ()
  (frame-exit *application-frame*))

(set-key 'com-quit 'global-esa-table '((#\x :control) (#\c :control)))

(define-command (com-extended-command
		 :name t
		 :command-table global-esa-table)
    ()
  (let ((item (handler-case
	       (accept
		`(command :command-table
			  ,(command-table (car (windows *application-frame*))))
		:prompt "Extended Command")
	       (error () (progn (beep)
				(display-message "No such command")
				(return-from com-extended-command nil))))))
    (execute-frame-command *application-frame* item)))

(set-key 'com-extended-command 'global-esa-table '((#\x :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Keyboard macros

(define-command-table keyboard-macro-table)

(define-command (com-start-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  (setf (recordingp *application-frame*) t)
  (setf (recorded-keys *application-frame*) '()))

(set-key 'com-start-kbd-macro 'keyboard-macro-table '((#\x :control) #\())

(define-command (com-end-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  (setf (recordingp *application-frame*) nil)
  (setf (recorded-keys *application-frame*)
	;; this won't work if the command was invoked in any old way
	(reverse (cddr (recorded-keys *application-frame*)))))

(set-key 'com-end-kbd-macro 'keyboard-macro-table '((#\x :control) #\)))

(define-command (com-call-last-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  (setf (remaining-keys *application-frame*)
	(recorded-keys *application-frame*))
  (setf (executingp *application-frame*) t))

(set-key 'com-call-last-kbd-macro 'keyboard-macro-table '((#\x :control) #\e))

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
  (format pane "Pane name: ~s" (pane-name (master-pane pane))))

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
			   :display-function 'display-my-pane
			   :command-table 'global-example-table))
	       (my-info-pane
		(make-pane 'example-info-pane
			   :master-pane my-pane
			   :width 900)))
	  (setf (windows *application-frame*) (list my-pane))
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
  (let ((frame (make-application-frame
		'example
		:width width :height height)))
    (run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands and key bindings

(define-command-table global-example-table
    :inherit-from (global-esa-table keyboard-macro-table))

