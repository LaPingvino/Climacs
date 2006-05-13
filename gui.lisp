;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

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

(defclass extended-pane (climacs-pane esa-pane-mixin)
  (;; for next-line and previous-line commands
   (goal-column :initform nil)
   ;; for dynamic abbrev expansion
   (original-prefix :initform nil)
   (prefix-start-offset :initform nil)
   (dabbrev-expansion-mark :initform nil)
   (overwrite-mode :initform nil)))

(defgeneric buffer-pane-p (pane)
  (:documentation "Returns T when a pane contains a buffer."))

(defmethod buffer-pane-p (pane)
  (declare (ignore pane))
  nil)

(defmethod buffer-pane-p ((pane extended-pane))
  t)

(defclass climacs-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defclass climacs-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :default-view +climacs-textual-view+))

(defparameter *with-scrollbars* t
  "If T, classic look and feel. If NIL, stripped-down look (:")

;;; Basic command tables follow. The global command table,
;;; `global-climacs-table', inherits from these, so they should not
;;; contain any overly syntax-specific commands. The idea is that it
;;; should be safe for any syntax to inherit its command-table from
;;; `global-climacs-table' (so the usual movement, search and
;;; navigation-commands are available), without risking adding alien
;;; commands that require the buffer to be in a specific syntax.

;;; Basic functionality
(make-command-table 'base-table :errorp nil)
;;; buffers
(make-command-table 'buffer-table :errorp nil)
;;; case
(make-command-table 'case-table :errorp nil)
;;; comments
(make-command-table 'comment-table :errorp nil)
;;; deleting
(make-command-table 'deletion-table :errorp nil)
;;; commands used for climacs development
(make-command-table 'development-table :errorp nil)
;;; editing - making changes to a buffer
(make-command-table 'editing-table :errorp nil)
;;; filling
(make-command-table 'fill-table :errorp nil)
;;; indentation
(make-command-table 'indent-table :errorp nil)
;;; information about the buffer
(make-command-table 'info-table :errorp nil)
;;; marking things
(make-command-table 'marking-table :errorp nil)
;;; moving around
(make-command-table 'movement-table :errorp nil)
;;; panes
(make-command-table 'pane-table :errorp nil)
;;; searching
(make-command-table 'search-table :errorp nil)
;;; self-insertion
(make-command-table 'self-insert-table :errorp nil)
;;; windows
(make-command-table 'window-table :errorp nil)

;;; customization of help.  FIXME: this might be better done by having
;;; the functions that the ESA commands call be customizeable generic
;;; functions; however, while they're not, scribbling over the ESA
;;; command tables is a bad thing.
(make-command-table 'climacs-help-table :inherit-from '(help-table)
                    :errorp nil)


(defvar *bg-color* +white+)
(defvar *fg-color* +black+)
(defvar *info-bg-color* +gray85+)
(defvar *info-fg-color* +black+)
(defvar *mini-bg-color* +white+)
(defvar *mini-fg-color* +black+)


(define-application-frame climacs (standard-application-frame
				   esa-frame-mixin)
  ((buffers :initform '() :accessor buffers))
  
  (:command-table (global-climacs-table
		   :inherit-from (global-esa-table
				  keyboard-macro-table
				  climacs-help-table
				  base-table
				  buffer-table
				  case-table
				  comment-table
				  deletion-table
				  development-table
				  editing-table
				  fill-table
				  indent-table
				  info-table
				  marking-table
				  movement-table
				  pane-table
				  search-table
				  self-insert-table
				  window-table)))
  (:menu-bar nil)
  (:panes
   (climacs-window
    (let* ((extended-pane 
	    (make-pane 'extended-pane
		       :width 900 :height 400
		       :end-of-line-action :scroll
		       :incremental-redisplay t
		       :background *bg-color*
		       :foreground *fg-color*
		       :display-function 'display-window
		       :command-table 'global-climacs-table))
	   (info-pane
	    (make-pane 'climacs-info-pane
		       :master-pane extended-pane
		       :background *info-bg-color*
		       :foreground *info-fg-color*
		       :width 900)))
      (setf (windows *application-frame*) (list extended-pane)
	    (buffers *application-frame*) (list (buffer extended-pane)))
	  
      (vertically ()
	(if *with-scrollbars*
	    (scrolling ()
	      extended-pane)
	    extended-pane)
	info-pane)))
   (minibuffer (make-pane 'climacs-minibuffer-pane
			  :background *mini-bg-color*
			  :foreground *mini-fg-color*
			  :width 900)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 climacs-window
	 minibuffer)))
  (:top-level (esa-top-level :prompt "M-x ")))

(defmethod frame-standard-input ((frame climacs))
  (get-frame-pane frame 'minibuffer))

(defun current-window ()
  (car (windows *application-frame*)))

(defun current-point ()
  "Return the current panes point."
  (point (current-window)))

(defun current-buffer ()
  "Return the current buffer."
  (buffer (current-window)))

(defun climacs (&key new-process (process-name "Climacs")
                (width 900) (height 400))
  "Starts up a climacs session"
  (let ((frame (make-application-frame 'climacs :width width :height height)))
    (flet ((run ()
	     (run-frame-top-level frame)))
      (if new-process
	  (clim-sys:make-process #'run :name process-name)
	  (run)))))

(defun climacs-rv (&key new-process (process-name "Climacs")
                (width 900) (height 400))
  "Starts up a climacs session"
  ;; SBCL doesn't inherit dynamic bindings when starting new
  ;; processes, so start a new processes and THEN setup the colors.
  (flet ((run ()
           (let ((*bg-color* +black+)
                 (*fg-color* +gray+)
                 (*info-bg-color* +darkslategray+)
                 (*info-fg-color* +gray+)
                 (*mini-bg-color* +black+)
                 (*mini-fg-color* +white+))
             (climacs :new-process nil :width width :height height))))
    (if new-process
      (clim-sys:make-process #'run :name process-name)
      (run))))

(defun display-info (frame pane)
  (let* ((master-pane (master-pane pane))
	 (buffer (buffer master-pane))
	 (size (size buffer))
	 (top (top master-pane))
	 (bot (bot master-pane)))
    (princ "   " pane)
    (princ (cond ((and (needs-saving buffer)
		       (read-only-p buffer)
		       "%*"))
		 ((needs-saving buffer) "**")
		 ((read-only-p buffer) "%%")
		 (t "--"))
	   pane)
    (princ "  " pane)
    (with-text-face (pane :bold)
      (format pane "~25A" (name buffer)))
    (format pane "  ~A  "
	    (cond ((and (mark= size bot)
			(mark= 0 top))
		   "")
		  ((mark= size bot)
		   "Bot")
		  ((mark= 0 top)
		   "Top")
		  (t (format nil "~a%"
			     (round (* 100 (/ (offset top)
					      size)))))))
    (with-text-family (pane :sans-serif)
      (princ #\( pane)
      (princ (name-for-info-pane (syntax buffer)) pane)
      (format pane "~{~:[~*~; ~A~]~}" (list
				       (slot-value master-pane 'overwrite-mode)
				       "Ovwrt"
				       (auto-fill-mode master-pane)
				       "Fill"
				       (isearch-mode master-pane)
				       "Isearch"))
      (princ #\) pane))
    (with-text-family (pane :sans-serif)
      (princ (if (recordingp frame)
		 "Def"
		 "")
	     pane))))

(defun display-window (frame pane)
  "The display function used by the climacs application frame."
  (redisplay-pane pane (eq pane (car (windows frame)))))

(defmethod handle-repaint :before ((pane extended-pane) region)
  (declare (ignore region))
  (redisplay-frame-pane *application-frame* pane))

(defvar *kill-ring* (make-instance 'kill-ring :max-size 7))

(defmethod execute-frame-command :around ((frame climacs) command)
  (let ((current-window (car (windows frame))))
    (handler-case
        (progn
          (if (buffer-pane-p current-window)
              (with-undo ((buffer current-window))
                (call-next-method))
              (call-next-method))
          (loop for buffer in (buffers frame)
                do (when (modified-p buffer)
                     (clear-modify buffer))))
      (offset-before-beginning ()
        (beep) (display-message "Beginning of buffer"))
      (offset-after-end ()
        (beep) (display-message "End of buffer"))
      (motion-before-beginning ()
        (beep) (display-message "Beginning of buffer"))
      (motion-after-end ()
        (beep) (display-message "End of buffer"))
      (no-expression ()
        (beep) (display-message "No expression around point"))
      (no-such-operation ()
        (beep) (display-message "Operation unavailable for syntax"))
      (buffer-read-only ()
        (beep) (display-message "Buffer is read only")))))

(defmethod execute-frame-command :after ((frame climacs) command)
  (loop for buffer in (buffers frame)
        do (update-syntax buffer (syntax buffer))
	do (when (modified-p buffer)
	     (setf (needs-saving buffer) t))))

(defmethod find-applicable-command-table ((frame climacs))
  (or
   (let ((syntax (and (buffer-pane-p (current-window))
		      (syntax (buffer (current-window))))))
      (and syntax
	   (slot-exists-p syntax 'command-table)
	   (slot-boundp syntax 'command-table)
	   (slot-value syntax 'command-table)
	   (find-command-table (slot-value syntax 'command-table))))
   (find-command-table 'global-climacs-table)))

(define-command (com-full-redisplay :name t :command-table base-table) ()
  "Redisplay the contents of the current window.
FIXME: does this really have that effect?"
  (full-redisplay (current-window)))

(set-key 'com-full-redisplay
	 'base-table
	 '((#\l :control)))

(defun load-file (file-name)
  (cond ((directory-pathname-p file-name)
	 (display-message "~A is a directory name." file-name)
	 (beep))
	(t
	 (cond ((probe-file file-name)
		(load file-name))
	       (t
		(display-message "No such file: ~A" file-name)
		(beep))))))

(define-command (com-load-file :name t :command-table base-table) ()
  "Prompt for a filename and CL:LOAD that file.
Signals and error if the file does not exist."
  (let ((filepath (accept 'pathname :prompt "Load File")))
    (load-file filepath)))

(set-key 'com-load-file
	 'base-table
	 '((#\c :control) (#\l :control)))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (set-key `(com-self-insert ,*numeric-argument-marker*)
	     'self-insert-table
	     (list (list (code-char code)))))

(set-key `(com-self-insert ,*numeric-argument-marker*)
	 'self-insert-table
	 '((#\Newline)))

;;;;;;;;;;;;;;;;;;;
;;; Pane commands

(defun make-buffer (&optional name)
  (let ((buffer (make-instance 'climacs-buffer)))
    (when name (setf (name buffer) name))
    (push buffer (buffers *application-frame*))
    buffer))

(defgeneric erase-buffer (buffer))

(defmethod erase-buffer ((buffer string))
  (let ((b (find buffer (buffers *application-frame*)
		 :key #'name :test #'string=)))
    (when b (erase-buffer b))))

(defmethod erase-buffer ((buffer climacs-buffer))
  (let* ((point (point buffer))
	 (mark (clone-mark point)))
    (beginning-of-buffer mark)
    (end-of-buffer point)
    (delete-region mark point)))

(define-presentation-method present (object (type buffer)
					    stream
					    (view textual-view)
					    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (name object) stream))

(define-presentation-method accept
    ((type buffer) stream (view textual-view) &key (default nil defaultp)
     (default-type type))
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far (buffers *application-frame*) '() :action action
			 :name-key #'name
			 :value-key #'identity))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (cond (success
	   (values object type))
	  ((and (zerop (length string)) defaultp)
	    (values default default-type))
	  (t (values string 'string)))))

(defgeneric switch-to-buffer (buffer))

(defmethod switch-to-buffer ((buffer climacs-buffer))
  (let* ((buffers (buffers *application-frame*))
	 (position (position buffer buffers))
	 (pane (current-window)))
    (if position
	(rotatef (car buffers) (nth position buffers))
	(push buffer (buffers *application-frame*)))
    (setf (offset (point (buffer pane))) (offset (point pane)))
    (setf (buffer pane) buffer)
    (full-redisplay pane)
    buffer))

(defmethod switch-to-buffer ((name string))
  (let ((buffer (find name (buffers *application-frame*)
		      :key #'name :test #'string=)))
    (switch-to-buffer (or buffer
			  (make-buffer name)))))

;;placeholder
(defmethod switch-to-buffer ((symbol (eql 'nil)))  
  (let ((default (second (buffers *application-frame*))))
    (when default
      (switch-to-buffer default))))

;; ;;; FIXME: see the comment by (SETF SYNTAX) :AROUND.  -- CSR,
;; ;;; 2005-10-31.
;; (defmethod (setf buffer) :around (buffer (pane extended-pane))
;;   (call-next-method)
;;   (note-pane-syntax-changed pane (syntax buffer)))

(define-command (com-switch-to-buffer :name t :command-table pane-table) ()
  "Prompt for a buffer name and switch to that buffer.
If the a buffer with that name does not exist, create it. Uses the name of the next buffer (if any) as a default."
  (let* ((default (second (buffers *application-frame*)))
	 (buffer (if default
		     (accept 'buffer
			     :prompt "Switch to buffer"
			     :default default)
		     (accept 'buffer
			     :prompt "Switch to buffer"))))
    (switch-to-buffer buffer)))

(set-key 'com-switch-to-buffer
	 'pane-table
	 '((#\x :control) (#\b)))

(defgeneric kill-buffer (buffer))

(defmethod kill-buffer ((buffer climacs-buffer))
  (with-slots (buffers) *application-frame*
     (when (and (needs-saving buffer)
		(handler-case (accept 'boolean :prompt "Save buffer first?")
		  (error () (progn (beep)
				   (display-message "Invalid answer")
				   (return-from kill-buffer nil)))))
       (com-save-buffer))
     (setf buffers (remove buffer buffers))
     ;; Always need one buffer.
     (when (null buffers)
       (make-buffer "*scratch*"))
     (setf (buffer (current-window)) (car buffers))
     (full-redisplay (current-window))
     (buffer (current-window))))

(defmethod kill-buffer ((name string))
  (let ((buffer (find name (buffers *application-frame*)
		      :key #'name :test #'string=)))
    (when buffer (kill-buffer buffer))))

(defmethod kill-buffer ((symbol (eql 'nil)))
  (kill-buffer (buffer (current-window))))

(define-command (com-kill-buffer :name t :command-table pane-table) ()
  "Prompt for a buffer name and kill that buffer.
If the buffer needs saving, will prompt you to do so before killing it. Uses the current buffer as a default."
  (let ((buffer (accept 'buffer
			:prompt "Kill buffer"
			:default (buffer (current-window))
			:default-type 'buffer)))
    (kill-buffer buffer)))

(set-key 'com-kill-buffer
	 'pane-table
	 '((#\x :control) (#\k)))

;;; For the ESA help functions.

(defmethod help-stream ((frame climacs) title)
  (typeout-window (format nil "~10T~A" title)))

