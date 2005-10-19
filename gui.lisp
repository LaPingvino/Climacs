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
;;; lisp-related commands
(make-command-table 'lisp-table :errorp nil)
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

(define-application-frame climacs (standard-application-frame
				   esa-frame-mixin)
  ((buffers :initform '() :accessor buffers))
  (:command-table (global-climacs-table
		   :inherit-from (global-esa-table
				  keyboard-macro-table
				  help-table
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
				  lisp-table
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
		       :display-function 'display-window
		       :command-table 'global-climacs-table))
	   (info-pane
	    (make-pane 'climacs-info-pane
		       :master-pane extended-pane
		       :width 900)))
      (setf (windows *application-frame*) (list extended-pane)
	    (buffers *application-frame*) (list (buffer extended-pane)))
	  
      (vertically ()
	(if *with-scrollbars*
	    (scrolling ()
	      extended-pane)
	    extended-pane)
	info-pane)))
   (minibuffer (make-pane 'climacs-minibuffer-pane :width 900)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 climacs-window
	 minibuffer)))
  (:top-level (esa-top-level)))

(defun current-window ()
  (car (windows *application-frame*)))

(defmethod redisplay-frame-panes :around ((frame climacs) &rest args)
  (declare (ignore args))
  (let ((buffers (remove-duplicates (loop for pane in (windows frame)
					  when (typep pane 'extended-pane)
					    collect (buffer pane)))))
    (loop for buffer in buffers
	  do (update-syntax buffer (syntax buffer)))
    (call-next-method)
    (loop for buffer in buffers
	  do (clear-modify buffer))))

(defun climacs (&key new-process (process-name "Climacs")
                (width 900) (height 400))
  "Starts up a climacs session"
  (let ((frame (make-application-frame 'climacs :width width :height height)))
    (flet ((run ()
	     (run-frame-top-level frame)))
      (if new-process
	  (clim-sys:make-process #'run :name process-name)
	  (run)))))

(defun display-info (frame pane)
  (declare (ignore frame))
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
      (princ (if (recordingp *application-frame*)
		 "Def"
		 "")
	     pane))))

(defun display-window (frame pane)
  "The display function used by the climacs application frame."
  (declare (ignore frame))
  (redisplay-pane pane (eq pane (current-window))))

(defmethod handle-repaint :before ((pane extended-pane) region)
  (declare (ignore region))
  (redisplay-frame-pane *application-frame* pane))

(defvar *kill-ring* (make-instance 'kill-ring :max-size 7))

(defmethod execute-frame-command :around ((frame climacs) command)
  (handler-case
      (if (typep (current-window) 'extended-pane)
	  (with-undo ((buffer (current-window)))
	    (call-next-method))
	  (call-next-method))
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
      (beep) (display-message "Buffer is read only"))))  

(defmethod execute-frame-command :after ((frame climacs) command)
  (loop for buffer in (buffers frame)
	do (when (modified-p buffer)
	     (setf (needs-saving buffer) t))))	

(define-command (com-overwrite-mode :name t :command-table editing-table) ()
  (with-slots (overwrite-mode) (current-window)
    (setf overwrite-mode (not overwrite-mode))))

(set-key 'com-overwrite-mode
	 'editing-table
	 '((:insert)))

(define-command (com-not-modified :name t :command-table buffer-table) ()
  (setf (needs-saving (buffer (current-window))) nil))

(set-key 'com-not-modified
	 'buffer-table
	 '((#\~ :meta :shift)))

(define-command (com-set-fill-column :name t :command-table fill-table)
    ((column 'integer :prompt "Column Number:"))
  (set-fill-column column))

(set-key `(com-set-fill-column ,*numeric-argument-marker*)
	 'fill-table
	 '((#\x :control) (#\f)))

(defun set-fill-column (column)
  (if (> column 1)
      (setf (auto-fill-column (current-window)) column)
      (progn (beep) (display-message "Set Fill Column requires an explicit argument."))))

(defun possibly-fill-line ()
  (let* ((pane (current-window))
         (buffer (buffer pane)))
    (when (auto-fill-mode pane)
      (let* ((fill-column (auto-fill-column pane))
             (point (point pane))
             (offset (offset point))
             (tab-width (tab-space-count (stream-default-view pane)))
             (syntax (syntax buffer)))
        (when (>= (buffer-display-column buffer offset tab-width)
                  (1- fill-column))
          (fill-line point
                     (lambda (mark)
                       (syntax-line-indentation mark tab-width syntax))
                     fill-column
                     tab-width))))))

(defun insert-character (char)
  (let* ((window (current-window))
	 (point (point window)))
    (unless (constituentp char)
      (possibly-expand-abbrev point))
    (when (whitespacep char)
      (possibly-fill-line))
    (if (and (slot-value window 'overwrite-mode) (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point char))
	(insert-object point char))))

(define-command com-self-insert ((count 'integer))
  (loop repeat count do (insert-character *current-gesture*)))

(define-command (com-beginning-of-line :name t :command-table movement-table) ()
  (beginning-of-line (point (current-window))))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((:home)))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((#\a :control)))

(define-command (com-end-of-line :name t :command-table movement-table) ()
  (end-of-line (point (current-window))))

(set-key 'com-end-of-line
	 'movement-table
	 '((#\e :control)))

(set-key 'com-end-of-line
	 'movement-table
	 '((:end)))

(define-command (com-delete-object :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of Objects")
     (killp 'boolean :prompt "Kill?"))
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (forward-object mark count)
    (when killp
      (kill-ring-standard-push *kill-ring*
			       (region-to-sequence point mark)))
    (delete-region point mark)))

(set-key `(com-delete-object ,*numeric-argument-marker*
			     ,*numeric-argument-p*)
	 'deletion-table
	 '(#\Rubout))

(set-key `(com-delete-object ,*numeric-argument-marker*
			     ,*numeric-argument-p*)
	 'deletion-table
	 '((#\d :control)))

(define-command (com-backward-delete-object :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of Objects")
     (killp 'boolean :prompt "Kill?"))
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (backward-object mark count)
    (when killp
      (kill-ring-standard-push *kill-ring*
			       (region-to-sequence mark point)))
  (delete-region mark point)))

(set-key `(com-backward-delete-object ,*numeric-argument-marker*
				      ,*numeric-argument-p*)
	 'deletion-table
	 '(#\Backspace))

(define-command (com-zap-to-object :name t :command-table deletion-table) ()
  (let* ((item (handler-case (accept 't :prompt "Zap to Object")
		(error () (progn (beep)
				 (display-message "Not a valid object")
				 (return-from com-zap-to-object nil)))))
	 (current-point (point (current-window)))
	 (item-mark (clone-mark current-point))
	 (current-offset (offset current-point)))
    (search-forward item-mark (vector item))
    (delete-range current-point (- (offset item-mark) current-offset))))

(define-command (com-zap-to-character :name t :command-table deletion-table) ()
  (let* ((item-string (handler-case (accept 'string :prompt "Zap to Character") ; Figure out how to get #\d and d.  (or 'string 'character)?
		(error () (progn (beep)
				 (display-message "Not a valid string. ")
				 (return-from com-zap-to-character nil)))))
       (item (subseq item-string 0 1))
       (current-point (point (current-window)))
       (item-mark (clone-mark current-point))

       (current-offset (offset current-point)))
  (if (> (length item-string) 1)
      (display-message "Using just the first character"))
  (search-forward item-mark item)
  (delete-range current-point (- (offset item-mark) current-offset))))

(set-key 'com-zap-to-character
	 'deletion-table
	 '((#\z :meta)))

(defun transpose-objects (mark)
  (unless (beginning-of-buffer-p mark)
    (when (end-of-line-p mark)
      (backward-object mark))
    (let ((object (object-after mark)))
      (delete-range mark)
      (backward-object mark)
      (insert-object mark object)
      (forward-object mark))))

(define-command (com-transpose-objects :name t :command-table editing-table) ()
  (transpose-objects (point (current-window))))

(set-key 'com-transpose-objects
	 'editing-table
	 '((#\t :control)))

(define-command (com-backward-object :name t :command-table movement-table)
    ((count 'integer :prompt "Number of Objects"))
  (backward-object (point (current-window)) count))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :control)))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((:left)))

(define-command (com-forward-object :name t :command-table movement-table)
    ((count 'integer :prompt "Number of Objects"))
  (forward-object (point (current-window)) count))

(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :control)))

(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((:right)))

(defun transpose-words (mark)
  (let (bw1 bw2 ew1 ew2)
    (backward-word mark)
    (setf bw1 (offset mark))
    (forward-word mark)
    (setf ew1 (offset mark))
    (forward-word mark)
    (when (= (offset mark) ew1)
      ;; this is emacs' message in the minibuffer
      (error "Don't have two things to transpose"))
    (setf ew2 (offset mark))
    (backward-word mark)
    (setf bw2 (offset mark))
    (let ((w2 (buffer-sequence (buffer mark) bw2 ew2))
	  (w1 (buffer-sequence (buffer mark) bw1 ew1)))
      (delete-word mark)
      (insert-sequence mark w1)
      (backward-word mark)
      (backward-word mark)
      (delete-word mark)
      (insert-sequence mark w2)
      (forward-word mark))))

(define-command (com-transpose-words :name t :command-table editing-table) ()
  (transpose-words (point (current-window))))

(set-key 'com-transpose-words
	 'editing-table
	 '((#\t :meta)))

(defun transpose-lines (mark)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (previous-line mark))
  (let* ((bol (offset mark))
	 (eol (progn (end-of-line mark)
		     (offset mark)))
	 (line (buffer-sequence (buffer mark) bol eol)))
    (delete-region bol mark)
    ;; Remove newline at end of line as well.
    (unless (end-of-buffer-p mark)
      (delete-range mark))
    ;; If the current line is at the end of the buffer, we want to
    ;; be able to insert past it, so we need to get an extra line
    ;; at the end.
    (end-of-line mark)
    (when (end-of-buffer-p mark)
      (insert-object mark #\Newline))
    (next-line mark 0)
    (insert-sequence mark line)
    (insert-object mark #\Newline)))

(define-command (com-transpose-lines :name t :command-table editing-table) ()
  (transpose-lines (point (current-window))))

(set-key 'com-transpose-lines
	 'editing-table
	 '((#\x :control) (#\t :control)))

(define-command (com-previous-line :name t :command-table movement-table)
    ((numarg 'integer :prompt "How many lines?"))
  (let* ((window (current-window))
	 (point (point window)))
    (unless (or (eq (previous-command window) 'com-previous-line)
		(eq (previous-command window) 'com-next-line))
      (setf (slot-value window 'goal-column) (column-number point)))
    (if (plusp numarg)
	(previous-line point (slot-value window 'goal-column) numarg)
	(next-line point (slot-value window 'goal-column) (- numarg)))))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\p :control)))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'movement-table
	 '((:up)))

(define-command (com-next-line :name t :command-table movement-table)
    ((numarg 'integer :prompt "How many lines?"))
  (let* ((window (current-window))
	 (point (point window)))
    (unless (or (eq (previous-command window) 'com-previous-line)
		(eq (previous-command window) 'com-next-line))
      (setf (slot-value window 'goal-column) (column-number point)))
    (if (plusp numarg)
	(next-line point (slot-value window 'goal-column) numarg)
	(previous-line point (slot-value window 'goal-column) (- numarg)))))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n :control)))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'movement-table
	 '((:down)))

(define-command (com-open-line :name t :command-table editing-table)
    ((numarg 'integer :prompt "How many lines?"))
  (open-line (point (current-window)) numarg))

(set-key `(com-open-line ,*numeric-argument-marker*)
	 'editing-table
	 '((#\o :control)))

(defun kill-line (mark &optional (count 1) (whole-lines-p nil) (concatenate-p nil))
  (let ((start (offset mark)))
    (cond ((= 0 count)
	   (beginning-of-line mark))
	  ((< count 0)
	   (loop repeat (- count)
		 until (beginning-of-buffer-p mark)
		 do (beginning-of-line mark)
		 until (beginning-of-buffer-p mark)
		 do (backward-object mark)))
	  ((or whole-lines-p (> count 1))
	   (loop repeat count
		 until (end-of-buffer-p mark)
		 do (end-of-line mark)
		 until (end-of-buffer-p mark)
		 do (forward-object mark)))
	  (t
	   (cond ((end-of-buffer-p mark) nil)
		 ((end-of-line-p mark)(forward-object mark))
		 (t (end-of-line mark)))))
    (unless (mark= mark start)
      (if concatenate-p
	  (kill-ring-concatenating-push *kill-ring*
					(region-to-sequence start mark))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence start mark)))
      (delete-region start mark))))

(define-command (com-kill-line :name t :command-table deletion-table)
    ((numarg 'integer :prompt "Kill how many lines?")
     (numargp 'boolean :prompt "Kill entire lines?"))
  (let* ((pane (current-window))
	 (point (point pane))
         (concatenate-p (eq (previous-command pane) 'com-kill-line)))
    (kill-line point numarg numargp concatenate-p)))	   

(set-key `(com-kill-line ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'deletion-table
	 '((#\k :control)))

(define-command (com-forward-word :name t :command-table movement-table)
    ((count 'integer :prompt "Number of words"))
  (if (plusp count)
      (forward-word (point (current-window)) count)
      (backward-word (point (current-window)) (- count))))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :meta)))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((:right :control)))

(define-command (com-backward-word :name t :command-table movement-table)
    ((count 'integer :prompt "Number of words"))
  (backward-word (point (current-window)) count))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :meta)))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((:left :control)))

(define-command (com-delete-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  (delete-word (point (current-window)) count))

(defun kill-word (mark &optional (count 1) (concatenate-p nil))
  (let ((start (offset mark)))
    (if (plusp count)
	(loop repeat count
	      until (end-of-buffer-p mark)
	      do (forward-word mark))
	(loop repeat (- count)
	      until (beginning-of-buffer-p mark)
	      do (backward-word mark)))
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

(define-command (com-kill-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (concatenate-p (eq (previous-command pane) 'com-kill-word)))
    (kill-word point count concatenate-p)))

(set-key `(com-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\d :meta)))

(define-command (com-backward-kill-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (concatenate-p (eq (previous-command pane) 'com-backward-kill-word)))
    (kill-word point (- count) concatenate-p)))

(set-key `(com-backward-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Backspace :meta)))

(define-command (com-mark-word :name t :command-table marking-table)
    ((count 'integer :prompt "Number of words"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane)))
    (unless (eq (previous-command pane) 'com-mark-word)
      (setf (offset mark) (offset point)))
    (if (plusp count)
	(forward-word mark count)
	(backward-word mark (- count)))))

(set-key `(com-mark-word ,*numeric-argument-marker*)
	 'marking-table
	 '((#\@ :meta :shift)))

(define-command (com-backward-delete-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  (backward-delete-word (point (current-window)) count))

(define-command (com-upcase-region :name t :command-table case-table) ()
  (let ((cw (current-window)))
    (upcase-region (mark cw) (point cw))))

(define-command (com-downcase-region :name t :command-table case-table) ()
  (let ((cw (current-window)))
    (downcase-region (mark cw) (point cw))))

(define-command (com-capitalize-region :name t :command-table case-table) ()
  (let ((cw (current-window)))
    (capitalize-region (mark cw) (point cw))))

(define-command (com-upcase-word :name t :command-table case-table) ()
  (upcase-word (point (current-window))))

(set-key 'com-upcase-word
	 'case-table
	 '((#\u :meta)))

(define-command (com-downcase-word :name t :command-table case-table) ()
  (downcase-word (point (current-window))))

(set-key 'com-downcase-word
	 'case-table
	 '((#\l :meta)))

(define-command (com-capitalize-word :name t :command-table case-table) ()
  (capitalize-word (point (current-window))))

(set-key 'com-capitalize-word
	 'case-table
	 '((#\c :meta)))

(define-command (com-tabify-region :name t :command-table editing-table) ()
  (let ((pane (current-window)))
    (tabify-region
     (mark pane) (point pane) (tab-space-count (stream-default-view pane)))))

(define-command (com-untabify-region :name t :command-table editing-table) ()
  (let ((pane (current-window)))
    (untabify-region
     (mark pane) (point pane) (tab-space-count (stream-default-view pane)))))

(defun indent-current-line (pane point)
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (indentation (syntax-line-indentation point
                                               tab-space-count
                                               (syntax buffer))))
    (indent-line point indentation (and (indent-tabs-mode buffer)
                                        tab-space-count))))

(define-command (com-indent-line :name t :command-table indent-table) ()
  (let* ((pane (current-window))
         (point (point pane)))
    (indent-current-line pane point)))

(set-key 'com-indent-line
	 'indent-table
	 '((#\Tab)))

(set-key 'com-indent-line
	 'indent-table
	 '((#\i :control)))

(define-command (com-newline-and-indent :name t :command-table indent-table) ()
  (let* ((pane (current-window))
	 (point (point pane)))
    (insert-object point #\Newline)
    (indent-current-line pane point)))

(set-key 'com-newline-and-indent
	 'indent-table
	 '((#\j :control)))

(define-command (com-delete-indentation :name t :command-table indent-table) ()
  (delete-indentation (point (current-window))))

(set-key 'com-delete-indentation
	 'indent-table
	 '((#\^ :shift :meta)))

(define-command (com-auto-fill-mode :name t :command-table fill-table) ()
  (let ((pane (current-window)))
    (setf (auto-fill-mode pane) (not (auto-fill-mode pane)))))

(define-command (com-fill-paragraph :name t :command-table fill-table) ()
  (let* ((pane (current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer))
	 (point (point pane))
         (begin-mark (clone-mark point))
         (end-mark (clone-mark point)))
    (unless (eql (object-before begin-mark) #\Newline)
      (backward-paragraph begin-mark syntax))
    (unless (eql (object-after end-mark) #\Newline)
      (forward-paragraph end-mark syntax))
    (do-buffer-region (object offset buffer
                       (offset begin-mark) (offset end-mark))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (let ((point-backup (clone-mark point)))
      (setf (offset point) (offset end-mark))
      (possibly-fill-line)
      (setf (offset point) (offset point-backup)))))

(set-key 'com-fill-paragraph
	 'fill-table
	 '((#\q :meta)))

(defun filename-completer (so-far mode)
  (flet ((remove-trail (s)
	   (subseq s 0 (let ((pos (position #\/ s :from-end t)))
			 (if pos (1+ pos) 0)))))
    (let* ((directory-prefix
	    (if (and (plusp (length so-far)) (eql (aref so-far 0) #\/))
		""
		(namestring #+sbcl *default-pathname-defaults*
                            #+cmu (ext:default-directory)
                            #-(or sbcl cmu) *default-pathname-defaults*)))
	   (full-so-far (concatenate 'string directory-prefix so-far))
	   (pathnames
	    (loop with length = (length full-so-far)
		  and wildcard = (concatenate 'string (remove-trail so-far) "*.*")
		  for path in
		  #+(or sbcl cmu lispworks) (directory wildcard)
		  #+openmcl (directory wildcard :directories t)
		  #+allegro (directory wildcard :directories-are-files nil)
		  #+cormanlisp (nconc (directory wildcard)
				      (cl::directory-subdirs dirname))
		  #-(or sbcl cmu lispworks openmcl allegro cormanlisp)
		    (directory wildcard)
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
		(values so-far t so-far 1 nil))
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

(define-presentation-method present (object (type pathname)
                                            stream (view climacs-textual-view) &key)
  (princ (namestring object) stream))

(define-presentation-method accept ((type pathname) stream (view climacs-textual-view)
                                    &key (default nil defaultp) (default-type type))
  (multiple-value-bind (pathname success string)
      (complete-input stream
		      #'filename-completer
		      :allow-any-input t)
    (cond (success
	   (values pathname type))
	  ((and (zerop (length string))
		defaultp)
	   (values default default-type))
	  (t (values string 'string)))))
    
(defun filepath-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
		   "." (pathname-type pathname))))

(defun syntax-class-name-for-filepath (filepath)
  (or (climacs-syntax::syntax-description-class-name
       (find (or (pathname-type filepath)
		 (pathname-name filepath))
	     climacs-syntax::*syntaxes*
	     :test (lambda (x y)
		     (member x y :test #'string-equal))
	     :key #'climacs-syntax::syntax-description-pathname-types))
      'basic-syntax))

;; Adapted from cl-fad/PCL
(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC does not designate a directory."
  (let ((name (pathname-name pathspec))
	(type (pathname-type pathspec)))
    (and (or (null name) (eql name :unspecific))
	 (or (null type) (eql type :unspecific)))))

(defun make-buffer (&optional name)
  (let ((buffer (make-instance 'climacs-buffer)))
    (when name (setf (name buffer) name))
    (push buffer (buffers *application-frame*))
    buffer))

(defun find-file (filepath)
  (cond ((null filepath)
	 (display-message "No file name given.")
	 (beep))
	((directory-pathname-p filepath)
	 (display-message "~A is a directory name." filepath)
	 (beep))
	(t
	 (let ((existing-buffer (find filepath (buffers *application-frame*)
			       :key #'filepath :test #'equal)))
	   (if existing-buffer
	       (switch-to-buffer existing-buffer)
	       (let ((buffer (make-buffer))
		     (pane (current-window)))
		 (setf (offset (point (buffer pane))) (offset (point pane)))
		 (setf (buffer (current-window)) buffer)
		 (setf (syntax buffer)
		       (make-instance (syntax-class-name-for-filepath filepath)
			  :buffer (buffer (point pane))))
		 ;; Don't want to create the file if it doesn't exist.
		 (when (probe-file filepath)
		   (with-open-file (stream filepath :direction :input)
		     (input-from-stream stream buffer 0)))
		 (setf (filepath buffer) filepath
		       (name buffer) (filepath-filename filepath)
		       (needs-saving buffer) nil)
		 (beginning-of-buffer (point pane))
		 ;; this one is needed so that the buffer modification protocol
		 ;; resets the low and high marks after redisplay
		 (redisplay-frame-panes *application-frame*)
		 buffer))))))

(define-command (com-find-file :name t :command-table buffer-table) ()
  (let* ((filepath (accept 'pathname :prompt "Find File")))
    (find-file filepath)))

(set-key 'com-find-file
	 'buffer-table
	 '((#\x :control) (#\f :control)))

(defun find-file-read-only (filepath)
  (cond ((null filepath)
	 (display-message "No file name given.")
	 (beep))
	((directory-pathname-p filepath)
	 (display-message "~A is a directory name." filepath)
	 (beep))
	(t
	 (let ((existing-buffer (find filepath (buffers *application-frame*)
				      :key #'filepath :test #'equal)))
	   (if (and existing-buffer (read-only-p existing-buffer))
	       (switch-to-buffer existing-buffer)
	       (if (probe-file filepath)
		   (let ((buffer (make-buffer))
			 (pane (current-window)))
		     (setf (offset (point (buffer pane))) (offset (point pane)))
		     (setf (buffer (current-window)) buffer)
		     (setf (syntax buffer)
			   (make-instance (syntax-class-name-for-filepath filepath)
			      :buffer (buffer (point pane))))
		     (with-open-file (stream filepath :direction :input)
		       (input-from-stream stream buffer 0))
		     (setf (filepath buffer) filepath
			   (name buffer) (filepath-filename filepath)
			   (needs-saving buffer) nil
			   (read-only-p buffer) t)
		     (beginning-of-buffer (point pane))
		     ;; this one is needed so that the buffer modification protocol
		     ;; resets the low and high marks after redisplay
		     (redisplay-frame-panes *application-frame*)
		     buffer)
		   (progn
		     (display-message "No such file: ~A" filepath)
		     (beep)
		     nil)))))))

(define-command (com-find-file-read-only :name t :command-table buffer-table) ()
  (let ((filepath (accept 'pathname :Prompt "Find file read only")))
    (find-file-read-only filepath)))

(set-key 'com-find-file-read-only
	 'buffer-table
	 '((#\x :control) (#\r :control)))

(define-command (com-read-only :name t :command-table buffer-table) ()
  (let ((buffer (buffer (current-window))))
    (setf (read-only-p buffer) (not (read-only-p buffer)))))

(set-key 'com-read-only
	 'buffer-table
	 '((#\x :control) (#\q :control)))

(defun set-visited-file-name (filename buffer)
  (setf (filepath buffer) filename
	(name buffer) (filepath-filename filename)
	(needs-saving buffer) t))

(define-command (com-set-visited-file-name :name t :command-table buffer-table) ()
  (let ((filename (accept 'pathname :prompt "New file name")))
    (set-visited-file-name filename (buffer (current-window)))))

(define-command (com-insert-file :name t :command-table buffer-table) ()
  (let ((filename (accept 'pathname :prompt "Insert File"))
	(pane (current-window)))
    (when (probe-file filename)
      (setf (mark pane) (clone-mark (point pane) :left))
      (with-open-file (stream filename :direction :input)
	(input-from-stream stream
			   (buffer pane)
			   (offset (point pane))))
      (psetf (offset (mark pane)) (offset (point pane))
	     (offset (point pane)) (offset (mark pane))))
    (redisplay-frame-panes *application-frame*)))

(set-key 'com-insert-file
	 'buffer-table
	 '((#\x :control) (#\i :control)))

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

(define-command (com-revert-buffer :name t :command-table buffer-table) ()
  (let* ((pane (current-window))
	 (buffer (buffer pane))
	 (filepath (filepath buffer))
	 (save (offset (point pane))))
    (when (accept 'boolean :prompt (format nil "Revert buffer from file ~A?"
					   (filepath buffer)))
      (cond ((directory-pathname-p filepath)
	   (display-message "~A is a directory name." filepath)
	   (beep))
	  ((probe-file filepath)
	   (erase-buffer buffer)
	   (with-open-file (stream filepath :direction :input)
	     (input-from-stream stream buffer 0))
	   (setf (offset (point pane))
		 (min (size buffer) save)))
	  (t
	   (display-message "No file ~A" filepath)
	   (beep))))))

(defun save-buffer (buffer)
  (let ((filepath (or (filepath buffer)
		      (accept 'pathname :prompt "Save Buffer to File"))))
    (cond
      ((directory-pathname-p filepath)
       (display-message "~A is a directory." filepath)
       (beep))
      (t
       (when (probe-file filepath)
	 (let ((backup-name (pathname-name filepath))
	       (backup-type (concatenate 'string (pathname-type filepath) "~")))
	   (rename-file filepath (make-pathname :name backup-name
						:type backup-type))))
       (with-open-file (stream filepath :direction :output :if-exists :supersede)
	 (output-to-stream stream buffer 0 (size buffer)))
       (setf (filepath buffer) filepath
	     (name buffer) (filepath-filename filepath))
       (display-message "Wrote: ~a" (filepath buffer))
       (setf (needs-saving buffer) nil)))))

(define-command (com-save-buffer :name t :command-table buffer-table) ()
  (let ((buffer (buffer (current-window))))
    (if (or (null (filepath buffer))
	    (needs-saving buffer))
	(save-buffer buffer)
	(display-message "No changes need to be saved from ~a" (name buffer)))))

(set-key 'com-save-buffer
	 'buffer-table
	 '((#\x :control) (#\s :control)))

(defmethod frame-exit :around ((frame climacs))
  (loop for buffer in (buffers frame)
	when (and (needs-saving buffer)
		  (filepath buffer)
		  (handler-case (accept 'boolean
					:prompt (format nil "Save buffer: ~a ?" (name buffer)))
		    (error () (progn (beep)
				     (display-message "Invalid answer")
				     (return-from frame-exit nil)))))
	  do (save-buffer buffer))
  (when (or (notany #'(lambda (buffer) (and (needs-saving buffer) (filepath buffer)))
		    (buffers frame))
	    (handler-case (accept 'boolean :prompt "Modified buffers exist.  Quit anyway?")
	      (error () (progn (beep)
			       (display-message "Invalid answer")
			       (return-from frame-exit nil)))))
    (call-next-method)))

(define-command (com-write-buffer :name t :command-table buffer-table) ()
  (let ((filepath (accept 'pathname :prompt "Write Buffer to File"))
	(buffer (buffer (current-window))))
    (cond
      ((directory-pathname-p filepath)
       (display-message "~A is a directory name." filepath))
      (t
       (with-open-file (stream filepath :direction :output :if-exists :supersede)
	 (output-to-stream stream buffer 0 (size buffer)))
       (setf (filepath buffer) filepath
	     (name buffer) (filepath-filename filepath)
	     (needs-saving buffer) nil)
       (display-message "Wrote: ~a" (filepath buffer))))))

(set-key 'com-write-buffer
	 'buffer-table
	 '((#\x :control) (#\w :control)))

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

(define-command (com-switch-to-buffer :name t :command-table pane-table) ()
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
     (setf (buffer (current-window)) (car buffers))))

(defmethod kill-buffer ((name string))
  (let ((buffer (find name (buffers *application-frame*)
		      :key #'name :test #'string=)))
    (when buffer (kill-buffer buffer))))

(defmethod kill-buffer ((symbol (eql 'nil)))
  (kill-buffer (buffer (current-window))))

(define-command (com-kill-buffer :name t :command-table pane-table) ()
  (let ((buffer (accept 'buffer
			:prompt "Kill buffer"
			:default (buffer (current-window))
			:default-type 'buffer)))
    (kill-buffer buffer)))

(set-key 'com-kill-buffer
	 'pane-table
	 '((#\x :control) (#\k)))

(define-command (com-full-redisplay :name t :command-table base-table) ()
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
  (let ((filepath (accept 'pathname :prompt "Load File")))
    (load-file filepath)))

(set-key 'com-load-file
	 'base-table
	 '((#\c :control) (#\l :control)))

(define-command (com-beginning-of-buffer :name t :command-table movement-table) ()
  (beginning-of-buffer (point (current-window))))

(set-key 'com-beginning-of-buffer
	 'movement-table
	 '((#\< :shift :meta)))

(set-key 'com-beginning-of-buffer
	 'movement-table
	 '((:home :control)))

(define-command (com-page-down :name t :command-table movement-table) ()
  (let ((pane (current-window)))
    (page-down pane)))

(set-key 'com-page-down
	 'movement-table
	 '((#\v :control)))

(set-key 'com-page-down
	 'movement-table
	 '((:next)))

(define-command (com-page-up :name t :command-table movement-table) ()
  (let ((pane (current-window)))
    (page-up pane)))

(set-key 'com-page-up
	 'movement-table
	 '((#\v :meta)))

(set-key 'com-page-up
	 'movement-table
	 '((:prior)))

(define-command (com-end-of-buffer :name t :command-table movement-table) ()
  (end-of-buffer (point (current-window))))

(set-key 'com-end-of-buffer
	 'movement-table
	 '((#\> :shift :meta)))

(set-key 'com-end-of-buffer
	 'movement-table
	 '((:end :control)))

(define-command (com-mark-whole-buffer :name t :command-table marking-table) ()
  (beginning-of-buffer (point (current-window)))
  (end-of-buffer (mark (current-window))))

(set-key 'com-mark-whole-buffer
	 'marking-table
	 '((#\x :control) (#\h)))

(defun back-to-indentation (mark)
  (beginning-of-line mark)
  (loop until (end-of-line-p mark)
	while (whitespacep (object-after mark))
	do (forward-object mark)))

(define-command (com-back-to-indentation :name t :command-table movement-table) ()
  (back-to-indentation (point (current-window))))

(set-key 'com-back-to-indentation
	 'movement-table
	 '((#\m :meta)))

(defun delete-horizontal-space (mark &optional (backward-only-p nil))
  (let ((mark2 (clone-mark mark)))
    (loop until (beginning-of-line-p mark)
	  while (whitespacep (object-before mark))
	  do (backward-object mark))
    (unless backward-only-p
      (loop until (end-of-line-p mark2)
	    while (whitespacep (object-after mark2))
	    do (forward-object mark2)))
    (delete-region mark mark2)))

(define-command (com-delete-horizontal-space :name t :command-table deletion-table)
    ((backward-only-p
      'boolean :prompt "Delete backwards only?"))
  (delete-horizontal-space (point (current-window)) backward-only-p))

(set-key `(com-delete-horizontal-space ,*numeric-argument-p*)
	 'deletion-table
	 '((#\\ :meta)))

(defun just-one-space (mark count)
  (let (offset)
    (loop until (beginning-of-line-p mark)
	  while (whitespacep (object-before mark))
	  do (backward-object mark))
    (loop until (end-of-line-p mark)
	  while (whitespacep (object-after mark))
	  repeat count do (forward-object mark)
	  finally (setf offset (offset mark)))
    (loop until (end-of-line-p mark)
	  while (whitespacep (object-after mark))
	  do (forward-object mark))
    (delete-region offset mark)))

(define-command (com-just-one-space :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of spaces"))
  (just-one-space (point (current-window)) count))

(set-key `(com-just-one-space ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Space :meta)))

(defun goto-position (mark pos)
  (setf (offset mark) pos))

(define-command (com-goto-position :name t :command-table movement-table) ()
  (goto-position
   (point (current-window))
   (handler-case (accept 'integer :prompt "Goto Position")
     (error () (progn (beep)
		      (display-message "Not a valid position")
		      (return-from com-goto-position nil))))))  

(defun goto-line (mark line-number)
  (loop with m = (clone-mark (low-mark (buffer mark))
		       :right)
	initially (beginning-of-buffer m)
	do (end-of-line m)
	until (end-of-buffer-p m)
	repeat (1- line-number)
	do (incf (offset m))
	   (end-of-line m)
	finally (beginning-of-line m)
		(setf (offset mark) (offset m))))

(define-command (com-goto-line :name t :command-table movement-table) ()
  (goto-line (point (current-window))
	     (handler-case (accept 'integer :prompt "Goto Line")
		 (error () (progn (beep)
				  (display-message "Not a valid line number")
				  (return-from com-goto-line nil))))))

(define-command (com-browse-url :name t :command-table base-table) ()
  (let ((url (accept 'url :prompt "Browse URL")))
    #+ (and sbcl darwin)
    (sb-ext:run-program "/usr/bin/open" `(,url) :wait nil)
    #+ (and openmcl darwin)
    (ccl:run-program "/usr/bin/open" `(,url) :wait nil)))

(define-command (com-set-mark :name t :command-table marking-table) ()
  (let ((pane (current-window)))
    (setf (mark pane) (clone-mark (point pane)))))

(set-key 'com-set-mark
	 'marking-table
	 '((#\Space :control)))

(define-command (com-exchange-point-and-mark :name t :command-table marking-table) ()
  (let ((pane (current-window)))
    (psetf (offset (mark pane)) (offset (point pane))
	   (offset (point pane)) (offset (mark pane)))))

(set-key 'com-exchange-point-and-mark
	 'marking-table
	 '((#\x :control) (#\x :control)))

(defgeneric set-syntax (buffer syntax))

(defmethod set-syntax ((buffer climacs-buffer) (syntax syntax))
  (setf (syntax buffer) syntax))

;;FIXME - what should this specialise on?
(defmethod set-syntax ((buffer climacs-buffer) syntax)
  (set-syntax buffer (make-instance syntax :buffer buffer)))

(defmethod set-syntax ((buffer climacs-buffer) (syntax string))
  (let ((syntax-class (syntax-from-name syntax)))
    (cond (syntax-class
	   (set-syntax buffer (make-instance syntax-class
				 :buffer buffer)))
	  (t
	   (beep)
	   (display-message "No such syntax: ~A." syntax)))))

(define-command (com-set-syntax :name t :command-table buffer-table) ()
  (let* ((pane (current-window))
	 (buffer (buffer pane)))
    (set-syntax buffer (accept 'syntax :prompt "Set Syntax"))))		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands for splitting windows

(defun replace-constellation (constellation additional-constellation vertical-p)
  (let* ((parent (sheet-parent constellation))
	 (children (sheet-children parent))
	 (first (first children))
	 (second (second children))
	 (third (third children))
         (adjust (make-pane 'clim-extensions:box-adjuster-gadget)))
    (assert (member constellation children))
    (sheet-disown-child parent constellation)
    (let ((new (if vertical-p
		   (vertically ()
		     constellation adjust additional-constellation)
		   (horizontally ()
		     constellation adjust additional-constellation))))
      (sheet-adopt-child parent new)
      (reorder-sheets parent 
		      (if (eq constellation first)
			  (if third
			      (list new second third)
			      (list new second))
			  (if third
			      (list first second new)
			      (list first new)))))))

(defun find-parent (sheet)
  (loop for parent = (sheet-parent sheet)
	  then (sheet-parent parent)
	until (typep parent 'vrack-pane)
	finally (return parent)))

(defclass typeout-pane (application-pane esa-pane-mixin) ())

(defun make-typeout-constellation (&optional label)
  (let* ((typeout-pane
	  (make-pane 'typeout-pane :width 900 :height 400 :display-time nil))
	 (label
	  (make-pane 'label-pane :label label))
	 (vbox
	  (vertically ()
	    (scrolling (:scroll-bar :vertical) typeout-pane) label)))
    (values vbox typeout-pane)))

(defun typeout-window (&optional (label "Typeout") (pane (current-window)))
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-typeout-constellation label)
      (let* ((current-window pane)
	     (constellation-root (find-parent current-window)))
	(push new-pane (windows *application-frame*))
	(other-window)
	(replace-constellation constellation-root vbox t)
	(full-redisplay current-window)
	new-pane))))

(define-command (com-describe-bindings :name t :command-table help-table)
    ((sort-by-keystrokes 'boolean :prompt "Sort by keystrokes?"))
  (let* ((window (current-window))
	 (buffer (buffer (current-window)))
	 (stream (typeout-window
		   (format nil "~10THelp: Describe Bindings for ~A" (name buffer))))
	 (command-table (command-table window)))
    (esa::describe-bindings stream command-table
		       (if sort-by-keystrokes
			   #'esa::sort-by-keystrokes
			   #'esa::sort-by-name))))

(set-key `(com-describe-bindings ,*numeric-argument-p*) 'help-table '((#\h :control) (#\b)))

(defun make-pane-constellation (&optional (with-scrollbars *with-scrollbars*))
  "make a vbox containing a scroller pane as its first child and an
info pane as its second child.  The scroller pane contains a viewport
which contains an extended pane.  Return the vbox and the extended pane
as two values.
If with-scrollbars nil, omit the scroller."
  (let* ((extended-pane
	  (make-pane 'extended-pane
		     :width 900 :height 400
		     :name 'window
		     :end-of-line-action :scroll
		     :incremental-redisplay t
		     :display-function 'display-window
		     :command-table 'global-climacs-table))
	 (vbox
	  (vertically ()
	    (if with-scrollbars
		(scrolling ()
		  extended-pane)
		extended-pane)
	    (make-pane 'climacs-info-pane
		       :master-pane extended-pane
		       :width 900))))
    (values vbox extended-pane)))

(defun split-window (&optional (vertically-p nil) (pane (current-window)))
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window pane)
	     (constellation-root (find-parent current-window)))
        (setf (offset (point (buffer current-window))) (offset (point current-window))
	      (buffer new-pane) (buffer current-window)
              (auto-fill-mode new-pane) (auto-fill-mode current-window)
              (auto-fill-column new-pane) (auto-fill-column current-window))
	(push new-pane (windows *application-frame*))
	(setf *standard-output* new-pane)
	(replace-constellation constellation-root vbox vertically-p)
	(full-redisplay current-window)
	(full-redisplay new-pane)
	new-pane))))

(define-command (com-split-window-vertically :name t :command-table window-table) ()
  (split-window t))

(set-key 'com-split-window-vertically
	 'window-table
	 '((#\x :control) (#\2)))

(define-command (com-split-window-horizontally :name t :command-table window-table) ()
  (split-window))

(set-key 'com-split-window-horizontally
	 'window-table
	 '((#\x :control) (#\3)))

(defun other-window (&optional pane)
  (if (and pane (find pane (windows *application-frame*)))
      (setf (windows *application-frame*)
	    (append (list pane)
		    (remove pane (windows *application-frame*))))
      (setf (windows *application-frame*)
	    (append (cdr (windows *application-frame*))
		    (list (car (windows *application-frame*))))))
  (setf *standard-output* (car (windows *application-frame*))))
  
(define-command (com-other-window :name t :command-table window-table) ()
  (other-window))

(set-key 'com-other-window
	 'window-table
	 '((#\x :control) (#\o)))

(defun click-to-offset (window x y)
  (with-slots (top bot) window
       (let ((new-x (floor x (stream-character-width window #\m)))
	     (new-y (floor y (stream-line-height window)))
	     (buffer (buffer window)))
	 (loop for scan from (offset top)
	       with lines = 0
	       until (= scan (offset bot))
	       until (= lines new-y)
	       when (eql (buffer-object buffer scan) #\Newline)
		 do (incf lines)
	       finally (loop for columns from 0
			     until (= scan (offset bot))
			     until (eql (buffer-object buffer scan) #\Newline)
			     until (= columns new-x)
			     do (incf scan))
		       (return scan)))))

(define-command (com-switch-to-this-window :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (other-window window)
  (when (typep window 'extended-pane)
    (setf (offset (point window))
	  (click-to-offset window x y))))

(define-presentation-to-command-translator blank-area-to-switch-to-this-window
    (blank-area com-switch-to-this-window window-table :echo nil)
    (window x y)
  (list window x y))

(define-gesture-name :select-other :pointer-button (:right) :unique nil)

(define-command (com-mouse-save :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (and (typep window 'extended-pane)
	     (eq window (current-window)))
    (setf (offset (mark window))
	  (click-to-offset window x y))
    (com-exchange-point-and-mark)
    (com-copy-region)))

(define-presentation-to-command-translator blank-area-to-mouse-save
    (blank-area com-mouse-save window-table :echo nil :gesture :select-other)
    (window x y)
  (list window x y))

(define-gesture-name :middle-button :pointer-button (:middle) :unique nil)

(define-command (com-yank-here :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (typep window 'extended-pane)
    (other-window window)
    (setf (offset (point window))
	  (click-to-offset window x y))
    (com-yank)))

(define-presentation-to-command-translator blank-area-to-yank-here
    (blank-area com-yank-here window-table :echo nil :gesture :middle-button)
    (window x y)
  (list window x y))

(defun single-window ()
  (loop until (null (cdr (windows *application-frame*)))
	do (rotatef (car (windows *application-frame*))
		    (cadr (windows *application-frame*)))
	   (com-delete-window))
  (setf *standard-output* (car (windows *application-frame*))))

(define-command (com-single-window :name t :command-table window-table) ()
  (single-window))

(set-key 'com-single-window
	 'window-table
	 '((#\x :control) (#\1)))

(define-command (com-scroll-other-window :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-down other-window))))

(set-key 'com-scroll-other-window
	 'window-table
	 '((#\v :control :meta)))

(define-command (com-scroll-other-window-up :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-up other-window))))

(set-key 'com-scroll-other-window-up
	 'window-table
	 '((#\V :control :meta :shift)))

(defun delete-window (&optional (window (current-window)))
  (unless (null (cdr (windows *application-frame*)))
    (let* ((constellation (find-parent window))
	   (box (sheet-parent constellation))
	   (box-children (sheet-children box))
	   (other (if (eq constellation (first box-children))
		      (third box-children)
		      (first box-children)))
	   (parent (sheet-parent box))
	   (children (sheet-children parent))
	   (first (first children))
	   (second (second children))
	   (third (third children)))
      (setf (windows *application-frame*)
	    (remove window (windows *application-frame*)))
      (setf *standard-output* (car (windows *application-frame*)))
      (sheet-disown-child box other)
      (sheet-disown-child parent box)
	 (sheet-adopt-child parent other)
      (reorder-sheets parent (if (eq box first)
				 (if third
				     (list other second third)
				     (list other second))
				 (if third
				     (list first second other)
				     (list first other)))))))

(define-command (com-delete-window :name t :command-table window-table) ()
  (delete-window))

(set-key 'com-delete-window
	 'window-table
	 '((#\x :control) (#\0)))

;;;;;;;;;;;;;;;;;;;;
;; Kill ring commands

;; Copies an element from a kill-ring to a buffer at the given offset
(define-command (com-yank :name t :command-table editing-table) ()
  (insert-sequence (point (current-window)) (kill-ring-yank *kill-ring*)))

(set-key 'com-yank
	 'editing-table
	 '((#\y :control)))

;; Destructively cut a given buffer region into the kill-ring
(define-command (com-kill-region :name t :command-table editing-table) ()
  (let ((pane (current-window)))
    (kill-ring-standard-push
     *kill-ring* (region-to-sequence (mark pane) (point pane)))
    (delete-region (mark pane) (point pane))))

(set-key 'com-kill-region
	 'editing-table
	 '((#\w :control)))

;; Non destructively copies buffer region to the kill ring
(define-command (com-copy-region :name t :command-table marking-table) ()
  (let ((pane (current-window)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence (point pane) (mark pane)))))

(set-key 'com-copy-region
	 'marking-table
	 '((#\w :meta)))

(define-command (com-rotate-yank :name t :command-table editing-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (last-yank (kill-ring-yank *kill-ring*)))
    (if (eq (previous-command pane)
	    'com-rotate-yank)
	(progn
	  (delete-range point (* -1 (length last-yank)))
	  (rotate-yank-position *kill-ring*)))
    (insert-sequence point (kill-ring-yank *kill-ring*))))

(set-key 'com-rotate-yank
	 'editing-table
	 '((#\y :meta)))

(define-command (com-resize-kill-ring :name t :command-table editing-table) ()
  (let ((size (handler-case (accept 'integer :prompt "New kill ring size")
		(error () (progn (beep)
				 (display-message "Not a valid kill ring size")
				 (return-from com-resize-kill-ring nil))))))
    (setf (kill-ring-max-size *kill-ring*) size)))

(define-command (com-append-next-kill :name t :command-table editing-table) ()
  (setf (append-next-p *kill-ring*) t))

(set-key 'com-append-next-kill
	 'editing-table
	 '((#\w :control :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Incremental search

(make-command-table 'isearch-climacs-table :errorp nil)

(defun isearch-command-loop (pane forwardp)
  (let ((point (point pane)))
    (unless (endp (isearch-states pane))
      (setf (isearch-previous-string pane)
            (search-string (first (isearch-states pane)))))
    (setf (isearch-mode pane) t)
    (setf (isearch-states pane)
          (list (make-instance 'isearch-state
                               :search-string ""
                               :search-mark (clone-mark point)
                               :search-forward-p forwardp
                               :search-success-p t)))
    (simple-command-loop 'isearch-climacs-table
                         (isearch-mode pane)
                         ((setf (isearch-mode pane) nil)))))

(defun isearch-from-mark (pane mark string forwardp)
  (flet ((object-equal (x y)
           (if (characterp x)
               (and (characterp y) (char-equal x y))
               (eql x y))))
    (let* ((point (point pane))
           (mark2 (clone-mark mark))
           (success (funcall (if forwardp #'search-forward #'search-backward)
                             mark2
                             string
                             :test #'object-equal)))
      (when success
        (setf (offset point) (offset mark2)
              (offset mark) (if forwardp
                                (- (offset mark2) (length string))
                                (+ (offset mark2) (length string)))))
      (display-message "~:[Failing ~;~]Isearch~:[ backward~;~]: ~A"
		       success forwardp string)
      (push (make-instance 'isearch-state
                           :search-string string
                           :search-mark mark
                           :search-forward-p forwardp
                           :search-success-p success)
            (isearch-states pane))
      (unless success
        (beep)))))

(define-command (com-isearch-forward :name t :command-table search-table) ()
  (display-message "Isearch: ")
  (isearch-command-loop (current-window) t))

(set-key 'com-isearch-forward
	 'search-table
	 '((#\s :control)))

(define-command (com-isearch-backward :name t :command-table search-table) ()
  (display-message "Isearch backward: ")
  (isearch-command-loop (current-window) nil))

(set-key 'com-isearch-backward
	 'search-table
	 '((#\r :control)))

(define-command (com-isearch-append-char :name t :command-table isearch-climacs-table) ()
  (let* ((pane (current-window))
         (states (isearch-states pane))
         (string (concatenate 'string
                              (search-string (first states))
                              (string *current-gesture*)))
         (mark (clone-mark (search-mark (first states))))
         (forwardp (search-forward-p (first states))))
    (unless forwardp
      (incf (offset mark)))
    (isearch-from-mark pane mark string forwardp)))

(define-command (com-isearch-delete-char :name t :command-table isearch-climacs-table) ()
  (let* ((pane (current-window)))
    (cond ((null (second (isearch-states pane)))
	   (display-message "Isearch: ")
           (beep))
          (t
           (pop (isearch-states pane))
           (loop until (endp (rest (isearch-states pane)))
                 until (search-success-p (first (isearch-states pane)))
                 do (pop (isearch-states pane)))
           (let ((state (first (isearch-states pane))))
             (setf (offset (point pane))
                   (if (search-forward-p state)
                       (+ (offset (search-mark state))
                          (length (search-string state)))
                       (- (offset (search-mark state))
                          (length (search-string state)))))
	     (display-message "Isearch~:[ backward~;~]: ~A"
			      (search-forward-p state)
			      (search-string state)))))))

(define-command (com-isearch-search-forward :name t :command-table isearch-climacs-table) ()
  (let* ((pane (current-window))
         (point (point pane))
         (states (isearch-states pane))
         (string (if (null (second states))
                     (isearch-previous-string pane)
                     (search-string (first states))))
         (mark (clone-mark point)))
    (isearch-from-mark pane mark string t)))

(define-command (com-isearch-search-backward :name t :command-table isearch-climacs-table) ()
  (let* ((pane (current-window))
         (point (point pane))
         (states (isearch-states pane))
         (string (if (null (second states))
                     (isearch-previous-string pane)
                     (search-string (first states))))
         (mark (clone-mark point)))
    (isearch-from-mark pane mark string nil)))

(define-command (com-isearch-exit :name t :command-table isearch-climacs-table) ()
  (setf (isearch-mode (current-window)) nil))

(defun isearch-set-key (gesture command)
  (add-command-to-command-table command 'isearch-climacs-table
                                :keystroke gesture :errorp nil))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (isearch-set-key (code-char code) 'com-isearch-append-char))

(isearch-set-key '(#\Newline) 'com-isearch-exit)
(isearch-set-key '(#\Backspace) 'com-isearch-delete-char)
(isearch-set-key '(#\s :control) 'com-isearch-search-forward)
(isearch-set-key '(#\r :control) 'com-isearch-search-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Query replace

(make-command-table 'query-replace-climacs-table :errorp nil)

(defun query-replace-find-next-match (mark string)
  (flet ((object-equal (x y)
           (and (characterp x)
                (characterp y)
                (char-equal x y))))
    (let ((offset-before (offset mark)))
      (search-forward mark string :test #'object-equal)
      (/= (offset mark) offset-before))))

(define-command (com-query-replace :name t :command-table search-table) ()
  (let* ((pane (current-window))
	 (old-state (query-replace-state pane))
	 (old-string1 (when old-state (string1 old-state)))
	 (old-string2 (when old-state (string2 old-state)))
	 (string1 (handler-case 
		      (if old-string1
			  (accept 'string 
				  :prompt "Query Replace"
				  :default old-string1
				  :default-type 'string)
			  (accept 'string :prompt "Query Replace"))
		    (error () (progn (beep)
				     (display-message "Empty string")
				     (return-from com-query-replace nil)))))
         (string2 (handler-case 
		      (if old-string2
			  (accept 'string
				  :prompt (format nil "Query Replace ~A with"
						  string1)
				  :default old-string2
				  :default-type 'string)
			  (accept 'string
				  :prompt (format nil "Query Replace ~A with" string1)))
		    (error () (progn (beep)
				     (display-message "Empty string")
				     (return-from com-query-replace nil)))))
         (point (point pane))
	 (occurrences 0))
    (declare (special string1 string2 occurrences))
    (when (query-replace-find-next-match point string1)
      (setf (query-replace-state pane) (make-instance 'query-replace-state
                                                      :string1 string1
                                                      :string2 string2)
            (query-replace-mode pane) t)
      (display-message "Query Replace ~A with ~A:"
		       string1 string2)
      (simple-command-loop 'query-replace-climacs-table
			   (query-replace-mode pane)
			   ((setf (query-replace-mode pane) nil))))
    (display-message "Replaced ~A occurrence~:P" occurrences)))

(set-key 'com-query-replace
	 'search-table
	 '((#\% :shift :meta)))

(define-command (com-query-replace-replace :name t :command-table query-replace-climacs-table) ()
  (declare (special string1 string2 occurrences))
  (let* ((pane (current-window))
         (point (point pane))
         (buffer (buffer pane))
         (string1-length (length string1)))
    (backward-object point string1-length)
    (let* ((offset1 (offset point))
           (offset2 (+ offset1 string1-length))
           (region-case (buffer-region-case buffer offset1 offset2)))
      (delete-range point string1-length)
      (insert-sequence point string2)
      (setf offset2 (+ offset1 (length string2)))
      (finish-output *error-output*)
      (unless (find-if #'upper-case-p string1)
        (case region-case
          (:upper-case (upcase-buffer-region buffer offset1 offset2))
          (:lower-case (downcase-buffer-region buffer offset1 offset2))
          (:capitalized (capitalize-buffer-region buffer offset1 offset2)))))
    (incf occurrences)
    (if (query-replace-find-next-match point string1)
	(display-message "Query Replace ~A with ~A:"
		       string1 string2)
	(setf (query-replace-mode pane) nil))))

(define-command (com-query-replace-skip :name t :command-table query-replace-climacs-table) ()
  (declare (special string1 string2))
  (let* ((pane (current-window))
         (point (point pane)))
    (if (query-replace-find-next-match point string1)
	(display-message "Query Replace ~A with ~A:"
			 string1 string2)
	(setf (query-replace-mode pane) nil))))

(define-command (com-query-replace-exit :name t :command-table query-replace-climacs-table) ()
  (setf (query-replace-mode (current-window)) nil))

(defun query-replace-set-key (gesture command)
  (add-command-to-command-table command 'query-replace-climacs-table
                                :keystroke gesture :errorp nil))

(query-replace-set-key '(#\Newline) 'com-query-replace-exit)
(query-replace-set-key '(#\Space) 'com-query-replace-replace)
(query-replace-set-key '(#\Backspace) 'com-query-replace-skip)
(query-replace-set-key '(#\Rubout) 'com-query-replace-skip)
(query-replace-set-key '(#\q) 'com-query-replace-exit)
(query-replace-set-key '(#\y) 'com-query-replace-replace)
(query-replace-set-key '(#\n) 'com-query-replace-skip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Undo/redo

(define-command (com-undo :name t :command-table editing-table) ()
  (handler-case (undo (undo-tree (buffer (current-window))))
    (no-more-undo () (beep) (display-message "No more undo")))
  (full-redisplay (current-window)))

(set-key 'com-undo
	 'editing-table
	 '((#\_ :shift :control)))

(set-key 'com-undo
	 'editing-table
	 '((#\x :control) (#\u)))

(define-command (com-redo :name t :command-table editing-table) ()
  (handler-case (redo (undo-tree (buffer (current-window))))
    (no-more-undo () (beep) (display-message "No more redo")))
  (full-redisplay (current-window)))

(set-key 'com-redo
	 'editing-table
	 '((#\_ :shift :meta)))

(set-key 'com-redo
	 'editing-table
	 '((#\x :control) (#\r :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dynamic abbrevs

(define-command (com-dabbrev-expand :name t :command-table editing-table) ()
  (let* ((window (current-window))
	 (point (point window)))
    (with-slots (original-prefix prefix-start-offset dabbrev-expansion-mark) window
       (flet ((move () (cond ((beginning-of-buffer-p dabbrev-expansion-mark)
			      (setf (offset dabbrev-expansion-mark)
				    (offset point))
			      (forward-word dabbrev-expansion-mark))
			     ((mark< dabbrev-expansion-mark point)
			      (backward-object dabbrev-expansion-mark))
			     (t (forward-object dabbrev-expansion-mark)))))
	 (unless (or (beginning-of-buffer-p point)
		     (not (constituentp (object-before point))))
	   (unless (and (eq (previous-command window) 'com-dabbrev-expand)
			(not (null prefix-start-offset)))
	     (setf dabbrev-expansion-mark (clone-mark point))
	     (backward-word dabbrev-expansion-mark)
	     (setf prefix-start-offset (offset dabbrev-expansion-mark))
	     (setf original-prefix (region-to-sequence prefix-start-offset point))
	     (move))
	   (loop until (or (end-of-buffer-p dabbrev-expansion-mark)
			   (and (or (beginning-of-buffer-p dabbrev-expansion-mark)
				    (not (constituentp (object-before dabbrev-expansion-mark))))
				(looking-at dabbrev-expansion-mark original-prefix)))
		 do (move))
	   (if (end-of-buffer-p dabbrev-expansion-mark)
	       (progn (delete-region prefix-start-offset point)
		      (insert-sequence point original-prefix)
		      (setf prefix-start-offset nil))
	       (progn (delete-region prefix-start-offset point)
		      (insert-sequence point
				       (let ((offset (offset dabbrev-expansion-mark)))
					 (prog2 (forward-word dabbrev-expansion-mark)
						(region-to-sequence offset dabbrev-expansion-mark)
						(setf (offset dabbrev-expansion-mark) offset))))
		      (move))))))))

(set-key 'com-dabbrev-expand
	 'editing-table
	 '((#\/ :meta)))

(define-command (com-backward-paragraph :name t :command-table movement-table)
    ((count 'integer :prompt "Number of paragraphs"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-paragraph point syntax))
	(loop repeat (- count) do (forward-paragraph point syntax)))))

(set-key `(com-backward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\{ :shift :meta)))

(define-command (com-forward-paragraph :name t :command-table movement-table)
    ((count 'integer :prompt "Number of paragraphs"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-paragraph point syntax))
	(loop repeat (- count) do (backward-paragraph point syntax)))))

(set-key `(com-forward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\} :shift :meta)))

(define-command (com-mark-paragraph :name t :command-table marking-table)
    ((count 'integer :prompt "Number of paragraphs"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-paragraph)
      (setf (offset mark) (offset point))
      (if (plusp count)
	  (backward-paragraph point syntax)
	  (forward-paragraph point syntax)))
    (if (plusp count)
	(loop repeat count do (forward-paragraph mark syntax))
	(loop repeat (- count) do (backward-paragraph mark syntax)))))

(set-key `(com-mark-paragraph ,*numeric-argument-marker*)
	 'marking-table
	 '((#\h :meta)))

(define-command (com-backward-sentence :name t :command-table movement-table)
    ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-sentence point syntax))
	(loop repeat (- count) do (forward-sentence point syntax)))))

(set-key `(com-backward-sentence ,*numeric-argument-marker*)
	 'movement-table
	 '((#\a :meta)))

(define-command (com-forward-sentence :name t :command-table movement-table)
    ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-sentence point syntax))
	(loop repeat (- count) do (backward-sentence point syntax)))))

(set-key `(com-forward-sentence ,*numeric-argument-marker*)
	 'movement-table
	 '((#\e :meta)))

(define-command (com-kill-sentence :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-sentence point syntax))
	(loop repeat (- count) do (backward-sentence point syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
    (delete-region point mark)))

(set-key `(com-kill-sentence ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\k :meta)))

(define-command (com-backward-kill-sentence :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-sentence point syntax))
	(loop repeat (- count) do (forward-sentence point syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
    (delete-region point mark)))

(set-key `(com-backward-kill-sentence ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\x :control) (#\Backspace)))

(defun forward-page (mark &optional (count 1))
  (loop repeat count
	unless (search-forward mark (coerce (list #\Newline #\Page) 'vector))
	  do (end-of-buffer mark)
	     (loop-finish)))

(define-command (com-forward-page :name t :command-table movement-table)
    ((count 'integer :prompt "Number of pages"))
  (let* ((pane (current-window))
	 (point (point pane)))
    (if (plusp count)
	(forward-page point count)
	(backward-page point count))))

(set-key `(com-forward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\])))

(defun backward-page (mark &optional (count 1))
  (loop repeat count
	  when (search-backward mark (coerce (list #\Newline #\Page) 'vector))
	    do (forward-object mark)
	  else do (beginning-of-buffer mark)
		  (loop-finish)))

(define-command (com-backward-page :name t :command-table movement-table)
    ((count 'integer :prompt "Number of pages"))
  (let* ((pane (current-window))
	 (point (point pane)))
    (if (plusp count)
	(backward-page point count)
	(forward-page point count))))

(set-key `(com-backward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\[)))

(define-command (com-mark-page :name t :command-table marking-table)
    ((count 'integer :prompt "Move how many pages")
     (numargp 'boolean :prompt "Move to another page?"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane)))
    (cond ((and numargp (/= 0 count))
	   (if (plusp count)
	       (forward-page point count)
	       (backward-page point (1+ count))))
	  (t (backward-page point count)))
    (setf (offset mark) (offset point))
	   (forward-page mark 1)))

(set-key `(com-mark-page ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'marking-table
	 '((#\x :control) (#\p :control)))

(define-command (com-count-lines-page :name t :command-table info-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (start (clone-mark point))
	 (end (clone-mark point)))
    (backward-page start)
    (forward-page end)
    (let ((total (number-of-lines-in-region start end))
	  (before (number-of-lines-in-region start point))
	  (after (number-of-lines-in-region point end)))
      (display-message "Page has ~A lines (~A + ~A)" total before after))))

(set-key 'com-count-lines-page
	 'info-table
	 '((#\x :control) (#\l)))

(define-command (com-count-lines-region :name t :command-table info-table) ()
  (let*  ((pane (current-window))
	  (point (point pane))
	  (mark (mark pane))
	  (lines (number-of-lines-in-region point mark))
	  (chars (abs (- (offset point) (offset mark)))))
    (display-message "Region has ~D line~:P, ~D character~:P." lines chars)))

(set-key 'com-count-lines-region
	 'info-table
	 '((#\= :meta)))

(define-command (com-what-cursor-position :name t :command-table info-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (buffer (buffer pane))
	 (offset (offset point))
	 (size (size buffer))
	 (char (object-after point))
	 (column (column-number point)))
    (display-message "Char: ~:C (#o~O ~:*~D ~:*#x~X) point=~D of ~D (~D%) column ~D"
		     char (char-code char) offset size
		     (round (* 100 (/ offset size))) column)))

(set-key 'com-what-cursor-position
	 'info-table
	 '((#\x :control) (#\=)))

(define-command (com-eval-expression :name t :command-table base-table)
    ((insertp 'boolean :prompt "Insert?"))
  (let* ((*package* (find-package :climacs-gui))
	 (string (handler-case (accept 'string :prompt "Eval")
		   (error () (progn (beep)
				    (display-message "Empty string")
				    (return-from com-eval-expression nil)))))
	 (values (multiple-value-list
		  (handler-case (eval (read-from-string string))
		    (error (condition) (progn (beep)
					      (display-message "~a" condition)
					      (return-from com-eval-expression nil))))))
	 (result (format nil "~:[; No values~;~:*~{~S~^,~}~]" values)))
    (if insertp
	(insert-sequence (point (current-window)) result)
	(display-message result))))

(set-key `(com-eval-expression ,*numeric-argument-p*)
	 'base-table
	 '((#\: :shift :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commenting

;;; figure out how to make commands without key bindings accept numeric arguments. 
(define-command (com-comment-region :name t :command-table comment-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (comment-region syntax point mark)))

(define-command (com-backward-expression :name t :command-table movement-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-expression point syntax))
	(loop repeat (- count) do (forward-expression point syntax)))))

(set-key `(com-backward-expression ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :control :meta)))

(define-command (com-forward-expression :name t :command-table movement-table)
    ((count 'integer :prompt "Number of expresssions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-expression point syntax))
	(loop repeat (- count) do (backward-expression point syntax)))))

(set-key `(com-forward-expression ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :control :meta)))

(define-command (com-mark-expression :name t :command-table marking-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-expression)
      (setf (offset mark) (offset point)))
    (if (plusp count)
	(loop repeat count do (forward-expression mark syntax))
	(loop repeat (- count) do (backward-expression mark syntax)))))

(set-key `(com-mark-expression ,*numeric-argument-marker*)
	 'marking-table
	 '((#\@ :shift :control :meta)))

(define-command (com-kill-expression :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-expression mark syntax))
	(loop repeat (- count) do (backward-expression mark syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence mark point))
    (delete-region mark point)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\k :control :meta)))

(define-command (com-backward-kill-expression :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-expression mark syntax))
	(loop repeat (- count) do (forward-expression mark syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence mark point))
    (delete-region mark point)))

(set-key `(com-backward-kill-expression ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Backspace :control :meta)))

;; (defparameter *insert-pair-alist*
;; 	      '((#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>) (#\" #\") (#\' #\') (#\` #\')))

(defun insert-pair (mark syntax &optional (count 0) (open #\() (close #\)))
  (cond ((> count 0)
	 (loop while (and (not (end-of-buffer-p mark))
			  (whitespacep (object-after mark)))
	       do (forward-object mark)))
	((< count 0)
	 (setf count (- count))
	 (loop repeat count do (backward-expression mark syntax))))
  (unless (or (beginning-of-buffer-p mark)
	      (whitespacep (object-before mark)))
    (insert-object mark #\Space))
  (insert-object mark open)
  (let ((here (clone-mark mark)))
    (loop repeat count
	  do (forward-expression here syntax))
    (insert-object here close)
    (unless (or (end-of-buffer-p here)
		(whitespacep (object-after here)))
      (insert-object here #\Space))))

(defun insert-parentheses (mark syntax count)
  (insert-pair mark syntax count #\( #\)))

(define-command (com-insert-parentheses :name t :command-table editing-table)
    ((count 'integer :prompt "Number of expressions")
     (wrap-p 'boolean :prompt "Wrap expressions?"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (unless wrap-p (setf count 0))
    (insert-parentheses point syntax count)))

(set-key `(com-insert-parentheses ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'editing-table
	 '((#\( :meta)))

(define-command (com-forward-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	 (loop repeat count do (forward-list point syntax))
	 (loop repeat (- count) do (backward-list point syntax)))))

(set-key `(com-forward-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n :control :meta)))

(define-command (com-backward-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-list point syntax))
	(loop repeat (- count) do (forward-list point syntax)))))

(set-key `(com-backward-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\p :control :meta)))

(define-command (com-down-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (down-list point syntax))
	(loop repeat (- count) do (backward-down-list point syntax)))))

(set-key `(com-down-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\d :control :meta)))

(define-command (com-backward-down-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-down-list point syntax))
	(loop repeat (- count) do (down-list point syntax)))))

(define-command (com-backward-up-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-up-list point syntax))
	(loop repeat (- count) do (up-list point syntax)))))

(set-key `(com-backward-up-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\u :control :meta)))

(define-command (com-up-list :name t :command-table movement-table) ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (up-list point syntax))
	(loop repeat (- count) do (backward-up-list point syntax)))))

(define-command (com-eval-defun :name t :command-table lisp-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (eval-defun point syntax)))

(set-key 'com-eval-defun
	 'lisp-table
	 '((#\x :control :meta)))

(define-command (com-beginning-of-definition :name t :command-table movement-table)
    ((count 'integer :prompt "Number of definitions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (beginning-of-definition point syntax))
	(loop repeat (- count) do (end-of-definition point syntax)))))

(set-key `(com-beginning-of-definition ,*numeric-argument-marker*)
	 'movement-table
	 '((#\a :control :meta)))

(define-command (com-end-of-definition :name t :command-table movement-table)
    ((count 'integer :prompt "Number of definitions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (end-of-definition point syntax))
	(loop repeat (- count) do (beginning-of-definition point syntax)))))

(set-key `(com-end-of-definition ,*numeric-argument-marker*)
	 'movement-table
	 '((#\e :control :meta)))

(define-command (com-mark-definition :name t :command-table marking-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-definition)
      (beginning-of-definition point syntax)
      (setf (offset mark) (offset point)))
    (end-of-definition mark syntax)))

(set-key 'com-mark-definition
	 'marking-table
	 '((#\h :control :meta)))

(define-command (com-package :name t :command-table lisp-table) ()
  (let* ((pane (current-window))
	 (syntax (syntax (buffer pane)))
	 (package (climacs-lisp-syntax::package-of syntax)))
    (display-message (format nil "~s" package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; For testing purposes

(define-command (com-reset-profile :name t :command-table development-table) ()
  #+sbcl (sb-profile:reset)
  #-sbcl nil)

(define-command (com-report-profile :name t :command-table development-table) ()
  #+sbcl (sb-profile:report)
  #-sbcl nil)

(define-command (com-recompile :name t :command-table development-table) ()
  (asdf:operate 'asdf:load-op :climacs))


(define-gesture-name :select-other :pointer-button-press (:left :meta) :unique nil)

(define-presentation-translator lisp-string-to-string
    (climacs-lisp-syntax::lisp-string string development-table
                  :gesture :select-other
                  :tester-definitive t
                  :menu nil
                  :priority 10)
    (object)
  object)

(define-command (com-accept-string :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'string))))
 
(define-command (com-accept-symbol :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'symbol))))	 

(define-command (com-accept-lisp-string :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'lisp-string))))

(define-command (com-visible-mark :name t :command-table marking-table) ()
  (setf (mark-visible-p (current-window)) (not (mark-visible-p (current-window)))))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (set-key `(com-self-insert ,*numeric-argument-marker*)
	     'self-insert-table
	     (list (list (code-char code)))))

(set-key `(com-self-insert ,*numeric-argument-marker*)
	 'self-insert-table
	 '((#\Newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Some Unicode stuff

(define-command (com-insert-charcode :name t :command-table self-insert-table)
    ((code 'integer :prompt "Code point"))
  (insert-object (point (current-window)) (code-char code)))

(set-key '(com-insert-charcode 193) 'self-insert-table '((:dead--acute)(#\A)))
(set-key '(com-insert-charcode 201) 'self-insert-table '((:dead--acute)(#\E)))
(set-key '(com-insert-charcode 205) 'self-insert-table '((:dead--acute)(#\I)))
(set-key '(com-insert-charcode 211) 'self-insert-table '((:dead--acute)(#\O)))
(set-key '(com-insert-charcode 218) 'self-insert-table '((:dead--acute)(#\U)))
(set-key '(com-insert-charcode 221) 'self-insert-table '((:dead--acute)(#\Y)))
(set-key '(com-insert-charcode 225) 'self-insert-table '((:dead--acute)(#\a)))
(set-key '(com-insert-charcode 233) 'self-insert-table '((:dead--acute)(#\e)))
(set-key '(com-insert-charcode 237) 'self-insert-table '((:dead--acute)(#\i)))
(set-key '(com-insert-charcode 243) 'self-insert-table '((:dead--acute)(#\o)))
(set-key '(com-insert-charcode 250) 'self-insert-table '((:dead--acute)(#\u)))
(set-key '(com-insert-charcode 253) 'self-insert-table '((:dead--acute)(#\y)))
(set-key '(com-insert-charcode 199) 'self-insert-table '((:dead--acute)(#\C)))
(set-key '(com-insert-charcode 231) 'self-insert-table '((:dead--acute)(#\c)))
(set-key '(com-insert-charcode 215) 'self-insert-table '((:dead--acute)(#\x)))
(set-key '(com-insert-charcode 247) 'self-insert-table '((:dead--acute)(#\-)))
(set-key '(com-insert-charcode 222) 'self-insert-table '((:dead--acute)(#\T)))
(set-key '(com-insert-charcode 254) 'self-insert-table '((:dead--acute)(#\t)))
(set-key '(com-insert-charcode 223) 'self-insert-table '((:dead--acute)(#\s)))
(set-key '(com-insert-charcode 39) 'self-insert-table '((:dead--acute)(#\Space)))

(set-key '(com-insert-charcode 197) 'self-insert-table '((:dead--acute)(:dead--acute)(#\A)))
(set-key '(com-insert-charcode 229) 'self-insert-table '((:dead--acute)(:dead--acute)(#\a)))

(set-key '(com-insert-charcode 192) 'self-insert-table '((:dead--grave)(#\A)))
(set-key '(com-insert-charcode 200) 'self-insert-table '((:dead--grave)(#\E)))
(set-key '(com-insert-charcode 204) 'self-insert-table '((:dead--grave)(#\I)))
(set-key '(com-insert-charcode 210) 'self-insert-table '((:dead--grave)(#\O)))
(set-key '(com-insert-charcode 217) 'self-insert-table '((:dead--grave)(#\U)))
(set-key '(com-insert-charcode 224) 'self-insert-table '((:dead--grave)(#\a)))
(set-key '(com-insert-charcode 232) 'self-insert-table '((:dead--grave)(#\e)))
(set-key '(com-insert-charcode 236) 'self-insert-table '((:dead--grave)(#\i)))
(set-key '(com-insert-charcode 242) 'self-insert-table '((:dead--grave)(#\o)))
(set-key '(com-insert-charcode 249) 'self-insert-table '((:dead--grave)(#\u)))
(set-key '(com-insert-charcode 96) 'self-insert-table '((:dead--grave)(#\Space)))

(set-key '(com-insert-charcode 196) 'self-insert-table '((:dead--diaeresis :shift)(#\A)))
(set-key '(com-insert-charcode 203) 'self-insert-table '((:dead--diaeresis :shift)(#\E)))
(set-key '(com-insert-charcode 207) 'self-insert-table '((:dead--diaeresis :shift)(#\I)))
(set-key '(com-insert-charcode 214) 'self-insert-table '((:dead--diaeresis :shift)(#\O)))
(set-key '(com-insert-charcode 220) 'self-insert-table '((:dead--diaeresis :shift)(#\U)))
(set-key '(com-insert-charcode 228) 'self-insert-table '((:dead--diaeresis :shift)(#\a)))
(set-key '(com-insert-charcode 235) 'self-insert-table '((:dead--diaeresis :shift)(#\e)))
(set-key '(com-insert-charcode 239) 'self-insert-table '((:dead--diaeresis :shift)(#\i)))
(set-key '(com-insert-charcode 246) 'self-insert-table '((:dead--diaeresis :shift)(#\o)))
(set-key '(com-insert-charcode 252) 'self-insert-table '((:dead--diaeresis :shift)(#\u)))
(set-key '(com-insert-charcode 255) 'self-insert-table '((:dead--diaeresis :shift)(#\y)))
(set-key '(com-insert-charcode 34) 'self-insert-table '((:dead--diaeresis :shift)(#\Space)))

(set-key '(com-insert-charcode 195) 'self-insert-table '((:dead--tilde :shift)(#\A)))
(set-key '(com-insert-charcode 209) 'self-insert-table '((:dead--tilde :shift)(#\N)))
(set-key '(com-insert-charcode 227) 'self-insert-table '((:dead--tilde :shift)(#\a)))
(set-key '(com-insert-charcode 241) 'self-insert-table '((:dead--tilde :shift)(#\n)))
(set-key '(com-insert-charcode 198) 'self-insert-table '((:dead--tilde :shift)(#\E)))
(set-key '(com-insert-charcode 230) 'self-insert-table '((:dead--tilde :shift)(#\e)))
(set-key '(com-insert-charcode 208) 'self-insert-table '((:dead--tilde :shift)(#\D)))
(set-key '(com-insert-charcode 240) 'self-insert-table '((:dead--tilde :shift)(#\d)))
(set-key '(com-insert-charcode 216) 'self-insert-table '((:dead--tilde :shift)(#\O)))
(set-key '(com-insert-charcode 248) 'self-insert-table '((:dead--tilde :shift)(#\o)))
(set-key '(com-insert-charcode 126) 'self-insert-table '((:dead--tilde :shift)(#\Space)))

(set-key '(com-insert-charcode 194) 'self-insert-table '((:dead--circumflex :shift)(#\A)))
(set-key '(com-insert-charcode 202) 'self-insert-table '((:dead--circumflex :shift)(#\E)))
(set-key '(com-insert-charcode 206) 'self-insert-table '((:dead--circumflex :shift)(#\I)))
(set-key '(com-insert-charcode 212) 'self-insert-table '((:dead--circumflex :shift)(#\O)))
(set-key '(com-insert-charcode 219) 'self-insert-table '((:dead--circumflex :shift)(#\U)))
(set-key '(com-insert-charcode 226) 'self-insert-table '((:dead--circumflex :shift)(#\a)))
(set-key '(com-insert-charcode 234) 'self-insert-table '((:dead--circumflex :shift)(#\e)))
(set-key '(com-insert-charcode 238) 'self-insert-table '((:dead--circumflex :shift)(#\i)))
(set-key '(com-insert-charcode 244) 'self-insert-table '((:dead--circumflex :shift)(#\o)))
(set-key '(com-insert-charcode 251) 'self-insert-table '((:dead--circumflex :shift)(#\u)))
(set-key '(com-insert-charcode 94) 'self-insert-table '((:dead--circumflex :shift)(#\Space)))

(define-command (com-regex-search-forward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-forward (point (current-window)) string)))

(define-command (com-regex-search-backward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search backward"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-backward (point (current-window)) string)))
