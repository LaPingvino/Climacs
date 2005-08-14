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
      :height 20 :max-height 20 :min-height 20))

(defparameter *with-scrollbars* t
  "If T, classic look and feel. If NIL, stripped-down look (:")

(define-application-frame climacs (standard-application-frame
				   esa-frame-mixin)
  ((buffers :initform '() :accessor buffers))
  (:command-table (global-climacs-table :inherit-from (global-esa-table)))
  (:menu-bar nil)
  (:panes
   (win (let* ((extended-pane 
		(make-pane 'extended-pane
			   :width 900 :height 400
			   :end-of-line-action :scroll
			   :incremental-redisplay t
			   :display-function 'display-win
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
   (int (make-pane 'climacs-minibuffer-pane :width 900)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 win
	 int)))
  (:top-level (esa-top-level)))

(defun current-window ()
  (car (windows *application-frame*)))

(defmethod redisplay-frame-panes :around ((frame climacs) &rest args)
  (declare (ignore args))
  (let ((buffers (remove-duplicates (mapcar #'buffer (windows frame)))))
    (loop for buffer in buffers
	  do (update-syntax buffer (syntax buffer)))
    (call-next-method)
    (loop for buffer in buffers
	  do (clear-modify buffer))))

(defun climacs (&key (width 900) (height 400))
  "Starts up a climacs session"
  (let ((frame (make-application-frame
		'climacs :width width :height height)))
    (run-frame-top-level frame)))

(defun display-info (frame pane)
  (declare (ignore frame))
  (let* ((master-pane (master-pane pane))
	 (buf (buffer master-pane))
	 (size (size buf))
	 (top (top master-pane))
	 (bot (bot master-pane))
	 (name-info (format nil "   ~a  ~a~:[~30t~a~;~*~]   ~:[(~;Syntax: ~]~a~a~a~a~:[)~;~]    ~a"
			    (if (needs-saving buf) "**" "--")
			    (name buf)
			    *with-scrollbars*
			    (cond ((and (mark= size bot)
					(mark= 0 top))
				   "")
				  ((mark= size bot)
				   "Bot")
				  ((mark= 0 top)
				   "Top")
				  (t (format nil "~a%"
					     (round (* 100 (/ (offset top)
							      size))))))
			    *with-scrollbars*
			    (name (syntax buf))
			    (if (slot-value master-pane 'overwrite-mode)
				" Ovwrt"
				"")
			    (if (auto-fill-mode master-pane)
				" Fill"
				"")
			    (if (isearch-mode master-pane)
				" Isearch"
				"")
			    *with-scrollbars*
			    (if (recordingp *application-frame*)
				"Def"
				""))))
    (princ name-info pane)))

(defun display-win (frame pane)
  "The display function used by the climacs application frame."
  (declare (ignore frame))
  (redisplay-pane pane (eq pane (current-window))))

(defmethod handle-repaint :before ((pane extended-pane) region)
  (declare (ignore region))
  (redisplay-frame-pane *application-frame* pane))

(defvar *kill-ring* (make-instance 'kill-ring :max-size 7))

(defmethod execute-frame-command :around ((frame climacs) command)
  (handler-case
      (with-undo ((buffer (current-window)))
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
      (beep) (display-message "Operation unavailable for syntax"))))  

(defmethod execute-frame-command :after ((frame climacs) command)
  (loop for buffer in (buffers frame)
	do (when (modified-p buffer)
	     (setf (needs-saving buffer) t))))	

(defmacro define-named-command (command-name args &body body)
  `(define-command ,(if (listp command-name)
			`(,@command-name :name t :command-table global-climacs-table)
			`(,command-name :name t :command-table global-climacs-table))
       ,args ,@body))

(define-named-command com-toggle-overwrite-mode ()
  (with-slots (overwrite-mode) (current-window)
    (setf overwrite-mode (not overwrite-mode))))

(define-named-command com-not-modified ()
  (setf (needs-saving (buffer (current-window))) nil))

(define-named-command com-set-fill-column ((column 'integer :prompt "Column Number:"))
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
  (let* ((win (current-window))
	 (point (point win)))
    (unless (constituentp char)
      (possibly-expand-abbrev point))
    (when (whitespacep char)
      (possibly-fill-line))
    (if (and (slot-value win 'overwrite-mode) (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point char))
	(insert-object point char))))

(define-command com-self-insert ((count 'integer))
  (loop repeat count do (insert-character *current-gesture*)))

(define-named-command com-beginning-of-line ()
  (beginning-of-line (point (current-window))))

(define-named-command com-end-of-line ()
  (end-of-line (point (current-window))))

(define-named-command com-delete-object ((count 'integer :prompt "Number of Objects")
					 (killp 'boolean :prompt "Kill?"))
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (forward-object mark count)
    (when killp
      (kill-ring-standard-push *kill-ring*
			       (region-to-sequence point mark)))
    (delete-region point mark)))

(define-named-command com-backward-delete-object ((count 'integer :prompt "Number of Objects")
						  (killp 'boolean :prompt "Kill?"))
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (backward-object mark count)
    (when killp
      (kill-ring-standard-push *kill-ring*
			       (region-to-sequence mark point)))
  (delete-region mark point)))

(define-named-command com-zap-to-object ()
  (let* ((item (handler-case (accept 't :prompt "Zap to Object")
		(error () (progn (beep)
				 (display-message "Not a valid object")
				 (return-from com-zap-to-object nil)))))
	 (current-point (point (current-window)))
	 (item-mark (clone-mark current-point))
	 (current-offset (offset current-point)))
    (search-forward item-mark (vector item))
    (delete-range current-point (- (offset item-mark) current-offset))))

(define-named-command com-zap-to-character ()
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

(define-named-command com-transpose-objects ()
  (let* ((point (point (current-window))))
    (unless (beginning-of-buffer-p point)
      (when (end-of-line-p point)
       (backward-object point))
       (let ((object (object-after point)))
        (delete-range point)
       (backward-object point)
       (insert-object point object)
       (forward-object point)))))

(define-named-command com-backward-object ((count 'integer :prompt "Number of Objects"))
  (backward-object (point (current-window)) count))

(define-named-command com-forward-object ((count 'integer :prompt "Number of Objects"))
  (forward-object (point (current-window)) count))

(define-named-command com-transpose-words ()
  (let* ((point (point (current-window))))
    (let (bw1 bw2 ew1 ew2)
      (backward-word point)
      (setf bw1 (offset point))
      (forward-word point)
      (setf ew1 (offset point))
      (forward-word point)
      (when (= (offset point) ew1)
        ;; this is emacs' message in the minibuffer
        (error "Don't have two things to transpose"))
      (setf ew2 (offset point))
      (backward-word point)
      (setf bw2 (offset point))
      (let ((w2 (buffer-sequence (buffer point) bw2 ew2))
            (w1 (buffer-sequence (buffer point) bw1 ew1)))
        (delete-word point)
        (insert-sequence point w1)
        (backward-word point)
        (backward-word point)
        (delete-word point)
        (insert-sequence point w2)
        (forward-word point)))))

(define-named-command com-transpose-lines ()
  (let ((point (point (current-window))))
    (beginning-of-line point)
    (unless (beginning-of-buffer-p point)
      (previous-line point))
    (let* ((bol (offset point))
           (eol (progn (end-of-line point)
                       (offset point)))
           (line (buffer-sequence (buffer point) bol eol)))
      (delete-region bol point)
      ;; Remove newline at end of line as well.
      (unless (end-of-buffer-p point)
        (delete-range point))
      ;; If the current line is at the end of the buffer, we want to
      ;; be able to insert past it, so we need to get an extra line
      ;; at the end.
      (end-of-line point)
      (when (end-of-buffer-p point)
        (insert-object point #\Newline))
      (next-line point 0)
      (insert-sequence point line)
      (insert-object point #\Newline))))

(define-named-command com-previous-line ((numarg 'integer :prompt "How many lines?"))
  (let* ((win (current-window))
	 (point (point win)))
    (unless (or (eq (previous-command win) 'com-previous-line)
		(eq (previous-command win) 'com-next-line))
      (setf (slot-value win 'goal-column) (column-number point)))
    (if (plusp numarg)
	(previous-line point (slot-value win 'goal-column) numarg)
	(next-line point (slot-value win 'goal-column) (- numarg)))))

(define-named-command com-next-line ((numarg 'integer :prompt "How many lines?"))
  (let* ((win (current-window))
	 (point (point win)))
    (unless (or (eq (previous-command win) 'com-previous-line)
		(eq (previous-command win) 'com-next-line))
      (setf (slot-value win 'goal-column) (column-number point)))
    (if (plusp numarg)
	(next-line point (slot-value win 'goal-column) numarg)
	(previous-line point (slot-value win 'goal-column) (- numarg)))))

(define-named-command com-open-line ((numarg 'integer :prompt "How many lines?"))
  (open-line (point (current-window)) numarg))

(define-named-command com-kill-line ((numarg 'integer :prompt "Kill how many lines?")
				     (numargp 'boolean :prompt "Kill entire lines?"))
  (let* ((pane (current-window))
	 (point (point pane))
         (mark (offset point)))
    (cond ((= 0 numarg)
	   (beginning-of-line point))
	  ((< numarg 0)
	   (loop repeat (- numarg)
		 until (beginning-of-buffer-p point)
		 do (beginning-of-line point)
		 until (beginning-of-buffer-p point)
		 do (backward-object point)))
	  ((or numargp (> numarg 1))
	   (loop repeat numarg
		 until (end-of-buffer-p point)
		 do (end-of-line point)
		 until (end-of-buffer-p point)
		 do (forward-object point)))
	  (t
	   (cond ((end-of-buffer-p point) nil)
		 ((end-of-line-p point)(forward-object point))
		 (t (end-of-line point)))))
    (unless (mark= point mark)
      (if (eq (previous-command pane) 'com-kill-line)
	  (kill-ring-concatenating-push *kill-ring*
					(region-to-sequence mark point))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence mark point)))
      (delete-region mark point))))	   

(define-named-command com-forward-word ((count 'integer :prompt "Number of words"))
  (if (plusp count)
      (forward-word (point (current-window)) count)
      (backward-word (point (current-window)) (- count))))

(define-named-command com-backward-word ((count 'integer :prompt "Number of words"))
  (backward-word (point (current-window)) count))

(define-named-command com-delete-word ((count 'integer :prompt "Number of words"))
  (delete-word (point (current-window)) count))

(define-named-command com-kill-word ((count 'integer :prompt "Number of words"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (offset point)))
    (loop repeat count
	  until (end-of-buffer-p point)
	  do (forward-word point))
    (unless (mark= point mark)
      (if (eq (previous-command pane) 'com-kill-word)
	  (kill-ring-concatenating-push *kill-ring*
					(region-to-sequence mark point))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence mark point)))
      (delete-region mark point))))

(define-named-command com-backward-kill-word ((count 'integer :prompt "Number of words"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (offset point)))
    (loop repeat count
	  until (end-of-buffer-p point)
	  do (backward-word point))
    (unless (mark= point mark)
      (if (eq (previous-command pane) 'com-backward-kill-word)
	  (kill-ring-reverse-concatenating-push *kill-ring*
					(region-to-sequence mark point))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence mark point)))
      (delete-region mark point))))

(define-named-command com-mark-word ((count 'integer :prompt "Number of words"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane)))
    (unless (eq (previous-command pane) 'com-mark-word)
      (setf (offset mark) (offset point)))
    (if (plusp count)
	(forward-word mark count)
	(backward-word mark (- count)))))

(define-named-command com-backward-delete-word ((count 'integer :prompt "Number of words"))
  (backward-delete-word (point (current-window)) count))

(define-named-command com-upcase-region ()
  (let ((cw (current-window)))
    (upcase-region (mark cw) (point cw))))

(define-named-command com-downcase-region ()
  (let ((cw (current-window)))
    (downcase-region (mark cw) (point cw))))

(define-named-command com-capitalize-region ()
  (let ((cw (current-window)))
    (capitalize-region (mark cw) (point cw))))

(define-named-command com-upcase-word ()
  (upcase-word (point (current-window))))

(define-named-command com-downcase-word ()
  (downcase-word (point (current-window))))

(define-named-command com-capitalize-word ()
  (capitalize-word (point (current-window))))

(define-named-command com-tabify-region ()
  (let ((pane (current-window)))
    (tabify-region
     (mark pane) (point pane) (tab-space-count (stream-default-view pane)))))

(define-named-command com-untabify-region ()
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

(define-named-command com-indent-line ()
  (let* ((pane (current-window))
         (point (point pane)))
    (indent-current-line pane point)))

(define-named-command com-newline-and-indent ()
  (let* ((pane (current-window))
	 (point (point pane)))
    (insert-object point #\Newline)
    (indent-current-line pane point)))

(define-named-command com-delete-indentation ()
  (delete-indentation (point (current-window))))

(define-named-command com-auto-fill-mode ()
  (let ((pane (current-window)))
    (setf (auto-fill-mode pane) (not (auto-fill-mode pane)))))

(define-named-command com-fill-paragraph ()
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

(eval-when (:compile-toplevel :load-toplevel)
  (define-presentation-type completable-pathname ()
  :inherit-from 'pathname))

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

(define-presentation-method accept
    ((type completable-pathname) stream (view textual-view) &key)
  (multiple-value-bind (pathname success string)
      (complete-input stream
		      #'filename-completer
		      :allow-any-input t)
    (declare (ignore success))
    (or pathname string)))

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
		     (member x y :test #'string=))
	     :key #'climacs-syntax::syntax-description-pathname-types))
      'basic-syntax))

(define-named-command com-find-file ()
  (let ((filepath (accept 'completable-pathname
			  :prompt "Find File"))
	(buffer (make-instance 'climacs-buffer))
	(pane (current-window)))
    (setf (offset (point (buffer pane))) (offset (point pane)))
    (push buffer (buffers *application-frame*))
    (setf (buffer (current-window)) buffer)
    (setf (syntax buffer)
	  (make-instance
	   (syntax-class-name-for-filepath filepath)
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
    (redisplay-frame-panes *application-frame*)))

(define-named-command com-insert-file ()
  (let ((filename (accept 'completable-pathname
			  :prompt "Insert File"))
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

(defun save-buffer (buffer)
  (let ((filepath (or (filepath buffer)
		      (accept 'completable-pathname
			      :prompt "Save Buffer to File"))))
    (with-open-file (stream filepath :direction :output :if-exists :supersede)
      (output-to-stream stream buffer 0 (size buffer)))
    (setf (filepath buffer) filepath
	  (name buffer) (filepath-filename filepath))
    (display-message "Wrote: ~a" (filepath buffer))
    (setf (needs-saving buffer) nil)))

(define-named-command com-save-buffer ()
  (let ((buffer (buffer (current-window))))
    (if (or (null (filepath buffer))
	    (needs-saving buffer))
	(save-buffer buffer)
	(display-message "No changes need to be saved from ~a" (name buffer)))))

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

(define-named-command com-write-buffer ()
  (let ((filepath (accept 'completable-pathname
			  :prompt "Write Buffer to File"))
	(buffer (buffer (current-window))))
    (with-open-file (stream filepath :direction :output :if-exists :supersede)
      (output-to-stream stream buffer 0 (size buffer)))
    (setf (filepath buffer) filepath
	  (name buffer) (filepath-filename filepath)
	  (needs-saving buffer) nil)
    (display-message "Wrote: ~a" (filepath buffer))))

(define-presentation-method accept
    ((type buffer) stream (view textual-view) &key)
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far (buffers *application-frame*) '() :action action
			 :name-key #'name
			 :value-key #'identity))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success))
    (or	object
	(car (push (make-instance 'climacs-buffer :name string)
		   (buffers *application-frame*))))))

(define-named-command com-switch-to-buffer ()
  (let ((buffer (accept 'buffer
			:prompt "Switch to buffer"))
	(pane (current-window)))
    (setf (offset (point (buffer pane))) (offset (point pane)))
    (setf (buffer pane) buffer)
    (full-redisplay pane)))

(define-named-command com-kill-buffer ()
  (with-slots (buffers) *application-frame*
    (let ((buffer (buffer (current-window))))
      (when (and (needs-saving buffer)
		 (handler-case (accept 'boolean :prompt "Save buffer first?")
		   (error () (progn (beep)
				    (display-message "Invalid answer")
				    (return-from com-kill-buffer nil)))))
        (com-save-buffer))
      (setf buffers (remove buffer buffers))
      ;; Always need one buffer.
      (when (null buffers)
	(push (make-instance 'climacs-buffer :name "*scratch*")
	      buffers))
      (setf (buffer (current-window)) (car buffers)))))

(define-named-command com-full-redisplay ()
  (full-redisplay (current-window)))

(define-named-command com-load-file ()
  (let ((filepath (accept 'completable-pathname
			  :prompt "Load File")))
    (load filepath)))

(define-named-command com-beginning-of-buffer ()
  (beginning-of-buffer (point (current-window))))

(define-named-command com-page-down ()
  (let ((pane (current-window)))
    (page-down pane)))

(define-named-command com-page-up ()
  (let ((pane (current-window)))
    (page-up pane)))

(define-named-command com-end-of-buffer ()
  (end-of-buffer (point (current-window))))

(define-named-command com-mark-whole-buffer ()
  (beginning-of-buffer (point (current-window)))
  (end-of-buffer (mark (current-window))))

(define-named-command com-back-to-indentation ()
  (let ((point (point (current-window))))
    (beginning-of-line point)
    (loop until (end-of-line-p point)
	  while (whitespacep (object-after point))
	  do (incf (offset point)))))

(define-named-command com-delete-horizontal-space ((backward-only-p
						    'boolean :prompt "Delete backwards only?"))
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (loop until (beginning-of-line-p point)
	  while (whitespacep (object-before point))
	  do (backward-object point))
    (unless backward-only-p
      (loop until (end-of-line-p mark)
	    while (whitespacep (object-after mark))
	    do (forward-object mark)))
    (delete-region point mark)))

(define-named-command com-just-one-space ((count 'integer :prompt "Number of spaces"))
  (let ((point (point (current-window)))
	offset)
    (loop until (beginning-of-line-p point)
	  while (whitespacep (object-before point))
	  do (backward-object point))
    (loop until (end-of-line-p point)
	  while (whitespacep (object-after point))
	  repeat count do (forward-object point)
	  finally (setf offset (offset point)))
    (loop until (end-of-line-p point)
	  while (whitespacep (object-after point))
	  do (forward-object point))
    (delete-region offset point)))

(define-named-command com-goto-position ()
  (setf (offset (point (current-window)))
	(handler-case (accept 'integer :prompt "Goto Position")
	  (error () (progn (beep)
			   (display-message "Not a valid position")
			   (return-from com-goto-position nil))))))  

(define-named-command com-goto-line ()
  (loop with mark = (let ((m (clone-mark
			      (low-mark (buffer (current-window)))
			      :right)))
		      (beginning-of-buffer m)
		      m)
	do (end-of-line mark)
	until (end-of-buffer-p mark)
	repeat (1- (handler-case (accept 'integer :prompt "Goto Line")
		 (error () (progn (beep)
				  (display-message "Not a valid line number")
				  (return-from com-goto-line nil)))))
	do (incf (offset mark))
	   (end-of-line mark)
	finally (beginning-of-line mark)
		(setf (offset (point (current-window)))
		      (offset mark))))

(define-named-command com-browse-url ()
  (let ((url (accept 'url :prompt "Browse URL")))
    #+ (and sbcl darwin)
    (sb-ext:run-program "/usr/bin/open" `(,url) :wait nil)))

(define-named-command com-set-mark ()
  (let ((pane (current-window)))
    (setf (mark pane) (clone-mark (point pane)))))

(define-named-command com-exchange-point-and-mark ()
  (let ((pane (current-window)))
    (psetf (offset (mark pane)) (offset (point pane))
	   (offset (point pane)) (offset (mark pane)))))

(define-named-command com-set-syntax ()
  (let* ((pane (current-window))
	 (buffer (buffer pane)))
    (setf (syntax buffer)
	  (make-instance (or (accept 'syntax :prompt "Set Syntax")
			     (progn (beep)
				    (display-message "No such syntax")
				    (return-from com-set-syntax nil)))
	     :buffer (buffer (point pane))))))

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
		     (1/2 constellation) adjust (1/2 additional-constellation))
		   (horizontally ()
		     (1/2 constellation) adjust (1/2 additional-constellation)))))
      (sheet-adopt-child parent new)
      (reorder-sheets parent 
		      (if (eq constellation first)
			  (if third
			      (list new second third)
			      (list new second))
			  (if third
			      (list first second new)
			      (list first new)))))))

(defun parent3 (sheet)
  (sheet-parent (sheet-parent (sheet-parent sheet))))

(defun make-pane-constellation ()
  "make a vbox containing a scroller pane as its first child and an
info pane as its second child.  The scroller pane contains a viewport
which contains an extended pane.  Return the vbox and the extended pane
as two values.
If *with-scrollbars nil, omit the scroller."

  (let* ((extended-pane
	  (make-pane 'extended-pane
		     :width 900 :height 400
		     :name 'win
		     :end-of-line-action :scroll
		     :incremental-redisplay t
		     :display-function 'display-win
		     :command-table 'global-climacs-table))
	 (vbox
	  (vertically ()
	    (if *with-scrollbars*
		(scrolling ()
		  extended-pane)
		extended-pane)
	    (make-pane 'climacs-info-pane
		       :master-pane extended-pane
		       :width 900))))
    (values vbox extended-pane)))

(define-named-command com-split-window-vertically ()
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window (current-window))
	     (constellation-root (if *with-scrollbars*
				     (parent3 current-window)
				     (sheet-parent current-window))))
        (setf (offset (point (buffer current-window))) (offset (point current-window))
	      (buffer new-pane) (buffer current-window)
              (auto-fill-mode new-pane) (auto-fill-mode current-window)
              (auto-fill-column new-pane) (auto-fill-column current-window))
	(push new-pane (windows *application-frame*))
	(setf *standard-output* new-pane)
	(replace-constellation constellation-root vbox t)
	(full-redisplay current-window)
	(full-redisplay new-pane)))))

(define-named-command com-split-window-horizontally ()
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window (current-window))
	     (constellation-root (if *with-scrollbars*
				     (parent3 current-window)
				     (sheet-parent current-window))))
        (setf (offset (point (buffer current-window))) (offset (point current-window))
	      (buffer new-pane) (buffer current-window)
              (auto-fill-mode new-pane) (auto-fill-mode current-window)
              (auto-fill-column new-pane) (auto-fill-column current-window))
	(push new-pane (windows *application-frame*))
	(setf *standard-output* new-pane)
	(replace-constellation constellation-root vbox nil)
	(full-redisplay current-window)
	(full-redisplay new-pane)))))

(define-named-command com-other-window ()
  (setf (windows *application-frame*)
	(append (cdr (windows *application-frame*))
		(list (car (windows *application-frame*)))))
  (setf *standard-output* (car (windows *application-frame*))))

(define-named-command com-single-window ()
  (loop until (null (cdr (windows *application-frame*)))
	do (rotatef (car (windows *application-frame*))
		    (cadr (windows *application-frame*)))
	   (com-delete-window))
  (setf *standard-output* (car (windows *application-frame*))))

(define-named-command com-scroll-other-window ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-down other-window))))

(define-named-command com-scroll-other-window-up ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-up other-window))))

(define-named-command com-delete-window ()
  (unless (null (cdr (windows *application-frame*)))
    (let* ((constellation (if *with-scrollbars*
			      (parent3 (current-window))
			      (sheet-parent (current-window))))
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
      (pop (windows *application-frame*))
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

;;;;;;;;;;;;;;;;;;;;
;; Kill ring commands

;; Copies an element from a kill-ring to a buffer at the given offset
(define-named-command com-yank ()
  (insert-sequence (point (current-window)) (kill-ring-yank *kill-ring*)))

;; Destructively cut a given buffer region into the kill-ring
(define-named-command com-kill-region ()
  (let ((pane (current-window)))
    (kill-ring-standard-push
     *kill-ring* (region-to-sequence (mark pane) (point pane)))
    (delete-region (mark pane) (point pane))))

;; Non destructively copies in buffer region to the kill ring
(define-named-command com-copy-region ()
  (let ((pane (current-window)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence (point pane) (mark pane)))))

(define-named-command com-rotate-yank ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (last-yank (kill-ring-yank *kill-ring*)))
    (if (eq (previous-command pane)
	    'com-rotate-yank)
	(progn
	  (delete-range point (* -1 (length last-yank)))
	  (rotate-yank-position *kill-ring*)))
    (insert-sequence point (kill-ring-yank *kill-ring*))))

(define-named-command com-resize-kill-ring ()
  (let ((size (handler-case (accept 'integer :prompt "New kill ring size")
		(error () (progn (beep)
				 (display-message "Not a valid kill ring size")
				 (return-from com-resize-kill-ring nil))))))
    (setf (kill-ring-max-size *kill-ring*) size)))

(define-named-command com-append-next-kill ()
  (setf (append-next-p *kill-ring*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Incremental search

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

(define-named-command com-isearch-mode-forward ()
  (display-message "Isearch: ")
  (isearch-command-loop (current-window) t))

(define-named-command com-isearch-mode-backward ()
  (display-message "Isearch backward: ")
  (isearch-command-loop (current-window) nil))

(define-named-command com-isearch-append-char ()
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

(define-named-command com-isearch-delete-char ()
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

(define-named-command com-isearch-forward ()
  (let* ((pane (current-window))
         (point (point pane))
         (states (isearch-states pane))
         (string (if (null (second states))
                     (isearch-previous-string pane)
                     (search-string (first states))))
         (mark (clone-mark point)))
    (isearch-from-mark pane mark string t)))

(define-named-command com-isearch-backward ()
  (let* ((pane (current-window))
         (point (point pane))
         (states (isearch-states pane))
         (string (if (null (second states))
                     (isearch-previous-string pane)
                     (search-string (first states))))
         (mark (clone-mark point)))
    (isearch-from-mark pane mark string nil)))

(define-named-command com-isearch-exit ()
  (setf (isearch-mode (current-window)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Query replace

(defun query-replace-find-next-match (mark string)
  (flet ((object-equal (x y)
           (and (characterp x)
                (characterp y)
                (char-equal x y))))
    (let ((offset-before (offset mark)))
      (search-forward mark string :test #'object-equal)
      (/= (offset mark) offset-before))))

(define-named-command com-query-replace ()
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

(define-named-command com-query-replace-replace ()
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
      (case region-case
        (:upper-case (upcase-buffer-region buffer offset1 offset2))
        (:lower-case (downcase-buffer-region buffer offset1 offset2))
        (:capitalized (capitalize-buffer-region buffer offset1 offset2))))
    (incf occurrences)
    (if (query-replace-find-next-match point string1)
	(display-message "Query Replace ~A with ~A:"
		       string1 string2)
	(setf (query-replace-mode pane) nil))))

(define-named-command com-query-replace-skip ()
  (declare (special string1 string2))
  (let* ((pane (current-window))
         (point (point pane)))
    (if (query-replace-find-next-match point string1)
	(display-message "Query Replace ~A with ~A:"
			 string1 string2)
	(setf (query-replace-mode pane) nil))))

(define-named-command com-query-replace-exit ()
  (setf (query-replace-mode (current-window)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Undo/redo

(define-named-command com-undo ()
  (handler-case (undo (undo-tree (buffer (current-window))))
    (no-more-undo () (beep) (display-message "No more undo")))
  (full-redisplay (current-window)))

(define-named-command com-redo ()
  (handler-case (redo (undo-tree (buffer (current-window))))
    (no-more-undo () (beep) (display-message "No more redo")))
  (full-redisplay (current-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dynamic abbrevs

(define-named-command com-dabbrev-expand ()
  (let* ((win (current-window))
	 (point (point win)))
    (with-slots (original-prefix prefix-start-offset dabbrev-expansion-mark) win
       (flet ((move () (cond ((beginning-of-buffer-p dabbrev-expansion-mark)
			      (setf (offset dabbrev-expansion-mark)
				    (offset point))
			      (forward-word dabbrev-expansion-mark))
			     ((mark< dabbrev-expansion-mark point)
			      (backward-object dabbrev-expansion-mark))
			     (t (forward-object dabbrev-expansion-mark)))))
	 (unless (or (beginning-of-buffer-p point)
		     (not (constituentp (object-before point))))
	   (unless (and (eq (previous-command win) 'com-dabbrev-expand)
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
	   
(define-named-command com-backward-paragraph ((count 'integer :prompt "Number of paragraphs"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-paragraph point syntax))
	(loop repeat (- count) do (forward-paragraph point syntax)))))

(define-named-command com-forward-paragraph ((count 'integer :prompt "Number of paragraphs"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-paragraph point syntax))
	(loop repeat (- count) do (backward-paragraph point syntax)))))

(define-named-command com-mark-paragraph ((count 'integer :prompt "Number of paragraphs"))
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

(define-named-command com-backward-sentence ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-sentence point syntax))
	(loop repeat (- count) do (forward-sentence point syntax)))))

(define-named-command com-forward-sentence ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-sentence point syntax))
	(loop repeat (- count) do (backward-sentence point syntax)))))

(define-named-command com-kill-sentence ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-sentence point syntax))
	(loop repeat (- count) do (backward-sentence point syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
    (delete-region point mark)))

(define-named-command com-backward-kill-sentence ((count 'integer :prompt "Number of sentences"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-sentence point syntax))
	(loop repeat (- count) do (forward-sentence point syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
    (delete-region point mark)))

(defun forward-page (mark &optional (count 1))
  (loop repeat count
	unless (search-forward mark (coerce (list #\Newline #\Page) 'vector))
	  do (end-of-buffer mark)
	     (loop-finish)))

(define-named-command com-forward-page ((count 'integer :prompt "Number of pages"))
  (let* ((pane (current-window))
	 (point (point pane)))
    (if (plusp count)
	(forward-page point count)
	(backward-page point count))))

(defun backward-page (mark &optional (count 1))
  (loop repeat count
	  when (search-backward mark (coerce (list #\Newline #\Page) 'vector))
	    do (forward-object mark)
	  else do (beginning-of-buffer mark)
		  (loop-finish)))

(define-named-command com-backward-page ((count 'integer :prompt "Number of pages"))
  (let* ((pane (current-window))
	 (point (point pane)))
    (if (plusp count)
	(backward-page point count)
	(forward-page point count))))

(define-named-command com-mark-page ((count 'integer :prompt "Move how many pages")
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

(define-named-command com-count-lines-page ()
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

(define-named-command com-count-lines-region ()
  (let*  ((pane (current-window))
	  (point (point pane))
	  (mark (mark pane))
	  (lines (number-of-lines-in-region point mark))
	  (chars (abs (- (offset point) (offset mark)))))
    (display-message "Region has ~D line~:P, ~D character~:P." lines chars)))

(define-named-command com-what-cursor-position ()
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

(define-named-command com-eval-expression ((insertp 'boolean :prompt "Insert?"))
  (let* ((*package* (find-package :climacs-gui))
	 (string (handler-case (accept 'string :prompt "Eval")
		   (error () (progn (beep)
				    (display-message "Empty string")
				    (return-from com-eval-expression nil)))))
	 (result (format nil "~a"
			 (handler-case (eval (read-from-string string))
			   (error (condition) (progn (beep)
						     (display-message "~a" condition)
						     (return-from com-eval-expression nil)))))))
    (if insertp
	(insert-sequence (point (current-window)) result)
	(display-message result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commenting

;;; figure out how to make commands without key bindings accept numeric arguments. 
(define-named-command com-comment-region ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (comment-region syntax point mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; For testing purposes

(define-named-command com-reset-profile ()
  #+sbcl (sb-profile:reset)
  #-sbcl nil)

(define-named-command com-report-profile ()
  #+sbcl (sb-profile:report)
  #-sbcl nil)

(define-named-command com-recompile ()
  (asdf:operate 'asdf:load-op :climacs))

(define-named-command com-backward-expression ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-expression point syntax))
	(loop repeat (- count) do (forward-expression point syntax)))))

(define-named-command com-forward-expression ((count 'integer :prompt "Number of expresssions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-expression point syntax))
	(loop repeat (- count) do (backward-expression point syntax)))))

(define-named-command com-mark-expression ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-expression)
      (setf (offset mark) (offset point)))
    (if (plusp count)
	(loop repeat count do (forward-expression mark syntax))
	(loop repeat (- count) do (backward-expression mark syntax)))))

(define-named-command com-kill-expression ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-expression mark syntax))
	(loop repeat (- count) do (backward-expression mark syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence mark point))
    (delete-region mark point)))

(define-named-command com-backward-kill-expression
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

(define-named-command com-forward-list ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	 (loop repeat count do (forward-list point syntax))
	 (loop repeat (- count) do (backward-list point syntax)))))

(define-named-command com-backward-list ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-list point syntax))
	(loop repeat (- count) do (forward-list point syntax)))))

(define-named-command com-down-list ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (down-list point syntax))
	(loop repeat (- count) do (backward-down-list point syntax)))))

(define-named-command com-backward-down-list ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-down-list point syntax))
	(loop repeat (- count) do (down-list point syntax)))))

(define-named-command com-backward-up-list ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-up-list point syntax))
	(loop repeat (- count) do (up-list point syntax)))))

(define-named-command com-up-list ((count 'integer :prompt "Number of lists"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (up-list point syntax))
	(loop repeat (- count) do (backward-up-list point syntax)))))

(define-named-command com-eval-defun ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (eval-defun point syntax)))

(define-named-command com-beginning-of-definition ((count 'integer :prompt "Number of definitions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (beginning-of-definition point syntax))
	(loop repeat (- count) do (end-of-definition point syntax)))))

(define-named-command com-end-of-definition ((count 'integer :prompt "Number of definitions"))
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (end-of-definition point syntax))
	(loop repeat (- count) do (beginning-of-definition point syntax)))))

(define-named-command com-mark-definition ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-definition)
      (beginning-of-definition point syntax)
      (setf (offset mark) (offset point)))
    (end-of-definition mark syntax)))

(define-named-command com-package ()
  (let* ((pane (current-window))
	 (syntax (syntax (buffer pane)))
	 (package (climacs-lisp-syntax::package-of syntax)))
    (display-message (format nil "~s" package))))

(define-gesture-name :select-other :pointer-button-press (:left :meta) :unique nil)

(define-presentation-translator lisp-string-to-string
    (climacs-lisp-syntax::lisp-string string global-climacs-table
                  :gesture :select-other
                  :tester-definitive t
                  :menu nil
                  :priority 10)
    (object)
  object)

(define-named-command com-accept-string ()
  (display-message (format nil "~s" (accept 'string))))
 
(define-named-command com-accept-symbol ()
  (display-message (format nil "~s" (accept 'symbol))))	 

(define-named-command com-accept-lisp-string ()
  (display-message (format nil "~s" (accept 'lisp-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dead-escape command tables

(make-command-table 'dead-escape-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "dead-escape"
				:menu 'dead-escape-climacs-table
				:keystroke '(:escape))

(defun dead-escape-set-key (gesture command)
  (add-command-to-command-table command 'dead-escape-climacs-table
				:keystroke gesture :errorp nil))

(defun global-set-key (gesture command)
  (add-command-to-command-table command 'global-climacs-table
				:keystroke gesture :errorp nil)
  (when (and 
	 (listp gesture)
	 (find :meta gesture))
    (dead-escape-set-key (remove :meta gesture)  command)))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (global-set-key (code-char code) `(com-self-insert ,*numeric-argument-marker*)))

(global-set-key #\Newline `(com-self-insert ,*numeric-argument-marker*))
(global-set-key #\Tab 'com-indent-line)
(global-set-key '(#\i :control) 'com-indent-line)
(global-set-key '(#\: :shift :meta) `(com-eval-expression ,*numeric-argument-p*))
(global-set-key '(#\j :control) 'com-newline-and-indent)
(global-set-key '(#\f :control) `(com-forward-object ,*numeric-argument-marker*))
(global-set-key '(#\b :control) `(com-backward-object ,*numeric-argument-marker*))
(global-set-key '(#\a :control) 'com-beginning-of-line)
(global-set-key '(#\e :control) 'com-end-of-line)
(global-set-key '(#\d :control) `(com-delete-object ,*numeric-argument-marker* ,*numeric-argument-p*))
(global-set-key '(#\p :control) `(com-previous-line ,*numeric-argument-marker*))
(global-set-key '(#\l :control) 'com-full-redisplay)
(global-set-key '(#\n :control) `(com-next-line ,*numeric-argument-marker*))
(global-set-key '(#\o :control) `(com-open-line ,*numeric-argument-marker*))
(global-set-key '(#\k :control) `(com-kill-line ,*numeric-argument-marker* ,*numeric-argument-p*))
(global-set-key '(#\t :control) 'com-transpose-objects)
(global-set-key '(#\Space :control) 'com-set-mark)
(global-set-key '(#\y :control) 'com-yank)
(global-set-key '(#\w :control) 'com-kill-region)
(global-set-key '(#\w :control :meta) 'com-append-next-kill)
(global-set-key '(#\e :meta) `(com-forward-sentence ,*numeric-argument-marker*))
(global-set-key '(#\a :meta) `(com-backward-sentence ,*numeric-argument-marker*))
(global-set-key '(#\k :meta) `(com-kill-sentence ,*numeric-argument-marker*))
(global-set-key '(#\@ :meta :control :shift) `(com-mark-expression ,*numeric-argument-marker*))
(global-set-key '(#\f :meta) `(com-forward-word ,*numeric-argument-marker*))
(global-set-key '(#\b :meta) `(com-backward-word ,*numeric-argument-marker*))
(global-set-key '(#\t :meta) 'com-transpose-words)
(global-set-key '(#\u :meta) 'com-upcase-word)
(global-set-key '(#\l :meta) 'com-downcase-word)
(global-set-key '(#\c :meta) 'com-capitalize-word)
(global-set-key '(#\y :meta) 'com-rotate-yank) 
(global-set-key '(#\z :meta) 'com-zap-to-character)
(global-set-key '(#\w :meta) 'com-copy-region)
(global-set-key '(#\v :control) 'com-page-down)
(global-set-key '(#\v :meta) 'com-page-up)
(global-set-key '(#\v :control :meta) 'com-scroll-other-window)
(global-set-key '(#\V :control :meta :shift) 'com-scroll-other-window-up)
(global-set-key '(#\< :shift :meta) 'com-beginning-of-buffer)
(global-set-key '(#\> :shift :meta) 'com-end-of-buffer)
(global-set-key '(#\m :meta) 'com-back-to-indentation)
(global-set-key '(#\\ :meta) `(com-delete-horizontal-space ,*numeric-argument-p*))
(global-set-key '(#\Space :meta) `(com-just-one-space ,*numeric-argument-marker*))
(global-set-key '(#\^ :shift :meta) 'com-delete-indentation)
(global-set-key '(#\q :meta) 'com-fill-paragraph)
(global-set-key '(#\d :meta) `(com-kill-word ,*numeric-argument-marker*))
(global-set-key '(#\Backspace :meta) `(com-backward-kill-word ,*numeric-argument-marker*))
(global-set-key '(#\@ :meta :shift) `(com-mark-word ,*numeric-argument-marker*))
(global-set-key '(#\/ :meta) 'com-dabbrev-expand)
(global-set-key '(#\{ :meta :shift) `(com-backward-paragraph ,*numeric-argument-marker*))
(global-set-key '(#\} :meta :shift) `(com-forward-paragraph ,*numeric-argument-marker*))
(global-set-key '(#\h :meta) `(com-mark-paragraph ,*numeric-argument-marker*))
(global-set-key '(#\s :control) 'com-isearch-mode-forward)
(global-set-key '(#\r :control) 'com-isearch-mode-backward)
(global-set-key '(#\_ :shift :meta) 'com-redo)
(global-set-key '(#\_ :shift :control) 'com-undo)
(global-set-key '(#\% :shift :meta) 'com-query-replace)
(global-set-key '(#\= :meta) 'com-count-lines-region)
(global-set-key '(:up) `(com-previous-line ,*numeric-argument-marker*))
(global-set-key '(:down) `(com-next-line ,*numeric-argument-marker*))
(global-set-key '(:left) `(com-backward-object ,*numeric-argument-marker*))
(global-set-key '(:right) `(com-forward-object ,*numeric-argument-marker*))
(global-set-key '(:left :control) `(com-backward-word ,*numeric-argument-marker*))
(global-set-key '(:right :control) `(com-forward-word ,*numeric-argument-marker*))
(global-set-key '(:home) 'com-beginning-of-line)
(global-set-key '(:end) 'com-end-of-line)
(global-set-key '(:prior) 'com-page-up)
(global-set-key '(:next) 'com-page-down)
(global-set-key '(:home :control) 'com-beginning-of-buffer)
(global-set-key '(:end :control) 'com-end-of-buffer)
(global-set-key #\Rubout `(com-delete-object ,*numeric-argument-marker* ,*numeric-argument-p*))
(global-set-key #\Backspace `(com-backward-delete-object ,*numeric-argument-marker* ,*numeric-argument-p*))

(global-set-key '(:insert) 'com-toggle-overwrite-mode)
(global-set-key '(#\~ :meta :shift) 'com-not-modified)

(global-set-key '(#\b :control :meta) `(com-backward-expression ,*numeric-argument-marker*))
(global-set-key '(#\f :control :meta) `(com-forward-expression ,*numeric-argument-marker*))
(global-set-key '(#\Backspace :control :meta) `(com-backward-kill-expression ,*numeric-argument-marker*))
(global-set-key '(#\k :control :meta) `(com-kill-expression ,*numeric-argument-marker*))
(global-set-key '(#\n :control :meta) `(com-forward-list ,*numeric-argument-marker*))
(global-set-key '(#\p :control :meta) `(com-backward-list ,*numeric-argument-marker*))
(global-set-key '(#\d :control :meta) `(com-down-list ,*numeric-argument-marker*))
(global-set-key '(#\u :control :meta) `(com-backward-up-list ,*numeric-argument-marker*))
(global-set-key '(#\x :control :meta) 'com-eval-defun)
(global-set-key '(#\a :control :meta) `(com-beginning-of-definition ,*numeric-argument-marker*))
(global-set-key '(#\e :control :meta) `(com-end-of-definition ,*numeric-argument-marker*))
(global-set-key '(#\h :control :meta) 'com-mark-definition)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; C-x command table

(make-command-table 'c-x-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "C-x"
				:menu 'c-x-climacs-table
				:keystroke '(#\x :control))

(defun c-x-set-key (gesture command)
  (add-command-to-command-table command 'c-x-climacs-table
				:keystroke gesture :errorp nil))

(c-x-set-key '(#\0) 'com-delete-window)
(c-x-set-key '(#\1) 'com-single-window)
(c-x-set-key '(#\2) 'com-split-window-vertically)
(c-x-set-key '(#\3) 'com-split-window-horizontally)
(c-x-set-key '(#\b) 'com-switch-to-buffer)
(c-x-set-key '(#\f :control) 'com-find-file)
(c-x-set-key '(#\f) `(com-set-fill-column ,*numeric-argument-marker*))
(c-x-set-key '(#\h) 'com-mark-whole-buffer)
(c-x-set-key '(#\i) 'com-insert-file)
(c-x-set-key '(#\k) 'com-kill-buffer)
(c-x-set-key '(#\o) 'com-other-window)
(c-x-set-key '(#\r) 'com-redo)
(c-x-set-key '(#\u) 'com-undo)
(c-x-set-key '(#\]) `(com-forward-page ,*numeric-argument-marker*))
(c-x-set-key '(#\[) `(com-backward-page ,*numeric-argument-marker*))
(c-x-set-key '(#\p :control) `(com-mark-page ,*numeric-argument-marker* ,*numeric-argument-p*))
(c-x-set-key '(#\l) 'com-count-lines-page)
(c-x-set-key '(#\s :control) 'com-save-buffer)
(c-x-set-key '(#\t :control) 'com-transpose-lines)
(c-x-set-key '(#\w :control) 'com-write-buffer)
(c-x-set-key '(#\x :control) 'com-exchange-point-and-mark)
(c-x-set-key '(#\=) 'com-what-cursor-position)
(c-x-set-key '(#\Backspace) `(com-backward-kill-sentence ,*numeric-argument-marker*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Some Unicode stuff

(define-named-command com-insert-charcode ((code 'integer :prompt "Code point"))
  (insert-object (point (current-window)) (code-char code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dead-acute command table

(make-command-table 'dead-acute-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "dead-acute"
				:menu 'dead-acute-climacs-table
				:keystroke '(:dead--acute))

(defun dead-acute-set-key (gesture command)
  (add-command-to-command-table command 'dead-acute-climacs-table
				:keystroke gesture :errorp nil))

(dead-acute-set-key '(#\A) '(com-insert-charcode 193))
(dead-acute-set-key '(#\E) '(com-insert-charcode 201))
(dead-acute-set-key '(#\I) '(com-insert-charcode 205))
(dead-acute-set-key '(#\O) '(com-insert-charcode 211))
(dead-acute-set-key '(#\U) '(com-insert-charcode 218))
(dead-acute-set-key '(#\Y) '(com-insert-charcode 221))
(dead-acute-set-key '(#\a) '(com-insert-charcode 225))
(dead-acute-set-key '(#\e) '(com-insert-charcode 233))
(dead-acute-set-key '(#\i) '(com-insert-charcode 237))
(dead-acute-set-key '(#\o) '(com-insert-charcode 243))
(dead-acute-set-key '(#\u) '(com-insert-charcode 250))
(dead-acute-set-key '(#\y) '(com-insert-charcode 253))
(dead-acute-set-key '(#\C) '(com-insert-charcode 199))
(dead-acute-set-key '(#\c) '(com-insert-charcode 231))
(dead-acute-set-key '(#\x) '(com-insert-charcode 215))
(dead-acute-set-key '(#\-) '(com-insert-charcode 247))
(dead-acute-set-key '(#\T) '(com-insert-charcode 222))
(dead-acute-set-key '(#\t) '(com-insert-charcode 254))
(dead-acute-set-key '(#\s) '(com-insert-charcode 223))
(dead-acute-set-key '(#\Space) '(com-insert-charcode 39))

(make-command-table 'dead-acute-dead-accute-climacs-table :errorp nil)

(add-menu-item-to-command-table 'dead-acute-climacs-table "dead-acute-dead-accute"
				:menu 'dead-acute-dead-accute-climacs-table
				:keystroke '(:dead--acute))

(defun dead-acute-dead-accute-set-key (gesture command)
  (add-command-to-command-table command 'dead-acute-dead-accute-climacs-table
				:keystroke gesture :errorp nil))

(dead-acute-dead-accute-set-key '(#\A) '(com-insert-charcode 197))
(dead-acute-dead-accute-set-key '(#\a) '(com-insert-charcode 229))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dead-grave command table

(make-command-table 'dead-grave-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "dead-grave"
				:menu 'dead-grave-climacs-table
				:keystroke '(:dead--grave))

(defun dead-grave-set-key (gesture command)
  (add-command-to-command-table command 'dead-grave-climacs-table
				:keystroke gesture :errorp nil))

(dead-grave-set-key '(#\A) '(com-insert-charcode 192))
(dead-grave-set-key '(#\E) '(com-insert-charcode 200))
(dead-grave-set-key '(#\I) '(com-insert-charcode 204))
(dead-grave-set-key '(#\O) '(com-insert-charcode 210))
(dead-grave-set-key '(#\U) '(com-insert-charcode 217))
(dead-grave-set-key '(#\a) '(com-insert-charcode 224))
(dead-grave-set-key '(#\e) '(com-insert-charcode 232))
(dead-grave-set-key '(#\i) '(com-insert-charcode 236))
(dead-grave-set-key '(#\o) '(com-insert-charcode 242))
(dead-grave-set-key '(#\u) '(com-insert-charcode 249))
(dead-grave-set-key '(#\Space) '(com-insert-charcode 96))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dead-diaeresis command table

(make-command-table 'dead-diaeresis-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "dead-diaeresis"
				:menu 'dead-diaeresis-climacs-table
				:keystroke '(:dead--diaeresis :shift))

(defun dead-diaeresis-set-key (gesture command)
  (add-command-to-command-table command 'dead-diaeresis-climacs-table
				:keystroke gesture :errorp nil))

(dead-diaeresis-set-key '(#\A) '(com-insert-charcode 196))
(dead-diaeresis-set-key '(#\E) '(com-insert-charcode 203))
(dead-diaeresis-set-key '(#\I) '(com-insert-charcode 207))
(dead-diaeresis-set-key '(#\O) '(com-insert-charcode 214))
(dead-diaeresis-set-key '(#\U) '(com-insert-charcode 220))
(dead-diaeresis-set-key '(#\a) '(com-insert-charcode 228))
(dead-diaeresis-set-key '(#\e) '(com-insert-charcode 235))
(dead-diaeresis-set-key '(#\i) '(com-insert-charcode 239))
(dead-diaeresis-set-key '(#\o) '(com-insert-charcode 246))
(dead-diaeresis-set-key '(#\u) '(com-insert-charcode 252))
(dead-diaeresis-set-key '(#\y) '(com-insert-charcode 255))
(dead-diaeresis-set-key '(#\Space) '(com-insert-charcode 34))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dead-tilde command table

(make-command-table 'dead-tilde-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "dead-tilde"
				:menu 'dead-tilde-climacs-table
				:keystroke '(:dead--tilde :shift))

(defun dead-tilde-set-key (gesture command)
  (add-command-to-command-table command 'dead-tilde-climacs-table
				:keystroke gesture :errorp nil))

(dead-tilde-set-key '(#\A) '(com-insert-charcode 195))
(dead-tilde-set-key '(#\N) '(com-insert-charcode 209))
(dead-tilde-set-key '(#\a) '(com-insert-charcode 227))
(dead-tilde-set-key '(#\n) '(com-insert-charcode 241))
(dead-tilde-set-key '(#\E) '(com-insert-charcode 198))
(dead-tilde-set-key '(#\e) '(com-insert-charcode 230))
(dead-tilde-set-key '(#\D) '(com-insert-charcode 208))
(dead-tilde-set-key '(#\d) '(com-insert-charcode 240))
(dead-tilde-set-key '(#\O) '(com-insert-charcode 216))
(dead-tilde-set-key '(#\o) '(com-insert-charcode 248))
(dead-tilde-set-key '(#\Space) '(com-insert-charcode 126))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dead-circumflex command table

(make-command-table 'dead-circumflex-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "dead-circumflex"
				:menu 'dead-circumflex-climacs-table
				:keystroke '(:dead--circumflex :shift))

(defun dead-circumflex-set-key (gesture command)
  (add-command-to-command-table command 'dead-circumflex-climacs-table
				:keystroke gesture :errorp nil))

(dead-circumflex-set-key '(#\A) '(com-insert-charcode 194))
(dead-circumflex-set-key '(#\E) '(com-insert-charcode 202))
(dead-circumflex-set-key '(#\I) '(com-insert-charcode 206))
(dead-circumflex-set-key '(#\O) '(com-insert-charcode 212))
(dead-circumflex-set-key '(#\U) '(com-insert-charcode 219))
(dead-circumflex-set-key '(#\a) '(com-insert-charcode 226))
(dead-circumflex-set-key '(#\e) '(com-insert-charcode 234))
(dead-circumflex-set-key '(#\i) '(com-insert-charcode 238))
(dead-circumflex-set-key '(#\o) '(com-insert-charcode 244))
(dead-circumflex-set-key '(#\u) '(com-insert-charcode 251))
(dead-circumflex-set-key '(#\Space) '(com-insert-charcode 94))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Isearch command table

(make-command-table 'isearch-climacs-table :errorp nil)

(defun isearch-set-key (gesture command)
  (add-command-to-command-table command 'isearch-climacs-table
                                :keystroke gesture :errorp nil))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (isearch-set-key (code-char code) 'com-isearch-append-char))

(isearch-set-key '(#\Newline) 'com-isearch-exit)
(isearch-set-key '(#\Backspace) 'com-isearch-delete-char)
(isearch-set-key '(#\s :control) 'com-isearch-forward)
(isearch-set-key '(#\r :control) 'com-isearch-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Query replace command table

(make-command-table 'query-replace-climacs-table :errorp nil)

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
;;; C-c command table

(make-command-table 'c-c-climacs-table :errorp nil)

(add-menu-item-to-command-table 'global-climacs-table "C-c"
				:menu 'c-c-climacs-table
				:keystroke '(#\c :control))

(defun c-c-set-key (gesture command)
  (add-command-to-command-table command 'c-c-climacs-table
				:keystroke gesture :errorp nil))

