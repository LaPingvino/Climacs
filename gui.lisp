;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004 by
;;;           Elliott Johnson (ejohnson@fasl.info)

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

(defclass filename-mixin ()
  ((filename :initform nil :accessor filename)))

(defclass climacs-buffer (standard-buffer abbrev-mixin filename-mixin name-mixin)
  ((needs-saving :initform nil :accessor needs-saving))
  (:default-initargs :name "*scratch*"))


(defclass climacs-pane (application-pane)
  ((buffer :initform (make-instance 'climacs-buffer) :accessor buffer)
   (point :initform nil :initarg :point :reader point)
   (syntax :initarg :syntax :accessor syntax)
   (mark :initform nil :initarg :mark :reader mark)
   ;; allows a certain number of commands to have some minimal memory
   (previous-command :initform nil :accessor previous-command)
   ;; for next-line and previous-line commands
   (goal-column :initform nil)
   ;; for dynamic abbrev expansion
   (original-prefix :initform nil)
   (prefix-start-offset :initform nil)
   (dabbrev-expansion-mark :initform nil)))

(defmethod initialize-instance :after ((pane climacs-pane) &rest args)
  (declare (ignore args))
  (with-slots (buffer point syntax mark) pane
     (when (null point)
       (setf point (make-instance 'standard-right-sticky-mark
		      :buffer buffer)))
     (when (null mark)
       (setf mark (make-instance 'standard-right-sticky-mark
		      :buffer buffer)))
     (setf syntax (make-instance 'texinfo-syntax :pane pane))))

(defclass minibuffer-pane (application-pane) ())

(defmethod stream-accept :before ((pane minibuffer-pane) type &rest args)
  (declare (ignore type args))
  (window-clear pane))

(define-application-frame climacs ()
  ((win :reader win))
  (:panes
   (win (make-pane 'climacs-pane
		   :width 900 :height 400
		   :name 'win
		   :incremental-redisplay t
		   :display-function 'display-win))
   
   (info :application
	 :width 900 :height 20 :max-height 20
	 :name 'info :background +light-gray+
	 :scroll-bars nil
	 :borders nil
	 :incremental-redisplay t
	 :display-function 'display-info)
   (int (make-pane 'minibuffer-pane
		   :width 900 :height 20 :max-height 20 :min-height 20
		   :scroll-bars nil)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 (scrolling (:width 900 :height 400) win)
	 info
	 int))
   (without-interactor
    (vertically (:scroll-bars nil)
      (scrolling (:width 900 :height 400) win)
      info)))
  (:top-level (climacs-top-level)))

(defmethod redisplay-frame-panes :after ((frame climacs) &rest args)
  (declare (ignore args))
  (clear-modify (buffer (win frame))))

(defun climacs ()
  "Starts up a climacs session"
  (let ((frame (make-application-frame 'climacs)))
    (run-frame-top-level frame)))

(defun display-message (format-string &rest format-args)
  (apply #'format *standard-input* format-string format-args))

(defvar *overwrite-mode* nil)

(defun display-info (frame pane)
  (let* ((win (win frame))
	 (buf (buffer win))
	 (name-info (format nil "   ~a   ~a   Syntax: ~a ~a"
			    (if (needs-saving buf) "**" "--")
			    (name buf)
			    (name (syntax win))
			    (if *overwrite-mode*
				"Ovwrt"
				""))))
    (princ name-info pane)))

(defun display-win (frame pane)
  "The display function used by the climacs application frame."
  (declare (ignore frame))
  (redisplay-pane pane))

(defun find-gestures (gestures start-table)
  (loop with table = (find-command-table start-table)
	for (gesture . rest) on gestures
	for item = (find-keystroke-item  gesture table :errorp nil)
	while item
	do (if (eq (command-menu-item-type item) :command)
	       (return (if (null rest) item nil))
	       (setf table (command-menu-item-value item)))
	finally (return item)))

(defvar *kill-ring* (make-instance 'kill-ring :max-size 7))
(defparameter *current-gesture* nil)

(defun meta-digit (gesture)
  (position gesture
	    '((#\0 :meta) (#\1 :meta) (#\2 :meta) (#\3 :meta) (#\4 :meta)
	      (#\5 :meta) (#\6 :meta) (#\7 :meta) (#\8 :meta) (#\9 :meta))
	    :test #'event-matches-gesture-name-p))

(defun climacs-read-gesture ()
  (loop for gesture = (read-gesture :stream *standard-input*)
	when (event-matches-gesture-name-p gesture '(#\g :control))
	  do (throw 'outer-loop nil)
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
	finally (return gesture)))	  

(defun read-numeric-argument (&key (stream *standard-input*))
  (let ((gesture (climacs-read-gesture)))
    (cond ((event-matches-gesture-name-p gesture '(:keyboard #\u 512)) ; FIXME
	   (let ((numarg 4))
	     (loop for gesture = (climacs-read-gesture)
		   while (event-matches-gesture-name-p gesture '(:keyboard #\u 512)) ; FIXME
		   do (setf numarg (* 4 numarg))
		   finally (unread-gesture gesture :stream stream))
	     (let ((gesture (climacs-read-gesture)))
	       (cond ((and (characterp gesture)
			   (digit-char-p gesture 10))
		      (setf numarg (- (char-code gesture) (char-code #\0)))
		      (loop for gesture = (climacs-read-gesture)
			    while (and (characterp gesture)
				       (digit-char-p gesture 10))
			    do (setf numarg (+ (* 10 numarg)
					       (- (char-code gesture) (char-code #\0))))
			    finally (unread-gesture gesture :stream stream)
				    (return (values numarg t))))
		     (t
		      (unread-gesture gesture :stream stream)
		      (values numarg t))))))
	  ((meta-digit gesture)
	   (let ((numarg (meta-digit gesture)))
	     (loop for gesture = (climacs-read-gesture)
		   while (meta-digit gesture)
		   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
		   finally (unread-gesture gesture :stream stream)
			   (return (values numarg t)))))
	  (t (unread-gesture gesture :stream stream)
	     (values 1 nil)))))

(defun climacs-top-level (frame &key
			  command-parser command-unparser 
			  partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (setf (slot-value frame 'win) (find-pane-named frame 'win))
  (let ((*standard-output* (find-pane-named frame 'win))
	(*standard-input* (find-pane-named frame 'int))
	(*print-pretty* nil)
	(*abort-gestures* nil))
    (redisplay-frame-panes frame :force-p t)
    (loop (catch 'outer-loop
	    (loop for gestures = '()
		  for numarg = (read-numeric-argument :stream *standard-input*)
		  do (loop (setf *current-gesture* (climacs-read-gesture))
			   (setf gestures (nconc gestures (list *current-gesture*)))
			   (let ((item (find-gestures gestures 'global-climacs-table)))
			     (cond ((not item)
				    (beep) (return))
				   ((eq (command-menu-item-type item) :command)
				    (let ((command (command-menu-item-value item)))
				      (unless (consp command)
					(setf command (list command)))
				      (setf command (substitute-numeric-argument-marker command numarg))
				      (handler-case 
					  (execute-frame-command frame command)
					(error (condition)
					  (beep)
					  (format *error-output* "~a~%" condition)))
				      (setf (previous-command *standard-output*)
					    (if (consp command)
						(car command)
						command))
				      (return)))
				   (t nil))))
		     (let ((buffer (buffer (win frame))))
		       (when (modified-p buffer)
			 (setf (needs-saving buffer) t)))
		     (redisplay-frame-panes frame)))
	  (beep)
	  (let ((buffer (buffer (win frame))))
	    (when (modified-p buffer)
	      (setf (needs-saving buffer) t)))
	  (redisplay-frame-panes frame))))

(defmacro define-named-command (command-name args &body body)
  `(define-climacs-command ,(if (listp command-name)
				`(,@command-name :name t)
				`(,command-name :name t)) ,args ,@body))

(define-named-command (com-quit) ()
  (frame-exit *application-frame*))

(define-named-command com-toggle-overwrite-mode ()
  (setf *overwrite-mode* (not *overwrite-mode*)))

(define-command com-self-insert ()
  (let ((point (point (win *application-frame*))))
    (unless (constituentp *current-gesture*)
      (possibly-expand-abbrev point))
    (if (and *overwrite-mode* (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point *current-gesture*))
	(insert-object point *current-gesture*))))

(define-named-command com-beginning-of-line ()
  (beginning-of-line (point (win *application-frame*))))

(define-named-command com-end-of-line ()
  (end-of-line (point (win *application-frame*))))

(define-named-command com-delete-object ((count 'integer :prompt "Number of Objects"))
  (delete-range (point (win *application-frame*)) count))

(define-named-command com-backward-delete-object ((count 'integer :prompt "Number of Objects"))
  (delete-range (point (win *application-frame*)) (- count)))

(define-named-command com-transpose-objects ()
  (let* ((point (point (win *application-frame*))))
    (unless (beginning-of-buffer-p point)
      (when (end-of-line-p point)
       (backward-object point))
       (let ((object (object-after point)))
        (delete-range point)
       (backward-object point)
       (insert-object point object)
       (forward-object point)))))

(define-named-command com-backward-object ((count 'integer :prompt "Number of Objects"))
  (backward-object (point (win *application-frame*)) count))

(define-named-command com-forward-object ((count 'integer :prompt "Number of Objects"))
  (forward-object (point (win *application-frame*)) count))

(define-named-command com-transpose-words ()
  (let* ((point (point (win *application-frame*))))
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
  (let ((point (point (win *application-frame*))))
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
      (when (progn (end-of-line point)
                   (end-of-buffer-p point))
        (insert-object point #\Newline))
      (next-line point)
      (insert-sequence point line)
      (insert-object point #\Newline))))

(define-named-command com-previous-line ()
  (let* ((win (win *application-frame*))
	 (point (point win)))
    (unless (or (eq (previous-command win) 'com-previous-line)
		(eq (previous-command win) 'com-next-line))
      (setf (slot-value win 'goal-column) (column-number point)))
    (previous-line point (slot-value win 'goal-column))))

(define-named-command com-next-line ()
  (let* ((win (win *application-frame*))
	 (point (point win)))
    (unless (or (eq (previous-command win) 'com-previous-line)
		(eq (previous-command win) 'com-next-line))
      (setf (slot-value win 'goal-column) (column-number point)))
    (next-line point (slot-value win 'goal-column))))

(define-named-command com-open-line ()
  (open-line (point (win *application-frame*))))

(define-named-command com-kill-line ()
  (let* ((pane (win *application-frame*))
	 (point (point pane))
         (mark (offset point)))
    (cond ((end-of-buffer-p point) nil)
	  ((end-of-line-p point)(forward-object point))
	  (t
	   (end-of-line point)
	   (cond ((beginning-of-buffer-p point) nil)
		 ((beginning-of-line-p point)(forward-object point)))))
    (if (eq (previous-command pane) 'com-kill-line)
	(kill-ring-concatenating-push *kill-ring*
				      (region-to-sequence mark point))
        (kill-ring-standard-push *kill-ring*
			       (region-to-sequence mark point)))
    (delete-region mark point)))

(define-named-command com-forward-word ()
  (forward-word (point (win *application-frame*))))

(define-named-command com-backward-word ()
  (backward-word (point (win *application-frame*))))

(define-named-command com-delete-word ()
  (delete-word (point (win *application-frame*))))

(define-named-command com-backward-delete-word ()
  (backward-delete-word (point (win *application-frame*))))

(define-named-command com-upcase-word ()
  (upcase-word (point (win *application-frame*))))

(define-named-command com-downcase-word ()
  (downcase-word (point (win *application-frame*))))

(define-named-command com-capitalize-word ()
  (capitalize-word (point (win *application-frame*))))

(define-named-command com-toggle-layout ()
  (setf (frame-current-layout *application-frame*)
	(if (eq (frame-current-layout *application-frame*) 'default)
	    'without-interactor
	    'default)))

(define-command com-extended-command ()
  (let ((item (accept 'command :prompt "Extended Command")))
    (execute-frame-command *application-frame* item)))

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
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success))
    (or pathname string)))

(defun pathname-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
		   "." (pathname-type pathname))))

(define-named-command com-find-file ()
  (let ((filename (accept 'completable-pathname
			  :prompt "Find File")))
    (with-slots (buffer point syntax) (win *application-frame*)
       (setf buffer (make-instance 'climacs-buffer)
	     point (make-instance 'standard-right-sticky-mark :buffer buffer)
	     syntax (make-instance 'texinfo-syntax :pane (win *application-frame*)))
       (with-open-file (stream filename :direction :input :if-does-not-exist :create)
	 (input-from-stream stream buffer 0))
       (setf (filename buffer) filename
	     (name buffer) (pathname-filename filename)
	     (needs-saving buffer) nil)
       (beginning-of-buffer point)
       ;; this one is needed so that the buffer modification protocol
       ;; resets the low and high marks after redisplay
       (redisplay-frame-panes *application-frame*))))

(define-named-command com-save-buffer ()
  (let* ((buffer (buffer (win *application-frame*)))
	 (filename (or (filename buffer)
		       (accept 'completable-pathname
			       :prompt "Save Buffer to File"))))
    (if (or (null (filename buffer))
	    (needs-saving buffer))
	(progn (with-open-file (stream filename :direction :output :if-exists :supersede)
		 (output-to-stream stream buffer 0 (size buffer)))
	       (setf (filename buffer) filename
		     (name buffer) (pathname-filename filename))
	       (display-message "Wrote: ~a" (filename buffer)))
	(display-message "No changes need to be saved from ~a" (name buffer)))
    (setf (needs-saving buffer) nil)))

(define-named-command com-write-buffer ()
  (let ((filename (accept 'completable-pathname
			  :prompt "Write Buffer to File"))
	(buffer (buffer (win *application-frame*))))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (output-to-stream stream buffer 0 (size buffer)))
    (setf (filename buffer) filename
	  (name buffer) (pathname-filename filename)
	  (needs-saving buffer) nil)
    (display-message "Wrote: ~a" (filename buffer))))

(define-named-command com-beginning-of-buffer ()
  (beginning-of-buffer (point (win *application-frame*))))

(define-named-command com-page-down ()
  (let ((pane (win *application-frame*)))
    (page-down pane (syntax pane))))

(define-named-command com-page-up ()
  (let ((pane (win *application-frame*)))
    (page-up pane (syntax pane))))

(define-named-command com-end-of-buffer ()
  (end-of-buffer (point (win *application-frame*))))

(define-named-command com-back-to-indentation ()
  (let ((point (point (win *application-frame*))))
    (beginning-of-line point)
    (loop until (end-of-line-p point)
	  while (whitespacep (object-after point))
	  do (incf (offset point)))))

(define-named-command com-goto-position ()
  (setf (offset (point (win *application-frame*)))
	(accept 'integer :prompt "Goto Position")))

(define-named-command com-goto-line ()
  (loop with mark = (make-instance 'standard-right-sticky-mark
		       :buffer (buffer (win *application-frame*)))
	do (end-of-line mark)
	until (end-of-buffer-p mark)
	repeat (accept 'integer :prompt "Goto Line")
	do (incf (offset mark))
	   (end-of-line mark)
	finally (beginning-of-line mark)
		(setf (offset (point (win *application-frame*)))
		      (offset mark))))

(define-named-command com-browse-url ()
  (accept 'url :prompt "Browse URL"))

(define-named-command com-set-mark ()
  (with-slots (point mark) (win *application-frame*)
     (setf mark (clone-mark point))))

(define-named-command com-exchange-point-and-mark ()
  (with-slots (point mark) (win *application-frame*)
     (psetf (offset mark) (offset point)
	    (offset point) (offset mark))))

(define-named-command com-set-syntax ()
  (setf (syntax (win *application-frame*))
	(make-instance (accept 'syntax :prompt "Set Syntax")
	   :pane (win *application-frame*))))

;;;;;;;;;;;;;;;;;;;;
;; Kill ring commands

;; Copies an element from a kill-ring to a buffer at the given offset
(define-named-command com-yank ()
  (insert-sequence (point (win *application-frame*)) (kill-ring-yank *kill-ring*)))

;; Destructively cut a given buffer region into the kill-ring
(define-named-command com-cut-out ()
  (with-slots (point mark)(win *application-frame*)
     (cond ((< (offset mark)(offset point))
	    (kill-ring-standard-push *kill-ring* (region-to-sequence mark point))
	    (delete-region (offset mark) point))
	   (t
	    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
	    (delete-region (offset point) mark)))))

;; Non destructively copies in buffer region to the kill ring
(define-named-command com-copy-out ()
  (with-slots (point mark)(win *application-frame*)
     (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))))


(define-named-command com-rotate-yank ()
  (let* ((pane (win *application-frame*))
	 (point (point pane))
	 (last-yank (kill-ring-yank *kill-ring*)))
    (if (eq (previous-command pane)
	    'com-rotate-yank)
	(progn
	  (delete-range point (* -1 (length last-yank)))
	  (rotate-yank-position *kill-ring*)))
    (insert-sequence point (kill-ring-yank *kill-ring*))))

(define-named-command com-resize-kill-ring ()
  (let ((size (accept 'integer :prompt "New kill ring size")))
    (setf (kill-ring-max-size *kill-ring*) size)))

(define-named-command com-search-forward ()
  (search-forward (point (win *application-frame*))
		  (accept 'string :prompt "Search Forward")
		  :test (lambda (a b)
			  (and (characterp b) (char-equal a b)))))

(define-named-command com-search-backward ()
  (search-backward (point (win *application-frame*))
		   (accept 'string :prompt "Search Backward")
		   :test (lambda (a b)
			   (and (characterp b) (char-equal a b)))))

(define-named-command com-dabbrev-expand ()
  (let* ((win (win *application-frame*))
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
(global-set-key #\tab 'com-self-insert)
(global-set-key '(#\f :control) `(com-forward-object ,*numeric-argument-marker*))
(global-set-key '(#\b :control) `(com-backward-object ,*numeric-argument-marker*))
(global-set-key '(#\a :control) 'com-beginning-of-line)
(global-set-key '(#\e :control) 'com-end-of-line)
(global-set-key '(#\d :control) `(com-delete-object ,*numeric-argument-marker*))
(global-set-key '(#\p :control) 'com-previous-line)
(global-set-key '(#\n :control) 'com-next-line)
(global-set-key '(#\o :control) 'com-open-line)
(global-set-key '(#\k :control) 'com-kill-line)
(global-set-key '(#\t :control) 'com-transpose-objects)
(global-set-key '(#\Space :control) 'com-set-mark)
(global-set-key '(#\y :control) 'com-yank)
(global-set-key '(#\w :control) 'com-cut-out)
(global-set-key '(#\f :meta) 'com-forward-word)
(global-set-key '(#\b :meta) 'com-backward-word)
(global-set-key '(#\t :meta) 'com-transpose-words)
(global-set-key '(#\u :meta) 'com-upcase-word)
(global-set-key '(#\l :meta) 'com-downcase-word)
(global-set-key '(#\c :meta) 'com-capitalize-word)
(global-set-key '(#\x :meta) 'com-extended-command)
(global-set-key '(#\y :meta) 'com-rotate-yank) 
(global-set-key '(#\w :meta) 'com-copy-out)
(global-set-key '(#\v :control) 'com-page-down)
(global-set-key '(#\v :meta) 'com-page-up)
(global-set-key '(#\< :shift :meta) 'com-beginning-of-buffer)
(global-set-key '(#\> :shift :meta) 'com-end-of-buffer)
(global-set-key '(#\m :meta) 'com-back-to-indentation)
(global-set-key '(#\d :meta) 'com-delete-word)
(global-set-key '(#\Backspace :meta) 'com-backward-delete-word)
(global-set-key '(#\/ :meta) 'com-dabbrev-expand)

(global-set-key '(:up) 'com-previous-line)
(global-set-key '(:down) 'com-next-line)
(global-set-key '(:left) `(com-backward-object ,*numeric-argument-marker*))
(global-set-key '(:right) `(com-forward-object *numeric-argument-marker*))
(global-set-key '(:left :control) 'com-backward-word)
(global-set-key '(:right :control) 'com-forward-word)
(global-set-key '(:home) 'com-beginning-of-line)
(global-set-key '(:end) 'com-end-of-line)
(global-set-key '(:prior) 'com-page-up)
(global-set-key '(:next) 'com-page-down)
(global-set-key '(:home :control) 'com-beginning-of-buffer)
(global-set-key '(:end :control) 'com-end-of-buffer)
(global-set-key #\Rubout `(com-delete-object ,*numeric-argument-marker*))
(global-set-key #\Backspace `(com-backward-delete-object ,*numeric-argument-marker*))

(global-set-key '(:insert) 'com-toggle-overwrite-mode)

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

(c-x-set-key '(#\c :control) 'com-quit)
(c-x-set-key '(#\f :control) 'com-find-file)
(c-x-set-key '(#\s :control) 'com-save-buffer)
(c-x-set-key '(#\t :control) 'com-transpose-lines)
(c-x-set-key '(#\w :control) 'com-write-buffer)
(c-x-set-key '(#\x :control) 'com-exchange-point-and-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Some Unicode stuff

(define-named-command com-insert-charcode ((code 'integer :prompt "Code point"))
  (insert-object (point (win *application-frame*)) (code-char code)))

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
