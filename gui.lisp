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

(defclass extended-pane (climacs-pane)
  (;; allows a certain number of commands to have some minimal memory
   (previous-command :initform nil :accessor previous-command)
   ;; for next-line and previous-line commands
   (goal-column :initform nil)
   ;; for dynamic abbrev expansion
   (original-prefix :initform nil)
   (prefix-start-offset :initform nil)
   (dabbrev-expansion-mark :initform nil)
   (overwrite-mode :initform nil)))

(defclass info-pane (application-pane)
  ((climacs-pane :initarg :climacs-pane)))

(defclass minibuffer-pane (application-pane) ())

(defmethod stream-accept :before ((pane minibuffer-pane) type &rest args)
  (declare (ignore type args))
  (window-clear pane))

(define-application-frame climacs ()
  ((windows :accessor windows)
   (buffers :initform '() :accessor buffers)
   (recordingp :initform nil :accessor recordingp)
   (executingp :initform nil :accessor executingp)
   (recorded-keys :initform '() :accessor recorded-keys)
   (remaining-keys :initform '() :accessor remaining-keys))
  (:panes
   (win (let* ((extended-pane 
		(make-pane 'extended-pane
			   :width 900 :height 400
			   :name 'bla
			   :end-of-line-action :scroll
			   :incremental-redisplay t
			   :display-function 'display-win))
	       (info-pane
		(make-pane 'info-pane
			   :climacs-pane extended-pane
			   :width 900 :height 20 :max-height 20 :min-height 20
			   ::background +gray85+
			   :scroll-bars nil
			   :borders nil
			   :incremental-redisplay t
			   :display-function 'display-info)))
	  (vertically ()
	    (scrolling ()
	      extended-pane)
	    info-pane)))
   (int (make-pane 'minibuffer-pane
		   :width 900 :height 20 :max-height 20 :min-height 20
		   :display-function 'display-minibuffer
		   :scroll-bars nil)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 win
	 int)))
  (:top-level (climacs-top-level)))

(defparameter *message* nil)

(defun display-message (format-string &rest format-args)
  (setf *message* 
	(apply #'format nil format-string format-args)))

(defun display-minibuffer (frame pane)
  (declare (ignore frame))
  (unless (null *message*)
    (princ *message* pane)
    (setf *message* nil)))

(defmacro current-window () ; shouldn't this be an inlined function? --amb
  `(car (windows *application-frame*)))

(defmethod execute-frame-command :around ((frame climacs) command)
  (declare (ignore command))
  (with-undo ((buffer (current-window)))
    (call-next-method)))

(defmethod redisplay-frame-panes :around ((frame climacs) &rest args)
  (declare (ignore args))
  (let ((buffers (remove-duplicates (mapcar #'buffer (windows frame)))))
    (loop for buffer in buffers
	  do (update-syntax buffer (syntax buffer)))
    (call-next-method)
    (loop for buffer in buffers
	  do (clear-modify buffer))))

(defun climacs ()
  "Starts up a climacs session"
  (let ((frame (make-application-frame 'climacs)))
    (run-frame-top-level frame)))

(defun display-info (frame pane)
  (declare (ignore frame))
  (with-slots (climacs-pane) pane
     (let* ((buf (buffer climacs-pane))
	    (name-info (format nil "   ~a   ~a   Syntax: ~a~a~a~a    ~a"
			       (if (needs-saving buf) "**" "--")
			       (name buf)
			       (name (syntax buf))
			       (if (slot-value climacs-pane 'overwrite-mode)
				   " Ovwrt"
				   "")
                               (if (auto-fill-mode climacs-pane)
                                   " Fill"
                                   "")
                               (if (isearch-mode climacs-pane)
                                   " Isearch"
                                   "")
			       (if (recordingp *application-frame*)
				   "Def"
				   ""))))
       (princ name-info pane))))

(defun display-win (frame pane)
  "The display function used by the climacs application frame."
  (declare (ignore frame))
  (redisplay-pane pane (eq pane (car (windows *application-frame*)))))

(defmethod handle-repaint :before ((pane extended-pane) region)
  (declare (ignore region))
  (redisplay-frame-panes *application-frame*))

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
  (unless (null (remaining-keys *application-frame*))
    (return-from climacs-read-gesture
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

(defun climacs-unread-gesture (gesture stream)
  (cond ((recordingp *application-frame*)
	 (pop (recorded-keys *application-frame*))
	 (unread-gesture gesture :stream stream))
	((executingp *application-frame*)
	 (push gesture (remaining-keys *application-frame*)))
	(t 
	 (unread-gesture gesture :stream stream))))

(defun read-numeric-argument (&key (stream *standard-input*))
  (let ((gesture (climacs-read-gesture)))
    (cond ((event-matches-gesture-name-p gesture '(:keyboard #\u 512)) ; FIXME
	   (let ((numarg 4))
	     (loop for gesture = (climacs-read-gesture)
		   while (event-matches-gesture-name-p gesture '(:keyboard #\u 512)) ; FIXME
		   do (setf numarg (* 4 numarg))
		   finally (climacs-unread-gesture gesture stream))
	     (let ((gesture (climacs-read-gesture)))
	       (cond ((and (characterp gesture)
			   (digit-char-p gesture 10))
		      (setf numarg (- (char-code gesture) (char-code #\0)))
		      (loop for gesture = (climacs-read-gesture)
			    while (and (characterp gesture)
				       (digit-char-p gesture 10))
			    do (setf numarg (+ (* 10 numarg)
					       (- (char-code gesture) (char-code #\0))))
			    finally (climacs-unread-gesture gesture stream)
				    (return (values numarg t))))
		     (t
		      (climacs-unread-gesture gesture stream)
		      (values numarg t))))))
	  ((meta-digit gesture)
	   (let ((numarg (meta-digit gesture)))
	     (loop for gesture = (climacs-read-gesture)
		   while (meta-digit gesture)
		   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
		   finally (climacs-unread-gesture gesture stream)
			   (return (values numarg t)))))
	  (t (climacs-unread-gesture gesture stream)
	     (values 1 nil)))))

;;; we know the vbox pane has a scroller pane and an info
;;; pane in it.  The scroller pane has a viewport in it,
;;; and the viewport contains the climacs-pane as its only child.
(defun find-climacs-pane (vbox)
  (first (sheet-children
	  (find-if-not (lambda (pane) (typep pane 'scroll-bar-pane))
		       (sheet-children
			(find-if (lambda (pane) (typep pane 'scroller-pane))
				 (sheet-children vbox)))))))

(defvar *numeric-argument-p* (list nil))

(defun substitute-numeric-argument-p (command numargp)
  (substitute numargp *numeric-argument-p* command :test #'eq))

(defun climacs-top-level (frame &key
			  command-parser command-unparser 
			  partial-command-parser prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (with-slots (windows) frame
     (setf windows (list (find-climacs-pane (find-pane-named frame 'win))))
     (push (buffer (car windows)) (buffers frame))
     (let ((*standard-output* (car windows))
	   (*standard-input* (find-pane-named frame 'int))
	   (*print-pretty* nil)
	   (*abort-gestures* '((:keyboard #\g 512))))
       (redisplay-frame-panes frame :force-p t)
       (loop (handler-case 
		 (loop for gestures = '()
		       do (multiple-value-bind (numarg numargp)
			      (read-numeric-argument :stream *standard-input*)
			    (loop (setf *current-gesture* (climacs-read-gesture))
				  (setf gestures (nconc gestures (list *current-gesture*)))
				  (let ((item (find-gestures gestures 'global-climacs-table)))
				    (cond ((not item)
					   (beep) (return))
					  ((eq (command-menu-item-type item) :command)
					   (let ((command (command-menu-item-value item)))
					     (unless (consp command)
					       (setf command (list command)))
					     (setf command (substitute-numeric-argument-marker command numarg))
					     (setf command (substitute-numeric-argument-p command numargp))
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
			    (let ((buffer (buffer (current-window))))
			      (when (modified-p buffer)
				(setf (needs-saving buffer) t)))
			    (when (null (remaining-keys *application-frame*))
			      (setf (executingp *application-frame*) nil)
			      (redisplay-frame-panes frame))))
	       (abort-gesture () nil))
	     (beep)
	     (let ((buffer (buffer (current-window))))
	       (when (modified-p buffer)
		 (setf (needs-saving buffer) t)))
	     (when (null (remaining-keys *application-frame*))
	       (setf (executingp *application-frame*) nil)
	       (redisplay-frame-panes frame))))))

(defmacro simple-command-loop (command-table loop-condition end-clauses)
  (let ((gesture (gensym))
        (item (gensym))
        (command (gensym))
        (condition (gensym)))
    `(progn 
       (redisplay-frame-panes *application-frame*)
       (loop while ,loop-condition
             as ,gesture = (climacs-read-gesture)
             as ,item = (find-gestures (list ,gesture) ,command-table)
             do (cond ((and ,item (eq (command-menu-item-type ,item) :command))
                       (setf *current-gesture* ,gesture)
                       (let ((,command (command-menu-item-value ,item)))
                         (unless (consp ,command)
                           (setf ,command (list ,command)))
                         (handler-case 
                             (execute-frame-command *application-frame*
                                                    ,command)
                           (error (,condition)
                             (beep)
                             (format *error-output* "~a~%" ,condition)))))
                      (t
                       (unread-gesture ,gesture)
                       ,@end-clauses))
             (redisplay-frame-panes *application-frame*)))))

(defun region-limits (pane)
  (if (mark< (mark pane) (point pane))
      (values (mark pane) (point pane))
      (values (point pane) (mark pane))))

(defmacro define-named-command (command-name args &body body)
  `(define-climacs-command ,(if (listp command-name)
				`(,@command-name :name t)
				`(,command-name :name t)) ,args ,@body))

(define-named-command com-toggle-overwrite-mode ()
  (with-slots (overwrite-mode) (current-window)
    (setf overwrite-mode (not overwrite-mode))))

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

(define-command com-self-insert ()
  (insert-character *current-gesture*))

(define-named-command com-beginning-of-line ()
  (beginning-of-line (point (current-window))))

(define-named-command com-end-of-line ()
  (end-of-line (point (current-window))))

(define-named-command com-delete-object ((count 'integer :prompt "Number of Objects"))
  (delete-range (point (current-window)) count))

(define-named-command com-backward-delete-object ((count 'integer :prompt "Number of Objects"))
  (delete-range (point (current-window)) (- count)))

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
    (previous-line point (slot-value win 'goal-column) numarg)))

(define-named-command com-next-line ((numarg 'integer :prompt "How many lines?"))
  (let* ((win (current-window))
	 (point (point win)))
    (unless (or (eq (previous-command win) 'com-previous-line)
		(eq (previous-command win) 'com-next-line))
      (setf (slot-value win 'goal-column) (column-number point)))
    (next-line point (slot-value win 'goal-column) numarg)))

(define-named-command com-open-line ((numarg 'integer :prompt "How many lines?"))
  (open-line (point (current-window)) numarg))

(define-named-command com-kill-line ((numarg 'integer :prompt "Kill how many lines?")
				     (numargp 'boolean :prompt "Kill entire lines?"))
  (let* ((pane (current-window))
	 (point (point pane))
         (mark (offset point)))
    (cond ((or numargp (> numarg 1))
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
  (forward-word (point (current-window)) count))

(define-named-command com-backward-word ((count 'integer :prompt "Number of words"))
  (backward-word (point (current-window)) count))

(define-named-command com-delete-word ((count 'integer :prompt "Number of words"))
  (delete-word (point (current-window)) count))

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
    (multiple-value-bind (start end) (region-limits pane)
      (tabify-region start end (tab-space-count (stream-default-view pane))))))

(define-named-command com-untabify-region ()
  (let ((pane (current-window)))
    (multiple-value-bind (start end) (region-limits pane)
      (untabify-region start end (tab-space-count (stream-default-view pane))))))

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
      (beginning-of-paragraph begin-mark syntax))
    (unless (eql (object-after end-mark) #\Newline)
      (end-of-paragraph end-mark syntax))
    (do-buffer-region (object offset buffer
                       (offset begin-mark) (offset end-mark))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (let ((point-backup (clone-mark point)))
      (setf (offset point) (offset end-mark))
      (possibly-fill-line)
      (setf (offset point) (offset point-backup)))))

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
		(namestring #+sbcl *default-pathname-defaults*
                            #+cmu (ext:default-directory)
                            #-(or sbcl cmu) *default-pathname-defaults*)))
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
			  :prompt "Find File"))
	(buffer (make-instance 'climacs-buffer))
	(pane (current-window)))
    (push buffer (buffers *application-frame*))
    (setf (buffer (current-window)) buffer)
    (setf (syntax buffer) (make-instance 'basic-syntax :buffer buffer))
    ;; Don't want to create the file if it doesn't exist.
    (when (probe-file filename) 
      (with-open-file (stream filename :direction :input)
	(input-from-stream stream buffer 0)))
    (setf (filename buffer) filename
	  (name buffer) (pathname-filename filename)
	  (needs-saving buffer) nil)
    (beginning-of-buffer (point pane))
    ;; this one is needed so that the buffer modification protocol
    ;; resets the low and high marks after redisplay
    (redisplay-frame-panes *application-frame*)))

(defun save-buffer (buffer)
  (let ((filename (or (filename buffer)
		      (accept 'completable-pathname
			      :prompt "Save Buffer to File"))))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (output-to-stream stream buffer 0 (size buffer)))
    (setf (filename buffer) filename
	  (name buffer) (pathname-filename filename))
    (display-message "Wrote: ~a" (filename buffer))
    (setf (needs-saving buffer) nil)))

(define-named-command com-save-buffer ()
  (let ((buffer (buffer (current-window))))
    (if (or (null (filename buffer))
	    (needs-saving buffer))
	(save-buffer buffer)
	(display-message "No changes need to be saved from ~a" (name buffer)))))

(define-named-command (com-quit) ()
  (loop for buffer in (buffers *application-frame*)
	when (and (needs-saving buffer)
		  (accept 'boolean
			  :prompt (format nil "Save buffer: ~a ?" (name buffer))))
	  do (save-buffer buffer))
  (when (or (notany #'needs-saving
		    (buffers *application-frame*))
	    (accept 'boolean :prompt "Modified buffers exist.  Quit anyway?"))
    (frame-exit *application-frame*)))

(define-named-command com-write-buffer ()
  (let ((filename (accept 'completable-pathname
			  :prompt "Write Buffer to File"))
	(buffer (buffer (current-window))))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (output-to-stream stream buffer 0 (size buffer)))
    (setf (filename buffer) filename
	  (name buffer) (pathname-filename filename)
	  (needs-saving buffer) nil)
    (display-message "Wrote: ~a" (filename buffer))))

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
			:prompt "Switch to buffer")))
    (setf (buffer (current-window)) buffer)
    (setf (syntax buffer) (make-instance 'basic-syntax :buffer buffer))
    (beginning-of-buffer (point (current-window)))
    (full-redisplay (current-window))))

(define-named-command com-kill-buffer ()
  (with-slots (buffers) *application-frame*
    (let ((buffer (buffer (current-window))))
      (when (and (needs-saving buffer)
		 (accept 'boolean :prompt "Save buffer first?"))
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
  (let ((filename (accept 'completable-pathname
			  :prompt "Load File")))
    (load filename)))

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

(define-named-command com-back-to-indentation ()
  (let ((point (point (current-window))))
    (beginning-of-line point)
    (loop until (end-of-line-p point)
	  while (whitespacep (object-after point))
	  do (incf (offset point)))))

(define-named-command com-goto-position ()
  (setf (offset (point (current-window)))
	(accept 'integer :prompt "Goto Position")))

(define-named-command com-goto-line ()
  (loop with mark = (make-instance 'standard-right-sticky-mark ;PB
		       :buffer (buffer (current-window)))
	do (end-of-line mark)
	until (end-of-buffer-p mark)
	repeat (accept 'integer :prompt "Goto Line")
	do (incf (offset mark))
	   (end-of-line mark)
	finally (beginning-of-line mark)
		(setf (offset (point (current-window)))
		      (offset mark))))

(define-named-command com-browse-url ()
  (accept 'url :prompt "Browse URL"))

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
	  (make-instance (accept 'syntax :prompt "Set Syntax")
	     :buffer buffer))
    (setf (offset (low-mark buffer)) 0
	  (offset (high-mark buffer)) (size buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Keyboard macros

(define-named-command com-start-kbd-macro ()
  (setf (recordingp *application-frame*) t)
  (setf (recorded-keys *application-frame*) '()))

(define-named-command com-end-kbd-macro ()
  (setf (recordingp *application-frame*) nil)
  (setf (recorded-keys *application-frame*)
	;; this won't work if the command was invoked in any old way
	(reverse (cddr (recorded-keys *application-frame*)))))

(define-named-command com-call-last-kbd-macro ()
  (setf (remaining-keys *application-frame*)
	(recorded-keys *application-frame*))
  (setf (executingp *application-frame*) t))

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

(defun parent3 (sheet)
  (sheet-parent (sheet-parent (sheet-parent sheet))))

(defun make-pane-constellation ()
  "make a vbox containing a scroller pane as its first child and an
info pane as its second child.  The scroller pane contains a viewport
which contains an extended pane.  Return the vbox and the extended pane
as two values"
  (let* ((extended-pane
	  (make-pane 'extended-pane
		     :width 900 :height 400
		     :name 'win
		     :end-of-line-action :scroll
		     :incremental-redisplay t
		     :display-function 'display-win))
	 (vbox
	  (vertically ()
	    (scrolling () extended-pane)
	    (make-pane 'info-pane
		       :climacs-pane extended-pane
		       :width 900 :height 20
		       :max-height 20 :min-height 20
		       ::background +gray85+
		       :scroll-bars nil
		       :borders nil
		       :incremental-redisplay t
		       :display-function 'display-info))))
    (values vbox extended-pane)))

(define-named-command com-split-window-vertically ()
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window (current-window))
	     (constellation-root (parent3 current-window)))
        (setf (buffer new-pane) (buffer current-window)
              (auto-fill-mode new-pane) (auto-fill-mode current-window)
              (auto-fill-column new-pane) (auto-fill-column current-window))
	(push new-pane (windows *application-frame*))
	(replace-constellation constellation-root vbox t)
	(full-redisplay current-window)
	(full-redisplay new-pane)))))

(define-named-command com-split-window-horizontally ()
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window (current-window))
	     (constellation-root (parent3 current-window)))
        (setf (buffer new-pane) (buffer current-window)
              (auto-fill-mode new-pane) (auto-fill-mode current-window)
              (auto-fill-column new-pane) (auto-fill-column current-window))
	(push new-pane (windows *application-frame*))
	(replace-constellation constellation-root vbox nil)
	(full-redisplay current-window)
	(full-redisplay new-pane)))))

(define-named-command com-other-window ()
  (setf (windows *application-frame*)
	(append (cdr (windows *application-frame*))
		(list (car (windows *application-frame*))))))

(define-named-command com-single-window ()
  (loop until (null (cdr (windows *application-frame*)))
	do (rotatef (car (windows *application-frame*))
		    (cadr (windows *application-frame*)))
	   (com-delete-window)))

(define-named-command com-delete-window ()
  (unless (null (cdr (windows *application-frame*)))
    (let* ((constellation (parent3 (current-window)))
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
(define-named-command com-cut-out ()
  (multiple-value-bind (start end) (region-limits (current-window))
    (kill-ring-standard-push *kill-ring* (region-to-sequence start end))
    (delete-region (offset start) end)))

;; Non destructively copies in buffer region to the kill ring
(define-named-command com-copy-out ()
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
  (let ((size (accept 'integer :prompt "New kill ring size")))
    (setf (kill-ring-max-size *kill-ring*) size)))

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
      (push (make-instance 'isearch-state
                           :search-string string
                           :search-mark mark
                           :search-forward-p forwardp
                           :search-success-p success)
            (isearch-states pane))
      (unless success
        (beep)))))

(define-named-command com-isearch-mode-forward ()
  (isearch-command-loop (current-window) t))

(define-named-command com-isearch-mode-backward ()
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
                          (length (search-string state))))))))))

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
  (let* ((string1 (accept 'string :prompt "Query replace"))
         (string2 (accept 'string
                          :prompt (format nil "Query replace ~A with"
                                          string1)))
         (pane (current-window))
         (point (point pane)))
    (when (query-replace-find-next-match point string1)
      (setf (query-replace-state pane) (make-instance 'query-replace-state
                                                      :string1 string1
                                                      :string2 string2)
            (query-replace-mode pane) t)
      (simple-command-loop 'query-replace-climacs-table
                           (query-replace-mode pane)
                           ((setf (query-replace-mode pane) nil))))))

(define-named-command com-query-replace-replace ()
  (let* ((pane (current-window))
         (point (point pane))
         (buffer (buffer pane))
         (state (query-replace-state pane))
         (string1-length (length (string1 state))))
    (backward-object point string1-length)
    (let* ((offset1 (offset point))
           (offset2 (+ offset1 string1-length))
           (region-case (buffer-region-case buffer offset1 offset2)))
      (delete-range point string1-length)
      (insert-sequence point (string2 state))
      (setf offset2 (+ offset1 (length (string2 state))))
      (finish-output *error-output*)
      (case region-case
        (:upper-case (upcase-buffer-region buffer offset1 offset2))
        (:lower-case (downcase-buffer-region buffer offset1 offset2))
        (:capitalized (capitalize-buffer-region buffer offset1 offset2))))
    (unless (query-replace-find-next-match point (string1 state))
      (setf (query-replace-mode pane) nil))))

(define-named-command com-query-replace-skip ()
  (let* ((pane (current-window))
         (point (point pane))
         (state (query-replace-state pane)))
    (unless (query-replace-find-next-match point (string1 state))
      (setf (query-replace-mode pane) nil))))

(define-named-command com-query-replace-exit ()
  (setf (query-replace-mode (current-window)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Undo/redo

(define-named-command com-undo ()
  (undo (undo-tree (buffer (current-window)))))

(define-named-command com-redo ()
  (redo (undo-tree (buffer (current-window)))))

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
	   
(define-named-command com-beginning-of-paragraph ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (beginning-of-paragraph point syntax)))

(define-named-command com-end-of-paragraph ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (end-of-paragraph point syntax)))

(define-named-command com-backward-to-error ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (display-message "~a" (backward-to-error point syntax))))

(define-named-command com-forward-to-error ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (display-message "~a" (forward-to-error point syntax))))

(define-named-command com-eval-expression ((insertp 'boolean :prompt "Insert?"))
  (let* ((*package* (find-package :climacs-gui))
	 (string (accept 'string :prompt "Eval"))
	 (result (format nil "~a" (eval (read-from-string string)))))
    (if insertp
	(insert-sequence (point (current-window)) result)
	(display-message result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Global and dead-escape command tables

(make-command-table 'global-climacs-table :errorp nil)

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
      do (global-set-key (code-char code) 'com-self-insert))

(global-set-key #\Newline 'com-self-insert)
(global-set-key #\Tab 'com-indent-line)
(global-set-key '(#\: :shift :meta) `(com-eval-expression ,*numeric-argument-p*))
(global-set-key '(#\j :control) 'com-newline-and-indent)
(global-set-key '(#\f :control) `(com-forward-object ,*numeric-argument-marker*))
(global-set-key '(#\b :control) `(com-backward-object ,*numeric-argument-marker*))
(global-set-key '(#\a :control) 'com-beginning-of-line)
(global-set-key '(#\e :control) 'com-end-of-line)
(global-set-key '(#\d :control) `(com-delete-object ,*numeric-argument-marker*))
(global-set-key '(#\p :control) `(com-previous-line ,*numeric-argument-marker*))
(global-set-key '(#\l :control) 'com-full-redisplay)
(global-set-key '(#\n :control) `(com-next-line ,*numeric-argument-marker*))
(global-set-key '(#\o :control) `(com-open-line ,*numeric-argument-marker*))
(global-set-key '(#\k :control) `(com-kill-line ,*numeric-argument-marker* ,*numeric-argument-p*))
(global-set-key '(#\t :control) 'com-transpose-objects)
(global-set-key '(#\Space :control) 'com-set-mark)
(global-set-key '(#\y :control) 'com-yank)
(global-set-key '(#\w :control) 'com-cut-out)
(global-set-key '(#\f :meta) `(com-forward-word ,*numeric-argument-marker*))
(global-set-key '(#\b :meta) `(com-backward-word ,*numeric-argument-marker*))
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
(global-set-key '(#\^ :shift :meta) 'com-delete-indentation)
(global-set-key '(#\q :meta) 'com-fill-paragraph)
(global-set-key '(#\d :meta) `(com-delete-word ,*numeric-argument-marker*))
(global-set-key '(#\Backspace :meta) `(com-backward-delete-word ,*numeric-argument-marker*))
(global-set-key '(#\/ :meta) 'com-dabbrev-expand)
(global-set-key '(#\a :control :meta) 'com-beginning-of-paragraph)
(global-set-key '(#\e :control :meta) 'com-end-of-paragraph)
(global-set-key '(#\s :control) 'com-isearch-mode-forward)
(global-set-key '(#\r :control) 'com-isearch-mode-backward)
(global-set-key '(#\% :shift :meta) 'com-query-replace)

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

(c-x-set-key '(#\0) 'com-delete-window)
(c-x-set-key '(#\1) 'com-single-window)
(c-x-set-key '(#\2) 'com-split-window-vertically)
(c-x-set-key '(#\3) 'com-split-window-horizontally)
(c-x-set-key '(#\() 'com-start-kbd-macro)
(c-x-set-key '(#\)) 'com-end-kbd-macro)
(c-x-set-key '(#\b) 'com-switch-to-buffer)
(c-x-set-key '(#\e) 'com-call-last-kbd-macro)
(c-x-set-key '(#\c :control) 'com-quit)
(c-x-set-key '(#\f :control) 'com-find-file)
(c-x-set-key '(#\k) 'com-kill-buffer)
(c-x-set-key '(#\l :control) 'com-load-file)
(c-x-set-key '(#\o) 'com-other-window)
(c-x-set-key '(#\r) 'com-redo)
(c-x-set-key '(#\u) 'com-undo)
(c-x-set-key '(#\s :control) 'com-save-buffer)
(c-x-set-key '(#\t :control) 'com-transpose-lines)
(c-x-set-key '(#\w :control) 'com-write-buffer)
(c-x-set-key '(#\x :control) 'com-exchange-point-and-mark)

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
