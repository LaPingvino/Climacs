;;; -*- Mode: Lisp; Package: CLIMACS-CORE -*-

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

(in-package :climacs-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Buffer handling

(defmethod frame-make-new-buffer ((application-frame climacs)
                                  &key (name "*scratch*"))
  (let ((buffer (make-instance 'climacs-buffer :name name)))
    (push buffer (buffers application-frame))
    buffer))

(defgeneric erase-buffer (buffer))

(defmethod erase-buffer ((buffer string))
  (let ((b (find buffer (buffers *application-frame*)
		 :key #'name :test #'string=)))
    (when b (erase-buffer b))))

(defmethod erase-buffer ((buffer drei-buffer))
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
    (cond ((and success (plusp (length string)))
           (if object
               (values object type)
               (values string 'string)))
	  ((and (zerop (length string)) defaultp)
           (values default default-type))
	  (t
           (values string 'string)))))

(defgeneric switch-to-buffer (pane buffer))

(defmethod switch-to-buffer ((pane drei) (buffer drei-buffer))
  (setf (buffer pane) buffer))

(defmethod switch-to-buffer ((pane typeout-pane) (buffer drei-buffer))
  (let ((usable-pane (or (find-if #'(lambda (pane)
                                      (typep pane 'drei))
                                  (windows *application-frame*))
                         (split-window t))))
    (switch-to-buffer usable-pane buffer)))

(defmethod switch-to-buffer (pane (name string))
  (let ((buffer (find name (buffers *application-frame*)
		      :key #'name :test #'string=)))
    (switch-to-buffer pane
                      (or buffer
			  (make-new-buffer :name name)))))

;; ;;; FIXME: see the comment by (SETF SYNTAX) :AROUND.  -- CSR,
;; ;;; 2005-10-31.
;; (defmethod (setf buffer) :around (buffer (pane drei))
;;   (call-next-method)
;;   (note-pane-syntax-changed pane (syntax buffer)))

(defgeneric kill-buffer (buffer))

(defmethod kill-buffer ((buffer drei-buffer))
  (with-accessors ((buffers buffers)) *application-frame*
     (when (and (needs-saving buffer)
		(handler-case (accept 'boolean :prompt "Save buffer first?")
		  (error () (progn (beep)
				   (display-message "Invalid answer")
				   (return-from kill-buffer nil)))))
       (save-buffer buffer))
     (setf buffers (remove buffer buffers))
     ;; Always need one buffer.
     (when (null buffers)
       (make-new-buffer :name "*scratch*"))
     (setf (buffer (current-window)) (car buffers))
     (full-redisplay (current-window))
     (buffer (current-window))))

(defmethod kill-buffer ((name string))
  (let ((buffer (find name (buffers *application-frame*)
		      :key #'name :test #'string=)))
    (when buffer (kill-buffer buffer))))

(defmethod kill-buffer ((symbol (eql 'nil)))
  (kill-buffer (buffer (current-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; File handling

(defun filepath-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
		   "." (pathname-type pathname))))

(defun syntax-class-name-for-filepath (filepath)
  (let ((syntax-description
         (find (or (pathname-type filepath)
                   (pathname-name filepath))
               drei-syntax::*syntaxes*
               :test (lambda (x y)
                       (member x y :test #'string-equal))
               :key #'drei-syntax::syntax-description-pathname-types)))
    (if syntax-description
        (drei-syntax::syntax-description-class-name
         syntax-description)
        *default-syntax*)))

(defun evaluate-attributes (buffer options)
  "Evaluate the attributes `options' and modify `buffer' as
  appropriate. `Options' should be an alist mapping option names
  to their values."
  ;; First, check whether we need to change the syntax (via the SYNTAX
  ;; option). MODE is an alias for SYNTAX for compatibility with
  ;; Emacs. If there is more than one option with one of these names,
  ;; only the first will be acted upon.
  (let ((specified-syntax
         (syntax-from-name
          (second (find-if #'(lambda (name)
                               (or (string-equal name "SYNTAX")
                                   (string-equal name "MODE")))
                           options
                           :key #'first)))))
    (when (and specified-syntax
               (not (eq (class-of (syntax buffer))
                        specified-syntax)))
      (setf (syntax buffer)
            (make-instance specified-syntax
                           :buffer buffer))))
  ;; Now we iterate through the options (discarding SYNTAX and MODE
  ;; options).
  (loop for (name value) in options
     unless (or (string-equal name "SYNTAX")
                (string-equal name "MODE"))
     do (eval-option (syntax buffer) name value)))

(defun split-attribute (string char)
  (let (pairs)
    (loop with start = 0
	  for ch across string
	  for i from 0
	  when (eql ch char)
	    do (push (string-trim '(#\Space #\Tab) (subseq string start i))
		     pairs)
	       (setf start (1+ i))
	  finally (unless (>= start i)
		    (push (string-trim '(#\Space #\Tab) (subseq string start))
			  pairs)))
    (nreverse pairs)))

(defun split-attribute-line (line)
  (when line
    (mapcar (lambda (pair) (split-attribute pair #\:))
	    (split-attribute line #\;))))

(defun find-attribute-line-position (buffer)
  (let ((scan (beginning-of-buffer (clone-mark (point buffer)))))
    ;; skip the leading whitespace
    (loop until (end-of-buffer-p scan)
       until (not (whitespacep (syntax buffer) (object-after scan)))
       do (forward-object scan))
    ;; stop looking if we're already 1,000 objects into the buffer
    (unless (> (offset scan) 1000)
      (let ((start-found
	     (loop with newlines = 0
                when (end-of-buffer-p scan)
                do (return nil)
                when (eql (object-after scan) #\Newline)
                do (incf newlines)
                when (> newlines 1)
                do (return nil)
                until (looking-at scan "-*-")
                do (forward-object scan)
                finally (return t))))
	(when start-found
          (let* ((end-scan (clone-mark scan))
                 (end-found
                  (loop when (end-of-buffer-p end-scan)
                     do (return nil)
                     when (eql (object-after end-scan) #\Newline)
                     do (return nil)
                     do (forward-object end-scan)
                     until (looking-at end-scan "-*-")
                     finally (return t))))
            (when end-found
              (values scan
                      (progn (forward-object end-scan 3)
                             end-scan)))))))))

(defun get-attribute-line (buffer)
  (multiple-value-bind (start-mark end-mark)
      (find-attribute-line-position buffer)
   (when (and start-mark end-mark)
     (let ((line (buffer-substring buffer
				   (offset start-mark)
				   (offset end-mark))))
       (when (>= (length line) 6)
	 (let ((end (search "-*-" line :from-end t :start2 3)))
	   (when end
	     (string-trim '(#\Space #\Tab) (subseq line 3 end)))))))))

(defun replace-attribute-line (buffer new-attribute-line)
  (let ((full-attribute-line (concatenate 'string
                                          "-*- "
                                          new-attribute-line
                                          "-*-")))
   (multiple-value-bind (start-mark end-mark)
       (find-attribute-line-position buffer)
     (cond ((not (null end-mark))
            ;; We have an existing attribute line.
            (delete-region start-mark end-mark)
            (let ((new-line-start (clone-mark start-mark :left)))
              (insert-sequence start-mark full-attribute-line)
              (comment-region (syntax buffer)
                              new-line-start
                              start-mark)))
           (t
            ;; Create a new attribute line at beginning of buffer.
            (let* ((mark1 (beginning-of-buffer (clone-mark (point buffer) :left)))
                   (mark2 (clone-mark mark1 :right)))
              (insert-sequence mark2 full-attribute-line)
              (insert-object mark2 #\Newline)
              (comment-region (syntax buffer)
                              mark1
                              mark2)))))))

(defun update-attribute-line (buffer)
  (replace-attribute-line buffer
                          (make-attribute-line (syntax buffer))))

(defun evaluate-attribute-line (buffer)
  (evaluate-attributes
   buffer
   (split-attribute-line (get-attribute-line buffer))))

;; Adapted from cl-fad/PCL
(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC does not designate a directory."
  (let ((name (pathname-name pathspec))
	(type (pathname-type pathspec)))
    (and (or (null name) (eql name :unspecific))
	 (or (null type) (eql type :unspecific)))))

(defun findablep (pathname)
  "Return non-NIL if `pathname' can be opened by Climacs. That
  is, check whether the file exists and is not a directory."
  (and (probe-file pathname)
       (not (directory-pathname-p pathname))))

(defun find-buffer-with-pathname (pathname)
  "Return the (first) buffer associated with the file designated
by `pathname'. Returns NIL if no buffer can be found."
  (flet ((usable-pathname (pathname)
           (if (probe-file pathname)
               (truename pathname)
               pathname)))
    (find pathname (buffers *application-frame*)
          :key #'filepath
          :test #'(lambda (fp1 fp2)
                    (and fp1 fp2
                         (equal (usable-pathname fp1)
                                (usable-pathname fp2)))))))

(defun ensure-open-file (pathname)
  "Make sure a buffer opened on `pathname' exists, finding the
file if necessary."
  (when (and (findablep pathname)
             (not (find-buffer-with-pathname pathname)))
    (find-file pathname)))

(defun find-file-impl (filepath &optional readonlyp)
  (cond ((null filepath)
	 (display-message "No file name given.")
	 (beep))
	((directory-pathname-p filepath)
	 (display-message "~A is a directory name." filepath)
	 (beep))
        (t
         (let ((existing-buffer (find-buffer-with-pathname filepath)))
           (if (and existing-buffer (if readonlyp (read-only-p existing-buffer) t))
               (switch-to-buffer *current-window* existing-buffer)
               (progn
                 (when readonlyp
                   (unless (probe-file filepath)
                     (beep)
                     (display-message "No such file: ~A" filepath)
                     (return-from find-file-impl nil)))
                 (let ((buffer (if (probe-file filepath)
                                   (with-open-file (stream filepath :direction :input)
                                     (make-buffer-from-stream stream))
                                   (make-new-buffer)))
                       (pane (current-window)))
                   (setf (offset (point (buffer pane))) (offset (point pane))
                         (buffer (current-window)) buffer
                         (syntax buffer) (make-instance (syntax-class-name-for-filepath filepath)
                                                        :buffer buffer)
                         (file-write-time buffer) (file-write-date filepath))
                   (evaluate-attribute-line buffer)
                   (setf (filepath buffer) filepath
                         (name buffer) (filepath-filename filepath)
                         (read-only-p buffer) readonlyp)
                   (beginning-of-buffer (point pane))
                   (update-syntax buffer (syntax buffer))
                   (clear-modify buffer)
                   buffer)))))))

(defmethod frame-find-file ((application-frame climacs) filepath)
  (find-file-impl filepath nil))

(defmethod frame-find-file-read-only ((application-frame climacs) filepath)
  (find-file-impl filepath t))

(defun directory-of-buffer (buffer)
  "Extract the directory part of the filepath to the file in BUFFER.
   If BUFFER does not have a filepath, the path to the user's home 
   directory will be returned."
  (make-pathname
   :directory
   (pathname-directory
    (or (filepath buffer)
	(user-homedir-pathname)))))

(defmethod frame-set-visited-filename ((application-frame climacs) filepath buffer)
  (setf (filepath buffer) filepath
	(file-saved-p buffer) nil
	(file-write-time buffer) nil
	(name buffer) (filepath-filename filepath)
	(needs-saving buffer) t))

(defun check-file-times (buffer filepath question answer)
  "Return NIL if filepath newer than buffer and user doesn't want
to overwrite."
  (let ((f-w-d (file-write-date filepath))
	(f-w-t (file-write-time buffer)))
    (if (and f-w-d f-w-t (> f-w-d f-w-t))
	(if (accept 'boolean
		    :prompt (format nil "File has changed on disk. ~a anyway?"
				    question))
	    t
	    (progn (display-message "~a not ~a" filepath answer)
		   nil))
	t)))

(defmethod frame-exit :around ((frame climacs) #-mcclim &key)
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