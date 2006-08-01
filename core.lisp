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
;;; Misc stuff

(defun display-string (string)
  (with-output-to-string (result)
    (loop for char across string
	  do (cond ((graphic-char-p char) (princ char result))
		((char= char #\Space) (princ char result))
		(t (prin1 char result))))))

(defun object-equal (x y)
  "Case insensitive equality that doesn't require characters"
  (if (characterp x)
      (and (characterp y) (char-equal x y))
      (eql x y)))

(defun object= (x y)
  "Case sensitive equality that doesn't require characters"
  (if (characterp x)
      (and (characterp y) (char= x y))
      (eql x y)))

(defun no-upper-p (string)
  "Does STRING contain no uppercase characters"
  (notany #'upper-case-p string))

(defun case-relevant-test (string)
  "Returns a test function based on the search-string STRING.
If STRING contains no uppercase characters the test is case-insensitive,
otherwise it is case-sensitive."
  (if (no-upper-p string)
      #'object-equal
      #'object=))

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
                     tab-width
                     (syntax buffer)))))))

(defun insert-character (char)
  (let* ((window (current-window))
	 (point (point window)))
    (unless (constituentp char)
      (possibly-expand-abbrev point))
    (when (whitespacep (syntax (buffer window)) char)
      (possibly-fill-line))
    (if (and (slot-value window 'overwrite-mode) (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point char))
	(insert-object point char))))

(defun back-to-indentation (mark syntax)
  (beginning-of-line mark)
  (loop until (end-of-line-p mark)
     while (whitespacep syntax (object-after mark))
     do (forward-object mark)))

(defun delete-horizontal-space (mark syntax &optional (backward-only-p nil))
  (let ((mark2 (clone-mark mark)))
    (loop until (beginning-of-line-p mark)
	  while (whitespacep syntax (object-before mark))
	  do (backward-object mark))
    (unless backward-only-p
      (loop until (end-of-line-p mark2)
	    while (whitespacep syntax (object-after mark2))
	    do (forward-object mark2)))
    (delete-region mark mark2)))

(defun goto-position (mark pos)
  (setf (offset mark) pos))

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

(defun indent-current-line (pane point)
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (indentation (syntax-line-indentation point
                                               tab-space-count
                                               (syntax buffer))))
    (indent-line point indentation (and (indent-tabs-mode buffer)
                                        tab-space-count))))

(defun insert-pair (mark syntax &optional (count 0) (open #\() (close #\)))
  (cond ((> count 0)
	 (loop while (and (not (end-of-buffer-p mark))
			  (whitespacep syntax (object-after mark)))
	       do (forward-object mark)))
	((< count 0)
	 (setf count (- count))
	 (loop repeat count do (backward-expression mark syntax))))
  (unless (or (beginning-of-buffer-p mark)
	      (whitespacep syntax (object-before mark)))
    (insert-object mark #\Space))
  (insert-object mark open)
  (let ((here (clone-mark mark)))
    (loop repeat count
	  do (forward-expression here syntax))
    (insert-object here close)
    (unless (or (end-of-buffer-p here)
		(whitespacep syntax (object-after here)))
      (insert-object here #\Space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

(defun downcase-word (mark &optional (n 1))
  "Convert the next N words to lowercase, leaving mark after the last word."
  (let ((syntax (syntax (buffer mark))))
    (loop repeat n
       do (forward-to-word-boundary mark syntax)
       (let ((offset (offset mark)))
         (forward-word mark syntax 1 nil)
         (downcase-region offset mark)))))

(defun upcase-word (mark syntax &optional (n 1))
  "Convert the next N words to uppercase, leaving mark after the last word."
  (loop repeat n
     do (forward-to-word-boundary mark syntax)
     (let ((offset (offset mark)))
       (forward-word mark syntax 1 nil)
       (upcase-region offset mark))))

(defun capitalize-word (mark &optional (n 1))
  "Capitalize the next N words, leaving mark after the last word."
  (let ((syntax (syntax (buffer mark))))
    (loop repeat n
       do (forward-to-word-boundary mark syntax)
       (let ((offset (offset mark)))
         (forward-word mark syntax 1 nil)
         (capitalize-region offset mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Indentation

(defun indent-region (pane mark1 mark2)
  "Indent all lines in the region delimited by `mark1' and `mark2'
   according to the rules of the active syntax in `pane'."
  (let* ((buffer (buffer pane))
         (view (clim:stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (tab-width (and (indent-tabs-mode buffer)
                         tab-space-count))
         (syntax (syntax buffer)))
    (do-buffer-region-lines (line mark1 mark2)
      (let ((indentation (syntax-line-indentation
                          line
                          tab-space-count
                          syntax)))
        (indent-line line indentation tab-width))
      ;; We need to update the syntax every time we perform an
      ;; indentation, so that subsequent indentations will be
      ;; correctly indented (this matters in list forms). FIXME: This
      ;; should probably happen automatically.
      (update-syntax buffer syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Auto fill

(defun fill-line (mark syntax-line-indentation-function fill-column tab-width syntax
		  &optional (compress-whitespaces t))
  "Breaks the contents of line pointed to by MARK up to MARK into
multiple lines such that none of them is longer than FILL-COLUMN. If
COMPRESS-WHITESPACES is non-nil, whitespaces are compressed after the
decision is made to break the line at a point. For now, the
compression means just the deletion of trailing whitespaces."
  (let ((begin-mark (clone-mark mark)))
    (beginning-of-line begin-mark)
    (loop with column = 0
          with line-beginning-offset = (offset begin-mark)
          with walking-mark = (clone-mark begin-mark)
          while (mark< walking-mark mark)
          as object = (object-after walking-mark)
          do (case object
               (#\Space
                (setf (offset begin-mark) (offset walking-mark))
                (incf column))
               (#\Tab
                (setf (offset begin-mark) (offset walking-mark))
                (incf column (- tab-width (mod column tab-width))))
               (t
                (incf column)))
             (when (and (>= column fill-column)
			(/= (offset begin-mark) line-beginning-offset))
	       (when compress-whitespaces
		 (let ((offset (buffer-search-backward
				(buffer begin-mark)
				(offset begin-mark)
				#(nil)
				:test #'(lambda (o1 o2)
					  (declare (ignore o2))
					  (not (whitespacep syntax o1))))))
		   (when offset
		     (delete-region begin-mark (1+ offset)))))
               (insert-object begin-mark #\Newline)
               (incf (offset begin-mark))
               (let ((indentation
                      (funcall syntax-line-indentation-function begin-mark)))
                 (indent-line begin-mark indentation tab-width))
               (beginning-of-line begin-mark)
               (setf line-beginning-offset (offset begin-mark))
               (setf (offset walking-mark) (offset begin-mark))
               (setf column 0))
             (incf (offset walking-mark)))))

(defun fill-region (mark1 mark2 syntax-line-indentation-function fill-column tab-width syntax
                    &optional (compress-whitespaces t))
  "Fill the region delimited by `mark1' and `mark2'. `Mark1' must be
mark<= `mark2.'"
  (let* ((buffer (buffer mark1)))
    (do-buffer-region (object offset buffer
                              (offset mark1) (offset mark2))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (when (>= (buffer-display-column buffer (offset mark2) tab-width)
              (1- fill-column))
      (fill-line mark2
                 syntax-line-indentation-function
                 fill-column
                 tab-width
                 compress-whitespaces
                 syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Indentation

(defgeneric indent-line (mark indentation tab-width)
  (:documentation "Indent the line containing mark with indentation
spaces. Use tabs and spaces if tab-width is not nil, otherwise use
spaces only."))

(defun indent-line* (mark indentation tab-width left)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop until (end-of-buffer-p mark2)
       as object = (object-after mark2)
       while (or (eql object #\Space) (eql object #\Tab))
       do (delete-range mark2 1))
    (loop until (zerop indentation)
       do (cond ((and tab-width (>= indentation tab-width))
		 (insert-object mark2 #\Tab)
		 (when left             ; spaces must follow tabs
		   (forward-object mark2))
		 (decf indentation tab-width))
		(t
		 (insert-object mark2 #\Space)
		 (decf indentation))))))

(defmethod indent-line ((mark left-sticky-mark) indentation tab-width)
  (indent-line* mark indentation tab-width t))

(defmethod indent-line ((mark right-sticky-mark) indentation tab-width)
  (indent-line* mark indentation tab-width nil))

(defun delete-indentation (mark)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (delete-range mark -1)
    (loop until (end-of-buffer-p mark)
          while (buffer-whitespacep (object-after mark))
          do (delete-range mark 1))
    (loop until (beginning-of-buffer-p mark)
          while (buffer-whitespacep (object-before mark))
          do (delete-range mark -1))
    (when (and (not (beginning-of-buffer-p mark))
	       (constituentp (object-before mark)))
      (insert-object mark #\Space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Syntax handling

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Buffer handling

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
    (when position
      (setf buffers (delete buffer buffers)))
    (push buffer (buffers *application-frame*))
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

(defgeneric kill-buffer (buffer))

(defmethod kill-buffer ((buffer climacs-buffer))
  (with-slots (buffers) *application-frame*
     (when (and (needs-saving buffer)
		(handler-case (accept 'boolean :prompt "Save buffer first?")
		  (error () (progn (beep)
				   (display-message "Invalid answer")
				   (return-from kill-buffer nil)))))
       (save-buffer buffer))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; File handling

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
  (mapcar (lambda (pair) (split-attribute pair #\:))
	  (split-attribute line #\;)))

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
  (multiple-value-bind (start-mark end-mark) (find-attribute-line-position buffer)
   (let ((line (buffer-substring buffer
                                 (offset start-mark)
                                 (offset end-mark))))
     (when (>= (length line) 6)
       (let ((end (search "-*-" line :from-end t :start2 3)))
         (when end
           (string-trim '(#\Space #\Tab) (subseq line 3 end))))))))

(defun replace-attribute-line (buffer new-attribute-line)
  (let ((full-attribute-line (concatenate 'string
                                          "-*- "
                                          new-attribute-line
                                          "-*-")))
   (multiple-value-bind (start-mark end-mark) (find-attribute-line-position buffer)
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

(defun find-file (filepath &optional readonlyp)
  (cond ((null filepath)
	 (display-message "No file name given.")
	 (beep))
	((directory-pathname-p filepath)
	 (display-message "~A is a directory name." filepath)
	 (beep))
        (t
         (flet ((usable-pathname (pathname)
                   (if (probe-file pathname)
                       (truename pathname)
                       pathname)))
           (let ((existing-buffer (find filepath (buffers *application-frame*)
                                        :key #'filepath
                                        :test #'(lambda (fp1 fp2)
                                                  (and fp1 fp2
                                                       (equal (usable-pathname fp1)
                                                              (usable-pathname fp2)))))))
             (if (and existing-buffer (if readonlyp (read-only-p existing-buffer) t))
                 (switch-to-buffer existing-buffer)
                 (progn
                   (when readonlyp
                     (unless (probe-file filepath)
                       (beep)
                       (display-message "No such file: ~A" filepath)
                       (return-from find-file nil)))
                   (let ((buffer (make-buffer))
                         (pane (current-window)))
                     ;; Clear the pane's cache; otherwise residue from the
                     ;; previously displayed buffer may under certain
                     ;; circumstances be displayed.
                     (clear-cache pane)
                     (setf (syntax buffer) nil)
                     (setf (offset (point (buffer pane))) (offset (point pane)))
                     (setf (buffer (current-window)) buffer)
                     (setf (syntax buffer)
                           (make-instance (syntax-class-name-for-filepath filepath)
                                          :buffer buffer))
                     ;; Don't want to create the file if it doesn't exist.
                     (when (probe-file filepath)
                       (with-open-file (stream filepath :direction :input)
                         (input-from-stream stream buffer 0))
                       (setf (file-write-time buffer) (file-write-date filepath))
                       ;; A file! That means we may have a local options
                       ;; line to parse.
                       (evaluate-attribute-line buffer))
                     (setf (filepath buffer) filepath
                           (name buffer) (filepath-filename filepath)
                           (needs-saving buffer) nil
                           (read-only-p buffer) readonlyp)
                     (beginning-of-buffer (point pane))
                     (update-syntax buffer (syntax buffer))
                     (clear-modify buffer)
                     buffer))))))))

(defun directory-of-buffer (buffer)
  "Extract the directory part of the filepath to the file in BUFFER.
   If BUFFER does not have a filepath, the path to the user's home 
   directory will be returned."
  (make-pathname
   :directory
   (pathname-directory
    (or (filepath buffer)
	(user-homedir-pathname)))))

(defun set-visited-file-name (filename buffer)
  (setf (filepath buffer) filename
	(file-saved-p buffer) nil
	(file-write-time buffer) nil
	(name buffer) (filepath-filename filename)
	(needs-saving buffer) t))

(defun extract-version-number (pathname)
  "Extracts the emacs-style version-number from a pathname."
  (let* ((type (pathname-type pathname))
	 (length (length type)))
    (when (and (> length 2) (char= (char type (1- length)) #\~))
      (let ((tilde (position #\~ type :from-end t :end (- length 2))))
	(when tilde
	  (parse-integer type :start (1+ tilde) :junk-allowed t))))))

(defun version-number (pathname)
  "Return the number of the highest versioned backup of PATHNAME
or 0 if there is no versioned backup. Looks for name.type~X~,
returns highest X."
  (let* ((wildpath (merge-pathnames (make-pathname :type :wild) pathname))
	 (possibilities (directory wildpath)))
    (loop for possibility in possibilities
	  for version = (extract-version-number possibility) 
	  if (numberp version)
	    maximize version into max
	  finally (return max))))

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

(defun save-buffer (buffer)
  (let ((filepath (or (filepath buffer)
		      (accept 'pathname :prompt "Save Buffer to File"))))
    (cond
      ((directory-pathname-p filepath)
       (display-message "~A is a directory." filepath)
       (beep))
      (t
       (unless (check-file-times buffer filepath "Overwrite" "written")
	 (return-from save-buffer))
       (when  (and (probe-file filepath) (not (file-saved-p buffer)))
	 (let ((backup-name (pathname-name filepath))
	       (backup-type (format nil "~A~~~D~~"
				    (pathname-type filepath)
				    (1+ (version-number filepath)))))
	   (rename-file filepath (make-pathname :name backup-name
						:type backup-type)))
	 (setf (file-saved-p buffer) t))
       (with-open-file (stream filepath :direction :output :if-exists :supersede)
	 (output-to-stream stream buffer 0 (size buffer)))
       (setf (filepath buffer) filepath
	     (file-write-time buffer) (file-write-date filepath)
	     (name buffer) (filepath-filename filepath))
       (display-message "Wrote: ~a" filepath)
       (setf (needs-saving buffer) nil)))))

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