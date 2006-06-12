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

;;; File commands for the Climacs editor. 

(in-package :climacs-gui)

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
	   (values (or pathname (parse-namestring string)) type))
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
    (when specified-syntax
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

(defun get-attribute-line (buffer)
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
		   do (forward-object scan)
		   until (looking-at scan "-*-")
		   finally (return t))))
	(when start-found
	  (let ((line (buffer-substring buffer
					(offset scan)
					(offset (end-of-line (clone-mark scan))))))
	    (when (>= (length line) 6)
	      (let ((end (search "-*-" line :from-end t :start2 3)))
		(when end
		  (string-trim '(#\Space #\Tab) (subseq line 3 end)))))))))))

(defun evaluate-attributes-line (buffer)
  (evaluate-attributes
   buffer
   (split-attribute-line (get-attribute-line buffer))))

(define-command (com-reparse-attribute-list :name t :command-table buffer-table) ()
  "Reparse the current buffer's attribute list.
An attribute list is a line of keyword-value pairs, each keyword separated
from the corresponding value by a colon. If another keyword-value pair
follows, the value should be terminated by a colon. The attribute list
is surrounded by '-*-' sequences, but the opening '-*-' need not be at the
beginning of the line. Climacs looks for the attribute list
on the first or second non-blank line of the file.

An example attribute-list is:

;; -*- Syntax: Lisp; Base: 10 -*- "
  (evaluate-attributes-line (buffer (current-window))))

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
                     ;; Don't want to create the file if it doesn't exist.
                     (when (probe-file filepath)
                       (with-open-file (stream filepath :direction :input)
                         (input-from-stream stream buffer 0))
                       (setf (file-write-time buffer) (file-write-date filepath))
                       ;; A file! That means we may have a local options
                       ;; line to parse.
                       (evaluate-attributes-line buffer))
                     ;; If the local options line didn't set a syntax, do
                     ;; it now.
                     (when (null (syntax buffer))
                       (setf (syntax buffer)
                             (make-instance (syntax-class-name-for-filepath filepath)
                                            :buffer buffer)))
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

(define-command (com-find-file :name t :command-table buffer-table)
    ((filepath 'pathname
	       :prompt "Find File"
	       :default (directory-of-buffer (buffer (current-window)))
	       :default-type 'pathname
	       :insert-default t))
  "Prompt for a filename then edit that file.
If a buffer is already visiting that file, switch to that buffer. Does not create a file if the filename given does not name an existing file."
  (find-file filepath))

(set-key `(com-find-file ,*unsupplied-argument-marker*)
	 'buffer-table
	 '((#\x :control) (#\f :control)))

(define-command (com-find-file-read-only :name t :command-table buffer-table)
    ((filepath 'pathname :Prompt "Find file read only"
	       :default (directory-of-buffer (buffer (current-window)))
	       :default-type 'pathname
	       :insert-default t))
  "Prompt for a filename then open that file readonly.
If a buffer is already visiting that file, switch to that buffer. If the filename given does not name an existing file, signal an error."
  (find-file filepath t))

(set-key `(com-find-file-read-only ,*unsupplied-argument-marker*)
	 'buffer-table
	 '((#\x :control) (#\r :control)))

(define-command (com-read-only :name t :command-table buffer-table) ()
  "Toggle the readonly status of the current buffer.
When a buffer is readonly, attempts to change the contents of the buffer signal an error."
  (let ((buffer (buffer (current-window))))
    (setf (read-only-p buffer) (not (read-only-p buffer)))))

(set-key 'com-read-only
	 'buffer-table
	 '((#\x :control) (#\q :control)))

(defun set-visited-file-name (filename buffer)
  (setf (filepath buffer) filename
	(file-saved-p buffer) nil
	(file-write-time buffer) nil
	(name buffer) (filepath-filename filename)
	(needs-saving buffer) t))

(define-command (com-set-visited-file-name :name t :command-table buffer-table)
    ((filename 'pathname :prompt "New file name"
	       :default (directory-of-buffer (buffer (current-window)))
	       :default-type 'pathname
	       :insert-default t))
  "Prompt for a new filename for the current buffer.
The next time the buffer is saved it will be saved to a file with that filename."
  (set-visited-file-name filename (buffer (current-window))))

(define-command (com-insert-file :name t :command-table buffer-table)
    ((filename 'pathname :prompt "Insert File"
	       :default (directory-of-buffer (buffer (current-window)))
	       :default-type 'pathname
	       :insert-default t))
  "Prompt for a filename and insert its contents at point.
Leaves mark after the inserted contents."
  (let ((pane (current-window)))
    (when (probe-file filename)
      (setf (mark pane) (clone-mark (point pane) :left))
      (with-open-file (stream filename :direction :input)
	(input-from-stream stream
			   (buffer pane)
			   (offset (point pane))))
      (psetf (offset (mark pane)) (offset (point pane))
	     (offset (point pane)) (offset (mark pane))))
    (redisplay-frame-panes *application-frame*)))

(set-key `(com-insert-file ,*unsupplied-argument-marker*)
	 'buffer-table
	 '((#\x :control) (#\i :control)))

(define-command (com-revert-buffer :name t :command-table buffer-table) ()
  "Replace the contents of the current buffer with the visited file.
Signals an error if the file does not exist."
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
	   (unless (check-file-times buffer filepath "Revert" "reverted")
	     (return-from com-revert-buffer))
	   (erase-buffer buffer)
	   (with-open-file (stream filepath :direction :input)
	     (input-from-stream stream buffer 0))
	   (setf (offset (point pane)) (min (size buffer) save)
		 (file-saved-p buffer) nil))
	  (t
	   (display-message "No file ~A" filepath)
	   (beep))))))

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
  "Return NIL if filepath newer than buffer and user doesn't want to overwrite"
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

(define-command (com-save-buffer :name t :command-table buffer-table) ()
  "Write the contents of the buffer to a file.
If there is filename associated with the buffer, write to that file, replacing its contents. If not, prompt for a filename."
  (let ((buffer (buffer (current-window))))
    (if (or (null (filepath buffer))
	    (needs-saving buffer))
	(save-buffer buffer)
	(display-message "No changes need to be saved from ~a" (name buffer)))))

(set-key 'com-save-buffer
	 'buffer-table
	 '((#\x :control) (#\s :control)))

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

(define-command (com-write-buffer :name t :command-table buffer-table)
    ((filepath 'pathname :prompt "Write Buffer to File"
	       :default (directory-of-buffer (buffer (current-window)))
	       :default-type 'pathname
	       :insert-default t))
  "Prompt for a filename and write the current buffer to it.
Changes the file visted by the buffer to the given file."
  (let ((buffer (buffer (current-window))))
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

(set-key `(com-write-buffer ,*unsupplied-argument-marker*)
	 'buffer-table
	 '((#\x :control) (#\w :control)))

