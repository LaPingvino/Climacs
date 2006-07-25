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

;;; File (and buffer) commands for the Climacs editor. 

(in-package :climacs-commands)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Buffer commands

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

(define-command (com-kill-buffer :name t :command-table pane-table)
    ((buffer 'buffer
             :prompt "Kill buffer"
             :default (buffer (current-window))
             :default-type 'buffer))
  "Prompt for a buffer name and kill that buffer.
If the buffer needs saving, will prompt you to do so before killing it. Uses the current buffer as a default."
  (kill-buffer buffer))

(set-key `(com-kill-buffer ,*unsupplied-argument-marker*)
	 'pane-table
	 '((#\x :control) (#\k)))

(define-command (com-toggle-read-only :name t :command-table base-table)
    ((buffer 'buffer :default (current-buffer)))
  (setf (read-only-p buffer) (not (read-only-p buffer))))

(define-presentation-to-command-translator toggle-read-only
    (read-only com-toggle-read-only base-table
               :gesture :menu)
    (object)
  (list object))

(define-command (com-toggle-modified :name t :command-table base-table)
    ((buffer 'buffer :default (current-buffer)))
  (setf (needs-saving buffer) (not (needs-saving buffer))))

(define-presentation-to-command-translator toggle-modified
    (modified com-toggle-modified base-table
              :gesture :menu)
    (object)
  (list object))