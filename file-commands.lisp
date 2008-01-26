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

;;; File (and buffer) commands for the Climacs editor. Note that many
;;; basic commands (such as Find File) are defined in ESA and made
;;; available to Climacs via the ESA-IO-TABLE command table.

(in-package :climacs-commands)

(define-command (com-reparse-attribute-list :name t :command-table buffer-table)
    ()
  "Reparse the current buffer's attribute list.
An attribute list is a line of keyword-value pairs, each keyword separated
from the corresponding value by a colon. If another keyword-value pair
follows, the value should be terminated by a colon. The attribute list
is surrounded by '-*-' sequences, but the opening '-*-' need not be at the
beginning of the line. Climacs looks for the attribute list
on the first or second non-blank line of the file.

An example attribute-list is:

;; -*- Syntax: Lisp; Base: 10 -*- "
  (evaluate-attribute-line (current-buffer)))

(define-command (com-update-attribute-list :name t :command-table buffer-table)
    ()
  "Update the current buffers attribute list to reflect the
settings of the syntax of the buffer.

After the attribute list has been updated, it will also be
re-evaluated. An attribute list is a line of keyword-value pairs,
each keyword separated from the corresponding value by a
colon. If another keyword-value pair follows, the value should be
terminated by a colon. The attribute list is surrounded by '-*-'
sequences, but the opening '-*-' need not be at the beginning of
the line. Climacs looks for the attribute list on the first or
second non-blank line of the file.

An example attribute-list is:

;; -*- Syntax: Lisp; Base: 10 -*- 

This command automatically comments the attribute line as
appropriate for the syntax of the buffer."
  (update-attribute-line (current-buffer))
  (evaluate-attribute-line (current-buffer)))

(define-command (com-insert-file :name t :command-table buffer-table)
    ((filename 'pathname :prompt "Insert File"
                         :default (directory-of-buffer (current-buffer))
                         :default-type 'pathname
                         :insert-default t))
  "Prompt for a filename and insert its contents at point.
Leaves mark after the inserted contents."
  (when (probe-file filename)
    (setf (mark) (clone-mark (point) :left))
    (with-open-file (stream filename :direction :input)
      (input-from-stream stream
                         (current-buffer)
                         (offset (point))))
    (psetf (offset (mark)) (offset (point))
           (offset (point)) (offset (mark))))
  (redisplay-frame-panes *application-frame*))

(set-key `(com-insert-file ,*unsupplied-argument-marker*)
	 'buffer-table
	 '((#\x :control) (#\i :control)))

(define-command (com-revert-buffer :name t :command-table buffer-table) ()
  "Replace the contents of the current buffer with the visited file.
Signals an error if the file does not exist."
  (let* ((save (offset (point)))
         (filepath (filepath (current-buffer))))
    (when (accept 'boolean :prompt (format nil "Revert buffer from file ~A?"
					   filepath))
      (cond ((directory-pathname-p filepath)
	   (display-message "~A is a directory name." filepath)
	   (beep))
	  ((probe-file filepath)
	   (unless (check-file-times (current-buffer) filepath "Revert" "reverted")
	     (return-from com-revert-buffer))
	   (erase-buffer (current-buffer))
	   (with-open-file (stream filepath :direction :input)
	     (input-from-stream stream (current-buffer) 0))
	   (setf (offset (point)) (min (size (current-buffer)) save)
		 (file-saved-p (current-buffer)) nil))
	  (t
	   (display-message "No file ~A" filepath)
	   (beep))))))

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

(define-command (com-switch-to-view :name t :command-table pane-table)
    ;; Perhaps the default should be an undisplayed view?
    ((view 'view :default (or (find (current-view) (views *application-frame*)
                               :test (complement #'eq))
                              (any-view))))
  "Prompt for a buffer name and switch to that buffer.
If the a buffer with that name does not exist, create it. Uses
the name of the next buffer (if any) as a default."
  (handler-case (switch-to-view (current-window) view)
    (view-already-displayed (condition)
      (other-window (window condition)))))

(set-key `(com-switch-to-view ,*unsupplied-argument-marker*)
	 'pane-table
	 '((#\x :control) (#\b)))

(define-command (com-kill-view :name t :command-table pane-table)
    ((view 'view :prompt "Kill view"
                 :default (current-view)))
  "Prompt for a view name and kill that view.
If the view is of a buffer and the buffer needs saving, you will
be prompted to do so before killing it. Uses the current view
as a default."
  (kill-view view))

(set-key `(com-kill-view ,*unsupplied-argument-marker*)
	 'pane-table
	 '((#\x :control) (#\k)))

(define-command (com-toggle-read-only :name t :command-table buffer-table)
    ((buffer 'buffer :default (current-buffer *application-frame*)))
  (setf (read-only-p buffer) (not (read-only-p buffer))))

(define-presentation-to-command-translator toggle-read-only
    (read-only com-toggle-read-only buffer-table
               :gesture :menu)
    (object)
  (list object))

(define-command (com-toggle-modified :name t :command-table buffer-table)
    ((buffer 'buffer :default (current-buffer *application-frame*)))
  (setf (needs-saving buffer) (not (needs-saving buffer))))

(define-presentation-to-command-translator toggle-modified
    (modified com-toggle-modified buffer-table
              :gesture :menu)
    (object)
  (list object))