;;; -*- Mode: Lisp; Package: CLIMACS-COMMANDS -*-

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

;;; Search commands for Climacs.

(in-package :climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Multiple query replace

(make-command-table 'multiple-query-replace-drei-table :errorp nil)

(defun multiple-query-replace-find-next-match (mark re list)
  (multiple-value-bind (foundp start)
      (re-search-forward mark re)
    (when foundp
      (loop with buffer = (buffer mark)
	    for string in list
	    when (buffer-looking-at buffer start string)
	      do (return string)))))

(define-command (com-multiple-query-replace :name t :command-table search-table) ()
  "Prompts for pairs of strings, replacing the first with the second.
Entering an empty search string stops the prompting."
  (let ((strings
	 (loop for string1 = (accept 'string :prompt "Multiple Query Replace")
	       until (string= string1 "")
	       for string2
		 = (accept 'string
			   :prompt (format nil
					   "Replace ~A with"
					   string1))
	       collecting (cons string1 string2))))
    (multiple-query-replace strings)))

(define-command (com-multiple-query-replace-from-buffer :name t :command-table search-table)
    ((buffer 'buffer :prompt "Buffer with Query Repace strings"))
  (unless (member buffer (buffers *application-frame*))
    (beep)
    (display-message "~A not an existing buffer" (name buffer))
    (return-from com-multiple-query-replace-from-buffer nil))
  (let* ((contents (buffer-substring buffer 0 (1- (size buffer))))
	 (strings (loop with length = (length contents)
			with index = 0
			with start = 0
			while (< index length)
			do (loop until (>= index length)
				 while (whitespacep (syntax buffer)
                                                    (char contents index))
				 do (incf index))
			   (setf start index)
			   (loop until (>= index length)
				 until (whitespacep (syntax buffer)
                                                    (char contents index))
				 do (incf index))
			until (= start index)
			collecting (string-trim '(#\Space #\Tab #\Newline)
						 (subseq contents start index)))))
    (unless (evenp (length strings))
      (beep)
      (display-message "Uneven number of strings in ~A" (name buffer))
      (return-from com-multiple-query-replace-from-buffer nil))
    (multiple-query-replace (loop for (string1 string2) on strings by #'cddr
				  collect (cons string1 string2)))))

(define-command (com-query-exchange :name t :command-table search-table) ()
  "Prompts for two strings to exchange for one another."
  (let* ((string1 (accept 'string :prompt "Query Exchange"))
	 (string2 (accept 'string :prompt (format nil
						  "Exchange ~A and"
						  string1))))
    (multiple-query-replace (list (cons string1 string2) (cons string2 string1)))))

(defun multiple-query-replace (strings)
  (declare (special strings))
  (let* ((occurrences 0)
	 (search-strings (mapcar #'car strings))
	 (re (format nil "~{~A~^|~}" search-strings)))
    (declare (special occurrences re))
    (when strings
      (let* ((pane (current-window))
	     (point (point pane)) 
	     (found (multiple-query-replace-find-next-match point re search-strings)))
	(when found
	  (setf (query-replace-state pane)
		(make-instance 'query-replace-state
		   :string1 found
		   :string2 (cdr (assoc found strings :test #'string=)))
		(query-replace-mode pane)
		t)
	  (display-message "Replace ~A with ~A: "
			   (string1 (query-replace-state pane))
			   (string2 (query-replace-state pane)))
	  (simple-command-loop 'multiple-query-replace-drei-table
			       (query-replace-mode pane)
			       ((setf (query-replace-mode pane) nil))))))
    (display-message "Replaced ~D occurrence~:P" occurrences)))

(define-command (com-multiple-query-replace-replace
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings occurrences re))
  (let* ((pane (current-window))
	 (point (point pane)) 
	 (state (query-replace-state pane))
	 (string1 (string1 state))
	 (string1-length (length string1)))
    (backward-object point string1-length)
    (replace-one-string point string1-length (string2 state) (no-upper-p string1))
    (incf occurrences)
    (let ((found (multiple-query-replace-find-next-match
		  point
		  re
		  (mapcar #'car strings))))
      (cond ((null found) (setf (query-replace-mode pane) nil))
	    (t (setf (query-replace-state pane)
		     (make-instance 'query-replace-state
			:string1 found
			:string2 (cdr (assoc found strings :test #'string=))))
	       (display-message "Replace ~A with ~A: "
				(string1 (query-replace-state pane))
				(string2 (query-replace-state pane))))))))


(define-command (com-multiple-query-replace-replace-and-quit
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings occurrences))
  (let* ((pane (current-window))
	 (point (point pane))
	 (state (query-replace-state pane))
	 (string1 (string1 state))
	 (string1-length (length string1)))
    (backward-object point string1-length)
    (replace-one-string point string1-length (string2 state) (no-upper-p string1))
    (incf occurrences)
    (setf (query-replace-mode pane) nil)))

(define-command (com-multiple-query-replace-replace-all
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings occurrences re))
  (let* ((pane (current-window))
	 (point (point pane)) 
	 (found nil))
    (loop for state = (query-replace-state pane)
	  for string1 = (string1 state)
	  for string1-length = (length string1)
	  do (backward-object point string1-length)
	     (replace-one-string point
		     string1-length
		     (string2 state)
		     (no-upper-p string1))
	     (incf occurrences)
	     (setf found (multiple-query-replace-find-next-match
				  point
				  re
				  (mapcar #'car strings)))
	  while found
	  do (setf (query-replace-state pane)
		   (make-instance 'query-replace-state
		      :string1 found
		      :string2 (cdr (assoc found strings :test #'string=))))
	  finally (setf (query-replace-state pane) nil))))

(define-command (com-multiple-query-replace-skip
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings re))
  (let* ((pane (current-window))
	 (point (point pane))
	 (found (multiple-query-replace-find-next-match
		 point
		 re
		 (mapcar #'car strings))))
    (cond ((null found) (setf (query-replace-mode pane) nil))
	    (t (setf (query-replace-state pane)
		     (make-instance 'query-replace-state
			:string1 found
			:string2 (cdr (assoc found strings :test #'string=))))
	       (display-message "Replace ~A with ~A: "
				(string1 (query-replace-state pane))
				(string2 (query-replace-state pane)))))))

(defun multiple-query-replace-set-key (gesture command)
  (add-command-to-command-table command 'multiple-query-replace-drei-table
				:keystroke gesture
				:errorp nil))

(multiple-query-replace-set-key '(#\Newline) 'com-query-replace-exit)
(multiple-query-replace-set-key '(#\Space) 'com-multiple-query-replace-replace)
(multiple-query-replace-set-key '(#\Backspace) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\Rubout) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\q) 'com-query-replace-exit)
(multiple-query-replace-set-key '(#\y) 'com-multiple-query-replace-replace)
(multiple-query-replace-set-key '(#\n) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\.) 'com-multiple-query-replace-replace-and-quit)
(multiple-query-replace-set-key '(#\!) 'com-multiple-query-replace-replace-all)
