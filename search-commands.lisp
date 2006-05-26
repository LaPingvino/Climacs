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

;;; Search commands for the Climacs editor. 

(in-package :climacs-gui)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; String search

(define-command (com-string-search :name t :command-table search-table)
    ((string 'string :prompt "String Search"))
  "Prompt for a string and search forward for it.
If found, leaves point after string. If not, leaves point where it is."
  (let* ((pane (current-window))
	 (point (point pane)))
    (search-forward point string :test (case-relevant-test string))))

(define-command (com-reverse-string-search :name t :command-table search-table)
    ((string 'string :prompt "Reverse String Search"))
  "Prompt for a string and search backward for it.
If found, leaves point before string. If not, leaves point where it is."
  (let* ((pane (current-window))
	 (point (point pane)))
    (search-backward point string :test (case-relevant-test string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Word search

(define-command (com-word-search :name t :command-table search-table)
    ((word 'string :prompt "Search word"))
  "Prompt for a whitespace delimited word and search forward for it.
If found, leaves point after the word. If not, leaves point where it is."
  (let* ((pane (current-window))
	 (point (point pane)))
    (climacs-base::search-word-forward point word)))

(define-command (com-reverse-word-search :name t :command-table search-table)
    ((word 'string :prompt "Search word"))
  "Prompt for a whitespace delimited word and search backward for it.
If found, leaves point before the word. If not, leaves point where it is."
  (let* ((pane (current-window))
	 (point (point pane)))
    (climacs-base::search-word-backward point word)))

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
  (let* ((point (point pane))
	 (mark2 (clone-mark mark))
	 (success (funcall (if forwardp #'search-forward #'search-backward)
			   mark2
			   string
			   :test (case-relevant-test string))))
    (when success
      (setf (offset point) (offset mark2)
	    (offset mark) (if forwardp
			      (- (offset mark2) (length string))
			      (+ (offset mark2) (length string)))))
    (display-message "~:[Failing ~;~]Isearch~:[ backward~;~]: ~A"
		     success forwardp (display-string string))
    (push (make-instance 'isearch-state
	     :search-string string
	     :search-mark mark
	     :search-forward-p forwardp
	     :search-success-p success)
	  (isearch-states pane))
    (unless success
      (beep))))

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

(defun isearch-append-char (char)
  (let* ((pane (current-window))
         (states (isearch-states pane))
         (string (concatenate 'string
                              (search-string (first states))
                              (string char)))
         (mark (clone-mark (search-mark (first states))))
         (forwardp (search-forward-p (first states))))
    (unless (or forwardp (end-of-buffer-p mark))
      (incf (offset mark)))
    (isearch-from-mark pane mark string forwardp)))

(define-command (com-isearch-append-char :name t :command-table isearch-climacs-table) ()
  (isearch-append-char *current-gesture*))

(define-command (com-isearch-append-newline :name t :command-table isearch-climacs-table) ()
  (isearch-append-char #\Newline))

(defun isearch-append-text (movement-function)
  (let* ((pane (current-window))
	 (states (isearch-states pane))
	 (buffer (buffer pane))
	 (point (point pane))
	 (start (clone-mark point))
	 (mark (clone-mark (search-mark (first states))))
	 (forwardp (search-forward-p (first states))))
    (funcall movement-function point)
    (let* ((start-offset (offset start))
	   (point-offset (offset point))
	   (string (concatenate 'string
				(search-string (first states))
				(buffer-substring buffer
						  start-offset
						  point-offset))))
      (unless (or forwardp (end-of-buffer-p mark))
	(incf (offset mark) (- point-offset start-offset)))
      (isearch-from-mark pane mark string forwardp))))

(define-command (com-isearch-append-word :name t :command-table isearch-climacs-table) ()
  (isearch-append-text #'forward-word))

(define-command (com-isearch-append-line :name t :command-table isearch-climacs-table) ()
  (isearch-append-text #'end-of-line))

(define-command (com-isearch-append-kill :name t :command-table isearch-climacs-table) ()
  (let* ((pane (current-window))
	 (states (isearch-states pane))
	 (yank (kill-ring-yank *kill-ring*))
	 (string (concatenate 'string
			      (search-string (first states))
			      yank))
	 (mark (clone-mark (search-mark (first states))))
	 (forwardp (search-forward-p (first states))))
    (unless (or forwardp (end-of-buffer-p mark))
      (incf (offset mark) (length yank)))
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
			      (display-string (search-string state))))))))

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
  (let* ((pane (current-window))
	 (states (isearch-states pane))
	 (string (search-string (first states)))
	 (search-forward-p (search-forward-p (first states))))
    (setf (isearch-mode pane) nil)
    (when (string= string "")
      (execute-frame-command *application-frame*
			     (funcall
			      *partial-command-parser*
			      (frame-command-table *application-frame*)
			      (frame-standard-input *application-frame*)
			      (if search-forward-p
				  `(com-string-search ,*unsupplied-argument-marker*)
				  `(com-reverse-string-search ,*unsupplied-argument-marker*))
			      0)))))

(defun isearch-set-key (gesture command)
  (add-command-to-command-table command 'isearch-climacs-table
                                :keystroke gesture :errorp nil))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (isearch-set-key (code-char code) 'com-isearch-append-char))

(isearch-set-key '(#\Newline) 'com-isearch-exit)
(isearch-set-key '(#\Backspace) 'com-isearch-delete-char)
(isearch-set-key '(#\s :control) 'com-isearch-search-forward)
(isearch-set-key '(#\r :control) 'com-isearch-search-backward)
(isearch-set-key '(#\j :control) 'com-isearch-append-newline)
(isearch-set-key '(#\w :control) 'com-isearch-append-word)
(isearch-set-key '(#\y :control) 'com-isearch-append-line)
(isearch-set-key '(#\y :meta) 'com-isearch-append-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Unconditional replace

(defun replace-one-string (mark length newstring &optional (use-region-case t))
  "Replace LENGTH objects at MARK with NEWSTRING,
using the case of those objects if USE-REGION-CASE is true."
  (let* ((start (offset mark))
	 (end (+ start length))
	 (region-case (and use-region-case
			   (buffer-region-case (buffer mark)
					       start
					       end)))) 
    (delete-range mark length)
    (insert-sequence mark newstring)
    (when (and use-region-case region-case)
      (let ((buffer (buffer mark))
	    (end2 (+ start (length newstring))))
	(funcall (case region-case
		   (:upper-case #'upcase-buffer-region)
		   (:lower-case #'downcase-buffer-region)
		   (:capitalized #'capitalize-buffer-region))
		 buffer
		 start
		 end2)))))

(define-command (com-replace-string :name t :command-table search-table)
    ()
  "Replace all occurrences of `string' with `newstring'."
  ;; We have to do it this way if we want to refer to STRING in NEWSTRING
  (let* ((string (accept 'string :prompt "Replace String"))
	 (newstring (accept'string :prompt (format nil "Replace ~A with" string))))
    (loop with point = (point (current-window))
	  with length = (length string)
	  with use-region-case = (no-upper-p string)
	  for occurrences from 0
	  while (query-replace-find-next-match point string)
	  do (backward-object point length)
	     (replace-one-string point length newstring use-region-case)
	  finally (display-message "Replaced ~A occurrence~:P" occurrences))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Query replace

(make-command-table 'query-replace-climacs-table :errorp nil)

(defun query-replace-find-next-match (mark string)
  (let ((offset-before (offset mark)))
    (search-forward mark string :test (case-relevant-test string))
    (/= (offset mark) offset-before)))

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
				  :prompt (format nil "Replace ~A with"
						  string1)
				  :default old-string2
				  :default-type 'string)
			  (accept 'string
				  :prompt (format nil "Replace ~A with" string1)))
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
      (display-message "Replace ~A with ~A:"
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
         (string1-length (length string1)))
    (backward-object point string1-length)
    (replace-one-string point string1-length string2 (no-upper-p string1))
    (incf occurrences)
    (if (query-replace-find-next-match point string1)
	(display-message "Replace ~A with ~A:"
		       string1 string2)
	(setf (query-replace-mode pane) nil))))

(define-command (com-query-replace-replace-and-quit
		 :name t
		 :command-table query-replace-climacs-table)
    ()
  (declare (special string1 string2 occurrences))
  (let* ((pane (current-window))
	 (point (point pane))
	 (string1-length (length string1)))
    (backward-object point string1-length)
    (replace-one-string point string1-length string2 (no-upper-p string1))
    (incf occurrences)
    (setf (query-replace-mode pane) nil)))

(define-command (com-query-replace-replace-all
		 :name t
		 :command-table query-replace-climacs-table)
    ()
  (declare (special string1 string2 occurrences))
  (let* ((pane (current-window))
	 (point (point pane))
	 (string1-length (length string1)))
    (loop do (backward-object point string1-length)
	     (replace-one-string point string1-length string2 (no-upper-p string1))
	     (incf occurrences)
	  while (query-replace-find-next-match point string1)
	  finally (setf (query-replace-mode pane) nil))))

(define-command (com-query-replace-skip :name t :command-table query-replace-climacs-table) ()
  (declare (special string1 string2))
  (let* ((pane (current-window))
         (point (point pane)))
    (if (query-replace-find-next-match point string1)
	(display-message "Replace ~A with ~A:"
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
(query-replace-set-key '(#\.) 'com-query-replace-replace-and-quit)
(query-replace-set-key '(#\!) 'com-query-replace-replace-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Regex search

(defparameter *whitespace-regex* (format nil "[~@{~A~}]+" #\Space #\Tab))

(defun normalise-minibuffer-regex (string)
  "Massages the regex STRING given to the minibuffer."
  (with-output-to-string (result)
    (loop for char across string
	  if (char= char #\Space)
	    do (princ *whitespace-regex* result)
	  else
	    do (princ char result))))

(define-command (com-regex-search-forward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-forward
     (point (current-window))
     (normalise-minibuffer-regex string))))

(define-command (com-regex-search-backward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search backward"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-backward
     (point (current-window))
     (normalise-minibuffer-regex string))))

(define-command (com-how-many :name t :command-table search-table)
    ((regex 'string :prompt "How many matches for"))
  (let* ((re (normalise-minibuffer-regex regex))
	 (mark (clone-mark (point (current-window))))
	 (occurrences (loop for count from 0
			    while (re-search-forward mark re)
			    finally (return count))))
    (display-message "~A occurrence~:P" occurrences)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Multiple query replace

(make-command-table 'multiple-query-replace-climacs-table :errorp nil)

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
				 while (whitespacep (char contents index))
				 do (incf index))
			   (setf start index)
			   (loop until (>= index length)
				 until (whitespacep (char contents index))
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
	  (simple-command-loop 'multiple-query-replace-climacs-table
			       (query-replace-mode pane)
			       ((setf (query-replace-mode pane) nil))))))
    (display-message "Replaced ~D occurrence~:P" occurrences)))

(define-command (com-multiple-query-replace-replace
		 :name t
		 :command-table multiple-query-replace-climacs-table)
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
		 :command-table multiple-query-replace-climacs-table)
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
		 :command-table multiple-query-replace-climacs-table)
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
		 :command-table multiple-query-replace-climacs-table)
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
  (add-command-to-command-table command 'multiple-query-replace-climacs-table
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