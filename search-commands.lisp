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

(define-command (com-isearch-append-char :name t :command-table isearch-climacs-table) ()
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
			      (search-string state)))))))

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
  (setf (isearch-mode (current-window)) nil))

(defun isearch-set-key (gesture command)
  (add-command-to-command-table command 'isearch-climacs-table
                                :keystroke gesture :errorp nil))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (isearch-set-key (code-char code) 'com-isearch-append-char))

(isearch-set-key '(#\Newline) 'com-isearch-exit)
(isearch-set-key '(#\Backspace) 'com-isearch-delete-char)
(isearch-set-key '(#\s :control) 'com-isearch-search-forward)
(isearch-set-key '(#\r :control) 'com-isearch-search-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Query replace

(make-command-table 'query-replace-climacs-table :errorp nil)

(defun query-replace-find-next-match (mark string)
  (flet ((object-equal (x y)
           (and (characterp x)
                (characterp y)
                (char-equal x y))))
    (let ((offset-before (offset mark)))
      (search-forward mark string :test #'object-equal)
      (/= (offset mark) offset-before))))

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

(set-key 'com-query-replace
	 'search-table
	 '((#\% :shift :meta)))

(define-command (com-query-replace-replace :name t :command-table query-replace-climacs-table) ()
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
      (unless (find-if #'upper-case-p string1)
        (case region-case
          (:upper-case (upcase-buffer-region buffer offset1 offset2))
          (:lower-case (downcase-buffer-region buffer offset1 offset2))
          (:capitalized (capitalize-buffer-region buffer offset1 offset2)))))
    (incf occurrences)
    (if (query-replace-find-next-match point string1)
	(display-message "Query Replace ~A with ~A:"
		       string1 string2)
	(setf (query-replace-mode pane) nil))))

(define-command (com-query-replace-skip :name t :command-table query-replace-climacs-table) ()
  (declare (special string1 string2))
  (let* ((pane (current-window))
         (point (point pane)))
    (if (query-replace-find-next-match point string1)
	(display-message "Query Replace ~A with ~A:"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Regex search

(define-command (com-regex-search-forward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-forward (point (current-window)) string)))

(define-command (com-regex-search-backward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search backward"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-backward (point (current-window)) string)))
