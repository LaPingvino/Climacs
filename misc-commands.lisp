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

;;; miscellaneous commands for the Climacs editor. 

(in-package :climacs-gui)

(define-command (com-overwrite-mode :name t :command-table editing-table) ()
  "Toggle overwrite mode for the current mode.
When overwrite is on, an object entered on the keyboard will replace the object after the point. When overwrite is off (the default), objects are inserted at point. In both cases point is positioned after the new object."
  (with-slots (overwrite-mode) (current-window)
    (setf overwrite-mode (not overwrite-mode))))

(set-key 'com-overwrite-mode
	 'editing-table
	 '((:insert)))

(define-command (com-not-modified :name t :command-table buffer-table) ()
  "Clear the modified flag for the current buffer.
The modified flag is automatically set when the contents of the buffer are changed. This flag is consulted, for instance, when deciding whether to prompt you to save the buffer before killing it."
  (setf (needs-saving (buffer (current-window))) nil))

(set-key 'com-not-modified
	 'buffer-table
	 '((#\~ :meta :shift)))

(define-command (com-set-fill-column :name t :command-table fill-table)
    ((column 'integer :prompt "Column Number:"))
  "Set the fill column to the specified value.
You must supply a numeric argument. The fill column is the column beyond which automatic line-wrapping will occur. The default fill column is 70."
  (set-fill-column column))

(set-key `(com-set-fill-column ,*numeric-argument-marker*)
	 'fill-table
	 '((#\x :control) (#\f)))

(defun set-fill-column (column)
  (if (> column 1)
      (setf (auto-fill-column (current-window)) column)
      (progn (beep) (display-message "Set Fill Column requires an explicit argument."))))

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
  (let* ((window (current-window))
	 (point (point window)))
    (unless (constituentp char)
      (possibly-expand-abbrev point))
    (when (whitespacep char)
      (possibly-fill-line))
    (if (and (slot-value window 'overwrite-mode) (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point char))
	(insert-object point char))))

(define-command com-self-insert ((count 'integer))
  (loop repeat count do (insert-character *current-gesture*)))

(define-command (com-beginning-of-line :name t :command-table movement-table) ()
  "Move point to the beginning of the current line."
  (beginning-of-line (point (current-window))))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((:home)))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((#\a :control)))

(define-command (com-end-of-line :name t :command-table movement-table) ()
  "Move point to the end of the current line."
  (end-of-line (point (current-window))))

(set-key 'com-end-of-line
	 'movement-table
	 '((#\e :control)))

(set-key 'com-end-of-line
	 'movement-table
	 '((:end)))

(define-command (com-delete-object :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of Objects")
     (killp 'boolean :prompt "Kill?"))
  "Delete the object after point.
With a numeric argument, kill that many objects after (or before, if negative) point."
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (forward-object mark count)
    (when killp
      (kill-ring-standard-push *kill-ring*
			       (region-to-sequence point mark)))
    (delete-region point mark)))

(set-key `(com-delete-object ,*numeric-argument-marker*
			     ,*numeric-argument-p*)
	 'deletion-table
	 '(#\Rubout))

(set-key `(com-delete-object ,*numeric-argument-marker*
			     ,*numeric-argument-p*)
	 'deletion-table
	 '((#\d :control)))

(define-command (com-backward-delete-object :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of Objects")
     (killp 'boolean :prompt "Kill?"))
  "Delete the object before point.
With a numeric argument, kills that many objects before (or after, if negative) point."
  (let* ((point (point (current-window)))
	 (mark (clone-mark point)))
    (backward-object mark count)
    (when killp
      (kill-ring-standard-push *kill-ring*
			       (region-to-sequence mark point)))
  (delete-region mark point)))

(set-key `(com-backward-delete-object ,*numeric-argument-marker*
				      ,*numeric-argument-p*)
	 'deletion-table
	 '(#\Backspace))

(define-command (com-zap-to-object :name t :command-table deletion-table) ()
  "Prompt for an object and kill the objects between point and the next occurence of that object after point.
Characters can be entered in #\ format."
  (let* ((item (handler-case (accept 't :prompt "Zap to Object")
		(error () (progn (beep)
				 (display-message "Not a valid object")
				 (return-from com-zap-to-object nil)))))
	 (current-point (point (current-window)))
	 (item-mark (clone-mark current-point))
	 (current-offset (offset current-point)))
    (search-forward item-mark (vector item))
    (delete-range current-point (- (offset item-mark) current-offset))))

(define-command (com-zap-to-character :name t :command-table deletion-table) ()
  "Prompt for a character and kill the objects between point and the next occurence of that character after point.
FIXME: Accepts a string (that is, zero or more characters) terminated by a #\NEWLINE. If a zero length string signals an error. If a string of length >1, uses the first character of the string."
  (let* ((item-string (handler-case (accept 'string :prompt "Zap to Character") ; Figure out how to get #\d and d.  (or 'string 'character)?
		(error () (progn (beep)
				 (display-message "Not a valid string. ")
				 (return-from com-zap-to-character nil)))))
       (item (subseq item-string 0 1))
       (current-point (point (current-window)))
       (item-mark (clone-mark current-point))

       (current-offset (offset current-point)))
  (if (> (length item-string) 1)
      (display-message "Using just the first character"))
  (search-forward item-mark item)
  (delete-range current-point (- (offset item-mark) current-offset))))

(set-key 'com-zap-to-character
	 'deletion-table
	 '((#\z :meta)))

(defun transpose-objects (mark)
  (unless (beginning-of-buffer-p mark)
    (when (end-of-line-p mark)
      (backward-object mark))
    (let ((object (object-after mark)))
      (delete-range mark)
      (backward-object mark)
      (insert-object mark object)
      (forward-object mark))))

(define-command (com-transpose-objects :name t :command-table editing-table) ()
  "Transpose the objects before and after point, advancing point.
At the end of a line transpose the previous two objects without advancing point. At the beginning of the buffer do nothing. At the beginning of any line other than the first effectively move the first object of that line to the end of the previous line.
FIXME: at the end of a single object line at the beginning of the buffer deletes that object."
  (transpose-objects (point (current-window))))

(set-key 'com-transpose-objects
	 'editing-table
	 '((#\t :control)))

(define-command (com-backward-object :name t :command-table movement-table)
    ((count 'integer :prompt "Number of Objects"))
  "Move point backward one object.
With a numeric argument, move point backward (or forward, if negative) that number of objects."
  (backward-object (point (current-window)) count))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :control)))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :left #-mcclim :left-arrow)))

(define-command (com-forward-object :name t :command-table movement-table)
    ((count 'integer :prompt "Number of Objects"))
  "Move point forward one object.
With a numeric argument, move point forward (or backward, if negative) that number of objects."
  (forward-object (point (current-window)) count))

(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :control)))

(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :right #-mcclim :right-arrow)))

(defun transpose-words (mark)
  (let (bw1 bw2 ew1 ew2)
    (backward-word mark)
    (setf bw1 (offset mark))
    (forward-word mark)
    (setf ew1 (offset mark))
    (forward-word mark)
    (when (= (offset mark) ew1)
      ;; this is emacs' message in the minibuffer
      (error "Don't have two things to transpose"))
    (setf ew2 (offset mark))
    (backward-word mark)
    (setf bw2 (offset mark))
    (let ((w2 (buffer-sequence (buffer mark) bw2 ew2))
	  (w1 (buffer-sequence (buffer mark) bw1 ew1)))
      (delete-word mark)
      (insert-sequence mark w1)
      (backward-word mark)
      (backward-word mark)
      (delete-word mark)
      (insert-sequence mark w2)
      (forward-word mark))))

(define-command (com-transpose-words :name t :command-table editing-table) ()
  "Transpose the words around point, leaving point at the end of them.
With point in the whitespace between words, transpose the words before and after point. With point inside a word, transpose that word with the next one. With point before the first word of the buffer, transpose the first two words of the buffer.
FIXME: with point after the penultimate word of the buffer, or if there are <2 words in the buffer, Strange Things (TM) happen (including breaking Climacs)."
  (transpose-words (point (current-window))))

(set-key 'com-transpose-words
	 'editing-table
	 '((#\t :meta)))

(defun transpose-lines (mark)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (previous-line mark))
  (let* ((bol (offset mark))
	 (eol (progn (end-of-line mark)
		     (offset mark)))
	 (line (buffer-sequence (buffer mark) bol eol)))
    (delete-region bol mark)
    ;; Remove newline at end of line as well.
    (unless (end-of-buffer-p mark)
      (delete-range mark))
    ;; If the current line is at the end of the buffer, we want to
    ;; be able to insert past it, so we need to get an extra line
    ;; at the end.
    (end-of-line mark)
    (when (end-of-buffer-p mark)
      (insert-object mark #\Newline))
    (next-line mark 0)
    (insert-sequence mark line)
    (insert-object mark #\Newline)))

(define-command (com-transpose-lines :name t :command-table editing-table) ()
  "Transpose current line and previous line, leaving point at the end of them.
If point is in the first line, transpose the first two lines. If point is in the last line of the buffer and there is no final #\Newline, add one."
  (transpose-lines (point (current-window))))

(set-key 'com-transpose-lines
	 'editing-table
	 '((#\x :control) (#\t :control)))

(define-command (com-previous-line :name t :command-table movement-table)
    ((numarg 'integer :prompt "How many lines?"))
  "Move point to the previous line.
With a numeric argument, move point up (down, if negative) that many lines. Successive line movement commands seek to respect the 'goal column'."
  (let* ((window (current-window))
	 (point (point window)))
    (unless (or (eq (previous-command window) 'com-previous-line)
		(eq (previous-command window) 'com-next-line))
      (setf (slot-value window 'goal-column) (column-number point)))
    (if (plusp numarg)
	(previous-line point (slot-value window 'goal-column) numarg)
	(next-line point (slot-value window 'goal-column) (- numarg)))))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\p :control)))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :up #-mcclim :up-arrow)))

(define-command (com-next-line :name t :command-table movement-table)
    ((numarg 'integer :prompt "How many lines?"))
  "Move point to the next line.
With a numeric argument, move point down (up, if negative) that many lines. Successive line movement commands seek to respect the 'goal column'."
  (let* ((window (current-window))
	 (point (point window)))
    (unless (or (eq (previous-command window) 'com-previous-line)
		(eq (previous-command window) 'com-next-line))
      (setf (slot-value window 'goal-column) (column-number point)))
    (if (plusp numarg)
	(next-line point (slot-value window 'goal-column) numarg)
	(previous-line point (slot-value window 'goal-column) (- numarg)))))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n :control)))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :down #-mcclim :down-arrow)))

(define-command (com-open-line :name t :command-table editing-table)
    ((numarg 'integer :prompt "How many lines?"))
  "Insert a #\Newline and leave point before it.
With a numeric argument greater than 1, insert that many #\Newlines."
  (open-line (point (current-window)) numarg))

(set-key `(com-open-line ,*numeric-argument-marker*)
	 'editing-table
	 '((#\o :control)))

(defun kill-line (mark &optional (count 1) (whole-lines-p nil) (concatenate-p nil))
  (let ((start (offset mark)))
    (cond ((= 0 count)
	   (beginning-of-line mark))
	  ((< count 0)
	   (loop repeat (- count)
		 until (beginning-of-buffer-p mark)
		 do (beginning-of-line mark)
		 until (beginning-of-buffer-p mark)
		 do (backward-object mark)))
	  ((or whole-lines-p (> count 1))
	   (loop repeat count
		 until (end-of-buffer-p mark)
		 do (end-of-line mark)
		 until (end-of-buffer-p mark)
		 do (forward-object mark)))
	  (t
	   (cond ((end-of-buffer-p mark) nil)
		 ((end-of-line-p mark)(forward-object mark))
		 (t (end-of-line mark)))))
    (unless (mark= mark start)
      (if concatenate-p
	  (kill-ring-concatenating-push *kill-ring*
					(region-to-sequence start mark))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence start mark)))
      (delete-region start mark))))

(define-command (com-kill-line :name t :command-table deletion-table)
    ((numarg 'integer :prompt "Kill how many lines?")
     (numargp 'boolean :prompt "Kill entire lines?"))
  "Kill the objects on the current line after point.
When at the end of a line, kill the #\Newline. With a numeric argument of 0, kill the objects on the current line before point. With a non-zero numeric argument, kill that many lines forward (backward, if negative) from point. Successive kills append to the kill ring."
  (let* ((pane (current-window))
	 (point (point pane))
         (concatenate-p (eq (previous-command pane) 'com-kill-line)))
    (kill-line point numarg numargp concatenate-p)))	   

(set-key `(com-kill-line ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'deletion-table
	 '((#\k :control)))

(define-command (com-forward-word :name t :command-table movement-table)
    ((count 'integer :prompt "Number of words"))
  "Move point to the next word end.
With a numeric argument, move point forward (backward, if negative) that many words."
  (if (plusp count)
      (forward-word (point (current-window)) count)
      (backward-word (point (current-window)) (- count))))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :meta)))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :right #-mcclim :right-arrow :control)))

(define-command (com-backward-word :name t :command-table movement-table)
    ((count 'integer :prompt "Number of words"))
  "Move point to the previous word beginning.
With a numeric argument, move point backward (forward, if negative) that many words."
  (backward-word (point (current-window)) count))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :meta)))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :left #-mcclim :left-arrow :control)))

(define-command (com-delete-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  "Delete from point until the next word end.
With a positive numeric argument, delete that many words forward."
  (delete-word (point (current-window)) count))

(defun kill-word (mark &optional (count 1) (concatenate-p nil))
  (let ((start (offset mark)))
    (if (plusp count)
	(loop repeat count
	      until (end-of-buffer-p mark)
	      do (forward-word mark))
	(loop repeat (- count)
	      until (beginning-of-buffer-p mark)
	      do (backward-word mark)))
    (unless (mark= mark start)
      (if concatenate-p
	  (if (plusp count)
	      (kill-ring-concatenating-push *kill-ring*
					(region-to-sequence start mark))
	      (kill-ring-reverse-concatenating-push *kill-ring*
						    (region-to-sequence start mark)))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence start mark)))
      (delete-region start mark))))

(define-command (com-kill-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  "Kill from point until the next word end.
With a numeric argument, kill forward (backward, if negative) that many words. Successive kills append to the kill ring."
  (let* ((pane (current-window))
	 (point (point pane))
	 (concatenate-p (eq (previous-command pane) 'com-kill-word)))
    (kill-word point count concatenate-p)))

(set-key `(com-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\d :meta)))

(define-command (com-backward-kill-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  "Kill from point until the previous word beginning.
With a numeric argument, kill backward (forward, if negative) that many words. Successive kills append to the kill ring."
  (let* ((pane (current-window))
	 (point (point pane))
	 (concatenate-p (eq (previous-command pane) 'com-backward-kill-word)))
    (kill-word point (- count) concatenate-p)))

(set-key `(com-backward-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Backspace :meta)))

(define-command (com-mark-word :name t :command-table marking-table)
    ((count 'integer :prompt "Number of words"))
  "Place mark at the next word end.
With a positive numeric argument, place mark at the end of that many words forward. With a negative numeric argument, place mark at the beginning of that many words backward. Successive invocations extend the selection."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane)))
    (unless (eq (previous-command pane) 'com-mark-word)
      (setf (offset mark) (offset point)))
    (if (plusp count)
	(forward-word mark count)
	(backward-word mark (- count)))))

(set-key `(com-mark-word ,*numeric-argument-marker*)
	 'marking-table
	 '((#\@ :meta :shift)))

(define-command (com-backward-delete-word :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of words"))
  "Delete from point until the previous word beginning.
With a positive numeric argument, delete that many words backward."
  (backward-delete-word (point (current-window)) count))

(define-command (com-upcase-region :name t :command-table case-table) ()
  "Convert the region to upper case."
  (let ((cw (current-window)))
    (upcase-region (mark cw) (point cw))))

(define-command (com-downcase-region :name t :command-table case-table) ()
  "Convert the region to lower case."
  (let ((cw (current-window)))
    (downcase-region (mark cw) (point cw))))

(define-command (com-capitalize-region :name t :command-table case-table) ()
  "Capitalize each word in the region."
  (let ((cw (current-window)))
    (capitalize-region (mark cw) (point cw))))

(define-command (com-upcase-word :name t :command-table case-table) ()
  "Convert the characters from point until the next word end to upper case.
Leave point at the word end."
  (upcase-word (point (current-window))))

(set-key 'com-upcase-word
	 'case-table
	 '((#\u :meta)))

(define-command (com-downcase-word :name t :command-table case-table) ()
  "Convert the characters from point until the next word end to lower case.
Leave point at the word end."
  (downcase-word (point (current-window))))

(set-key 'com-downcase-word
	 'case-table
	 '((#\l :meta)))

(define-command (com-capitalize-word :name t :command-table case-table) ()
  "Capitalize the next word.
If point is in a word, convert the next character to upper case and the remaining letters in the word to lower case. If point is before the start of a word, convert the first character of that word to upper case and the rest of the letters to lower case. Leave point at the word end."
  (capitalize-word (point (current-window))))

(set-key 'com-capitalize-word
	 'case-table
	 '((#\c :meta)))

(define-command (com-tabify-region :name t :command-table editing-table) ()
  "Replace runs of spaces with tabs in region where possible.
Uses TAB-SPACE-COUNT of the STREAM-DEFAULT-VIEW of the pane."
  (let ((pane (current-window)))
    (tabify-region
     (mark pane) (point pane) (tab-space-count (stream-default-view pane)))))

(define-command (com-untabify-region :name t :command-table editing-table) ()
  "Replace tabs with equivalent runs of spaces in the region.
Uses TAB-SPACE-COUNT of the STREAM-DEFAULT-VIEW of the pane."
  (let ((pane (current-window)))
    (untabify-region
     (mark pane) (point pane) (tab-space-count (stream-default-view pane)))))

(defun indent-current-line (pane point)
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (indentation (syntax-line-indentation point
                                               tab-space-count
                                               (syntax buffer))))
    (indent-line point indentation (and (indent-tabs-mode buffer)
                                        tab-space-count))))

(define-command (com-indent-line :name t :command-table indent-table) ()
  (let* ((pane (current-window))
         (point (point pane)))
    (indent-current-line pane point)))

(set-key 'com-indent-line
	 'indent-table
	 '((#\Tab)))

(set-key 'com-indent-line
	 'indent-table
	 '((#\i :control)))

(define-command (com-newline-and-indent :name t :command-table indent-table) ()
  (let* ((pane (current-window))
	 (point (point pane)))
    (insert-object point #\Newline)
    (indent-current-line pane point)))

(set-key 'com-newline-and-indent
	 'indent-table
	 '((#\j :control)))

(defun indent-region (pane mark1 mark2)
  "Indent all lines in the region delimited by `mark1' and `mark2'
   according to the rules of the active syntax in `pane'."
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (tab-width (and (climacs-pane:indent-tabs-mode buffer)
                         tab-space-count))
         (syntax (climacs-syntax:syntax buffer)))
    (do-buffer-region-lines (line mark1 mark2)
      (let ((indentation (climacs-syntax:syntax-line-indentation  
                          line
                          tab-space-count
                          syntax)))
        (indent-line line indentation tab-width))
      ;; We need to update the syntax every time we perform an
      ;; indentation, so that subsequent indentations will be
      ;; correctly indented (this matters in list forms). FIXME: This
      ;; should probably happen automatically.
      (update-syntax buffer syntax))))

(define-command (com-indent-region :name t :command-table indent-table) ()
  "Indent every line of the current region as specified by the
syntax for the buffer."
  (let* ((pane (current-window))
         (point (point pane))
         (mark (mark pane)))
    (indent-region pane point mark)))

(define-command (com-delete-indentation :name t :command-table indent-table) ()
  "Join current line to previous non-blank line.
Leaves a single space between the last non-whitespace object of the previous line and the first non-whitespace object of the current line, and point after that space. If there is no previous non-blank line, deletes all whitespace at the beginning of the buffer at leaves point there."
  (delete-indentation (point (current-window))))

(set-key 'com-delete-indentation
	 'indent-table
	 '((#\^ :shift :meta)))

(define-command (com-auto-fill-mode :name t :command-table fill-table) ()
  (let ((pane (current-window)))
    (setf (auto-fill-mode pane) (not (auto-fill-mode pane)))))

(define-command (com-fill-paragraph :name t :command-table fill-table) ()
  (let* ((pane (current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer))
	 (point (point pane))
         (begin-mark (clone-mark point))
         (end-mark (clone-mark point)))
    (unless (eql (object-before begin-mark) #\Newline)
      (backward-paragraph begin-mark syntax))
    (unless (eql (object-after end-mark) #\Newline)
      (forward-paragraph end-mark syntax))
    (do-buffer-region (object offset buffer
                       (offset begin-mark) (offset end-mark))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (let ((point-backup (clone-mark point)))
      (setf (offset point) (offset end-mark))
      (possibly-fill-line)
      (setf (offset point) (offset point-backup)))))

(set-key 'com-fill-paragraph
	 'fill-table
	 '((#\q :meta)))

(define-command (com-beginning-of-buffer :name t :command-table movement-table) ()
  "Move point to the beginning of the buffer."
  (beginning-of-buffer (point (current-window))))

(set-key 'com-beginning-of-buffer
	 'movement-table
	 '((#\< :shift :meta)))

(set-key 'com-beginning-of-buffer
	 'movement-table
	 '((:home :control)))

(define-command (com-page-down :name t :command-table movement-table) ()
  (let ((pane (current-window)))
    (page-down pane)))

(set-key 'com-page-down
	 'movement-table
	 '((#\v :control)))

(set-key 'com-page-down
	 'movement-table
	 '((:next)))

(define-command (com-page-up :name t :command-table movement-table) ()
  (let ((pane (current-window)))
    (page-up pane)))

(set-key 'com-page-up
	 'movement-table
	 '((#\v :meta)))

(set-key 'com-page-up
	 'movement-table
	 '((:prior)))

(define-command (com-end-of-buffer :name t :command-table movement-table) ()
  "Move point to the end of the buffer."
  (end-of-buffer (point (current-window))))

(set-key 'com-end-of-buffer
	 'movement-table
	 '((#\> :shift :meta)))

(set-key 'com-end-of-buffer
	 'movement-table
	 '((:end :control)))

(define-command (com-mark-whole-buffer :name t :command-table marking-table) ()
  "Place point at the beginning and mark at the end of the buffer."
  (beginning-of-buffer (point (current-window)))
  (end-of-buffer (mark (current-window))))

(set-key 'com-mark-whole-buffer
	 'marking-table
	 '((#\x :control) (#\h)))

(defun back-to-indentation (mark)
  (beginning-of-line mark)
  (loop until (end-of-line-p mark)
	while (whitespacep (object-after mark))
	do (forward-object mark)))

(define-command (com-back-to-indentation :name t :command-table movement-table) ()
  "Move point to the first non-whitespace object on the current line.
If there is no non-whitespace object, leaves point at the end of the line."
  (back-to-indentation (point (current-window))))

(set-key 'com-back-to-indentation
	 'movement-table
	 '((#\m :meta)))

(defun delete-horizontal-space (mark &optional (backward-only-p nil))
  (let ((mark2 (clone-mark mark)))
    (loop until (beginning-of-line-p mark)
	  while (whitespacep (object-before mark))
	  do (backward-object mark))
    (unless backward-only-p
      (loop until (end-of-line-p mark2)
	    while (whitespacep (object-after mark2))
	    do (forward-object mark2)))
    (delete-region mark mark2)))

(define-command (com-delete-horizontal-space :name t :command-table deletion-table)
    ((backward-only-p
      'boolean :prompt "Delete backwards only?"))
  "Delete whitespace around point.
With a numeric argument, only delete whitespace before point."
  (delete-horizontal-space (point (current-window)) backward-only-p))

(set-key `(com-delete-horizontal-space ,*numeric-argument-p*)
	 'deletion-table
	 '((#\\ :meta)))

(defun just-one-space (mark count)
  (let (offset)
    (loop until (beginning-of-line-p mark)
	  while (whitespacep (object-before mark))
	  do (backward-object mark))
    (loop until (end-of-line-p mark)
	  while (whitespacep (object-after mark))
	  repeat count do (forward-object mark)
	  finally (setf offset (offset mark)))
    (loop until (end-of-line-p mark)
	  while (whitespacep (object-after mark))
	  do (forward-object mark))
    (delete-region offset mark)))

(define-command (com-just-one-space :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of spaces"))
  "Delete whitespace around point, leaving a single space.
With a positive numeric argument, leave that many spaces.
FIXME: should distinguish between types of whitespace."
  (just-one-space (point (current-window)) count))

(set-key `(com-just-one-space ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Space :meta)))

(defun goto-position (mark pos)
  (setf (offset mark) pos))

(define-command (com-goto-position :name t :command-table movement-table) ()
  "Prompts for an integer, and sets the offset of point to that integer."
  (goto-position
   (point (current-window))
   (handler-case (accept 'integer :prompt "Goto Position")
     (error () (progn (beep)
		      (display-message "Not a valid position")
		      (return-from com-goto-position nil))))))  

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

(define-command (com-goto-line :name t :command-table movement-table) ()
  "Prompts for a line number, and sets point to the beginning of that line.
The first line of the buffer is 1. Giving a number <1 leaves point at the beginning of the buffer. Giving a line number larger than the number of the last line in the buffer leaves point at the beginning of the last line of the buffer."
  (goto-line (point (current-window))
	     (handler-case (accept 'integer :prompt "Goto Line")
		 (error () (progn (beep)
				  (display-message "Not a valid line number")
				  (return-from com-goto-line nil))))))

(define-command (com-browse-url :name t :command-table base-table) ()
  (let ((url (accept 'url :prompt "Browse URL")))
    #+ (and sbcl darwin)
    (sb-ext:run-program "/usr/bin/open" `(,url) :wait nil)
    #+ (and openmcl darwin)
    (ccl:run-program "/usr/bin/open" `(,url) :wait nil)))

(define-command (com-set-mark :name t :command-table marking-table) ()
  "Set mark to the current position of point."
  (let ((pane (current-window)))
    (setf (mark pane) (clone-mark (point pane)))))

(set-key 'com-set-mark
	 'marking-table
	 '((#\Space :control)))

(define-command (com-exchange-point-and-mark :name t :command-table marking-table) ()
  "Exchange the positions of point and mark."
  (let ((pane (current-window)))
    (psetf (offset (mark pane)) (offset (point pane))
	   (offset (point pane)) (offset (mark pane)))))

(set-key 'com-exchange-point-and-mark
	 'marking-table
	 '((#\x :control) (#\x :control)))

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

(define-command (com-set-syntax :name t :command-table buffer-table) ()
  "Prompts for a syntax to set for the current buffer.
Setting a syntax will cause the buffer to be reparsed using the new syntax."
  (let* ((pane (current-window))
	 (buffer (buffer pane)))
    (handler-case (set-syntax buffer (accept 'syntax :prompt "Set Syntax"))
                  (input-not-of-required-type 
                   (message)
                   (display-message "Invalid syntax: ~A." message)))))

;;;;;;;;;;;;;;;;;;;;
;; Kill ring commands

;; Copies an element from a kill-ring to a buffer at the given offset
(define-command (com-yank :name t :command-table editing-table) ()
  "Insert the objects most recently added to the kill ring at point."
  (insert-sequence (point (current-window)) (kill-ring-yank *kill-ring*)))

(set-key 'com-yank
	 'editing-table
	 '((#\y :control)))

;; Destructively cut a given buffer region into the kill-ring
(define-command (com-kill-region :name t :command-table editing-table) ()
  "Kill the objects between point and mark.
That is, push them onto the kill ring, and delete them from the buffer."
  (let ((pane (current-window)))
    (kill-ring-standard-push
     *kill-ring* (region-to-sequence (mark pane) (point pane)))
    (delete-region (mark pane) (point pane))))

(set-key 'com-kill-region
	 'editing-table
	 '((#\w :control)))

;; Non destructively copies buffer region to the kill ring
(define-command (com-copy-region :name t :command-table marking-table) ()
  "Copy the objects between point and mark to the kill ring."
  (let ((pane (current-window)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence (point pane) (mark pane)))))

(set-key 'com-copy-region
	 'marking-table
	 '((#\w :meta)))

(define-command (com-rotate-yank :name t :command-table editing-table) ()
  "Replace the immediately previously yanked objects with others.
Must be given immediately following a Yank or Rotate Yank command. The replacement objects are those before the previously yanked objects in the kill ring."
  (let* ((pane (current-window))
	 (point (point pane))
	 (last-yank (kill-ring-yank *kill-ring*)))
    (if (eq (previous-command pane)
	    'com-rotate-yank)
	(progn
	  (delete-range point (* -1 (length last-yank)))
	  (rotate-yank-position *kill-ring*)))
    (insert-sequence point (kill-ring-yank *kill-ring*))))

(set-key 'com-rotate-yank
	 'editing-table
	 '((#\y :meta)))

(define-command (com-resize-kill-ring :name t :command-table editing-table) ()
  "Prompt for a new size for the kill ring.
The default is 5. A number less than 5 will be replaced by 5."
  (let ((size (handler-case (accept 'integer :prompt "New kill ring size")
		(error () (progn (beep)
				 (display-message "Not a valid kill ring size")
				 (return-from com-resize-kill-ring nil))))))
    (setf (kill-ring-max-size *kill-ring*) size)))

(define-command (com-append-next-kill :name t :command-table editing-table) ()
  "Set the kill ring to append the next kill to the previous one."
  (setf (append-next-p *kill-ring*) t))

(set-key 'com-append-next-kill
	 'editing-table
	 '((#\w :control :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Undo/redo

(define-command (com-undo :name t :command-table editing-table) ()
  (handler-case (undo (undo-tree (buffer (current-window))))
    (no-more-undo () (beep) (display-message "No more undo")))
  (full-redisplay (current-window)))

(set-key 'com-undo
	 'editing-table
	 '((#\_ :shift :control)))

(set-key 'com-undo
	 'editing-table
	 '((#\x :control) (#\u)))

(define-command (com-redo :name t :command-table editing-table) ()
  (handler-case (redo (undo-tree (buffer (current-window))))
    (no-more-undo () (beep) (display-message "No more redo")))
  (full-redisplay (current-window)))

(set-key 'com-redo
	 'editing-table
	 '((#\_ :shift :meta)))

(set-key 'com-redo
	 'editing-table
	 '((#\x :control) (#\r :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dynamic abbrevs

(define-command (com-dabbrev-expand :name t :command-table editing-table) ()
  "Expand word before point dynamically.
Search from point (first backward to the beginning of the buffer, then forward) for words for which the word before point is a prefix, inserting each in turn at point as an expansion."
  (let* ((window (current-window))
	 (point (point window)))
    (with-slots (original-prefix prefix-start-offset dabbrev-expansion-mark) window
       (flet ((move () (cond ((beginning-of-buffer-p dabbrev-expansion-mark)
			      (setf (offset dabbrev-expansion-mark)
				    (offset point))
			      (forward-word dabbrev-expansion-mark))
			     ((mark< dabbrev-expansion-mark point)
			      (backward-object dabbrev-expansion-mark))
			     (t (forward-object dabbrev-expansion-mark)))))
	 (unless (or (beginning-of-buffer-p point)
		     (not (constituentp (object-before point))))
	   (unless (and (eq (previous-command window) 'com-dabbrev-expand)
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

(set-key 'com-dabbrev-expand
	 'editing-table
	 '((#\/ :meta)))

(define-command (com-backward-paragraph :name t :command-table movement-table)
    ((count 'integer :prompt "Number of paragraphs"))
  "Move point to the previous paragraph start.
With a numeric argument, move point backward (forward, if negative) that many paragraphs."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-paragraph point syntax))
	(loop repeat (- count) do (forward-paragraph point syntax)))))

(set-key `(com-backward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\{ :shift :meta)))

(define-command (com-forward-paragraph :name t :command-table movement-table)
    ((count 'integer :prompt "Number of paragraphs"))
  "Move point to the next paragraph end.
With a numeric argument, move point forward (backward, if negative) that many paragraphs."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-paragraph point syntax))
	(loop repeat (- count) do (backward-paragraph point syntax)))))

(set-key `(com-forward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\} :shift :meta)))

(define-command (com-mark-paragraph :name t :command-table marking-table)
    ((count 'integer :prompt "Number of paragraphs"))
  "Place point and mark around the current paragraph.
Put point at the beginning of the current paragraph, and mark at the end. With a positive numeric argument, put mark that many paragraphs forward. With a negative numeric argument, put point at the end of the current paragraph and mark that many paragraphs backward. Successive invocations extend the selection.
FIXME: when called with point already at the beginning or end of a paragraph marks 2 paras."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-paragraph)
      (setf (offset mark) (offset point))
      (if (plusp count)
	  (backward-paragraph point syntax)
	  (forward-paragraph point syntax)))
    (if (plusp count)
	(loop repeat count do (forward-paragraph mark syntax))
	(loop repeat (- count) do (backward-paragraph mark syntax)))))

(set-key `(com-mark-paragraph ,*numeric-argument-marker*)
	 'marking-table
	 '((#\h :meta)))

(define-command (com-backward-sentence :name t :command-table movement-table)
    ((count 'integer :prompt "Number of sentences"))
  "Move point to the previous sentence beginning.
With a numeric argument, move point backward (forward if negative) that many sentences."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-sentence point syntax))
	(loop repeat (- count) do (forward-sentence point syntax)))))

(set-key `(com-backward-sentence ,*numeric-argument-marker*)
	 'movement-table
	 '((#\a :meta)))

(define-command (com-forward-sentence :name t :command-table movement-table)
    ((count 'integer :prompt "Number of sentences"))
  "Move point to the next sentence end.
With a numeric argument, move point forward (backward if negative) that many sentences."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-sentence point syntax))
	(loop repeat (- count) do (backward-sentence point syntax)))))

(set-key `(com-forward-sentence ,*numeric-argument-marker*)
	 'movement-table
	 '((#\e :meta)))

(define-command (com-kill-sentence :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of sentences"))
  "Kill the objects from point to the next sentence end.
With a numeric argument, kill forward (backward if negative) that many sentences."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-sentence point syntax))
	(loop repeat (- count) do (backward-sentence point syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
    (delete-region point mark)))

(set-key `(com-kill-sentence ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\k :meta)))

(define-command (com-backward-kill-sentence :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of sentences"))
  "Kill the objects from point to the previous sentence beginning.
With a numeric argument, kill backward (forward if negative) that many sentences."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-sentence point syntax))
	(loop repeat (- count) do (forward-sentence point syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence point mark))
    (delete-region point mark)))

(set-key `(com-backward-kill-sentence ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\x :control) (#\Backspace)))

(defun forward-page (mark &optional (count 1))
  (loop repeat count
	unless (search-forward mark (coerce (list #\Newline #\Page) 'vector))
	  do (end-of-buffer mark)
	     (loop-finish)))

(define-command (com-forward-page :name t :command-table movement-table)
    ((count 'integer :prompt "Number of pages"))
  "Move point to the beginning of the next page.
With a numeric argument, move point forward (backward if negative) that many pages. When no page delimeter is found, leave point at the end of the buffer. A page is delimited by the sequence #\Newline #\Page."
  (let* ((pane (current-window))
	 (point (point pane)))
    (if (plusp count)
	(forward-page point count)
	(backward-page point count))))

(set-key `(com-forward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\])))

(defun backward-page (mark &optional (count 1))
  (loop repeat count
	  when (search-backward mark (coerce (list #\Newline #\Page) 'vector))
	    do (forward-object mark)
	  else do (beginning-of-buffer mark)
		  (loop-finish)))

(define-command (com-backward-page :name t :command-table movement-table)
    ((count 'integer :prompt "Number of pages"))
  "Move point to the end of the previous page.
With a numeric argument, move point backward (forward if negative) that many pages. When no page delimeter is found, leave point at the beginning of the buffer. A page is delimited by the sequence #\Newline #\Page."
  (let* ((pane (current-window))
	 (point (point pane)))
    (if (plusp count)
	(backward-page point count)
	(forward-page point count))))

(set-key `(com-backward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\[)))

(define-command (com-mark-page :name t :command-table marking-table)
    ((count 'integer :prompt "Move how many pages")
     (numargp 'boolean :prompt "Move to another page?"))
  "Place point and mark around the current page.
With a numeric argument, move point that many pages forward (backward if negative) before marking the surrounding page. When no page delimeters are found, leave point at the beginning and mark at the end of the buffer. A page is delimited by the sequence #\Newline #\Page."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane)))
    (cond ((and numargp (/= 0 count))
	   (if (plusp count)
	       (forward-page point count)
	       (backward-page point (1+ count))))
	  (t (backward-page point count)))
    (setf (offset mark) (offset point))
	   (forward-page mark 1)))

(set-key `(com-mark-page ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'marking-table
	 '((#\x :control) (#\p :control)))

(define-command (com-count-lines-page :name t :command-table info-table) ()
  "Print the number of lines in the current page.
Also prints the number of lines before and after point (as '(b + a)').
FIXME: the count is off by one."
  (let* ((pane (current-window))
	 (point (point pane))
	 (start (clone-mark point))
	 (end (clone-mark point)))
    (backward-page start)
    (forward-page end)
    (let ((total (number-of-lines-in-region start end))
	  (before (number-of-lines-in-region start point))
	  (after (number-of-lines-in-region point end)))
      (display-message "Page has ~A lines (~A + ~A)" total before after))))

(set-key 'com-count-lines-page
	 'info-table
	 '((#\x :control) (#\l)))

(define-command (com-count-lines-region :name t :command-table info-table) ()
  "Print the number of lines in the region.
Also prints the number of objects (as 'o character[s]').
FIXME: line count is off by one."
  (let*  ((pane (current-window))
	  (point (point pane))
	  (mark (mark pane))
	  (lines (number-of-lines-in-region point mark))
	  (chars (abs (- (offset point) (offset mark)))))
    (display-message "Region has ~D line~:P, ~D character~:P." lines chars)))

(set-key 'com-count-lines-region
	 'info-table
	 '((#\= :meta)))

(define-command (com-what-cursor-position :name t :command-table info-table) ()
  "Print information about point.
Gives the character after point (name and octal, decimal and hexidecimal charcode), the offset of point, the total objects in the buffer, and the percentage of the buffers objects before point.
FIXME: gives no information at end of buffer."
  (let* ((pane (current-window))
	 (point (point pane))
	 (buffer (buffer pane))
	 (offset (offset point))
	 (size (size buffer))
	 (char (object-after point))
	 (column (column-number point)))
    (display-message "Char: ~:C (#o~O ~:*~D ~:*#x~X) point=~D of ~D (~D%) column ~D"
		     char (char-code char) offset size
		     (round (* 100 (/ offset size))) column)))

(set-key 'com-what-cursor-position
	 'info-table
	 '((#\x :control) (#\=)))

(define-command (com-eval-expression :name t :command-table base-table)
    ((insertp 'boolean :prompt "Insert?"))
  "Prompt for and evaluate a lisp expression.
With a numeric argument inserts the result at point as a string; otherwise prints the result."
  (let* ((*package* (find-package :climacs-gui))
	 (string (handler-case (accept 'string :prompt "Eval")
		   (error () (progn (beep)
				    (display-message "Empty string")
				    (return-from com-eval-expression nil)))))
	 (values (multiple-value-list
		  (handler-case (eval (read-from-string string))
		    (error (condition) (progn (beep)
					      (display-message "~a" condition)
					      (return-from com-eval-expression nil))))))
	 (result (format nil "~:[; No values~;~:*~{~S~^,~}~]" values)))
    (if insertp
	(insert-sequence (point (current-window)) result)
	(display-message result))))

(set-key `(com-eval-expression ,*numeric-argument-p*)
	 'base-table
	 '((#\: :shift :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commenting

;;; figure out how to make commands without key bindings accept numeric arguments. 
(define-command (com-comment-region :name t :command-table comment-table) ()
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (comment-region syntax point mark)))

(define-command (com-backward-expression :name t :command-table movement-table)
    ((count 'integer :prompt "Number of expressions"))
  "Move point backward one expression.
With a numeric argument, move backward (forward if negative) that many expressions. The meaning of 'expression' is given by the relevant syntax. The Lisp syntax, for example, uses s-expressions.
FIXME: I'm not sure it does."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-expression point syntax))
	(loop repeat (- count) do (forward-expression point syntax)))))

(set-key `(com-backward-expression ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :control :meta)))

(define-command (com-forward-expression :name t :command-table movement-table)
    ((count 'integer :prompt "Number of expresssions"))
  "Move point forward one expression.
With a numeric argument, move forward (backward if negative) that many expressions. The meaning of 'expression' is given by the relevant syntax. The Lisp syntax, for example, uses s-expressions."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-expression point syntax))
	(loop repeat (- count) do (backward-expression point syntax)))))

(set-key `(com-forward-expression ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :control :meta)))

(define-command (com-mark-expression :name t :command-table marking-table)
    ((count 'integer :prompt "Number of expressions"))
  "Place mark at the next expression end.
With a numeric argument, place mark forward (backward if negative) that many expressions. Successive invocations extend the selection."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-expression)
      (setf (offset mark) (offset point)))
    (if (plusp count)
	(loop repeat count do (forward-expression mark syntax))
	(loop repeat (- count) do (backward-expression mark syntax)))))

(set-key `(com-mark-expression ,*numeric-argument-marker*)
	 'marking-table
	 '((#\@ :shift :control :meta)))

(define-command (com-kill-expression :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of expressions"))
  "Kill objects up to the next expression end.
With a numeric argument, kill forward (backward if negative) that many expressions."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (forward-expression mark syntax))
	(loop repeat (- count) do (backward-expression mark syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence mark point))
    (delete-region mark point)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\k :control :meta)))

(define-command (com-backward-kill-expression :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of expressions"))
  "Kill objects back to the previous expression beginning.
With a numeric argument, kill backward (forward if negative) that many expressions."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (clone-mark point))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-expression mark syntax))
	(loop repeat (- count) do (forward-expression mark syntax)))
    (kill-ring-standard-push *kill-ring* (region-to-sequence mark point))
    (delete-region mark point)))

(set-key `(com-backward-kill-expression ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Backspace :control :meta)))

;; (defparameter *insert-pair-alist*
;; 	      '((#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>) (#\" #\") (#\' #\') (#\` #\')))

(defun insert-pair (mark syntax &optional (count 0) (open #\() (close #\)))
  (cond ((> count 0)
	 (loop while (and (not (end-of-buffer-p mark))
			  (whitespacep (object-after mark)))
	       do (forward-object mark)))
	((< count 0)
	 (setf count (- count))
	 (loop repeat count do (backward-expression mark syntax))))
  (unless (or (beginning-of-buffer-p mark)
	      (whitespacep (object-before mark)))
    (insert-object mark #\Space))
  (insert-object mark open)
  (let ((here (clone-mark mark)))
    (loop repeat count
	  do (forward-expression here syntax))
    (insert-object here close)
    (unless (or (end-of-buffer-p here)
		(whitespacep (object-after here)))
      (insert-object here #\Space))))

(defun insert-parentheses (mark syntax count)
  (insert-pair mark syntax count #\( #\)))

(define-command (com-insert-parentheses :name t :command-table editing-table)
    ((count 'integer :prompt "Number of expressions")
     (wrap-p 'boolean :prompt "Wrap expressions?"))
  "Insert a pair of parentheses, leaving point in between.
With a numeric argument, enclose that many expressions forward (backward if negative).
FIXME: no it doesn't."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (unless wrap-p (setf count 0))
    (insert-parentheses point syntax count)))

(set-key `(com-insert-parentheses ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'editing-table
	 '((#\( :meta)))

(define-command (com-forward-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  "Move point forward across a delimited list.
With a numeric argument, move forward (backward if negative) that many lists."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	 (loop repeat count do (forward-list point syntax))
	 (loop repeat (- count) do (backward-list point syntax)))))

(set-key `(com-forward-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n :control :meta)))

(define-command (com-backward-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  "Move point backward across a delimited list.
With a numeric argument, move backward (forward if negative) that many lists."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-list point syntax))
	(loop repeat (- count) do (forward-list point syntax)))))

(set-key `(com-backward-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\p :control :meta)))

(define-command (com-down-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  "Move point forward down one level of delimited list.
With a numeric argument, move forward (backward if negative) down that many lists."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (down-list point syntax))
	(loop repeat (- count) do (backward-down-list point syntax)))))

(set-key `(com-down-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\d :control :meta)))

(define-command (com-backward-down-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  "Move point backward down one level of delimited list.
With a numeric argument, move backward (forward if negative) down that many lists."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-down-list point syntax))
	(loop repeat (- count) do (down-list point syntax)))))

(define-command (com-backward-up-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  "Move point backward up one level of delimited list.
With a numeric argument, move backward (forward if negative) up that many lists."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (backward-up-list point syntax))
	(loop repeat (- count) do (up-list point syntax)))))

(set-key `(com-backward-up-list ,*numeric-argument-marker*)
	 'movement-table
	 '((#\u :control :meta)))

(define-command (com-up-list :name t :command-table movement-table)
    ((count 'integer :prompt "Number of lists"))
  "Move point forward up one level of delimited list.
With a numeric argument, move forward (backward if negative) up that many lists."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (up-list point syntax))
	(loop repeat (- count) do (backward-up-list point syntax)))))

(define-command (com-beginning-of-definition :name t :command-table movement-table)
    ((count 'integer :prompt "Number of definitions"))
  "Move point to the previous definition beginning.
With a numeric argument, move backward (forward if negative) that many definitions.
FIXME: negative arg actually does end-of-definition.
FIXME: should take account of CLHS 3.2.3.1 .../HyperSpec/Body/03_bca.htm"
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (beginning-of-definition point syntax))
	(loop repeat (- count) do (end-of-definition point syntax)))))

(set-key `(com-beginning-of-definition ,*numeric-argument-marker*)
	 'movement-table
	 '((#\a :control :meta)))

(define-command (com-end-of-definition :name t :command-table movement-table)
    ((count 'integer :prompt "Number of definitions"))
  "Move point to the following definition end.
With a numeric argument, move forward (backward if negative) that many defintions.
FIXME: negative arg actually does beginning-of-definition.
FIXME: see second FIXME for com-beginning-of-definition."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (if (plusp count)
	(loop repeat count do (end-of-definition point syntax))
	(loop repeat (- count) do (beginning-of-definition point syntax)))))

(set-key `(com-end-of-definition ,*numeric-argument-marker*)
	 'movement-table
	 '((#\e :control :meta)))

(define-command (com-mark-definition :name t :command-table marking-table) ()
  "Place point and mark around the current definition.
Successive invocations extend the selection."
  (let* ((pane (current-window))
	 (point (point pane))
	 (mark (mark pane))
	 (syntax (syntax (buffer pane))))
    (unless (eq (previous-command pane) 'com-mark-definition)
      (beginning-of-definition point syntax)
      (setf (offset mark) (offset point)))
    (end-of-definition mark syntax)))

(set-key 'com-mark-definition
	 'marking-table
	 '((#\h :control :meta)))

(define-command (com-visible-mark :name t :command-table marking-table) ()
  "Toggle the visibility of the mark in the current pane.
This is particularly (only?) useful for experimenting with marking commands."
  (setf (mark-visible-p (current-window)) (not (mark-visible-p (current-window)))))
