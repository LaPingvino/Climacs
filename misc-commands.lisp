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

(in-package :climacs-commands)

(define-command (com-overwrite-mode :name t :command-table editing-table) ()
  "Toggle overwrite mode for the current mode.
When overwrite is on, an object entered on the keyboard 
will replace the object after the point. 
When overwrite is off (the default), objects are inserted at point. 
In both cases point is positioned after the new object."
  (with-slots (overwrite-mode) (current-window)
    (setf overwrite-mode (not overwrite-mode))))

(set-key 'com-overwrite-mode
	 'editing-table
	 '((:insert)))

(define-command (com-not-modified :name t :command-table buffer-table) ()
  "Clear the modified flag for the current buffer.
The modified flag is automatically set when the contents 
of the buffer are changed. This flag is consulted, for instance, 
when deciding whether to prompt you to save the buffer before killing it."
  (setf (needs-saving (buffer (current-window))) nil))

(set-key 'com-not-modified
	 'buffer-table
	 '((#\~ :meta :shift)))

(defun set-fill-column (column)
  (if (> column 1)
      (setf (auto-fill-column (current-window)) column)
      (progn (beep) (display-message "Set Fill Column requires an explicit argument."))))

(define-command (com-set-fill-column :name t :command-table fill-table)
    ((column 'integer :prompt "Column Number:"))
  "Set the fill column to the specified value.
You must supply a numeric argument. The fill column is 
the column beyond which automatic line-wrapping will occur. 

The default fill column is 70."
  (set-fill-column column))

(set-key `(com-set-fill-column ,*numeric-argument-marker*)
	 'fill-table
	 '((#\x :control) (#\f)))

(define-command (com-zap-to-object :name t :command-table deletion-table) ()
  "Prompt for an object and kill to the next occurence of that object after point.
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
  "Prompt for a character and kill to the next occurence of that character after point.
FIXME: Accepts a string (that is, zero or more characters) 
terminated by a #\NEWLINE. If a zero length string signals an error. 
If a string of length >1, uses the first character of the string."
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

(define-command (com-open-line :name t :command-table editing-table)
    ((numarg 'integer :prompt "How many lines?"))
  "Insert a #\Newline and leave point before it.
With a numeric argument greater than 1, insert that many #\Newlines."
  (open-line (point (current-window)) numarg))

(set-key `(com-open-line ,*numeric-argument-marker*)
	 'editing-table
	 '((#\o :control)))

(defmacro define-mark-unit-command (unit command-table &key
                                    move-point
                                    noun
                                    plural)
  "Define a COM-MARK-<UNIT> for `unit' command and put it in
  `command-table'."
  (labels ((symbol (&rest strings)
             (intern (apply #'concat strings)))
           (concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings))))
    (let ((forward (symbol "FORWARD-" unit))
          (backward (symbol "BACKWARD-" unit))
          (noun (or noun (string-downcase unit)))
          (plural (or plural (concat (string-downcase unit) "s"))))
      `(define-command (,(symbol "COM-MARK-" unit)
                         :name t
                         :command-table ,command-table)
           ((count 'integer :prompt ,(concat "Number of " plural)))
           ,(if (not (null move-point))
                (concat "Place point and mark around the current " noun ".
Put point at the beginning of the current " noun ", and mark at the end. 
With a positive numeric argument, put mark that many " plural " forward. 
With a negative numeric argument, put point at the end of the current 
" noun " and mark that many " plural " backward. 
Successive invocations extend the selection.")
                (concat "Place mark at the next " noun " end.
With a positive numeric argument, place mark at the end of 
that many " plural " forward. With a negative numeric argument, 
place mark at the beginning of that many " plural " backward. 

Successive invocations extend the selection."))
         (let* ((pane (current-window))
                (point (point pane))
                (mark (mark pane)))
           (unless (eq (previous-command pane) 'com-mark-word)
             (setf (offset mark) (offset point))
             ,(when (not (null move-point))
                    `(if (plusp count)
                         (,backward point (syntax (buffer pane)))
                         (,forward point (syntax (buffer pane))))))
           (,forward mark (syntax (buffer pane)) count))))))

(define-mark-unit-command word marking-table)
(define-mark-unit-command expression marking-table)
(define-mark-unit-command paragraph marking-table :move-point t)
(define-mark-unit-command definition marking-table :move-point t)

(set-key `(com-mark-word ,*numeric-argument-marker*)
	 'marking-table
	 '((#\@ :meta :shift)))

(set-key `(com-mark-paragraph ,*numeric-argument-marker*)
	 'marking-table
	 '((#\h :meta)))

(set-key 'com-mark-definition
	 'marking-table
	 '((#\h :control :meta)))

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
  (upcase-word (point (current-window))
               (syntax (buffer (current-window)))))

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
If point is in a word, convert the next character to 
upper case and the remaining letters in the word to lower case. 
If point is before the start of a word, convert the first character 
of that word to upper case and the rest of the letters to lower case. 

Leave point at the word end."
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

(define-command (com-indent-region :name t :command-table indent-table) ()
  "Indent every line of the current region as specified by the
syntax for the buffer."
  (let* ((pane (current-window))
         (point (point pane))
         (mark (mark pane)))
    (indent-region pane point mark)))

(define-command (com-delete-indentation :name t :command-table indent-table) ()
  "Join current line to previous non-blank line.
Leaves a single space between the last non-whitespace object 
of the previous line and the first non-whitespace object of 
the current line, and point after that space. If there is no 
previous non-blank line, deletes all whitespace at the 
beginning of the buffer at leaves point there."
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

(define-command (com-back-to-indentation :name t :command-table movement-table) ()
  "Move point to the first non-whitespace object on the current line.
If there is no non-whitespace object, leaves point at the end of the line."
  (back-to-indentation (point (current-window))
                       (syntax (buffer (current-window)))))

(set-key 'com-back-to-indentation
	 'movement-table
	 '((#\m :meta)))

(define-command (com-delete-horizontal-space :name t :command-table deletion-table)
    ((backward-only-p
      'boolean :prompt "Delete backwards only?"))
  "Delete whitespace around point.
With a numeric argument, only delete whitespace before point."
  (delete-horizontal-space (point (current-window))
                           (syntax (buffer (current-window)))
                           backward-only-p))

(set-key `(com-delete-horizontal-space ,*numeric-argument-p*)
	 'deletion-table
	 '((#\\ :meta)))

(define-command (com-just-one-space :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of spaces"))
  "Delete whitespace around point, leaving a single space.
With a positive numeric argument, leave that many spaces.

FIXME: should distinguish between types of whitespace."
  (just-n-spaces (point (current-window))
                 count))

(set-key `(com-just-one-space ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Space :meta)))

(define-command (com-goto-position :name t :command-table movement-table) 
    ((position 'integer :prompt "Goto Position"))
  "Prompts for an integer, and sets the offset of point to that integer."
  (goto-position
   (point (current-window))
   position))  

(define-command (com-goto-line :name t :command-table movement-table) 
    ((line-number 'integer :prompt "Goto Line"))
  "Prompts for a line number, and sets point to the beginning of that line.
The first line of the buffer is 1. Giving a number <1 leaves 
point at the beginning of the buffer. Giving a line number 
larger than the number of the last line in the buffer leaves 
point at the beginning of the last line of the buffer."
  (goto-line (point (current-window)) line-number))

(define-command (com-browse-url :name t :command-table base-table) 
    ((url 'url :prompt "Browse URL"))
  (declare (ignorable url))
  #+ (and sbcl darwin)
     (sb-ext:run-program "/usr/bin/open" `(,url) :wait nil)
  #+ (and openmcl darwin)
     (ccl:run-program "/usr/bin/open" `(,url) :wait nil))

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

(define-command (com-set-syntax :name t :command-table buffer-table) 
    ((syntax 'syntax
      :prompt "Name of syntax"))
  "Prompts for a syntax to set for the current buffer.
   Setting a syntax will cause the buffer to be reparsed using the new syntax."
  (set-syntax (current-buffer) syntax))

;;;;;;;;;;;;;;;;;;;;
;; Kill ring commands

;; Copies an element from a kill-ring to a buffer at the given offset
(define-command (com-yank :name t :command-table editing-table) ()
  "Insert the objects most recently added to the kill ring at point."
  (handler-case (insert-sequence (point (current-window)) (kill-ring-yank *kill-ring*))
    (flexichain:at-end-error ()
      (display-message "Kill ring is empty"))))

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
Must be given immediately following a Yank or Rotate Yank command. 
The replacement objects are those before the previously yanked 
objects in the kill ring."
  (handler-case (let* ((pane (current-window))
                       (point (point pane))
                       (last-yank (kill-ring-yank *kill-ring*)))
                  (if (eq (previous-command pane)
                          'com-rotate-yank)
                      (progn
                        (delete-range point (* -1 (length last-yank)))
                        (rotate-yank-position *kill-ring*)))
                  (insert-sequence point (kill-ring-yank *kill-ring*)))
    (flexichain:at-end-error ()
      (display-message "Kill ring is empty"))))

(set-key 'com-rotate-yank
	 'editing-table
	 '((#\y :meta)))

(define-command (com-resize-kill-ring :name t :command-table editing-table) 
    ((size 'integer :prompt "New kill ring size"))
  "Prompt for a new size for the kill ring.
The default is 5. A number less than 5 will be replaced by 5."
     (setf (kill-ring-max-size *kill-ring*) size))

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
Search from point (first backward to the beginning of the buffer, 
then forward) for words for which the word before point is a prefix, 
inserting each in turn at point as an expansion."
  (let* ((window (current-window))
	 (point (point window))
         (syntax (syntax (buffer window))))
    (with-accessors ((original-prefix original-prefix)
                     (prefix-start-offset prefix-start-offset)
                     (dabbrev-expansion-mark dabbrev-expansion-mark)) window
       (flet ((move () (cond ((beginning-of-buffer-p dabbrev-expansion-mark)
			      (setf (offset dabbrev-expansion-mark)
				    (offset point))
			      (forward-word dabbrev-expansion-mark syntax))
			     ((mark< dabbrev-expansion-mark point)
			      (backward-object dabbrev-expansion-mark))
			     (t (forward-object dabbrev-expansion-mark)))))
	 (unless (or (beginning-of-buffer-p point)
		     (not (constituentp (object-before point))))
	   (unless (and (eq (previous-command window) 'com-dabbrev-expand)
			(not (null prefix-start-offset)))
	     (setf dabbrev-expansion-mark (clone-mark point))
	     (backward-word dabbrev-expansion-mark syntax)
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
					 (prog2 (forward-word dabbrev-expansion-mark syntax)
						(region-to-sequence offset dabbrev-expansion-mark)
						(setf (offset dabbrev-expansion-mark) offset))))
		      (move))))))))

(set-key 'com-dabbrev-expand
	 'editing-table
	 '((#\/ :meta)))

(define-command (com-mark-page :name t :command-table marking-table)
    ((count 'integer :prompt "Move how many pages")
     (numargp 'boolean :prompt "Move to another page?"))
  "Place point and mark around the current page.
With a numeric argument, move point that many 
pages forward (backward if negative) before marking the 
surrounding page. When no page delimeters are found, 
leave point at the beginning and mark at the end of the buffer. 

A page is delimited by the sequence #\Newline #\Page."
  (let* ((pane (current-window))
	 (syntax (syntax (buffer pane)))
         (point (point pane))
	 (mark (mark pane)))
    (cond ((and numargp (/= 0 count))
	   (if (plusp count)
	       (forward-page point syntax count)
	       (backward-page point syntax (1+ count))))
	  (t (backward-page point syntax count)))
    (setf (offset mark) (offset point))
    (forward-page mark syntax 1)))

(set-key `(com-mark-page ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'marking-table
	 '((#\x :control) (#\p :control)))

(define-command (com-count-lines-page :name t :command-table info-table) ()
  "Print the number of lines in the current page.
Also prints the number of lines before and after point (as '(b + a)')."
  (let* ((pane (current-window))
         (syntax (syntax (buffer pane)))
	 (point (point pane))
	 (start (clone-mark point))
	 (end (clone-mark point)))
    (backward-page start syntax)
    (forward-page end syntax)
    (let ((total (number-of-lines-in-region start end))
	  (before (number-of-lines-in-region start point))
	  (after (number-of-lines-in-region point end)))
      (display-message "Page has ~A lines (~A + ~A)" (1+ total) before after))))

(set-key 'com-count-lines-page
	 'info-table
	 '((#\x :control) (#\l)))

(define-command (com-count-lines-region :name t :command-table info-table) ()
  "Print the number of lines in the region.
Also prints the number of objects (as 'o character[s]')."
  (let*  ((pane (current-window))
	  (point (point pane))
	  (mark (mark pane))
	  (lines (number-of-lines-in-region point mark))
	  (chars (abs (- (offset point) (offset mark)))))
    (display-message "Region has ~D line~:P, ~D character~:P." (1+ lines) chars)))

(set-key 'com-count-lines-region
	 'info-table
	 '((#\= :meta)))

(define-command (com-what-cursor-position :name t :command-table info-table) ()
  "Print information about point.
Gives the character after point (name and octal, decimal and hexidecimal charcode), 
the offset of point, the total objects in the buffer, 
and the percentage of the buffers objects before point.

FIXME: gives no information at end of buffer."
  (let* ((pane (current-window))
	 (point (point pane))
	 (buffer (buffer pane))
	 (offset (offset point))
	 (size (size buffer))
	 (char (or (end-of-buffer-p point) (object-after point)))
	 (column (column-number point)))
    (display-message "Char: ~:[none~*~;~:*~:C (#o~O ~:*~D ~:*#x~X)~] point=~D of ~D (~D%) column ~D"
		     (and (characterp char) char)
		     (and (characterp char) (char-code char))
		     offset size
		     (if size (round (* 100 (/ offset size))) 100)
		     column)))

(set-key 'com-what-cursor-position
	 'info-table
	 '((#\x :control) (#\=)))

(define-command (com-eval-expression :name t :command-table base-table)
    ((exp 'expression :prompt "Eval")
     (insertp 'boolean :prompt "Insert?"))
  "Prompt for and evaluate a lisp expression.
With a numeric argument inserts the result at point as a string; 
otherwise prints the result."
  (let* ((*package* (find-package :climacs-gui))
         (values (multiple-value-list
		  (handler-case (eval exp)
		    (error (condition) (progn (beep)
					      (display-message "~a" condition)
					      (return-from com-eval-expression nil))))))
	 (result (format nil "~:[; No values~;~:*~{~S~^,~}~]" values)))
    (if insertp
	(insert-sequence (point (current-window)) result)
	(display-message result))))

(set-key `(com-eval-expression ,*unsupplied-argument-marker* ,*numeric-argument-p*)
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

;; (defparameter *insert-pair-alist*
;; 	      '((#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>) (#\" #\") (#\' #\') (#\` #\')))

(defun insert-parentheses (mark syntax count)
  (insert-pair mark syntax count #\( #\)))

(define-command (com-insert-parentheses :name t :command-table editing-table)
    ((count 'integer :prompt "Number of expressions")
     (wrap-p 'boolean :prompt "Wrap expressions?"))
  "Insert a pair of parentheses, leaving point in between.
With a numeric argument, enclose that many expressions 
forward (backward if negative).

FIXME: no it doesn't."
  (let* ((pane (current-window))
	 (point (point pane))
	 (syntax (syntax (buffer pane))))
    (unless wrap-p (setf count 0))
    (insert-parentheses point syntax count)))

(set-key `(com-insert-parentheses ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'editing-table
	 '((#\( :meta)))

(define-command (com-visible-region :name t :command-table marking-table) ()
  "Toggle the visibility of the region in the current pane."
  (setf (region-visible-p (current-window)) (not (region-visible-p (current-window)))))
