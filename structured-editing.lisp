;;; -*- Mode: Lisp; Package: CLIMACS-STRUCTEDIT -*-

;;;  (c) copyright 2008 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
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

;;; Implementation of structural editing commands for the Lisp syntax
;;; in Climacs. These commands were inspired by Paredit, which was
;;; originally written by Taylor "Riastradh" Campbell for GNU
;;; Emacs. In particular, many docstrings have been copied verbatim,
;;; then modified.

;;; This is a work in progress, be aware that problems are likely to
;;; arise, and that the editing commands are not yet completely
;;; comprehensive. Patches are, of course, welcome.

;;; You must do M-x Structedit Mode to enable these commands.

(in-package :climacs-structedit)

(define-syntax-mode structedit-mode ()
  ()
  (:documentation "A mode for Paredit-style editing in Lisp syntax.")
  (:applicable-syntaxes lisp-syntax))

(define-mode-toggle-commands com-structedit-mode (structedit-mode "Structedit")
  :command-table lisp-table)

(make-command-table 'structedit-table
 :errorp nil)

(defmethod syntax-command-tables append ((syntax structedit-mode))
  '(structedit-table))

(defun delete-form (buffer form)
  "Delete `form' from `buffer'."
  (delete-buffer-range
   buffer (start-offset form) (size form)))

(define-command (com-open-list :name t :command-table structedit-table)
    ((n 'integer :default 0))
  "Insert a balanced parenthesis pair.
With an argument N, put the closing parentheses after N
S-expressions forward.  If in string or comment, insert a single
opening parenthesis.  If in a character literal, replace the
character literal with #\(."
  (cond ((in-string-p (current-syntax) (point))
         (insert-character #\())
        ((in-comment-p (current-syntax) (point))
         (insert-character #\())
        ((in-character-p (current-syntax) (point))
         (delete-form (current-buffer) (form-around (current-syntax) (offset (point))))
         (insert-sequence (point) "#\\("))
        (t
         (when (and (not (zerop n))
                    (forward-expression (point) (current-syntax) 1 nil))
           (backward-expression (point) (current-syntax) 1 nil))
         (insert-character #\()
         (forward-expression (point) (current-syntax) n nil)
         (insert-character #\))
         (backward-object (point))
         (backward-expression (point) (current-syntax) n nil))))

(define-command (com-wrap-expression :name t :command-table structedit-table)
    ((n 'integer :default 1))
  "Wrap the following N S-expressions in a list.
Automatically indent the newly wrapped S-expressions.  As a
special case, if the point is at the end of a list, simply insert
a pair of parentheses, rather than insert a lone opening
parenthesis and then signal an error, in the interest of
preserving structural validity."
  (com-open-list n))

(define-command (com-close-list-and-newline :name t :command-table structedit-table)
    ()
  "Move past one closing delimiter, add a newline, and reindent."
  (cond ((or (in-string-p (current-syntax) (point))
             (in-comment-p (current-syntax) (point)))
         (insert-character #\)))
        ((in-character-p (current-syntax) (point))
         (delete-form (current-buffer) (form-around (current-syntax) (offset (point))))
         (insert-sequence (point) "#\\)"))
        ((forward-up (point) (current-syntax) 1 nil)
         (insert-object (point) #\Newline)
         (indent-current-line (current-view) (point)))))

(defun delete-object-structurally (delete-fn move-fn immediate-form-fn
                                   border-offset-fn
                                   at-border-fn)
  "Delete an object at `(point)' structurally. `Delete-fn' is
either `forward-delete-object' or `backward-delete-object',
`move-fn' is either `forward-object' or `backward-object',
`immediate-form-fn' is some form selector, `border-offset-fn' is
either `end-offset' or `begin-offset', `at-border-fn' is a
function used to determine whether or not `(point)' is at the end
of a structural object."
  (let ((immediate-form (funcall immediate-form-fn (current-syntax) (offset (point))))
        (form-around (form-around (current-syntax) (offset (point)))))
    (cond ((and (or (form-string-p immediate-form)
                    (form-list-p immediate-form))
                (= (funcall border-offset-fn immediate-form)
                   (offset (point))))
           (funcall move-fn (point)))
          ((funcall at-border-fn (current-syntax) (point))
           (when (null (form-children (list-at-mark (current-syntax) (point))))
             (delete-form (current-buffer) form-around)))
          ((and (form-character-p immediate-form)
                (= (funcall border-offset-fn immediate-form)
                   (offset (point))))
           (delete-form (current-buffer) immediate-form))
          (t (funcall delete-fn (point))))))

(define-command (com-forward-delete-object-structurally
                 :name t :command-table structedit-table)
    ((force 'boolean :default nil))
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
S-expression. If on a closing S-expression delimiter, refuse to
delete unless the S-expression is empty, in which case delete the
whole S-expression. If `force' is true, simply delete a character
forward, without regard for delimiter balancing."
  (if force
      (forward-delete-object (point))
      (delete-object-structurally #'forward-delete-object #'forward-object
                                  #'form-after #'start-offset
                                  #'location-at-end-of-form)))

(define-command (com-backward-delete-object-structurally
                 :name t :command-table structedit-table)
    ((force 'boolean :default nil))
  "Delete a character backward or move backward over a delimiter.
If on an ending S-expression delimiter, move backward into the
S-expression. If on an opening S-expression delimiter, refuse to
delete unless the S-expression is empty, in which case delete the
whole S-expression. If `force' is true, simply delete a
character backward, without regard for delimiter balancing."
  (if force
      (backward-delete-object (point))
      (delete-object-structurally #'backward-delete-object #'backward-object
                                  #'form-before #'end-offset
                                  #'location-at-beginning-of-form)))

(define-command (com-insert-double-quote-structurally
                 :name t :command-table structedit-table)
    ((n 'integer :default 0))
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, replace the character literal with #\\\"."
  (cond ((in-comment-p (current-syntax) (point))
         (insert-character #\"))
        ((at-end-of-string-p (current-syntax) (point))
         (forward-object (point)))
        ((in-string-p (current-syntax) (point))
         (insert-sequence (point) "\\\""))
        ((in-character-p (current-syntax) (point))
         (delete-form (current-buffer) (form-around (current-syntax) (offset (point))))
         (insert-sequence (point) "#\\\""))
        (t
         (let ((old-offset (offset (point))))
           (forward-expression (point) (current-syntax) n nil)
           (insert-buffer-object (current-buffer) old-offset #\")
           (insert-character #\")
           (backward-object (point))
           (backward-expression (point) (current-syntax) (min 1 n) nil)))))

(define-command (com-wrap-expression-in-doublequote :name t :command-table structedit-table)
    ((n 'integer :default 1))
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `Insert Double Quote Structurally'; if
no prefix argument is specified, the default is to wrap one
S-expression, however, not zero."
  (if (in-string-p (current-syntax) (point))
      (setf (offset (point))
            (1+ (end-offset (form-around (current-syntax) (point)))))
      (com-insert-double-quote-structurally n)))

(define-command (com-splice-list :name t :command-table structedit-table)
    ((kill-backward 'boolean :default nil))
  "Splice the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kill all S-expressions
backward in the current list before splicing all S-expressions
forward into the enclosing list."
  (let ((list (list-at-mark (current-syntax) (point))))
    (when list
      (let ((begin-mark (make-buffer-mark (current-buffer) (start-offset list)))
            (end-mark (make-buffer-mark (current-buffer) (end-offset list))))
        (when kill-backward
          (loop until (eq (list-at-mark (current-syntax) (offset (point)))
                          (or (form-before (current-syntax) (offset (point)))
                              (form-around (current-syntax) (offset (point)))))
             do (backward-delete-expression (point) (current-syntax) 1 nil)))
        (delete-buffer-range (current-buffer) (offset begin-mark) 1)
        (delete-buffer-range (current-buffer) (1- (offset end-mark)) 1)))))

(define-command (com-kill-line-structurally :name t :command-table structedit-table)
    ()
  "Kill a line as if with \"Kill Line\", but respecting delimiters.
In a string, act exactly as \"Kill Line\" but do not kill past
the closing string delimiter.  On a line with no S-expressions on
it starting after the point or within a comment, act exactly as
\"Kill Line\".  Otherwise, kill all S-expressions that start
after the point."
  (let ((form-around (form-around (current-syntax) (offset (point))))
        (form-after (form-after (current-syntax) (offset (point))))
        (comment (comment-at-mark (current-syntax) (point))))
    (cond ((empty-line-p (point))
           (forward-delete-object (point)))
          ((in-string-p (current-syntax) (point))
           (if (= (buffer-line-number (current-buffer) (end-offset form-around))
                  (line-number (point)))
               ;; Delete from point until the end of the string, but
               ;; keep the ending delimiter.
               (kill-region (point) (1- (end-offset form-around)))
               ;; Delete from point until end of line.
               (kill-region (point) (end-of-line (clone-mark (point))))))
          ((in-line-comment-p (current-syntax) (point))
           ;; Delete until end of line
           (kill-region (point) (end-of-line (clone-mark (point)))))
          ((in-long-comment-p (current-syntax) (point))
           (if (= (buffer-line-number (current-buffer) (end-offset comment))
                  (line-number (point)))
               ;; End of comment on same line as point, if a complete
               ;; long comment, don't delete the ending delimiter
               (kill-region (point) (- (end-offset comment)
                                       (if (form-complete-p comment)
                                           2 0)))
               ;; Delete from point until end of line.
               (kill-region (point) (end-of-line (clone-mark (point))))))
          ((= (buffer-line-number (current-buffer) (start-offset form-after))
              (line-number (point)))
           (forward-kill-expression (point) (current-syntax))
           (loop for form-after = (form-after (current-syntax) (offset (point)))
              while (and form-after
                         (= (buffer-line-number (current-buffer) (start-offset form-after))
                            (line-number (point))))
              do (forward-kill-expression (point) (current-syntax) 1 t))))))

(set-key `(com-open-list ,*numeric-argument-marker* ,*numeric-argument-marker*)
         'structedit-table
         '(#\())

(set-key `(com-wrap-expression ,*numeric-argument-marker*)
         'structedit-table
         '((#\( :meta :shift)))

(set-key 'com-close-list-and-newline
         'structedit-table
         '(#\)))

(set-key `(com-forward-delete-object-structurally ,*numeric-argument-marker*)
         'structedit-table
         '((#\d :control)))

(set-key `(com-backward-delete-object-structurally ,*numeric-argument-marker*)
         'structedit-table
         '((#\Backspace)))

(set-key `(com-insert-double-quote-structurally ,*numeric-argument-marker*)
         'structedit-table
         '((#\")))

(set-key `(com-wrap-expression-in-doublequote ,*numeric-argument-marker*)
         'structedit-table
         '((#\" :meta :shift)))

(set-key `(com-splice-list ,*numeric-argument-marker*)
         'structedit-table
         '((#\s :meta)))

(set-key 'com-kill-line-structurally
         'structedit-table
         '((#\k :control)))
