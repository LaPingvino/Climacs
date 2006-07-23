;;; -*- Mode: Lisp; Package: CLIMACS-LISP-SYNTAX; -*-

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

;;; Commands specific to the Lisp syntax for Climacs.

(in-package :climacs-lisp-syntax)

;; Movement commands.
(climacs-commands:define-motion-commands expression lisp-table)
(climacs-commands:define-motion-commands definition lisp-table)
(climacs-commands:define-motion-commands up lisp-table
  :noun "nesting level up"
  :plural "levels")
(climacs-commands:define-motion-commands down lisp-table
  :noun "nesting level down"
  :plural "levels")
(climacs-commands:define-motion-commands list lisp-table)

(climacs-commands:define-editing-commands expression lisp-table)
(climacs-commands:define-deletion-commands expression lisp-table)

(define-command (com-eval-defun :name t :command-table lisp-table) ()
  (let* ((pane (current-window))
         (point (point pane))
         (syntax (syntax (buffer pane))))
    (eval-defun point syntax)))

(define-command (com-package :name t :command-table lisp-table) ()
  (let* ((pane (current-window))
         (syntax (syntax (buffer pane)))
         (package (package-at-mark syntax (point pane))))
    (esa:display-message (format nil "~A" (if (packagep package)
                                              (package-name package)
                                              package)))))

(define-command (com-fill-paragraph :name t :command-table lisp-table) 
    ()
  "Fill paragraph at point. Will have no effect unless there is a
string at point."
  (let* ((pane (current-window))
         (buffer (buffer pane))
         (implementation (implementation buffer))
         (syntax (syntax buffer))
         (token (form-around syntax (offset (point pane))))
         (fill-column (auto-fill-column pane))
         (tab-width (tab-space-count (stream-default-view pane))))
    (when (typep token 'string-form)
      (with-accessors ((offset1 start-offset) 
                       (offset2 end-offset)) token
        (climacs-editing:fill-region (make-instance 'standard-right-sticky-mark
                                                    :buffer implementation
                                                    :offset offset1)
                                     (make-instance 'standard-right-sticky-mark
                                                    :buffer implementation
                                                    :offset offset2)
                                     #'(lambda (mark)
                                         (syntax-line-indentation mark tab-width syntax))
                                     fill-column
                                     tab-width
                                     syntax
                                     t)))))

(define-command (com-indent-expression :name t :command-table lisp-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
         (point (point pane))
         (mark (clone-mark point))
         (syntax (syntax (buffer pane))))
    (if (plusp count)
        (loop repeat count do (forward-expression mark syntax))
        (loop repeat (- count) do (backward-expression mark syntax)))
    (climacs-editing:indent-region pane (clone-mark point) mark)))

(define-command (com-eval-last-expression :name t :command-table lisp-table)
    ((insertp 'boolean :prompt "Insert?"))
  "Evaluate the expression before point in the local Lisp image."
  (let* ((syntax (syntax (buffer (current-window))))
         (mark (point (current-window)))
         (token (form-before syntax (offset mark))))
    (if token
        (with-syntax-package syntax mark (package)
          (let ((*package* package))
            (climacs-gui::com-eval-expression
             (token-to-object syntax token :read t)
             insertp)))
        (esa:display-message "Nothing to evaluate."))))

(define-command (com-macroexpand-1 :name t :command-table lisp-table)
    ()
  "Macroexpand-1 the expression at point.

The expanded expression will be displayed in a
\"*Macroexpansion*\"-buffer."
  (let* ((syntax (syntax (buffer (current-window))))
         (token (expression-at-mark (point (current-window)) syntax)))
    (if token
        (macroexpand-token syntax token)
        (esa:display-message "Nothing to expand at point."))))

(define-command (com-macroexpand-all :name t :command-table lisp-table)
    ()
  "Completely macroexpand the expression at point.

The expanded expression will be displayed in a
\"*Macroexpansion*\"-buffer."
  (let* ((syntax (syntax (buffer (current-window))))
         (token (expression-at-mark (point (current-window)) syntax)))
    (if token
        (macroexpand-token syntax token t)
        (esa:display-message "Nothing to expand at point."))))

(define-command (com-eval-region :name t :command-table lisp-table)
    ()
  "Evaluate the current region."
  (let ((mark (mark (current-window)))
        (point (point (current-window))))
    (when (mark> mark point)
      (rotatef mark point))
    (evaluating-interactively
     (eval-region mark point
                  (syntax (buffer (current-window)))))))

(define-command (com-compile-definition :name t :command-table lisp-table)
    ()
  "Compile and load definition at point."
  (evaluating-interactively 
   (compile-definition-interactively (point (current-window))
                                     (syntax (buffer (current-window))))))

(define-command (com-compile-and-load-file :name t :command-table lisp-table)
    ()
  "Compile and load the current file.

Compiler notes will be displayed in a seperate buffer."
  (compile-file-interactively (buffer (current-window)) t))

(define-command (com-compile-file :name t :command-table lisp-table)
    ()
  "Compile the file open in the current buffer.

This command does not load the file after it has been compiled."
  (compile-file-interactively (buffer (current-window)) nil))

(define-command (com-goto-location :name t :command-table lisp-table)
    ((note 'compiler-note))
  "Move point to the part of a given file that caused the
compiler note.

If the file is not already open, a new buffer will be opened with
that file."
  (goto-location (location note)))

(define-presentation-to-command-translator compiler-note-to-goto-location-translator
    (compiler-note com-goto-location lisp-table)
    (presentation)
  (list (presentation-object presentation)))

(define-command (com-goto-xref :name t :command-table lisp-table)
    ((xref 'xref))
  "Go to the referenced location of a code cross-reference."
  (goto-location xref))

(define-presentation-to-command-translator xref-to-goto-location-translator
    (xref com-goto-xref lisp-table)
    (presentation)
    (list (presentation-object presentation)))

(define-command (com-edit-this-definition :command-table lisp-table)
    ()
  "Edit definition of the symbol at point.
If there is no symbol at point, this is a no-op."
  (let* ((buffer (buffer (current-window)))
         (point (point (current-window)))
         (syntax (syntax buffer))
         (token (this-form point syntax))
         (this-symbol (when token (token-to-object syntax token))))
    (when (and this-symbol (symbolp this-symbol))
      (edit-definition this-symbol))))

(define-command (com-return-from-definition :name t :command-table lisp-table)
    ()
  "Return point to where it was before the previous Edit
Definition command was issued."
  (pop-find-definition-stack))

(define-command (com-lookup-arglist-for-this-symbol :command-table lisp-table)
    ()
  "Show argument list for symbol at point."
  (let* ((pane (current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer))
         (mark (point pane))
         (token (this-form mark syntax)))
    (if (and token (typep token 'complete-token-lexeme))
        (com-lookup-arglist (token-to-object syntax token))
        (esa:display-message "Could not find symbol at point."))))

(define-command (com-lookup-arglist :name t :command-table lisp-table)
    ((symbol 'symbol :prompt "Symbol"))
  "Show argument list for a given symbol."
  (show-arglist symbol))

(define-command (com-space :command-table lisp-table)
    ()
  "Insert a space and display argument hints in the minibuffer."
  (let* ((window (current-window))
         (mark (point window))
         (syntax (syntax (buffer window))))
    ;; It is important that the space is inserted before we look up
    ;; any symbols, but at the same time, there must not be a space
    ;; between the mark and the symbol.
    (insert-character #\Space)
    (backward-object mark)
    ;; We must update the syntax in order to reflect any changes to
    ;; the parse tree our insertion of a space character may have
    ;; done.
    (update-syntax (buffer syntax) syntax)
    (show-arglist-for-form-at-mark mark syntax)
    (forward-object mark)
    (clear-completions)))

(define-command (com-complete-symbol :name t :command-table lisp-table) ()
  "Attempt to complete the symbol at mark. If successful, move point
to end of symbol.  

If more than one completion is available, a list of
possible completions will be displayed."
  (let* ((pane (current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer))
         (mark (point pane))
	 (token (symbol-at-mark mark
                                syntax)))
    (when token
      (with-syntax-package syntax mark (package)
        (let ((completion (show-completions syntax token package)))
          (unless (= (length completion) 0)
            (replace-symbol-at-mark mark syntax completion)))))))

(define-command (com-fuzzily-complete-symbol :name t :command-table lisp-table) ()
  "Attempt to fuzzily complete the abbreviation at mark.

Fuzzy completion tries to guess which symbol is abbreviated. If
the abbreviation is ambiguous, a list of possible completions
will be displayed."
  (let* ((pane (current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer))
         (mark (mark pane))
	 (name (symbol-name-at-mark mark
				    syntax)))
    (when name
      (with-syntax-package syntax mark (package)
        (let ((completion (show-fuzzy-completions syntax name package)))
          (unless (= (length completion) 0)
            (replace-symbol-at-mark mark syntax completion)))))))

(define-presentation-to-command-translator lookup-symbol-arglist
    (symbol com-lookup-arglist lisp-table
            :gesture :describe
            :tester ((object presentation)
                     (declare (ignore object))
                     (not (eq (presentation-type presentation) 'unknown-symbol)))
            :documentation "Lookup arglist")
    (object)
    (list object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gesture bindings

(esa:set-key 'com-fill-paragraph
             'lisp-table
             '((#\q :meta)))

(esa:set-key 'com-eval-defun
             'lisp-table
             '((#\x :control :meta)))

(esa:set-key `(com-indent-expression ,*numeric-argument-marker*)
             'lisp-table
             '((#\q :meta :control)))

(esa:set-key `(com-backward-up ,*numeric-argument-marker*)
             'lisp-table
             '((#\u :control :meta)))

(esa:set-key `(com-forward-down ,*numeric-argument-marker*)
             'lisp-table
             '((#\d :control :meta)))

(esa:set-key `(com-backward-expression ,*numeric-argument-marker*)
             'lisp-table
             '((#\b :control :meta)))

(esa:set-key `(com-forward-expression ,*numeric-argument-marker*)
             'lisp-table
             '((#\f :control :meta)))

(esa:set-key `(com-backward-definition ,*numeric-argument-marker*)
             'lisp-table
             '((#\a :control :meta)))

(esa:set-key `(com-forward-definition ,*numeric-argument-marker*)
             'lisp-table
             '((#\e :control :meta)))

(esa:set-key `(com-forward-list ,*numeric-argument-marker*)
             'lisp-table
             '((#\n :control :meta)))

(esa:set-key `(com-backward-list ,*numeric-argument-marker*)
             'lisp-table
             '((#\p :control :meta)))

(esa:set-key `(com-kill-expression ,*numeric-argument-marker*)
             'lisp-table
             '((#\k :control :meta)))

(esa:set-key `(com-eval-last-expression ,esa:*numeric-argument-p*)
	     'lisp-table
	     '((#\c :control) (#\e :control)))

(esa:set-key 'com-macroexpand-1
             'lisp-table
             '((#\c :control) (#\Newline)))

(esa:set-key 'com-macroexpand-1
             'lisp-table
             '((#\c :control) (#\m :control)))

(esa:set-key 'com-eval-region
	     'lisp-table
	     '((#\c :control) (#\r :control)))

(esa:set-key 'com-compile-definition
	     'lisp-table
	     '((#\c :control) (#\c :control)))

(esa:set-key 'com-compile-and-load-file
	     'lisp-table
	     '((#\c :control) (#\k :control)))

(esa:set-key  'com-compile-file
	      'lisp-table
	      '((#\c :control) (#\k :meta)))

(esa:set-key `(com-edit-this-definition)
             'lisp-table
             '((#\. :meta)))

(esa:set-key  'com-return-from-definition
	      'lisp-table
	      '((#\, :meta)))

(esa:set-key  'com-hyperspec-lookup
              'lisp-table
              '((#\c :control) (#\d :control) (#\h)))

(esa:set-key `(com-lookup-arglist-for-this-symbol)
             'lisp-table
             '((#\c :control) (#\d :control) (#\a)))

(esa:set-key 'com-space
             'lisp-table
             '((#\Space)))

(esa:set-key 'com-complete-symbol
	     'lisp-table
	     '((#\Tab :meta)))

(esa:set-key 'com-fuzzily-complete-symbol
	     'lisp-table
	     '((#\c :control) (#\i :meta)))

