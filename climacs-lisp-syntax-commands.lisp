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

(in-package :drei-lisp-syntax)

(make-command-table 'climacs-lisp-table
                    :errorp nil)

(defmethod additional-command-tables append ((frame climacs-gui:climacs) (command-table lisp-table))
  '(climacs-lisp-table))

(define-command (com-package :name t :command-table climacs-lisp-table) ()
  (let ((package (package-at-mark (current-syntax) (point))))
    (esa:display-message (format nil "~A" (if (packagep package)
                                              (package-name package)
                                              package)))))

(define-command (com-set-base :name t :command-table climacs-lisp-table)
    ((base '(integer 2 36)))
  "Set the base for the current buffer."
  (setf (base (current-syntax)) base))

(define-command (com-set-package :name t :command-table climacs-lisp-table)
    ((package 'package))
  "Set the package for the current buffer."
  (setf (option-specified-package (current-syntax)) package))

(define-command (com-macroexpand-1 :name t :command-table climacs-lisp-table)
    ()
  "Macroexpand-1 the expression at point.

The expanded expression will be displayed in a
\"*Macroexpansion*\"-buffer."
  (let* ((token (expression-at-mark (current-syntax) (point))))
    (if token
        (macroexpand-token (current-syntax) token)
        (esa:display-message "Nothing to expand at point."))))

(define-command (com-macroexpand-all :name t :command-table climacs-lisp-table)
    ()
  "Completely macroexpand the expression at point.

The expanded expression will be displayed in a
\"*Macroexpansion*\"-buffer."
  (let ((token (expression-at-mark (current-syntax) (point))))
    (if token
        (macroexpand-token (current-syntax) token t)
        (esa:display-message "Nothing to expand at point."))))

(define-command (com-compile-and-load-file :name t :command-table climacs-lisp-table)
    ()
  "Compile and load the current file.

Compiler notes will be displayed in a seperate buffer."
  (compile-file-interactively (current-buffer) t))

(define-command (com-compile-file :name t :command-table climacs-lisp-table)
    ()
  "Compile the file open in the current buffer.

This command does not load the file after it has been compiled."
  (compile-file-interactively (current-view) nil))

(define-command (com-goto-location :name t :command-table climacs-lisp-table)
    ((note 'compiler-note))
  "Move point to the part of a given file that caused the
compiler note.

If the file is not already open, a new buffer will be opened with
that file."
  (goto-location (location note)))

(define-presentation-to-command-translator compiler-note-to-goto-location-translator
    (compiler-note com-goto-location climacs-lisp-table)
    (presentation)
  (list (presentation-object presentation)))

(define-command (com-goto-xref :name t :command-table climacs-lisp-table)
    ((xref 'xref))
  "Go to the referenced location of a code cross-reference."
  (goto-location xref))

(define-presentation-to-command-translator xref-to-goto-location-translator
    (xref com-goto-xref climacs-lisp-table)
    (presentation)
    (list (presentation-object presentation)))

(define-command (com-edit-this-definition :command-table climacs-lisp-table)
    ()
  "Edit definition of the symbol at point.
If there is no symbol at point, this is a no-op."
  (let* ((token (this-form (current-syntax) (point)))
         (this-symbol (form-to-object (current-syntax) token)))
    (when (and this-symbol (symbolp this-symbol))
      (edit-definition this-symbol))))

(define-command (com-return-from-definition :name t :command-table climacs-lisp-table)
    ()
  "Return point to where it was before the previous Edit
Definition command was issued."
  (pop-find-definition-stack))

(define-command (com-compile-definition :name t :command-table pane-lisp-table)
    ()
  "Compile and load definition at point."
  (evaluating-interactively
    (compile-definition-interactively (current-view) (point))))

(esa:set-key 'com-eval-defun
             'climacs-lisp-table
             '((#\x :control :meta)))

(esa:set-key 'com-macroexpand-1
             'climacs-lisp-table
             '((#\c :control) (#\Newline)))

(esa:set-key 'com-macroexpand-all
             'climacs-lisp-table
             '((#\c :control) (#\m :control)))

(esa:set-key 'com-compile-and-load-file
	     'climacs-lisp-table
	     '((#\c :control) (#\k :control)))

(esa:set-key 'com-compile-file
             'climacs-lisp-table
             '((#\c :control) (#\k :meta)))

(esa:set-key 'com-edit-this-definition
             'climacs-lisp-table
             '((#\. :meta)))

(esa:set-key  'com-return-from-definition
	      'climacs-lisp-table
	      '((#\, :meta)))

(set-key 'com-compile-definition
         'pane-lisp-table
         '((#\c :control) (#\c :control)))
