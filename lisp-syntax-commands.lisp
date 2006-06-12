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
(climacs-motion-commands:define-motion-commands expression lisp-table)
(climacs-motion-commands:define-motion-commands definition lisp-table)
(climacs-motion-commands:define-motion-commands up lisp-table
  :noun "nesting level up"
  :plural "levels")
(climacs-motion-commands:define-motion-commands down lisp-table
  :noun "nesting level down"
  :plural "levels")
(climacs-motion-commands:define-motion-commands list lisp-table)

(climacs-editing-commands:define-editing-commands expression lisp-table)
(climacs-editing-commands:define-deletion-commands expression lisp-table)

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