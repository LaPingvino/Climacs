;;; -*- Mode: Lisp; Package: CLIMACS-C-SYNTAX; -*-

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
;;;  (c) copyright 2007 by
;;;           John Q. Splittist (splittist@splittist.com)
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

;;; Commands specific to the C syntax for Climacs.

(in-package :climacs-c-syntax)

;;; This command table is used when Drei runs as a pane.
(make-command-table 'pane-c-table
                    :errorp nil)

(defmethod additional-command-tables append ((drei drei-pane) (command-table c-table))
  '(pane-c-table))

;; Movement commands.
(drei-commands:define-motion-commands expression c-table)
;; (drei-commands:define-motion-commands definition c-table)
(drei-commands:define-motion-commands up c-table
  :noun "nesting level up"
  :plural "levels")
(drei-commands:define-motion-commands down c-table
  :noun "nesting level down"
  :plural "levels")
(drei-commands:define-motion-commands list c-table)

(drei-commands:define-editing-commands expression c-table)
(drei-commands:define-deletion-commands expression c-table)

(define-command (com-fill-paragraph :name t :command-table c-table) 
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
    (when (form-string-p token)
      (with-accessors ((offset1 start-offset) 
                       (offset2 end-offset)) token
        (fill-region (make-instance 'standard-right-sticky-mark
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

(define-command (com-indent-expression :name t :command-table c-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
         (point (point pane))
         (mark (clone-mark point))
         (syntax (syntax (buffer pane))))
    (if (plusp count)
        (loop repeat count do (forward-expression mark syntax))
        (loop repeat (- count) do (backward-expression mark syntax)))
    (indent-region pane (clone-mark point) mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gesture bindings

(set-key 'com-fill-paragraph
         'c-table
         '((#\q :meta)))

(set-key `(com-indent-expression ,*numeric-argument-marker*)
         'c-table
         '((#\q :meta :control)))

(set-key `(com-backward-up ,*numeric-argument-marker*)
         'c-table
         '((#\u :control :meta)))

(set-key `(com-forward-down ,*numeric-argument-marker*)
         'c-table
         '((#\d :control :meta)))

(set-key `(com-backward-expression ,*numeric-argument-marker*)
         'c-table
         '((#\b :control :meta)))

(set-key `(com-forward-expression ,*numeric-argument-marker*)
         'c-table
         '((#\f :control :meta)))

;; (set-key `(com-backward-definition ,*numeric-argument-marker*)
;;          'c-table
;;          '((#\a :control :meta)))

;; (set-key `(com-forward-definition ,*numeric-argument-marker*)
;;          'c-table
;;          '((#\e :control :meta)))

(set-key `(com-forward-list ,*numeric-argument-marker*)
         'c-table
         '((#\n :control :meta)))

(set-key `(com-backward-list ,*numeric-argument-marker*)
         'c-table
         '((#\p :control :meta)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
         'c-table
         '((#\k :control :meta)))

(set-key `(com-transpose-expressions)
         'c-table
         '((#\t :control :meta)))