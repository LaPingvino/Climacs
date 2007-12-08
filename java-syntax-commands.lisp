;;; -*- Mode: Lisp; Package: CLIMACS-JAVA-SYNTAX; -*-

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

;;; Commands specific to the Java syntax for Climacs.

(in-package :climacs-java-syntax)

;;; This command table is used when Drei runs as a pane.
(make-command-table 'pane-java-table
                    :errorp nil)

(defmethod additional-command-tables append ((drei drei-pane) 
					     (command-table java-table))
  '(pane-java-table))

;; Movement commands.
(drei-commands:define-motion-commands expression java-table)
;; (drei-commands:define-motion-commands definition java-table)
(drei-commands:define-motion-commands up java-table
  :noun "nesting level up"
  :plural "levels")
(drei-commands:define-motion-commands down java-table
  :noun "nesting level down"
  :plural "levels")
(drei-commands:define-motion-commands list java-table)

(drei-commands:define-editing-commands expression java-table)
(drei-commands:define-deletion-commands expression java-table)

(define-command (com-fill-paragraph :name t :command-table java-table) 
    ()
  "Fill paragraph at point. Will have no effect unless there is a
string at point."
  (let* ((token (form-around (current-syntax) (offset (point))))
         (fill-column (auto-fill-column (current-view))))
    (when (typep token 'string-form)
      (with-accessors ((offset1 start-offset)
                       (offset2 end-offset)) token
        (fill-region (make-buffer-mark (current-buffer) offset1 :right)
                     (make-buffer-mark (current-buffer) offset2 :right)
                     #'(lambda (mark)
                         (syntax-line-indentation
                          mark (tab-space-count (current-view)) syntax))
                     fill-column
                     (tab-space-count (current-view))
                     syntax
                     t)))))

(define-command (com-indent-expression :name t :command-table java-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((mark (clone-mark (point))))
    (if (plusp count)
        (loop repeat count do (forward-expression mark (current-syntax)))
        (loop repeat (- count) do (backward-expression mark (current-syntax))))
    (indent-region *drei-instance* (point) mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gesture bindings

(set-key 'com-fill-paragraph
         'java-table
         '((#\q :meta)))

(set-key `(com-indent-expression ,*numeric-argument-marker*)
         'java-table
         '((#\q :meta :control)))

(set-key `(com-backward-up ,*numeric-argument-marker*)
         'java-table
         '((#\u :control :meta)))

(set-key `(com-forward-down ,*numeric-argument-marker*)
         'java-table
         '((#\d :control :meta)))

(set-key `(com-backward-expression ,*numeric-argument-marker*)
         'java-table
         '((#\b :control :meta)))

(set-key `(com-forward-expression ,*numeric-argument-marker*)
         'java-table
         '((#\f :control :meta)))

;; (set-key `(com-backward-definition ,*numeric-argument-marker*)
;;          'java-table
;;          '((#\a :control :meta)))

;; (set-key `(com-forward-definition ,*numeric-argument-marker*)
;;          'java-table
;;          '((#\e :control :meta)))

(set-key `(com-forward-list ,*numeric-argument-marker*)
         'java-table
         '((#\n :control :meta)))

(set-key `(com-backward-list ,*numeric-argument-marker*)
         'java-table
         '((#\p :control :meta)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
         'java-table
         '((#\k :control :meta)))

(set-key `(com-transpose-expressions)
         'java-table
         '((#\t :control :meta)))