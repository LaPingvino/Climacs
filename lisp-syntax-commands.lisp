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

(define-command (com-eval-defun :name t :command-table lisp-table) ()
  (let* ((pane (current-window))
         (point (point pane))
         (syntax (syntax (buffer pane))))
    (eval-defun point syntax)))

(esa:set-key 'com-eval-defun
             'lisp-table
             '((#\x :control :meta)))

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
                     t)))))

(esa:set-key 'com-fill-paragraph
             'lisp-table
             '((#\q :meta)))

(define-command (com-indent-expression :name t :command-table lisp-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane (current-window))
         (point (point pane))
         (mark (clone-mark point))
         (syntax (syntax (buffer pane)))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view)))
    (if (plusp count)
        (loop repeat count do (forward-expression mark syntax))
        (loop repeat (- count) do (backward-expression mark syntax)))
    (indent-region pane (clone-mark point) mark)))

(esa:set-key `(com-indent-expression ,*numeric-argument-marker*)
             'lisp-table
             '((#\q :meta :control)))