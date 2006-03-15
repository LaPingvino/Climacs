;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

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
	 (package (climacs-lisp-syntax::package-of syntax)))
    (esa:display-message (format nil "~A" (package-name package)))))

(define-command (com-fill-paragraph :name t :command-table lisp-table) ()
  )

(esa:set-key 'com-fill-paragraph
	     'lisp-table
	     '((#\q :meta)))