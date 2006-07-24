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

;;; Commands for developing the Climacs editor. 

(in-package :climacs-commands)

(define-command (com-reset-profile :name t :command-table development-table) ()
  #+sbcl (sb-profile:reset)
  #-sbcl nil)

(define-command (com-report-profile :name t :command-table development-table) ()
  #+sbcl (sb-profile:report)
  #-sbcl nil)

(define-command (com-recompile :name t :command-table development-table) ()
  (asdf:operate 'asdf:load-op :climacs))


(define-gesture-name :select-other #+mcclim :pointer-button-press #-mcclim :pointer-button (:left :meta) :unique nil)

(define-presentation-translator lisp-string-to-string
    (climacs-lisp-syntax::lisp-string string development-table
                  :gesture :select-other
                  :tester-definitive t
                  :menu nil
                  :priority 10)
    (object)
  object)

(define-command (com-accept-string :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'string))))
 
(define-command (com-accept-symbol :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'symbol))))	 

(define-command (com-accept-lisp-string :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'lisp-string))))
