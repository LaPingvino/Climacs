;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Package definitions for the Climacs editor.

(in-package :cl-user)

(defpackage :climacs-gui
    (:use :clim-lisp :clim :drei-buffer :drei-base
          :drei-abbrev :drei-syntax :drei-motion
          :drei-kill-ring :drei :clim-extensions
          :drei-undo :esa :drei-editing :drei-motion
          :esa-buffer :esa-io :esa-utils)
    ;;(:import-from :lisp-string)
    (:export #:climacs                  ; Frame.

             #:climacs-buffer #:external-format
             #:climacs-pane
             #:climacs-info-pane
             #:typeout-pane
             #:kill-ring
           
             ;; GUI functions follow.
             #:any-buffer
             #:point
             #:syntax
             #:mark
             #:buffers
             #:active-group
             #:groups
             #:display-window
             #:split-window
             #:typeout-window
             #:delete-window
             #:other-window
             #:buffer-pane-p
           
             ;; Some configuration variables
             #:*bg-color*
             #:*fg-color*
             #:*info-bg-color*
             #:*info-fg-color*
             #:*mini-bg-color*
             #:*mini-fg-color*
             #:*with-scrollbars*
             #:*default-external-format*

             ;; The command tables
             #:global-climacs-table #:keyboard-macro-table #:climacs-help-table
             #:base-table #:buffer-table #:case-table 
              #:development-table
              #:info-table #:pane-table
             #:window-table))

(defpackage :climacs-core
  (:use :clim-lisp :drei-base :drei-buffer :drei-fundamental-syntax
        :drei-syntax :drei-motion :drei :drei-kill-ring
        :drei-editing :climacs-gui :clim :drei-abbrev :esa :esa-buffer :esa-io
        :esa-utils :drei-core)
  (:export #:display-string
           #:object-equal
           #:object=
           #:no-upper-p
           #:case-relevant-test
           
           #:switch-to-buffer
           #:make-new-buffer
           #:make-new-named-buffer
           #:erase-buffer
           #:kill-buffer

           #:filepath-filename
           #:update-attribute-line
           #:evaluate-attribute-line
           #:directory-pathname-p
           #:find-file #:find-file-read-only
           #:directory-of-buffer
           #:set-visited-filename
           #:check-file-times
           #:save-buffer

           #:input-from-stream
           #:save-buffer-to-stream
           #:make-buffer-from-stream

           #:group
           #:group-element
           #:standard-group
           #:current-buffer-group
           #:add-group
           #:get-group
           #:get-active-group
           #:deselect-group
           #:with-group-buffers
           #:define-group
           #:group-not-found
           #:group-buffers
           #:ensure-group-buffers
           #:select-group
           #:display-group-contents)
  (:documentation "Package for editor functionality that is
  syntax-aware, but yet not specific to certain
  syntaxes. Contains stuff like indentation, filling and other
  features that require a fairly high-level view of the
  application, but are not solely GUI-specific."))

(defpackage :climacs-commands
  (:use :clim-lisp :clim :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei-editing
        :climacs-gui :esa :drei-kill-ring :drei
        :drei-abbrev :drei-undo :climacs-core :drei-core)
  (:documentation "This package is meant to contain Climacs'
  command definitions, as well as some useful automatic
  command-defining facilities."))

(defpackage :climacs-html-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base
	:drei-syntax :flexichain :drei :drei-fundamental-syntax))

(defpackage :climacs-prolog-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base
	:drei-syntax :flexichain :drei :climacs-core :drei-fundamental-syntax
        :drei)
  (:shadow #:atom #:close #:exp #:integer #:open #:variable))

(defpackage :climacs-cl-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base 
	:drei-syntax :flexichain :drei :drei-fundamental-syntax)
  (:export))

(defpackage :climacs-c-syntax
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base
	:drei-syntax :drei-fundamental-syntax :flexichain :drei
	:drei-motion :drei-editing :esa-utils :esa :drei-core :esa-io
	:drei-lr-syntax)
  (:shadow clim:form)
  (:export #:c-syntax)
  (:documentation "Implementation of the syntax module used for
editing C code."))

(defpackage :climacs
  (:use :clim-lisp :clim :clim-sys :clim-extensions :climacs-gui :drei)
  (:export #:climacs
           #:climacs-rv
           #:edit-definition)
  (:documentation "Package containing entry points to Climacs."))