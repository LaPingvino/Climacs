;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)

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

(defpackage :climacs-buffer
  (:use :clim-lisp :flexichain :binseq)
  (:export #:buffer #:standard-buffer
	   #:mark #:left-sticky-mark #:right-sticky-mark
	   #:standard-left-sticky-mark #:standard-right-sticky-mark
	   #:clone-mark #:no-such-offset #:size #:number-of-lines
	   #:offset #:mark< #:mark<= #:mark= #:mark> #:mark>=
	   #:beginning-of-buffer #:end-of-buffer
	   #:beginning-of-buffer-p #:end-of-buffer-p
	   #:beginning-of-line #:end-of-line
	   #:beginning-of-line-p #:end-of-line-p
	   #:buffer-line-number #:buffer-column-number
	   #:line-number #:column-number
	   #:insert-buffer-object #:insert-buffer-sequence
	   #:insert-object #:insert-sequence
	   #:delete-buffer-range #:delete-range
	   #:delete-region
	   #:buffer-object #:buffer-sequence
	   #:object-before #:object-after #:region-to-sequence
	   #:low-mark #:high-mark #:modified-p #:clear-modify

	   #:binseq-buffer #:obinseq-buffer
	   #:persistent-left-sticky-mark #:persistent-right-sticky-mark))

(defpackage :climacs-base
  (:use :clim-lisp :climacs-buffer)
  (:export #:do-buffer-region
           #:forward-object #:backward-object
	   #:previous-line #:next-line
	   #:open-line #:kill-line
           #:empty-line-p
           #:line-indentation
           #:buffer-display-column
	   #:number-of-lines-in-region
	   #:constituentp #:whitespacep
	   #:forward-word #:backward-word
	   #:delete-word #:backward-delete-word
           #:buffer-region-case
           #:upcase-buffer-region #:upcase-region
           #:downcase-buffer-region #:downcase-region
           #:capitalize-buffer-region #:capitalize-region
           #:upcase-word #:downcase-word #:capitalize-word
           #:tabify-region #:untabify-region
           #:indent-line
           #:delete-indentation
           #:fill-line
	   #:input-from-stream #:output-to-stream
	   #:name-mixin #:name
	   #:buffer-lookin-at #:looking-at
	   #:buffer-search-forward #:buffer-search-backward
	   #:search-forward #:search-backward))

(defpackage :climacs-abbrev
  (:use :clim-lisp :clim :climacs-buffer :climacs-base)
  (:export #:abbrev-expander #:dictionary-abbrev-expander #:dictionary
	   #:expand-abbrev #:abbrev-mixin #:possibly-expand-abbrev
	   #:add-abbrev))

(defpackage :climacs-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :flexichain)
  (:export #:syntax #:define-syntax
	   #:basic-syntax
	   #:update-syntax
           #:syntax-line-indentation
	   #:beginning-of-paragraph #:end-of-paragraph))

(defpackage :climacs-kill-ring
  (:use :clim-lisp :flexichain)
  (:export #:kill-ring      #:kill-ring-length      #:kill-ring-max-size 
	   #:reset-yank-position #:rotate-yank-position #:kill-ring-yank
	   #:kill-ring-standard-push    #:kill-ring-concatenating-push))

(defpackage :undo
  (:use :common-lisp)
  (:export #:no-more-undo
	   #:undo-tree #:standard-undo-tree
	   #:undo-record #:standard-undo-record
	   #:add-undo #:flip-undo-record #:undo #:redo))

(defpackage :climacs-pane
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :climacs-abbrev
	:climacs-syntax :flexichain :undo)
  (:export #:climacs-buffer #:needs-saving #:filename
	   #:climacs-pane #:point #:mark
	   #:redisplay-pane #:full-redisplay
	   #:page-down #:page-up
           #:tab-space-count
           #:indent-tabs-mode
           #:auto-fill-mode #:auto-fill-column
           #:isearch-state #:search-string #:search-mark
           #:search-forward-p #:search-success-p
           #:isearch-mode #:isearch-states #:isearch-previous-string
           #:query-replace-state #:string1 #:string2
           #:query-replace-mode
	   #:with-undo
	   #:url))

(defpackage :climacs-gui
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :climacs-abbrev :climacs-syntax
	:climacs-kill-ring :climacs-pane :clim-extensions :undo))

