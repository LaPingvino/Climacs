;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)

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
  (:use :clim-lisp :flexichain)
  (:export #:buffer #:standard-buffer
	   #:mark #:left-sticky-mark #:right-sticky-mark
	   #:standard-left-sticky-mark #:standard-right-sticky-mark
	   #:clone-mark #:no-such-offset #:size #:number-of-lines
	   #:offset #:mark< #:mark<= #:mark= #:mark> #:mark>=
	   #:beginning-of-buffer #:end-of-buffer
	   #:beginning-of-buffer-p #:end-of-buffer-p
	   #:beginning-of-line #:end-of-line
	   #:beginning-of-line-p #:end-of-line-p
	   #:line-number #:column-number
	   #:insert-buffer-object #:insert-buffer-sequence
	   #:insert-object #:insert-sequence
	   #:delete-buffer-range #:delete-range
	   #:delete-region
	   #:buffer-object #:buffer-sequence
	   #:object-before #:object-after #:region-to-sequence
	   #:low-mark #:high-mark #:modified-p #:clear-modify))

(defpackage :climacs-base
  (:use :clim-lisp :climacs-buffer)
  (:export #:previous-line #:next-line
	   #:open-line #:kill-line
	   #:number-of-lines-in-region
	   #:constituentp #:whitespacep
	   #:forward-word #:backward-word
	   #:delete-word #:backward-delete-word
	   #:input-from-stream #:output-to-stream))

(defpackage :climacs-abbrev
  (:use :clim-lisp :clim :climacs-buffer :climacs-base)
  (:export #:abbrev-expander #:dictionary-abbrev-expander #:dictionary
	   #:expand-abbrev #:abbrev-mixin #:possibly-expand-abbrev))

(defpackage :climacs-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :flexichain)
  (:export #:syntax #:basic-syntax #:texinfo-syntax
	   #:redisplay-pane #:redisplay-with-syntax #:full-redisplay
	   #:url))

(defpackage :climacs-kill-ring
  (:use :clim-lisp :climacs-buffer :flexichain)
  (:export #:initialize-kill-ring #:kr-length
	   #:kr-resize #:kr-rotate #:kill-ring
	   #:kr-copy #:kr-push #:kr-pop))

(defpackage :climacs-gui
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :climacs-abbrev :climacs-syntax :climacs-kill-ring))

