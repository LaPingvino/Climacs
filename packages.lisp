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
	   #:clone-mark
	   #:no-such-offset #:offset-before-beginning #:offset-after-end
	   #:invalid-motion #:motion-before-beginning #:motion-after-end
	   #:size #:number-of-lines
	   #:offset #:mark< #:mark<= #:mark= #:mark> #:mark>=
           #:forward-object #:backward-object
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

	   #:binseq-buffer #:obinseq-buffer #:binseq2-buffer
	   #:persistent-left-sticky-mark #:persistent-right-sticky-mark
	   #:persistent-left-sticky-line-mark #:persistent-right-sticky-line-mark
	   #:p-line-mark-mixin #:buffer-line-offset

	   #:delegating-buffer #:implementation))

(defpackage :climacs-base
  (:use :clim-lisp :climacs-buffer)
  (:export #:do-buffer-region
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
	   #:buffer-looking-at #:looking-at
	   #:buffer-search-forward #:buffer-search-backward
	   #:buffer-re-search-forward #:buffer-re-search-backward
	   #:search-forward #:search-backward
	   #:re-search-forward #:re-search-backward))

(defpackage :climacs-abbrev
  (:use :clim-lisp :clim :climacs-buffer :climacs-base)
  (:export #:abbrev-expander #:dictionary-abbrev-expander #:dictionary
	   #:expand-abbrev #:abbrev-mixin #:possibly-expand-abbrev
	   #:add-abbrev))

(defpackage :climacs-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :flexichain)
  (:export #:syntax #:define-syntax
	   #:syntax-from-name
	   #:basic-syntax
	   #:update-syntax #:update-syntax-for-display
	   #:grammar #:grammar-rule #:add-rule
	   #:parser #:initial-state
	   #:advance-parse
	   #:parse-tree #:start-offset #:end-offset
	   #:lexer #:nb-lexemes #:lexeme #:insert-lexeme
	   #:incremental-lexer #:next-lexeme
	   #:delete-invalid-lexemes #:inter-lexeme-object-p
	   #:skip-inter-lexeme-objects #:update-lex
	   #:parse-stack-top #:target-parse-tree #:parse-state-empty-p
	   #:parse-stack-next #:parse-stack-symbol
	   #:parse-stack-parse-trees #:map-over-parse-trees
	   #:no-such-operation #:no-expression
	   #:name-for-info-pane
           #:syntax-line-indentation
	   #:forward-expression #:backward-expression
	   #:eval-defun
	   #:beginning-of-definition #:end-of-definition
	   #:redisplay-pane-with-syntax
	   #:backward-paragraph #:forward-paragraph
	   #:backward-sentence #:forward-sentence
	   #:forward-list #:backward-list
	   #:down-list #:up-list
	   #:backward-down-list #:backward-up-list
	   #:syntax-line-comment-string
	   #:line-comment-region #:comment-region
	   #:line-uncomment-region #:uncomment-region))

(defpackage :climacs-kill-ring
  (:use :clim-lisp :flexichain)
  (:export #:kill-ring      #:kill-ring-length      #:kill-ring-max-size
	   #:append-next-p 
	   #:reset-yank-position #:rotate-yank-position #:kill-ring-yank
	   #:kill-ring-standard-push    #:kill-ring-concatenating-push
	   #:kill-ring-reverse-concatenating-push))

(defpackage :undo
  (:use :common-lisp)
  (:export #:no-more-undo
	   #:undo-tree #:standard-undo-tree
	   #:undo-record #:standard-undo-record
	   #:add-undo #:flip-undo-record #:undo #:redo))

(defpackage :climacs-pane
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :climacs-abbrev
	:climacs-syntax :flexichain :undo)
  (:export #:climacs-buffer #:needs-saving #:filepath
	   #:read-only-p #:buffer-read-only
	   #:climacs-pane #:point #:mark
	   #:redisplay-pane #:full-redisplay
	   #:display-cursor
	   #:display-mark
	   #:page-down #:page-up
	   #:top #:bot
           #:tab-space-count #:space-width #:tab-width
           #:indent-tabs-mode
           #:auto-fill-mode #:auto-fill-column
           #:isearch-state #:search-string #:search-mark
           #:search-forward-p #:search-success-p
           #:isearch-mode #:isearch-states #:isearch-previous-string
           #:query-replace-state #:string1 #:string2
           #:query-replace-mode
	   #:mark-visible-p
	   #:with-undo
	   #:url
	   #:climacs-textual-view #:+climacs-textual-view+))

(defpackage :climacs-fundamental-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane)
  (:export))

(defpackage :climacs-html-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base
	:climacs-syntax :flexichain :climacs-pane))

(defpackage :climacs-prolog-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base
	:climacs-syntax :flexichain :climacs-pane)
  (:shadow "ATOM" "CLOSE" "EXP" "INTEGER" "OPEN" "VARIABLE"))

(defpackage :climacs-cl-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane)
  (:export))

(defpackage :climacs-lisp-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane)
  (:export :lisp-string))

(defpackage :esa
  (:use :clim-lisp :clim)
  (:export #:minibuffer-pane #:display-message
	   #:esa-pane-mixin #:previous-command
	   #:info-pane #:master-pane
	   #:esa-frame-mixin #:windows #:recordingp #:executingp
	   #:*numeric-argument-p* #:*current-gesture*
	   #:esa-top-level #:simple-command-loop
	   #:global-esa-table #:keyboard-macro-table
	   #:help-table
	   #:set-key
	   #:find-applicable-command-table))

(defpackage :climacs-gui
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :climacs-abbrev :climacs-syntax
	:climacs-kill-ring :climacs-pane :clim-extensions :undo :esa)
  (:import-from :climacs-lisp-syntax :lisp-string)
  (:export :climacs))
