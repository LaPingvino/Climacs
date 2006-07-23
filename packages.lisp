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
           #:forward-object
           #:backward-object
           #:forward-line-start #:backward-line-start
           #:forward-line-end #:backward-line-end
	   #:beginning-of-buffer #:end-of-buffer
	   #:beginning-of-buffer-p #:end-of-buffer-p
	   #:beginning-of-line #:end-of-line
	   #:beginning-of-line-p #:end-of-line-p
	   #:buffer-line-number #:buffer-column-number
	   #:line-number #:column-number
	   #:insert-buffer-object #:insert-buffer-sequence
           #:buffer-substring
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
	   #:delegating-buffer #:implementation)
  (:documentation "An implementation of the Climacs buffer
  protocol. This package is quite low-level, not syntax-aware,
  not CLIM-aware and not user-oriented at all."))

(defpackage :climacs-kill-ring
  (:use :clim-lisp :flexichain)
  (:export #:kill-ring
           #:kill-ring-length #:kill-ring-max-size
	   #:append-next-p
	   #:reset-yank-position #:rotate-yank-position #:kill-ring-yank
	   #:kill-ring-standard-push #:kill-ring-concatenating-push
	   #:kill-ring-reverse-concatenating-push)
  (:documentation "An implementation of a kill ring."))

(defpackage :climacs-base
  (:use :clim-lisp :climacs-buffer :climacs-kill-ring)
  (:export #:as-offsets
           #:do-buffer-region
           #:do-buffer-region-lines
	   #:previous-line #:next-line
           #:open-line
           #:delete-line
           #:empty-line-p
           #:line-indentation
           #:buffer-display-column
	   #:number-of-lines-in-region
	   #:constituentp
           #:just-n-spaces
           #:buffer-whitespacep
	   #:forward-word #:backward-word
           #:buffer-region-case
	   #:input-from-stream #:output-to-stream
	   #:name-mixin #:name
	   #:buffer-looking-at #:looking-at
	   #:buffer-search-forward #:buffer-search-backward
	   #:buffer-re-search-forward #:buffer-re-search-backward
	   #:search-forward #:search-backward
	   #:re-search-forward #:re-search-backward
           #:downcase-buffer-region #:downcase-region
           #:upcase-buffer-region #:upcase-region
           #:capitalize-buffer-region #:capitalize-region
           #:tabify-region #:untabify-region
           #:indent-line #:delete-indentation
           #:*kill-ring*)
  (:documentation "Basic functionality built on top of the buffer
 protocol. Here is where we define slightly higher level
 functions, that can be directly implemented in terms of the
 buffer protocol, but that are not, strictly speaking, part of
 that protocol. The functions in this package are not
 syntax-aware, and are thus limited in what they can do. They
 percieve the buffer as little more than a sequence of
 characters."))

(defpackage :climacs-abbrev
  (:use :clim-lisp :clim :climacs-buffer :climacs-base)
  (:export #:abbrev-expander #:dictionary-abbrev-expander #:dictionary
	   #:expand-abbrev #:abbrev-mixin #:possibly-expand-abbrev
	   #:add-abbrev))

(defpackage :climacs-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :flexichain)
  (:export #:syntax #:define-syntax
           #:eval-option
           #:define-option-for-syntax
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
	   #:line-uncomment-region #:uncomment-region
           #:word-constituentp
           #:whitespacep
           #:page-delimiter
           #:paragraph-delimiter)
  (:documentation "The Climacs syntax protocol. Contains
  functions that can be used to implement higher-level operations
  on buffer contents."))

(defpackage :undo
  (:use :clim-lisp)
  (:export #:no-more-undo
	   #:undo-tree #:standard-undo-tree
	   #:undo-record #:standard-undo-record
	   #:add-undo #:flip-undo-record #:undo #:redo))

(defpackage :climacs-pane
  (:use :clim-lisp :clim :climacs-buffer :climacs-base :climacs-abbrev
	:climacs-syntax :flexichain :undo)
  (:export #:climacs-buffer #:needs-saving
	   #:filepath #:file-saved-p #:file-write-time
	   #:read-only-p #:buffer-read-only
	   #:climacs-pane #:point #:mark
           #:clear-cache
	   #:redisplay-pane #:full-redisplay
	   #:display-cursor
	   #:display-region
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
	   #:region-visible-p
	   #:with-undo
	   #:url
	   #:climacs-textual-view #:+climacs-textual-view+))

(defpackage :climacs-motion
  (:use :clim-lisp :climacs-base :climacs-buffer :climacs-syntax)
  (:export #:forward-to-word-boundary #:backward-to-word-boundary
           #:define-motion-fns
           #:beep-limit-action #:revert-limit-action #:error-limit-action
           #:motion-limit-error
           #:make-diligent-motor

           ;; Lines
           #:forward-one-line
           #:backward-one-line
           #:forward-line
           #:backward-line

           ;; Words
           #:forward-one-word
           #:backward-one-word
           #:forward-word
           #:backward-word

           ;; Pages
           #:forward-one-page
           #:backward-one-page
           #:forward-page
           #:backward-page

           ;; Expressions
           #:forward-one-expression
           #:backward-one-expression
           #:forward-expression
           #:backward-expression

           ;; Definitions
           #:forward-one-definition
           #:backward-one-definition
           #:forward-definition
           #:backward-definition

           ;; Up
           #:forward-one-up
           #:backward-one-up
           #:forward-up
           #:backward-up

           ;; Down
           #:forward-one-down
           #:backward-one-down
           #:forward-down
           #:backward-down

           ;; Paragraphs
           #:forward-one-paragraph
           #:backward-one-paragraph
           #:forward-paragraph
           #:backward-paragraph

           ;; Sentences
           #:forward-one-sentence
           #:backward-one-sentence
           #:forward-sentence
           #:backward-sentence)
  (:documentation "Functions and facilities for moving a mark
  around by syntactical elements. The functions in this package
  are syntax-aware, and their behavior is based on the semantics
  defined by the syntax of the buffer, that the mark they are
  manipulating belong to. These functions are also directly used
  to implement the motion commands."))

(defpackage :climacs-editing
  (:use :clim-lisp :climacs-base :climacs-buffer
        :climacs-syntax :climacs-motion :climacs-pane :climacs-kill-ring)
  (:export #:transpose-objects
           
           ;; Lines
           #:forward-delete-line #:backward-delete-line
           #:forward-kill-line #:backward-kill-line
           #:transpose-lines
           #:forward-delete-line-start #:backward-delete-line-start
           #:forward-kill-line-start #:backward-kill-line-start
           #:transpose-line-starts
           
           ;; Words
           #:forward-delete-word #:backward-delete-word
           #:forward-kill-word #:backward-kill-word
           #:transpose-words

           ;; Pages
           #:forward-delete-page #:backward-delete-page
           #:forward-kill-page #:backward-kill-page
           #:transpose-page
           
           ;; Expressions
           #:forward-delete-expression #:backward-delete-expression
           #:forward-kill-expression #:backward-kill-expression
           #:transpose-expressions

           ;; Definitions
           #:forward-delete-definition #:backward-delete-definition
           #:forward-kill-definition #:backward-kill-definition
           #:transpose-definitions

           ;; Paragraphs
           #:forward-delete-paragraph #:backward-delete-paragraph
           #:forward-kill-paragraph #:backward-kill-paragraph
           #:transpose-paragraphs

           ;; Sentences
           #:forward-delete-sentence #:backward-delete-sentence
           #:forward-kill-sentence #:backward-kill-sentence
           #:transpose-sentences
           

           #:downcase-word #:upcase-word #:capitalize-word
 
           #:indent-region
           #:fill-line
           #:fill-region)
  (:documentation "Functions and facilities for changing the
  buffer contents by syntactical elements. The functions in this package
  are syntax-aware, and their behavior is based on the semantics
  defined by the syntax of the buffer, that the mark they are
  manipulating belong to. These functions are also directly used
  to implement the editing commands."))

(defpackage :climacs-gui
  (:use :clim-lisp :clim :climacs-buffer :climacs-base
	:climacs-abbrev :climacs-syntax :climacs-motion
	:climacs-kill-ring :climacs-pane :clim-extensions
        :undo :esa :climacs-editing :climacs-motion)
  ;;(:import-from :lisp-string)
  (:export #:climacs ; Frame.
           
           ;; GUI functions follow.
           #:current-window
           #:current-point
           #:current-buffer
           #:current-buffer
           #:point
           #:syntax
           #:mark
           #:insert-character
           #:base-table
           #:buffer-table
           #:case-table
           #:comment-table
           #:deletion-table
           #:development-table
           #:editing-table
           #:fill-table
           #:indent-table
           #:info-table
           #:marking-table
           #:movement-table
           #:pane-table
           #:search-table
           #:self-insert-table
           #:window-table
           
           ;; Some configuration variables
           #:*bg-color*
           #:*fg-color*
           #:*info-bg-color*
           #:*info-fg-color*
           #:*mini-bg-color*
           #:*mini-fg-color*))

(defpackage :climacs-commands
  (:use :clim-lisp :clim :climacs-base :climacs-buffer
        :climacs-syntax :climacs-motion :climacs-editing
        :climacs-gui :esa :climacs-kill-ring)
  (:export #:define-motion-commands
           #:define-deletion-commands
           #:define-editing-commands)
  (:documentation "This package is meant to contain Climacs'
  command definitions, as well as some useful automatic
  command-defining facilities."))

(defpackage :climacs-fundamental-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane)
  (:export #:fundamental-syntax))

(defpackage :climacs-html-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base
	:climacs-syntax :flexichain :climacs-pane))

(defpackage :climacs-prolog-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base
	:climacs-syntax :flexichain :climacs-pane)
  (:shadow #:atom #:close #:exp #:integer #:open #:variable))

(defpackage :climacs-cl-syntax
  (:use :clim-lisp :clim :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane)
  (:export))

(defpackage :climacs-lisp-syntax
  (:use :clim-lisp :clim :clim-extensions :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane :climacs-gui :climacs-motion :climacs-editing)
  (:export #:lisp-string
           #:edit-definition))

(defpackage :climacs
  (:use :clim-lisp :clim :clim-sys :clim-extensions :climacs-gui)
  (:export #:climacs
           #:climacs-rv
           #:edit-definition)
  (:documentation "Package containing entry points to Climacs."))