;;; -*- Mode: Lisp; Package: CLIMACS-LISP-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
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

;;; Alternative syntax module for analysing Common Lisp

(in-package :climacs-lisp-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the syntax object

(define-syntax lisp-syntax (basic-syntax)
  ((stack-top :initform nil)
   (potentially-valid-trees)
   (lookahead-lexeme :initform nil :accessor lookahead-lexeme)
   (current-state)
   (current-start-mark)
   (current-size)
   (scan)
   (package))
  (:name "Lisp")
  (:pathname-types "lisp" "lsp" "cl"))

(defmethod initialize-instance :after ((syntax lisp-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defgeneric skip-inter (syntax state scan)
  (:documentation "advance scan until the beginning of a new
    lexeme.  Return T if one can be found and NIL otherwise."))

(defgeneric lex (syntax state scan)
  (:documentation "Return the next lexeme starting at scan."))

(defclass lexer-state ()
  ()
  (:documentation "These states are used to determine how the lexer 
    should behave."))

(defmacro define-lexer-state (name superclasses &body body)
  `(defclass ,name (,@superclasses lexer-state)
      ,@body))

(define-lexer-state lexer-error-state ()
  ()
  (:documentation "In this state, the lexer returns error lexemes
    consisting of entire lines of text"))

(define-lexer-state lexer-toplevel-state ()
  ()
  (:documentation "In this state, the lexer assumes it can skip 
    whitespace and should recognize ordinary lexemes of the language
    except for the right parenthesis"))

(define-lexer-state lexer-list-state (lexer-toplevel-state)
  ()
  (:documentation "In this state, the lexer assumes it can skip 
    whitespace and should recognize ordinary lexemes of the language"))

(define-lexer-state lexer-string-state ()
  ()
  (:documentation "In this state, the lexer is working inside a string 
    delimited by double quote characters."))

(define-lexer-state lexer-line-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a line 
    comment (starting with a semicolon."))

(define-lexer-state lexer-long-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a long
    comment delimited by #| and |#."))

(define-lexer-state lexer-escaped-token-state ()
  ()
  (:documentation "In this state, the lexer is accumulating a token
    and an odd number of multiple escapes have been seen."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this should go in syntax.lisp or lr-syntax.lisp
;;; and should inherit from parse-tree

(defclass parser-symbol ()
  ((start-mark :initform nil :initarg :start-mark :reader start-mark)
   (size :initform nil :initarg :size)
   (parent :initform nil :accessor parent)
   (children :initform '() :initarg :children :reader children)
   (preceding-parse-tree :initform nil :reader preceding-parse-tree)
   (parser-state :initform nil :initarg :parser-state :reader parser-state)))

(defmethod start-offset ((state parser-symbol))
  (let ((mark (start-mark state)))
    (when mark
      (offset mark))))

(defmethod end-offset ((state parser-symbol))
  (with-slots (start-mark size) state
     (when start-mark
       (+ (offset start-mark) size))))

(defgeneric action (syntax state lexeme))
(defgeneric new-state (syntax state parser-symbol))

(defclass parser-state () ())

(defmacro define-parser-state (name superclasses &body body)
  `(progn 
     (defclass ,name ,superclasses
	  ,@body)
     (defvar ,name (make-instance ',name))))

(defclass lexeme (parser-symbol) ())

(defmethod print-object ((lexeme lexeme) stream)
  (print-unreadable-object (lexeme stream :type t :identity t)
    (format stream "~s ~s" (start-offset lexeme) (end-offset lexeme))))

(defclass nonterminal (parser-symbol) ())

(defmethod initialize-instance :after ((parser-symbol nonterminal) &rest args)
  (declare (ignore args))
  (with-slots (children start-mark size) parser-symbol
     (loop for child in children
	   do (setf (parent child) parser-symbol))
     (let ((start (find-if-not #'null children :key #'start-offset))
	   (end (find-if-not #'null children :key #'end-offset :from-end t)))
       (when start
	 (setf start-mark (slot-value start 'start-mark)
	       size (- (end-offset end) (start-offset start)))))))  

;;; until here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-nonterminal (nonterminal) ())     
(defclass form (lisp-nonterminal) ())
(defclass incomplete-form-mixin () ())

(defclass comment (lisp-nonterminal) ())

(defclass lisp-lexeme (lexeme)
  ((ink)
   (face)))

(defclass error-lexeme (lisp-lexeme) ())
(defclass left-parenthesis-lexeme (lisp-lexeme) ())
(defclass simple-vector-start-lexeme (lisp-lexeme) ())
(defclass right-parenthesis-lexeme (lisp-lexeme) ())
(defclass quote-lexeme (lisp-lexeme) ())
(defclass backquote-lexeme (lisp-lexeme) ())
(defclass comma-lexeme (lisp-lexeme) ())
(defclass comma-at-lexeme (lisp-lexeme) ())
(defclass comma-dot-lexeme (lisp-lexeme) ())
(defclass form-lexeme (form lisp-lexeme) ())
(defclass character-lexeme (form-lexeme) ())
(defclass function-lexeme (lisp-lexeme) ())
(defclass line-comment-start-lexeme (lisp-lexeme) ())
(defclass long-comment-start-lexeme (lisp-lexeme) ())
(defclass comment-end-lexeme (lisp-lexeme) ())
(defclass string-start-lexeme (lisp-lexeme) ())
(defclass string-end-lexeme (lisp-lexeme) ())
(defclass word-lexeme (lisp-lexeme) ())
(defclass delimiter-lexeme (lisp-lexeme) ())
(defclass text-lexeme (lisp-lexeme) ())
(defclass sharpsign-equals-lexeme (lisp-lexeme) ())
(defclass sharpsign-sharpsign-lexeme (form-lexeme) ())
(defclass reader-conditional-positive-lexeme (lisp-lexeme) ())
(defclass reader-conditional-negative-lexeme (lisp-lexeme) ())
(defclass uninterned-symbol-lexeme (lisp-lexeme) ())
(defclass readtime-evaluation-lexeme (lisp-lexeme) ())
(defclass array-start-lexeme (lisp-lexeme) ())
(defclass structure-start-lexeme (lisp-lexeme) ())
(defclass pathname-start-lexeme (lisp-lexeme) ())
(defclass undefined-reader-macro-lexeme (lisp-lexeme) ())
(defclass bit-vector-lexeme (form-lexeme) ())
(defclass token-mixin () ())
(defclass complete-token-lexeme (token-mixin form-lexeme) ())
(defclass multiple-escape-start-lexeme (lisp-lexeme) ())
(defclass multiple-escape-end-lexeme (lisp-lexeme) ())

(defmethod skip-inter ((syntax lisp-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
	    do (return nil)
	  until (not (whitespacep (object-after scan)))
	  do (fo)
	  finally (return t))))

(defmethod lex :around (syntax state scan)
  (when (skip-inter syntax state scan)
    (let* ((start-offset (offset scan))
	   (lexeme (call-next-method))
	   (new-size (- (offset scan) start-offset)))
      (with-slots (start-mark size) lexeme
	 (setf (offset scan) start-offset)
	 (setf start-mark scan
	       size new-size))
      lexeme)))		  

(defmethod lex ((syntax lisp-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\( (fo) (make-instance 'left-parenthesis-lexeme))
	;#\) is an error
	(#\' (fo) (make-instance 'quote-lexeme))
	(#\; (fo)
	     (loop until (or (end-of-buffer-p scan)
			     (end-of-line-p scan)
			     (not (eql (object-after scan) #\;)))
		   do (fo))
	     (make-instance 'line-comment-start-lexeme))
	(#\" (fo) (make-instance 'string-start-lexeme))
	(#\` (fo) (make-instance 'backquote-lexeme))
	(#\, (fo)
	     (cond ((end-of-buffer-p scan)
		    (make-instance 'error-lexeme))
		   (t
		    (case (object-after scan)
		      (#\@ (fo) (make-instance 'comma-at-lexeme))
		      (#\. (fo) (make-instance 'comma-dot-lexeme))
		      (t (make-instance 'comma-lexeme))))))
	(#\# (fo)
	     (cond ((end-of-buffer-p scan)
		    (make-instance 'error-lexeme))
		   (t 
		    (loop until (end-of-buffer-p scan)
		          while (digit-char-p (object-after scan))
		          do (fo))
		    (if (end-of-buffer-p scan)
			(make-instance 'error-lexeme)
			(case (object-after scan)
			  ((#\Backspace #\Tab #\Newline #\Linefeed 
			    #\Page #\Return #\Space #\))
			   (fo)
			   (make-instance 'error-lexeme))
			  (#\\ (fo)
			       (cond ((end-of-buffer-p scan)
				      (make-instance 'error-lexeme))
				     ((not (constituentp (object-after scan)))
				      (fo) (make-instance 'character-lexeme))
				     (t (loop until (end-of-buffer-p scan)
					   while (constituentp (object-after scan))
					   do (fo))
					(make-instance 'character-lexeme))))
			  (#\' (fo)
			       (make-instance 'function-lexeme))
			  (#\( (fo)
			       (make-instance 'simple-vector-start-lexeme))
			  (#\* (fo)
			       (loop until (end-of-buffer-p scan)
				     while (or (eql (object-after scan) #\1)
					       (eql (object-after scan) #\0))
				     do (fo))
			       (if (and (not (end-of-buffer-p scan))
					(constituentp (object-after scan)))
				   (make-instance 'error-lexeme)
				   (make-instance 'bit-vector-lexeme)))
			  (#\: (fo)
			       (make-instance 'uninterned-symbol-lexeme))
			  (#\. (fo)
			       (make-instance 'readtime-evaluation-lexeme))
			  ;((#\B #\b) )
			  ;((#\O #\o) )
			  ;((#\X #\x) )
			  ;((#\R #\r) )
			  ;((#\C #\c) )
			  ((#\A #\a) (fo)
			   (make-instance 'array-start-lexeme))
			  ((#\S #\s) (fo)
			   (cond ((and (not (end-of-buffer-p scan))
				       (eql (object-after scan) #\())
				  (fo)
				  (make-instance 'structure-start-lexeme))
				 (t (make-instance 'error-lexeme))))
			  ((#\P #\p) (fo)
			   (make-instance 'pathname-start-lexeme))
			  (#\= (fo)
			       (make-instance 'sharpsign-equals-lexeme))
			  (#\# (fo)
			       (make-instance 'sharpsign-sharpsign-lexeme))
			  (#\+ (fo)
			       (make-instance 'reader-conditional-positive-lexeme))
			  (#\- (fo)
			       (make-instance 'reader-conditional-negative-lexeme))
			  (#\| (fo)
			       (make-instance 'long-comment-start-lexeme))
			  (#\< (fo)
			       (make-instance 'error-lexeme))
			  (t (fo) (make-instance 'undefined-reader-macro-lexeme)))))))
	(#\| (fo) (make-instance 'multiple-escape-start-lexeme))
	(t (cond ((or (constituentp object)
		      (eql object #\\))
		  (lex-token scan))
		 (t (fo) (make-instance 'error-lexeme))))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-list-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\) (fo) (make-instance 'right-parenthesis-lexeme))
	(t (call-next-method))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-string-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\") (fo) (make-instance 'string-end-lexeme))
	    ((eql object #\\)
	     (fo)
	     (unless (end-of-buffer-p scan)
	       (fo))
	     (make-instance 'delimiter-lexeme))
	    ((constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-long-comment-state) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\|)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\#)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'comment-end-lexeme))))
	    ((eql object #\#)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\|)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'long-comment-start-lexeme))))
	    ((constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod skip-inter ((syntax lisp-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (or (end-of-line-p scan)
		    (not (whitespacep (object-after scan))))
	  do (fo)
	  finally (return t))))

(defmethod lex ((syntax lisp-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((end-of-line-p scan)
	   (make-instance 'comment-end-lexeme))
	  ((constituentp (object-after scan))
	   (loop until (or (end-of-buffer-p scan)
			   (not (constituentp (object-after scan))))
		 do (fo))
	   (make-instance 'word-lexeme))
	  (t (fo) (make-instance 'delimiter-lexeme)))))

(defun lex-token (scan)
  (macrolet ((fo () `(forward-object scan)))
    (tagbody
       start
       (when (end-of-buffer-p scan)
	 (return-from lex-token (make-instance 'complete-token-lexeme)))
       (when (constituentp (object-after scan))
	 (fo)
	 (go start))
       (when (eql (object-after scan) #\\)
	 (fo)
	 (when (end-of-buffer-p scan)
	   (return-from lex-token (make-instance 'error-lexeme)))
	 (fo)
	 (go start))
       (when (eql (object-after scan) #\|)
	 (fo)
	 (return-from lex-token (make-instance 'multiple-escape-start-lexeme)))
       (return-from lex-token (make-instance 'complete-token-lexeme)))))

(defmethod lex ((syntax lisp-syntax) (state lexer-escaped-token-state) scan)
  (let ((bars-seen 0))
    (macrolet ((fo () `(forward-object scan)))
      (tagbody
       start
	 (when (end-of-buffer-p scan)
	   (return-from lex (make-instance 'text-lexeme)))
	 (when (eql (object-after scan) #\\)
	   (fo)
	   (when (end-of-buffer-p scan)
	     (return-from lex (make-instance 'error-lexeme)))
	   (fo)
	   (go start))
	 (when (eql (object-after scan) #\|)
	   (incf bars-seen)
	   (fo)
	   (go start))
         (if (evenp bars-seen)
             (unless (whitespacep (object-after scan))
               (fo)
               (go start))
             (when (constituentp (object-after scan))
               (fo)
               (go start)))
	 (return-from lex 
	   (if (oddp bars-seen)
	       (make-instance 'multiple-escape-end-lexeme)
	       (make-instance 'text-lexeme)))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-error-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (end-of-line-p scan)
	  do (fo))
    (make-instance 'error-lexeme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nonterminals

(defclass line-comment (lisp-nonterminal) ())
(defclass long-comment (lisp-nonterminal) ())  
(defclass error-symbol (lisp-nonterminal) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defmacro define-lisp-action ((state lexeme) &body body)
  `(defmethod action ((syntax lisp-syntax) (state ,state) (lexeme ,lexeme))
     ,@body))

(defmacro define-new-lisp-state ((state parser-symbol) &body body)
  `(defmethod new-state ((syntax lisp-syntax) (state ,state) (tree ,parser-symbol))
     ,@body))

(defun pop-one (syntax)
  (with-slots (stack-top current-state) syntax
     (with-slots (preceding-parse-tree parser-state) stack-top
	(prog1 stack-top
	       (setf current-state parser-state
		     stack-top preceding-parse-tree)))))

(defun pop-number (syntax how-many)
  (loop with result = '()
	repeat how-many
	do (push (pop-one syntax) result)
	finally (return result)))

(defmacro reduce-fixed-number (symbol nb-children)
  `(let ((result (make-instance ',symbol :children (pop-number syntax ,nb-children))))
     (when (zerop ,nb-children)
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))

(defun pop-until-type (syntax type)
  (with-slots (stack-top) syntax
     (loop with result = '()
	   for child = stack-top
	   do (push (pop-one syntax) result)
	   until (typep child type)
	   finally (return result))))

(defmacro reduce-until-type (symbol type)
  `(let ((result (make-instance ',symbol
		    :children (pop-until-type syntax ',type))))
     (when (null (children result))
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))

(defun pop-all (syntax)
  (with-slots (stack-top) syntax
     (loop with result = '()
	   until (null stack-top)
	   do (push (pop-one syntax) result)
	   finally (return result))))

(defmacro reduce-all (symbol)
  `(let ((result (make-instance ',symbol :children (pop-all syntax))))
     (when (null (children result))
       (with-slots (scan) syntax
	  (with-slots (start-mark size) result
	     (setf start-mark (clone-mark scan :right)
		   size 0))))
     result))     

(define-parser-state error-state (lexer-error-state parser-state) ())
(define-parser-state error-reduce-state (lexer-toplevel-state parser-state) ())

(define-lisp-action (error-reduce-state (eql nil))
  (throw 'done nil)) 

;;; the default action for any lexeme is shift
(define-lisp-action (t lisp-lexeme)
  lexeme)

;;; the action on end-of-buffer is to reduce to the error symbol
(define-lisp-action (t (eql nil))
  (reduce-all error-symbol))

;;; the default new state is the error state
(define-new-lisp-state (t parser-symbol) error-state)

;;; the new state when an error-state 
(define-new-lisp-state (t error-symbol) error-reduce-state)


;;;;;;;;;;;;;;;; Top-level 

#| rules
   form* -> 
   form* -> form* form
|#

;;; parse trees
(defclass form* (lisp-nonterminal) ())

(define-parser-state |form* | (lexer-toplevel-state parser-state) ())
(define-parser-state form-may-follow (lexer-toplevel-state parser-state) ())
(define-parser-state |initial-state | (form-may-follow) ())

(define-new-lisp-state (|initial-state | form) |initial-state |)
(define-new-lisp-state (|initial-state | comment) |initial-state |)

(define-lisp-action (|initial-state | (eql nil))
  (reduce-all form*))

(define-new-lisp-state (|initial-state | form*) |form* | )
  
(define-lisp-action (|form* | (eql nil))
  (throw 'done nil))

;;;;;;;;;;;;;;;; List

#| rules
   form -> ( form* )
|#

;;; parse trees
(defclass list-form (form) ())
(defclass complete-list-form (list-form) ())
(defclass incomplete-list-form (list-form incomplete-form-mixin) ())

(define-parser-state |( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow left-parenthesis-lexeme) |( form* |)
(define-new-lisp-state (|( form* | form) |( form* |)
(define-new-lisp-state (|( form* | comment) |( form* |)
(define-new-lisp-state (|( form* | right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ( form* )
(define-lisp-action (|( form* ) | t)
  (reduce-until-type complete-list-form left-parenthesis-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|( form* | (eql nil))
  (reduce-until-type incomplete-list-form left-parenthesis-lexeme))

;;;;;;;;;;;;;;;; Simple Vector

;;; parse trees
(defclass simple-vector-form (list-form) ())
(defclass complete-simple-vector-form (complete-list-form) ())
(defclass incomplete-simple-vector-form (incomplete-list-form) ())

(define-parser-state |#( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |#( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow simple-vector-start-lexeme) |#( form* |)
(define-new-lisp-state (|#( form* | form) |#( form* |)
(define-new-lisp-state (|#( form* | comment) |#( form* |)
(define-new-lisp-state (|#( form* | right-parenthesis-lexeme) |#( form* ) |)

;;; reduce according to the rule form -> #( form* )
(define-lisp-action (|#( form* ) | t)
  (reduce-until-type complete-simple-vector-form simple-vector-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#( form* | (eql nil))
  (reduce-until-type incomplete-simple-vector-form simple-vector-start-lexeme))

;;;;;;;;;;;;;;;; String

;;; parse trees
(defclass string-form (form) ())
(defclass complete-string-form (string-form) ())
(defclass incomplete-string-form (string-form incomplete-form-mixin) ())

(define-parser-state |" word* | (lexer-string-state parser-state) ())
(define-parser-state |" word* " | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|" word* | word-lexeme) |" word* |)
(define-new-lisp-state (|" word* | delimiter-lexeme) |" word* |)
(define-new-lisp-state (form-may-follow string-start-lexeme) |" word* |)
(define-new-lisp-state (|" word* | string-end-lexeme) |" word* " |)

;;; reduce according to the rule form -> " word* "
(define-lisp-action (|" word* " | t)
  (reduce-until-type complete-string-form string-start-lexeme))

;;; reduce at the end of the buffer 
(define-lisp-action (|" word* | (eql nil))
  (reduce-until-type incomplete-string-form string-start-lexeme))

;;;;;;;;;;;;;;;; Line comment

;;; parse trees
(defclass line-comment-form (comment) ())

(define-parser-state |; word* | (lexer-line-comment-state parser-state) ())
(define-parser-state |; word* NL | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow line-comment-start-lexeme) |; word* |)
(define-new-lisp-state (|; word* | word-lexeme) |; word* |)
(define-new-lisp-state (|; word* | delimiter-lexeme) |; word* |)
(define-new-lisp-state (|; word* | comment-end-lexeme) |; word* NL |)

;;; reduce according to the rule form -> ; word* NL
(define-lisp-action (|; word* NL | t)
  (reduce-until-type line-comment-form line-comment-start-lexeme))

;;;;;;;;;;;;;;;; Long comment

;;; parse trees
(defclass long-comment-form (comment) ())
(defclass complete-long-comment-form (long-comment-form) ())
(defclass incomplete-long-comment-form (long-comment-form incomplete-form-mixin) ())

(define-parser-state |#\| word* | (lexer-long-comment-state parser-state) ())
(define-parser-state |#\| word* \|# | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|#\| word* | word-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | delimiter-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | long-comment-start-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | long-comment-form) |#\| word* |)
(define-new-lisp-state (form-may-follow long-comment-start-lexeme) |#\| word* |)
(define-new-lisp-state (|#\| word* | comment-end-lexeme) |#\| word* \|# |)

;;; reduce according to the rule form -> #| word* |#
(define-lisp-action (|#\| word* \|# | t)
  (reduce-until-type complete-long-comment-form long-comment-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#\| word* | (eql nil))
  (reduce-until-type incomplete-long-comment-form long-comment-start-lexeme))

;;;;;;;;;;;;;;;; Token (number or symbol)

;;; parse trees
(defclass token-form (form token-mixin) ())
(defclass complete-token-form (token-form) ())
(defclass incomplete-token-form (token-form) ())

(define-parser-state | m-e-start text* | (lexer-escaped-token-state parser-state) ())
(define-parser-state | m-e-start text* m-e-end | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow multiple-escape-start-lexeme) | m-e-start text* |)
(define-new-lisp-state (| m-e-start text* | text-lexeme) | m-e-start text* |)
(define-new-lisp-state (| m-e-start text* | multiple-escape-end-lexeme) | m-e-start text* m-e-end |)

;;; reduce according to the rule form -> m-e-start text* m-e-end
(define-lisp-action (| m-e-start text* m-e-end | t)
  (reduce-until-type complete-token-form multiple-escape-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (| m-e-start text* | (eql nil))
  (reduce-until-type incomplete-token-form multiple-escape-start-lexeme))

;;;;;;;;;;;;;;;; Quote

;;; parse trees
(defclass quote-form (form) ())

(define-parser-state |' | (form-may-follow) ())
(define-parser-state |' form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow quote-lexeme) |' |)
(define-new-lisp-state (|' | form) |' form |)
(define-new-lisp-state (|' | comment) |' |)


;;; reduce according to the rule form -> ' form
(define-lisp-action (|' form | t)
  (reduce-until-type quote-form quote-lexeme))

;;;;;;;;;;;;;;;; Backquote

;;; parse trees
(defclass backquote-form (form) ())

(define-parser-state |` | (form-may-follow) ())
(define-parser-state |` form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow backquote-lexeme) |` |)
(define-new-lisp-state (|` | form) |` form |)
(define-new-lisp-state (|` | comment) |` |)

;;; reduce according to the rule form -> ` form
(define-lisp-action (|` form | t)
  (reduce-until-type backquote-form backquote-lexeme))

;;;;;;;;;;;;;;;; Comma

;;; parse trees
(defclass comma-form (form) ())
(defclass comma-at-form (form) ())
(defclass comma-dot-form (form) ())

(define-parser-state |, | (form-may-follow) ())
(define-parser-state |, form | (lexer-toplevel-state parser-state) ())
(define-parser-state |,@ | (form-may-follow) ())
(define-parser-state |,@ form | (lexer-toplevel-state parser-state) ())
(define-parser-state |,. | (form-may-follow) ())
(define-parser-state |,. form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow comma-lexeme) |, |)
(define-new-lisp-state (form-may-follow comma-at-lexeme) |,@ |)
(define-new-lisp-state (form-may-follow comma-dot-lexeme) |,. |)
(define-new-lisp-state (|, | form) |, form |)
(define-new-lisp-state (|, | comment) |, |)
(define-new-lisp-state (|,@ | form) |,@ form |)
(define-new-lisp-state (|,@ | comment) |,@ |)
(define-new-lisp-state (|,. | form) |,. form |)
(define-new-lisp-state (|,. | comment) |,. |)

;;; reduce according to the rule form -> , form
(define-lisp-action (|, form | t)
  (reduce-until-type comma-form comma-lexeme))
(define-lisp-action (|,@ form | t)
  (reduce-until-type comma-at-form comma-at-lexeme))
(define-lisp-action (|,. form | t)
  (reduce-until-type comma-dot-form comma-dot-lexeme))

;;;;;;;;;;;;;;;; Function

;;; parse trees
(defclass function-form (form) ())

(define-parser-state |#' | (form-may-follow) ())
(define-parser-state |#' form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow function-lexeme) |#' |)
(define-new-lisp-state (|#' | form) |#' form |)
(define-new-lisp-state (|#' | comment) |#' |)

;;; reduce according to the rule form -> #' form
(define-lisp-action (|#' form | t)
  (reduce-until-type function-form function-lexeme))

;;;;;;;;;;;;;;;; Reader conditionals

;;; parse trees
(defclass reader-conditional-positive-form (form) ())
(defclass reader-conditional-negative-form (form) ())

(define-parser-state |#+ | (form-may-follow) ())
(define-parser-state |#+ form | (form-may-follow) ())
(define-parser-state |#+ form form | (lexer-toplevel-state parser-state) ())
(define-parser-state |#- | (form-may-follow) ())
(define-parser-state |#- form | (form-may-follow) ())
(define-parser-state |#- form form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow reader-conditional-positive-lexeme) |#+ |)
(define-new-lisp-state (|#+ | form) |#+ form |)
(define-new-lisp-state (|#+ form | form) |#+ form form |)
(define-new-lisp-state (|#+ | comment) |#+ |)
(define-new-lisp-state (|#+ form | comment) |#+ form |)
(define-new-lisp-state (form-may-follow reader-conditional-negative-lexeme) |#- |)
(define-new-lisp-state (|#- | form) |#- form |)
(define-new-lisp-state (|#- form | form) |#- form form |)
(define-new-lisp-state (|#- | comment) |#- |)
(define-new-lisp-state (|#- form | comment) |#- form |)
  
(define-lisp-action (|#+ form form | t)
  (reduce-until-type reader-conditional-positive-form reader-conditional-positive-lexeme))

(define-lisp-action (|#- form form | t)
  (reduce-until-type reader-conditional-negative-form reader-conditional-negative-lexeme))

;;;;;;;;;;;;;;;; uninterned symbol

;;; parse trees
(defclass uninterned-symbol-form (form) ())

(define-parser-state |#: | (form-may-follow) ())
(define-parser-state |#: form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow uninterned-symbol-lexeme) |#: |)
(define-new-lisp-state (|#: | form) |#: form |)

;;; reduce according to the rule form -> #: form
(define-lisp-action (|#: form | t)
  (reduce-fixed-number uninterned-symbol-form 2))

;;;;;;;;;;;;;;;; readtime evaluation

;;; parse trees
(defclass readtime-evaluation-form (form) ())

(define-parser-state |#. | (form-may-follow) ())
(define-parser-state |#. form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow readtime-evaluation-lexeme) |#. |)
(define-new-lisp-state (|#. | form) |#. form |)
(define-new-lisp-state (|#. | comment) |#. |)

;;; reduce according to the rule form -> #. form
(define-lisp-action (|#. form | t)
  (reduce-until-type readtime-evaluation-form readtime-evaluation-lexeme))

;;;;;;;;;;;;;;;; sharpsign equals

;;; parse trees
(defclass sharpsign-equals-form (form) ())

(define-parser-state |#= | (form-may-follow) ())
(define-parser-state |#= form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow sharpsign-equals-lexeme) |#= |)
(define-new-lisp-state (|#= | form) |#= form |)
(define-new-lisp-state (|#= | comment) |#= |)

;;; reduce according to the rule form -> #= form
(define-lisp-action (|#= form | t)
  (reduce-until-type sharpsign-equals-form sharpsign-equals-lexeme))

;;;;;;;;;;;;;;;; array

;;; parse trees
(defclass array-form (form) ())

(define-parser-state |#A | (form-may-follow) ())
(define-parser-state |#A form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow array-start-lexeme) |#A |)
(define-new-lisp-state (|#A | form) |#A form |)
(define-new-lisp-state (|#A | comment) |#A |)

;;; reduce according to the rule form -> #A form
(define-lisp-action (|#A form | t)
  (reduce-until-type array-start-form array-start-lexeme))

;;;;;;;;;;;;;;;; structure

;;; parse trees
(defclass structure-form (list-form) ())
(defclass complete-structure-form (complete-list-form) ())
(defclass incomplete-structure-form (incomplete-list-form) ())

(define-parser-state |#S( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |#S( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow structure-start-lexeme) |#S( form* |)
(define-new-lisp-state (|#S( form* | form) |#S( form* |)
(define-new-lisp-state (|#S( form* | right-parenthesis-lexeme) |#S( form* ) |)

;;; reduce according to the rule form -> #S( form* )
(define-lisp-action (|#S( form* ) | t)
  (reduce-until-type complete-structure-form structure-start-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|#S( form* | (eql nil))
  (reduce-until-type incomplete-structure-form structure-start-lexeme))


;;;;;;;;;;;;;;;; pathname

;;; NB: #P need not be followed by a string,
;;;  as it could be followed by a #. construct instead (or some other reader macro)

;;; parse trees
(defclass pathname-form (form) ())

(define-parser-state |#P | (form-may-follow) ())
(define-parser-state |#P form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow pathname-start-lexeme) |#P |)
(define-new-lisp-state (|#P | form) |#P form |)
(define-new-lisp-state (|#P | comment) |#P |)

;;; reduce according to the rule form -> #P form
(define-lisp-action (|#P form | t)
  (reduce-until-type pathname-form pathname-start-lexeme))

;;;;;;;;;;;;;;;; undefined reader macro

;;; parse trees
(defclass undefined-reader-macro-form (form) ())

(define-parser-state |#<other> | (form-may-follow) ())
(define-parser-state |#<other> form | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow undefined-reader-macro-lexeme) |#<other> |)
(define-new-lisp-state (|#<other> | form) |#<other> form |)

;;; reduce according to the rule form -> #<other> form
(define-lisp-action (|#<other> form | t)
  (reduce-fixed-number undefined-reader-macro-form 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser step

(defgeneric parser-step (syntax))

(defmethod parser-step ((syntax lisp-syntax))
  (with-slots (lookahead-lexeme stack-top current-state scan) syntax
     (setf lookahead-lexeme (lex syntax current-state (clone-mark scan :right)))
     (let* ((new-parser-symbol (action syntax current-state lookahead-lexeme))
	    (new-state (new-state syntax current-state new-parser-symbol)))
       (with-slots (parser-state parser-symbol preceding-parse-tree children) new-parser-symbol
	  (setf parser-state current-state
		current-state new-state
		preceding-parse-tree stack-top
		stack-top new-parser-symbol)))
     (setf (offset scan) (end-offset stack-top))))

(defun prev-tree (tree)
  (assert (not (null tree)))
  (if (null (children tree))
      (preceding-parse-tree tree)
      (car (last (children tree)))))

(defun next-tree (tree)
  (assert (not (null tree)))
  (if (null (parent tree))
      nil
      (let* ((parent (parent tree))
	     (siblings (children parent)))
	(cond ((null parent) nil)
	      ((eq tree (car (last siblings))) parent)
	      (t (loop with new-tree = (cadr (member tree siblings :test #'eq))
		       until (null (children new-tree))
		       do (setf new-tree (car (children new-tree)))
		       finally (return new-tree)))))))	 

(defun find-last-valid-lexeme (parse-tree offset)
  (cond ((or (null parse-tree) (null (start-offset parse-tree))) nil)
	((> (start-offset parse-tree) offset)
	 (find-last-valid-lexeme (preceding-parse-tree parse-tree) offset))
	((not (typep parse-tree 'lexeme))
	 (find-last-valid-lexeme (car (last (children parse-tree))) offset))
	((>= (end-offset parse-tree) offset)
	 (find-last-valid-lexeme (preceding-parse-tree parse-tree) offset))
	(t parse-tree)))  

(defun find-first-potentially-valid-lexeme (parse-trees offset)
  (cond ((null parse-trees) nil)
	((or (null (start-offset (car parse-trees)))
	     (< (end-offset (car parse-trees)) offset))
	 (find-first-potentially-valid-lexeme (cdr parse-trees) offset))
	((not (typep (car parse-trees) 'lexeme))
	 (find-first-potentially-valid-lexeme (children (car parse-trees)) offset))
	((< (start-offset (car parse-trees)) offset)
	 (loop with tree = (next-tree (car parse-trees))
	       until (or (null tree) (>= (start-offset tree) offset))
	       do (setf tree (next-tree tree))
	       finally (return tree)))
	(t (car parse-trees))))

(defun parse-tree-equal (tree1 tree2)
  (and (eq (class-of tree1) (class-of tree2))
       (eq (parser-state tree1) (parser-state tree2))
       (= (end-offset tree1) (end-offset tree2))))

(defmethod print-object ((mark mark) stream)
  (print-unreadable-object (mark stream :type t :identity t)
    (format stream "~s" (offset mark))))

(defun parse-patch (syntax)
  (with-slots (current-state stack-top scan potentially-valid-trees) syntax
     (parser-step syntax)
     (finish-output *trace-output*)
     (cond ((parse-tree-equal stack-top potentially-valid-trees)
	    (unless (or (null (parent potentially-valid-trees))
			(eq potentially-valid-trees
			    (car (last (children (parent potentially-valid-trees))))))
	      (loop for tree = (cadr (member potentially-valid-trees
					     (children (parent potentially-valid-trees))
					     :test #'eq))
		      then (car (children tree))
		    until (null tree)
		    do (setf (slot-value tree 'preceding-parse-tree)
			     stack-top))
	      (setf stack-top (prev-tree (parent potentially-valid-trees))))
	    (setf potentially-valid-trees (parent potentially-valid-trees))
	    (setf current-state (new-state syntax (parser-state stack-top) stack-top))
	    (setf (offset scan) (end-offset stack-top)))
	   (t (loop until (or (null potentially-valid-trees)
			      (>= (start-offset potentially-valid-trees)
				  (end-offset stack-top)))
		    do (setf potentially-valid-trees
			     (next-tree potentially-valid-trees)))))))	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defmethod update-syntax-for-display (buffer (syntax lisp-syntax) top bot)
  nil)

(defun package-of (syntax)
  (let ((buffer (buffer syntax)))
    (flet ((test (x)
	     (and (typep x 'list-form)
		  (not (null (cdr (children x))))
		  (buffer-looking-at buffer
				     (start-offset (cadr (children x)))
				     "in-package"
				     :test #'char-equal))))
      (with-slots (stack-top) syntax
	(let ((form (find-if #'test (children stack-top))))
	  (and form
	       (not (null (cddr (children form))))
	       (let* ((package-form (caddr (children form)))
		      (package-name (coerce (buffer-sequence
					     buffer
					     (start-offset package-form)
					     (end-offset package-form))
					    'string))
		      (package-symbol
		       (let ((*package* (find-package :common-lisp)))
			 (ignore-errors
			   (read-from-string package-name nil nil)))))
		 (find-package package-symbol))))))))

(defmethod update-syntax (buffer (syntax lisp-syntax))
  (let* ((low-mark (low-mark buffer))
	 (high-mark (high-mark buffer)))
    (when (mark<= low-mark high-mark)
      (catch 'done
	(with-slots (current-state stack-top scan potentially-valid-trees) syntax
	   (setf potentially-valid-trees
		 (if (null stack-top)
		     nil
		     (find-first-potentially-valid-lexeme (children stack-top)
							  (offset high-mark))))
	   (setf stack-top (find-last-valid-lexeme stack-top (offset low-mark)))
	   (setf (offset scan) (if (null stack-top) 0 (end-offset stack-top))
		 current-state (if (null stack-top)
				   |initial-state |
				   (new-state syntax
					      (parser-state stack-top)
					      stack-top)))
	   (loop do (parse-patch syntax))))))
  (with-slots (package) syntax
    (setf package (package-of syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; accessing parser forms

(defun first-form (list)
  "Returns the first non-comment in list."
  (find-if-not #'(lambda (item) (typep item 'comment)) list))

(defun nth-form (n list)
  "Returns the nth non-comment in list."
  (loop for item in list
	count (not (typep item 'comment))
	  into forms
	until (= forms n)
	finally (return item)))

(defun second-form (list)
  "Returns the second non-comment in list."
  (nth-form 2 list))

(defun third-form (list)
  "Returns the third non-comment in list."
  (nth-form 3 list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

(defparameter *standard-faces*
	      `((:error ,+red+ nil)
		(:string ,+foreground-ink+ ,(make-text-style nil :italic nil))
		(:keyword ,+dark-violet+ nil)
		(:lambda-list-keyword ,+dark-green+ nil)
		(:comment ,+maroon+ nil)
		(:reader-conditional ,+gray50+ nil)))

(defparameter *reader-conditional-faces*
	      `((:error ,+red+ nil)
		(:string ,+gray50+ ,(make-text-style nil :italic nil))
		(:keyword ,+gray50+ nil)
		(:lambda-list-keyword ,+gray50+ nil)
		(:comment ,+gray50+ nil)
		(:reader-conditional ,+gray50+ nil)))

(defvar *current-faces* nil)

(defun face-colour (type)
  (first (cdr (assoc type *current-faces*))))

(defun face-style (type)
  (second (cdr (assoc type *current-faces*))))

(defmacro with-face ((face) &body body)
  `(with-drawing-options (pane :ink (face-colour ,face)
			       :text-style (face-style ,face))
     ,@body))

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
	(tab-width (tab-width pane)))
    (loop while (< start end)
       do (ecase (buffer-object buffer start)
	    (#\Newline (terpri pane)
		       (setf (aref *cursor-positions* (incf *current-line*))
			     (multiple-value-bind (x y) (stream-cursor-position pane)
			       (declare (ignore x))
			       y)))
	    ((#\Page #\Return #\Space) (stream-increment-cursor-position
		      pane space-width 0))
	    (#\Tab (let ((x (stream-cursor-position pane)))
		     (stream-increment-cursor-position
		      pane (- tab-width (mod x tab-width)) 0))))
	 (incf start))))		    

(defgeneric display-parse-tree (parse-symbol syntax pane))

(defmethod display-parse-tree ((parse-symbol (eql nil)) syntax pane)
  nil)

(defmethod display-parse-tree :around (parse-symbol syntax pane)
  (with-slots (top bot) pane
     (when (and (start-offset parse-symbol) 
		(mark< (start-offset parse-symbol) bot)
		(mark> (end-offset parse-symbol) top))
       (call-next-method))))  

(defmethod display-parse-tree (parse-symbol syntax pane)
  (loop for child in (children parse-symbol)
	do (display-parse-tree child syntax pane)))

(defmethod display-parse-tree ((parse-symbol error-symbol) (syntax lisp-syntax) pane)
  (let ((children (children parse-symbol)))
    (loop until (or (null (cdr children))
		    (typep (parser-state (cadr children)) 'error-state))
	  do (display-parse-tree (pop children) syntax pane))
    (if (and (null (cdr children))
	     (not (typep (parser-state parse-symbol) 'error-state)))
	(display-parse-tree (car children) syntax pane)
	(with-face (:error)
	  (loop for child in children
		do (display-parse-tree child syntax pane))))))

(defmethod display-parse-tree ((parse-symbol error-lexeme) (syntax lisp-syntax) pane)
  (with-face (:error)
    (call-next-method)))

(define-presentation-type unknown-symbol () :inherit-from 'symbol
                          :description "unknown symbol")

(define-presentation-method presentation-typep (object (type unknown-symbol))
  (or (symbolp object) (stringp object)))

(defmethod display-parse-tree ((parse-symbol token-mixin) (syntax lisp-syntax) pane)
  (if (> (end-offset parse-symbol) (start-offset parse-symbol))
      (let ((string (coerce (buffer-sequence (buffer syntax)
                                             (start-offset parse-symbol)
                                             (end-offset parse-symbol))
                            'string)))
        (multiple-value-bind (symbol status)
            (token-to-symbol syntax parse-symbol)
          (with-output-as-presentation
              (pane (if status symbol string) (if status 'symbol 'unknown-symbol)
               :single-box :highlighting)
            (cond ((eql (buffer-object (buffer syntax) (start-offset parse-symbol)) #\:)
                   (with-face (:keyword)
                     (call-next-method)))
                  ((eql (buffer-object (buffer syntax) (start-offset parse-symbol)) #\&)
                   (with-face (:lambda-list-keyword)
                     (call-next-method)))
                  (t (call-next-method)))
            )))
      (call-next-method)))

(defmethod display-parse-tree ((parser-symbol lisp-lexeme) (syntax lisp-syntax) pane)
  (flet ((cache-test (t1 t2)
	   (and (eq t1 t2)
		(eq (slot-value t1 'ink)
		    (medium-ink (sheet-medium pane)))
		(eq (slot-value t1 'face)
		    (text-style-face (medium-text-style (sheet-medium pane)))))))
    (updating-output
	(pane :unique-id parser-symbol
	      :id-test #'eq
	      :cache-value parser-symbol
	      :cache-test #'cache-test)
      (with-slots (ink face) parser-symbol
	(setf ink (medium-ink (sheet-medium pane))
	      face (text-style-face (medium-text-style (sheet-medium pane))))
	(let ((string (coerce (buffer-sequence (buffer syntax)
					       (start-offset parser-symbol)
					       (end-offset parser-symbol))
			      'string)))
          (present string 'string :stream pane))))))
          
(defmethod display-parse-tree :before ((parse-symbol lisp-lexeme) (syntax lisp-syntax) pane)
  (handle-whitespace pane (buffer pane) *white-space-start* (start-offset parse-symbol))
  (setf *white-space-start* (end-offset parse-symbol)))

(define-presentation-type lisp-string () 
                          :description "lisp string")

(defmethod display-parse-tree ((parse-symbol complete-string-form) (syntax lisp-syntax) pane)
  (let ((children (children parse-symbol)))
    (if (third children)
        (let ((string (coerce (buffer-sequence (buffer syntax)
                                               (start-offset (second children))
                                               (end-offset (car (last children 2))))
                              'string)))
          (with-output-as-presentation (pane string 'lisp-string
                                             :single-box :highlighting)
            (display-parse-tree  (pop children) syntax pane)
            (with-face (:string)
	      (loop until (null (cdr children))
                 do (display-parse-tree (pop children) syntax pane)))
            (display-parse-tree (pop children) syntax pane)))
        (progn (display-parse-tree (pop children) syntax pane)
               (display-parse-tree (pop children) syntax pane)))))

(defmethod display-parse-tree ((parse-symbol incomplete-string-form) (syntax lisp-syntax) pane)
  (let ((children (children parse-symbol)))
    (if (second children)
        (let ((string (coerce (buffer-sequence (buffer syntax)
                                               (start-offset (second children))
                                               (end-offset (car (last children))))
                              'string)))
          (with-output-as-presentation (pane string 'lisp-string
                                             :single-box :highlighting)
            (display-parse-tree  (pop children) syntax pane)
            (with-face (:string)
              (loop until (null children)
                 do (display-parse-tree (pop children) syntax pane)))))
        (display-parse-tree  (pop children) syntax pane))))

(defmethod display-parse-tree ((parse-symbol line-comment-form) (syntax lisp-syntax) pane)
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol long-comment-form) (syntax lisp-syntax) pane)
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol reader-conditional-positive-form)
			       (syntax lisp-syntax) pane)
  (let ((conditional (second-form (children parse-symbol))))
    (if (eval-feature-conditional conditional syntax)
	(call-next-method)
	(let ((*current-faces* *reader-conditional-faces*))
	  (with-face (:reader-conditional)
	    (call-next-method))))))

(defmethod display-parse-tree ((parse-symbol reader-conditional-negative-form)
				(syntax lisp-syntax) pane)
  (let ((conditional (second-form (children parse-symbol))))
    (if (eval-feature-conditional conditional syntax)
	(let ((*current-faces* *reader-conditional-faces*))
	  (with-face (:reader-conditional)
	    (call-next-method)))
	(call-next-method))))

(defparameter climacs-gui::*climacs-features* (copy-list *features*))

(defgeneric eval-feature-conditional (conditional-form syntax))

(defmethod eval-feature-conditional (conditional-form (syntax lisp-syntax))
  nil)

;; Adapted from slime.el

(defconstant keyword-package (find-package :keyword)
  "The KEYWORD package.")

(defmethod eval-feature-conditional ((conditional token-mixin) (syntax lisp-syntax))
  (let* ((string (coerce (buffer-sequence (buffer syntax)
					 (start-offset conditional)
					 (end-offset conditional))
		  'string))
	 (symbol (parse-symbol string keyword-package)))
    (member symbol climacs-gui::*climacs-features*)))

(defmethod eval-feature-conditional ((conditional list-form) (syntax lisp-syntax))
  (let ((children (children conditional)))
    (when (third-form children)
      (flet ((eval-fc (conditional)
	       (funcall #'eval-feature-conditional conditional syntax)))
	(let* ((type (second-form children))
	       (conditionals  (butlast
			       (nthcdr
				2
				(remove-if
				 #'(lambda (child) (typep child 'comment))
				 children))))
	       (type-string (coerce (buffer-sequence (buffer syntax)
						     (start-offset type)
						     (end-offset type))
				    'string))
	       (type-symbol (parse-symbol type-string keyword-package)))
	  (case type-symbol
	    (:and (funcall #'every #'eval-fc conditionals))
	    (:or (funcall #'some #'eval-fc conditionals))
	    (:not (when conditionals
		    (funcall #'(lambda (f l) (not (apply f l)))
			     #'eval-fc conditionals)))))))))
	  
(defmethod display-parse-tree ((parse-symbol complete-list-form) (syntax lisp-syntax) pane)
  (let ((children (children parse-symbol)))
    (if (= (end-offset parse-symbol) (offset (point pane)))
	(with-text-face (pane :bold)
	  (display-parse-tree (car children) syntax pane))
	(display-parse-tree (car children) syntax pane))
    (loop for child in (cdr children)
	  do (display-parse-tree child syntax pane))))
    
(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax lisp-syntax) current-p)
  (declare (ignore current-p))
  (with-slots (top bot) pane
     (setf *cursor-positions* (make-array (1+ (number-of-lines-in-region top bot)))
	   *current-line* 0
	   (aref *cursor-positions* 0) (stream-cursor-position pane))
     (setf *white-space-start* (offset top)))
  (let ((*current-faces* *standard-faces*))
    (with-slots (stack-top) syntax
       (display-parse-tree stack-top syntax pane)))
  (with-slots (top) pane
    (let* ((cursor-line (number-of-lines-in-region top (point pane)))
	   (style (medium-text-style pane))
	   (ascent (text-style-ascent style pane))
	   (descent (text-style-descent style pane))
	   (height (+ ascent descent))
	   (cursor-y (+ (* cursor-line (+ height (stream-vertical-spacing pane)))))
	   (cursor-column 
	    (buffer-display-column
	     (buffer (point pane)) (offset (point pane))
	     (round (tab-width pane) (space-width pane))))
	   (cursor-x (* cursor-column (text-style-width (medium-text-style pane) pane))))
      (updating-output (pane :unique-id -1)
	(draw-rectangle* pane
			 (1- cursor-x) cursor-y
			 (+ cursor-x 2) (+ cursor-y ascent descent)
			 :ink (if current-p +red+ +blue+))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

(defun form-before-in-children (children offset)
  (loop for (first . rest) on children
	unless (typep first 'comment)
	do (cond ((< (start-offset first) offset (end-offset first))
		  (return (if (null (children first))
			      nil
			      (form-before-in-children (children first) offset))))
		 ((and (>= offset (end-offset first))
		       (or (null rest)
			   (<= offset (start-offset (first-form rest)))))
		  (return (let ((potential-form
				 (when (typep first 'list-form)
				   (form-before-in-children (children first) offset))))
			    (or potential-form
				(when (typep first 'form)
				  first)))))
		 (t nil))))
		 
(defun form-before (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (<= offset (start-offset stack-top)))
	nil
	(form-before-in-children (children stack-top) offset))))

(defun form-after-in-children (children offset)
  (loop for child in children
	unless (typep child 'comment)
	  do (cond ((< (start-offset child) offset (end-offset child))
		    (return (if (null (children child))
				nil
				(form-after-in-children (children child) offset))))
		   ((<= offset (start-offset child))
		    (return (let ((potential-form (form-after-in-children (children child) offset)))
			      (or potential-form
				  (when (typep child 'form)
				    child)))))
		   (t nil))))
		 
(defun form-after (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (>= offset (end-offset stack-top)))
	nil
	(form-after-in-children (children stack-top) offset))))
	     
(defun form-around-in-children (children offset)
  (loop for child in children
	unless (typep child 'comment)
	do (cond ((< (start-offset child) offset (end-offset child))
		  (return (if (null (children child))
			      (when (typep child 'form)
				child)
			      (form-around-in-children (children child) offset))))
		 ((<= offset (start-offset child))
		  (return nil))
		 (t nil))))

(defun form-around (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (>= offset (end-offset stack-top))
	    (<= offset (start-offset stack-top)))
	nil
	(form-around-in-children (children stack-top) offset))))

(defmethod backward-expression (mark (syntax lisp-syntax))
  (let ((potential-form (or (form-before syntax (offset mark))
			    (form-around syntax (offset mark)))))
    (if potential-form
	(setf (offset mark) (start-offset potential-form))
	(error 'no-expression))))

(defmethod forward-expression (mark (syntax lisp-syntax))
  (let ((potential-form (or (form-after syntax (offset mark))
			    (form-around syntax (offset mark)))))
    (if potential-form
	(setf (offset mark) (end-offset potential-form))
	(error 'no-expression))))

(defmethod forward-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
	  then (end-offset potential-form)
	for potential-form = (or (form-after syntax start)
				 (form-around syntax start))
	until (null potential-form)
	when (typep potential-form 'list-form)
	  do (setf (offset mark) (end-offset potential-form))
	     (return)
	finally (error 'no-expression)))

(defmethod backward-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
	  then (start-offset potential-form)
	for potential-form = (or (form-before syntax start)
				 (form-around syntax start))
	until (null potential-form)
	when (typep potential-form 'list-form)
	  do (setf (offset mark) (start-offset potential-form))
	     (return)
	finally (error 'no-expression)))

(defmethod down-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
	  then (end-offset potential-form)
	for potential-form = (or (form-after syntax start)
				 (form-around syntax start))
	until (null potential-form)
	when (typep potential-form 'list-form)
	  do (setf (offset mark) (1+ (start-offset potential-form)))
	     (return)
	finally (error 'no-expression)))

(defmethod backward-down-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
	  then (start-offset potential-form)
	for potential-form = (or (form-before syntax start)
				 (form-around syntax start))
	until (null potential-form)
	when (typep potential-form 'list-form)
	  do (setf (offset mark) (1- (end-offset potential-form)))
	     (return)
	finally (error 'no-expression)))

(defmethod backward-up-list (mark (syntax lisp-syntax))
  (let ((form (or (form-around syntax (offset mark))
		  (form-before syntax (offset mark))
		  (form-after syntax (offset mark)))))
    (if form
	(let ((parent (parent form)))
	  (if (typep parent 'list-form)
	      (setf (offset mark) (start-offset parent))
	      (error 'no-expression)))
	(error 'no-expression))))

(defmethod up-list (mark (syntax lisp-syntax))
  (let ((form (or (form-around syntax (offset mark))
		  (form-before syntax (offset mark))
		  (form-after syntax (offset mark)))))
    (if form
	(let ((parent (parent form)))
	  (if (typep parent 'list-form)
	      (setf (offset mark) (end-offset parent))
	      (error 'no-expression)))
	(error 'no-expression))))

(defmethod eval-defun (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
     (loop for form in (children stack-top)
	   when (and (mark<= (start-offset form) mark)
		     (mark<= mark (end-offset form)))
	     do (return (eval (read-from-string 
			       (coerce (buffer-sequence (buffer syntax)
							(start-offset form)
							(end-offset form))
				       'string)))))))

(defmethod beginning-of-definition (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
    (loop for form in (children stack-top)
	  with last-toplevel-list = nil
	  when (and (typep form 'form)
		    (mark< mark (end-offset form)))
          do (if (mark< (start-offset form) mark)
		 (setf (offset mark) (start-offset form))
		 (when last-toplevel-list form
		       (setf (offset mark) (start-offset last-toplevel-list))))
	     (return t)
	  when (typep form 'form)
	  do (setf last-toplevel-list form)
	  finally (when last-toplevel-list form
		       (setf (offset mark) (start-offset last-toplevel-list))))))

(defmethod end-of-definition (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
    (loop for form in (children stack-top)
	  when (and (typep form 'form)
		    (mark< mark (end-offset form)))
	  do (setf (offset mark) (end-offset form))
	     (loop-finish))))

;;; shamelessly replacing SWANK code
;; We first work through the string removing the characters and noting
;; which ones are escaped. We then replace each character with the
;; appropriate case version, according to the readtable.
;; Finally, we extract the package and symbol names.
;; Being in an editor, we are waaay more lenient than the reader.

(defun parse-escapes (string)
  "Return a string and a list of escaped character positions.
Uses part of the READ algorithm in CLTL2 22.1.1."
  (let ((length (length string))
	(index 0)
	irreplaceables chars)
    (tagbody
     step-8
       (unless (< index length) (go end))
       (cond 
	 ((char/= (char string index) #\\ #\|)
	  (push (char string index) chars)
	  (incf index)
	  (go step-8))
	 ((char= (char string index) #\\)
	  (push (length chars) irreplaceables)
	  (incf index)
	  (unless (< index length) (go end))
	  (push (char string index) chars)
	  (incf index)
	  (go step-8))
	 ((char= (char string index) #\|)
	  (incf index)
	  (go step-9)))
     step-9
       (unless (< index length) (go end))
       (cond 
	 ((char/= (char string index) #\\ #\|)
	  (push (length chars) irreplaceables)
	  (push (char string index) chars)
	  (incf index)
	  (go step-9))
	 ((char= (char string index) #\\)
	  (push (length chars) irreplaceables)
	  (incf index)
	  (unless (< index length) (go end))
	  (push (char string index) chars)
	  (incf index)
	  (go step-9))
	 ((char= (char string index) #\|)
	  (incf index)
	  (go step-8)))
     end
       (return-from parse-escapes
	 (values (coerce (nreverse chars) 'string)
		 (nreverse irreplaceables))))))

(defun invert-cases (string &optional (irreplaceables nil))
  "Returns two flags: unescaped upper-case and lower-case chars in STRING."
  (loop for index below (length string)
       with upper = nil
       with lower = nil
       when (not (member index irreplaceables))
        if (upper-case-p (char string index))
         do (setf upper t) end
        if (lower-case-p (char string index))
         do (setf lower t) end
     finally (return (values upper lower))))

(defun replace-case (string &optional (case (readtable-case *readtable*))
		                      (irreplaceables nil))
  "Convert string according to readtable-case."
  (multiple-value-bind (upper lower) (invert-cases string irreplaceables)
    (loop for index below (length string)
       as char = (char string index) then (char string index)
       if (member index irreplaceables)
         collect char into chars
       else
         collect (ecase case
		   (:preserve char)
		   (:upcase (char-upcase char))
		   (:downcase (char-downcase char))
		   (:invert (cond ((and lower upper) char)
				  (lower (char-upcase char))
				  (upper (char-downcase char))
				  (t char)))) into chars
       finally (return (coerce chars 'string)))))

(defun parse-token (string &optional (case (readtable-case *readtable*)))
  "Extracts the symbol-name and package name from STRING
and whether the symbol-name was separated from the package by a double colon."
  (multiple-value-bind (string irreplaceables) (parse-escapes string)
    (let ((string (replace-case string case irreplaceables))
	  package-name symbol-name internalp)
      (loop for index below (length string)
	   with symbol-start = 0
	   when (and (char= (char string index) #\:)
		     (not (member index irreplaceables)))
	        do (setf package-name (subseq string 0 index))
	           (if (and (< (incf index) (length string))
			    (char= (char string index) #\:)
			    (not (member index irreplaceables)))
		       (setf symbol-start (1+ index)
			     internalp t)
		       (setf symbol-start index))
	           (loop-finish)
	   finally (setf symbol-name (subseq string symbol-start)))
      (values symbol-name package-name internalp))))

#|
;;; Compare CLHS 23.1.2.1
 (defun test-parse-token ()
  (let ((*readtable* (copy-readtable nil)))
    (format t "READTABLE-CASE  Input         Symbol-name   Token-name~
             ~%------------------------------------------------------~
             ~%")
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (dolist (input '("ZEBRA" "Zebra" "zebra" "\\zebra" "\\Zebra" "z|ebr|a"
		       "|ZE\\bRA|" "ze\\|bra"))
	(format t "~&:~A~16T~A~30T~A~44T~A"
		(string-upcase readtable-case)
		input
		(progn (setf (readtable-case *readtable*) readtable-case)
		       (symbol-name (read-from-string input)))
		(parse-token input readtable-case))))))
|#

(defun parse-symbol (string &optional (package *package*))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbol was found."
  (multiple-value-bind (symbol-name package-name) (parse-token string)
    (let ((package (cond ((string= package-name "") keyword-package)
                         (package-name              (find-package package-name))
                         (t                         package))))
      (if package
          (find-symbol symbol-name package)
          (values nil nil)))))

(defun token-to-symbol (syntax token)
  (let ((package (or (slot-value syntax 'package)
		     (find-package :common-lisp)))
	(token-string (coerce (buffer-sequence (buffer syntax)
					       (start-offset token)
					       (end-offset token))
			      'string)))
    (parse-symbol token-string package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; indentation

(defmethod indent-form ((syntax lisp-syntax) (tree form*) path)
  (cond ((or (null path)
	     (and (null (cdr path)) (zerop (car path))))
	 (values tree 0))
	((null (cdr path))
	 (values (elt (children tree) (1- (car path))) 0))
	(t (indent-form syntax (elt (children tree) (car path)) (cdr path)))))

(defmethod indent-form ((syntax lisp-syntax) (tree list-form) path)
  (if (= (car path) 1)
      ;; before first element
      (values tree 1)
      (let ((first-child (elt (children tree) 1)))
	(cond ((and (typep first-child 'token-mixin)
		    (token-to-symbol syntax first-child))
	       (compute-list-indentation syntax (token-to-symbol syntax first-child) tree path))
	      ((null (cdr path))
	       ;; top level
	       (if (= (car path) 2)
		   ;; indent like first element
		   (values (elt (children tree) 1) 0)
		   ;; indent like second element
		   (values (elt (children tree) 2) 0)))
	      (t
	       ;; inside a subexpression
	       (indent-form syntax (elt (children tree) (car path)) (cdr path)))))))	    

(defmethod indent-form ((syntax lisp-syntax) (tree string-form) path)
  (values tree 1))

(defmethod indent-form ((syntax lisp-syntax) (tree token-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree error-symbol) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree long-comment-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree backquote-form) path)
  (indent-form syntax (elt (children tree) (car path)) (cdr path)))

(defmethod indent-binding ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; top level
      (cond ((= (car path) 1)
	     ;; before variable, indent 1
	     (values tree 1))
	    ((= (car path) 2)
	     ;; between variable and value
	     (values (elt (children tree) 1) 0))
	    (t
	     ;; after value
	     (values (elt (children tree) 2) 0)))
      (indent-form syntax (elt (children tree) (car path)) (cdr path))))

(defmethod indent-bindings ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; entire bind form
      (if (= (car path) 1)
	  ;; before first binding, indent 1
	  (values tree 1)
	  ;; after some bindings, align with first binding
	  (values (elt (children tree) 1) 0))
      ;; inside a bind form
      (indent-binding syntax (elt (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation ((syntax lisp-syntax) symbol tree path)
  (if (null (cdr path))
      ;; top level
      (if (= (car path) 2)
	  ;; indent like first child
	  (values (elt (children tree) 1) 0)
	  ;; indent like second child
	  (values (elt (children tree) 2) 0))
      ;; inside a subexpression
      (indent-form syntax (elt (children tree) (car path)) (cdr path))))

(defmacro define-list-indentor (name element-indentor)
  `(defun ,name (syntax tree path)
     (if (null (cdr path))
	 ;; top level
	 (if (= (car path) 1)
	     ;; indent one more than the list
	     (values tree 1)
	     ;; indent like the first element
	     (values (elt (children tree) 1) 0))
	 ;; inside an element
	 (,element-indentor syntax (elt (children tree) (car path)) (cdr path)))))

;;; line up the elements vertically
(define-list-indentor indent-list indent-list)

;;; for now the same as indent-list, but try to do better with
;;; optional parameters with default values
(define-list-indentor indent-ordinary-lambda-list indent-list)
;;; again, can do better
(define-list-indentor indent-macro-lambda-list indent-list)
;;; FIXME: also BOA, DEFSETF, DEFTYPE, SPECIALIZED, GENERIC-FUNCTION,
;;; DESTRUCTURING, DEFINE-MODIFY-MACRO and
;;; DEFINE-METHOD-COMBINATION-ARGUMENTS

(defmacro define-simple-indentor (template)
  `(defmethod compute-list-indentation
       ((syntax lisp-syntax) (symbol (eql ',(car template))) tree path)
     (cond ((null (cdr path))
	    (values tree (if (<= (car path) ,(length template)) 4 2)))
	   ,@(loop for fun in (cdr template)
		  for i from 2
		  collect `((= (car path) ,i) (,fun syntax (elt (children tree) ,i) (cdr path))))
	   (t (indent-form syntax (elt (children tree) (car path)) (cdr path))))))

(define-simple-indentor (progn))
(define-simple-indentor (prog1 indent-form))
(define-simple-indentor (prog2 indent-form indent-form))
(define-simple-indentor (locally))
(define-simple-indentor (let indent-bindings))
(define-simple-indentor (let* indent-bindings))
(define-simple-indentor (multiple-value-bind indent-list indent-form))
(define-simple-indentor (defun indent-list indent-ordinary-lambda-list))
(define-simple-indentor (defmacro indent-list indent-macro-lambda-list))
(define-simple-indentor (with-slots indent-bindings indent-form))
(define-simple-indentor (with-accessors indent-bindings indent-form))
(define-simple-indentor (when indent-form))
(define-simple-indentor (unless indent-form))
(define-simple-indentor (print-unreadable-object indent-list))
(define-simple-indentor (defvar indent-form))
(define-simple-indentor (defparameter indent-form))
(define-simple-indentor (defconstant indent-form))

;;; non-simple-cases: LOOP, MACROLET, FLET, LABELS

;;; do this better 
(define-list-indentor indent-slot-specs indent-list)

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defclass)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
	((2 3)
	 ;; in the class name or superclasses respectively
	 (indent-list syntax (elt (children tree) (car path)) (cdr path)))
	(4
	 ;; in the slot specs 
	 (indent-slot-specs syntax (elt (children tree) 4) (cdr path)))
	(t
	 ;; this is an approximation, might want to do better
	 (indent-list syntax (elt (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defgeneric)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
	(2
	 ;; in the function name
	 (indent-list syntax (elt (children tree) 2) (cdr path)))
	(3
	 ;; in the lambda-list
	 (indent-lambda-list syntax (elt (children tree) 3) (cdr path)))
	(t
	 ;; in the options or method specifications
	 (indent-list syntax (elt (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defmethod)) tree path)
  (let ((lambda-list-pos (position-if (lambda (x) (typep x 'list-form))
				      (children tree))))
    (cond ((null (cdr path))
	   ;; top level
	   (values tree (if (or (null lambda-list-pos)
				(<= (car path) lambda-list-pos))
			    4
			    2)))
	  ((or (null lambda-list-pos)
	       (< (car path) lambda-list-pos))
	   (indent-list syntax (elt (children tree) (car path)) (cdr path)))
	  ((= (car path) lambda-list-pos)
	   (indent-lambda-list syntax (elt (children tree) (car path)) (cdr path)))
	  (t
	   (indent-form syntax (elt (children tree) (car path)) (cdr path))))))

(defun indent-clause (syntax tree path)
  (if (null (cdr path))
      ;; top level
      (case (car path)
        (1 (values tree 1))
        (2 (values tree 1))
        (t (values (elt (children tree) 2) 0)))
      (indent-form syntax (elt (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'cond)) tree path)
  (if (null (cdr path))
      ;; top level
      (if (= (car path) 2)
	  ;; after `cond' 
	  (values tree 2)
	  ;; indent like the first clause
	  (values (elt (children tree) 2) 0))
      ;; inside a clause
      (indent-clause syntax (elt (children tree) (car path)) (cdr path))))

(macrolet ((def (symbol)
               `(defmethod compute-list-indentation
                 ((syntax lisp-syntax) (symbol (eql ',symbol)) tree path)
                 (if (null (cdr path))
                     (case (car path)
                       (2 (values tree 4))
                       (3 (values tree 2))
                       (t (values (elt (children tree) 3) 0)))
                     (indent-clause syntax (elt (children tree) (car path)) (cdr path))))))
  (def case)
  (def ccase)
  (def ecase)
  (def typecase)
  (def ctypecase)
  (def etypecase))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'tagbody)) tree path)
  (if (null (cdr path))
      ;; this TOKEN-MIXIN test is not quite right.  It should be a
      ;; test for symbolness of the token, but it shouldn't depend on
      ;; the symbol existing in the current image.  (Arguably, too,
      ;; this is a broken indentation form because it doesn't carry
      ;; over to the implicit tagbodies in macros such as DO.
      (if (typep (elt (children tree) (car path)) 'token-mixin) 
          (values tree 2)
          (values tree 4))
      (indent-form syntax (elt (children tree) (car path)) (cdr path))))

(defun compute-path-in-trees (trees n offset)
  (cond ((or (null trees)
	     (>= (start-offset (car trees)) offset))    
	 (list n))
	((or (< (start-offset (car trees)) offset (end-offset (car trees)))
	     (typep (car trees) 'incomplete-form-mixin))
	 (cons n (compute-path-in-tree (car trees) offset)))
	(t (compute-path-in-trees (cdr trees) (1+ n) offset))))

(defun compute-path-in-tree (tree offset)
  (if (null (children tree))
      '()
      (compute-path-in-trees (children tree) 0 offset)))

(defun compute-path (syntax offset)
  (with-slots (stack-top) syntax
    (compute-path-in-tree stack-top offset)))

(defun real-column-number (mark tab-width)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop with column = 0
	  until (mark= mark mark2)
	  do (if (eql (object-after mark2) #\Tab)
		 (loop do (incf column)
		       until (zerop (mod column tab-width)))
		 (incf column))
	  do (incf (offset mark2))
          finally (return column))))

(defmethod syntax-line-indentation (mark tab-width (syntax lisp-syntax))
  (setf mark (clone-mark mark))
  (beginning-of-line mark)
  (with-slots (stack-top) syntax
    (let ((path (compute-path syntax (offset mark))))
      (multiple-value-bind (tree offset)
	  (indent-form syntax stack-top path)
	(setf (offset mark) (start-offset tree))
	(+ (real-column-number mark tab-width)
	   offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commenting

(defmethod syntax-line-comment-string ((syntax lisp-syntax))
  ";;; ")

(defmethod comment-region ((syntax lisp-syntax) mark1 mark2)
  (line-comment-region syntax mark1 mark2))

(defmethod uncomment-region ((syntax lisp-syntax) mark1 mark2)
  (line-uncomment-region syntax mark1 mark2))

