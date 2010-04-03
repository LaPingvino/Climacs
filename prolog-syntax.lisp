;;; -*- Mode: Lisp; Package: CLIMACS-PROLOG-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Christophe Rhodes (c.rhodes@gold.ac.uk)
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

;;; Syntax for analysing ISO Prolog

(in-package #:climacs-prolog-syntax)

(defclass prolog-parse-tree (parse-tree)
  ())

(define-syntax-command-table prolog-table :errorp nil)

(define-syntax prolog-syntax (fundamental-syntax)
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser)
   (operator-directives :initform nil :accessor operator-directives))
  (:name "Prolog")
  (:pathname-types "pl")
  (:command-table prolog-table))

(defparameter *prolog-grammar* (grammar))

;;; *THIS-SYNTAX* is bound around calls to the parser, so that the
;;; parser rules can update the operator directive table.  Possibly
;;; this functionality ought to be offered by the syntax module
;;; itself?
(defvar *this-syntax*)

(defmacro define-prolog-rule ((&rest rule) &body body)
  `(add-rule (grammar-rule (,@rule ,@body)) *prolog-grammar*))

(defmethod initialize-instance :after ((syntax prolog-syntax) &rest args)
  (declare (ignore args))
  (let ((buffer (buffer syntax)))
    (with-slots (parser lexer) syntax
      (setf parser (make-instance 'parser
                                  :grammar *prolog-grammar*
                                  :target 'prolog-text))
      (setf lexer (make-instance 'prolog-lexer :buffer buffer :syntax syntax))
      (let ((m (make-buffer-mark buffer 0 :left))
            (lexeme (make-instance 'start-lexeme :state (initial-state parser))))
        (setf (offset m) 0)
        (setf (start-offset lexeme) m
              (end-offset lexeme) 0)
        (insert-lexeme lexer 0 lexeme)))))

;;; grammar

(defclass prolog-nonterminal (prolog-parse-tree)
  ())

(defclass prolog-token (prolog-parse-tree)
  ((ink) (face) (start) (end)))

;;; lexer

(defclass prolog-lexeme (prolog-token)
  ((state :initarg :state)))
(defmethod print-object ((o prolog-lexeme) s)
  (print-unreadable-object (o s :type t)
    (format s (lexeme-string o))))

(defclass start-lexeme (prolog-lexeme) ())

(defgeneric display-parse-tree (entity syntax stream drei))

(defclass layout-text (prolog-nonterminal)
  ((comment :initarg :comment :accessor comment :initform nil)
   (cont :initarg :cont :accessor cont)))
(defmethod display-parse-tree
    ((entity layout-text) (syntax prolog-syntax) (stream extended-output-stream) (drei drei))
  (when (cont entity)
    (display-parse-tree (cont entity) syntax stream drei))
  (when (comment entity)
    (with-drawing-options (stream :ink (make-rgb-color 0.7 0.0 0.0))
      (display-parse-tree (comment entity) syntax stream drei))))

(defgeneric syntactic-lexeme (thing))
(defmethod syntactic-lexeme ((lexeme prolog-lexeme))
  lexeme)
(macrolet ((def ((name &optional tokenp) &rest subs)
	     (flet ((f (x) (intern (format-sym "~A-~A" x '#:lexeme))))
	       `(progn
		  (defclass ,(f name) (prolog-lexeme) ())

                  ,@(when tokenp
                      `((defclass ,name (prolog-nonterminal)
                          ((layout-text :initarg :layout-text :accessor layout-text :initform nil)
                           (syntactic-lexeme :initarg :syntactic-lexeme :accessor syntactic-lexeme)))
                        (defmethod display-parse-tree
                            ((entity ,name) (syntax prolog-syntax) (stream extended-output-stream) (drei drei))
                          (when (layout-text entity)
                            (display-parse-tree
                             (layout-text entity) syntax stream drei))
                          (display-parse-tree
                           (syntactic-lexeme entity) syntax stream drei))
                        (define-prolog-rule (,name -> (,(f name)))
                          (make-instance ',name :syntactic-lexeme ,(f name)))
                        (define-prolog-rule (,name -> (layout-text ,(f name)))
                          (make-instance ',name :layout-text layout-text
                           :syntactic-lexeme ,(f name)))))
		  ,@(loop for sub in subs collect
                          `(defclass ,(f sub) (,(f name)) ()))))))
  (def (comment) single-line-comment bracketed-comment)
  
  (def (name t) identifier graphic quoted semicolon cut)
  (def (variable t) anonymous named)
  (def (integer t) integer-constant character-code-constant binary-constant
                   octal-constant hexadecimal-constant)
  (def (float-number t))
  (def (char-code-list t))
  (def (open-ct))
  (def (open t))
  (def (close t))
  (def (open-list t))
  (def (close-list t))
  (def (open-curly t))
  (def (close-curly t))
  (def (head-tail-separator t))
  (def (comma t))
  (def (end t))
  
  (def (error)))

;;; open-ct is a special case: by 6.5.1 it cannot be preceded by
;;; layout text.  We could elide this and its grammar rules, but this
;;; way we get a clearer relationship between the standard and its
;;; expression here.
(defclass open-ct (prolog-nonterminal)
  ((syntactic-lexeme :initarg :syntactic-lexeme :accessor syntactic-lexeme)))
(defmethod display-parse-tree ((entity open-ct) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (syntactic-lexeme entity) syntax stream drei))
(define-prolog-rule (open-ct -> (open-ct-lexeme))
  (make-instance 'open-ct :syntactic-lexeme open-ct-lexeme))

;;; 6.4.1
(define-prolog-rule (layout-text -> (layout-text comment-lexeme))
  (make-instance 'layout-text :comment comment-lexeme :cont layout-text))
(define-prolog-rule (layout-text -> ())
  (make-instance 'layout-text :cont nil))

(defclass prolog-lexer (incremental-lexer)
  ((valid-lex :initarg :valid-lex :initform 1)
   (syntax :initarg :syntax :reader syntax)))

(defmethod next-lexeme ((lexer prolog-lexer) scan)
  (let ((string (make-array 0 :element-type 'character
			    :fill-pointer 0 :adjustable t)))
    (flet ((fo ()
	     (vector-push-extend (object-after scan) string)
	     (forward-object scan))
	   (bo ()
	     (vector-pop string)
	     (backward-object scan)))
      (macrolet ((read-quoted-char (char)
		   `(block read-quoted-char
		     (let ((o (object-after scan)))
		       (tagbody
			START
			  (cond 
			    ((eql o #\\) (fo) (go ESCAPE))
			    ((eql o ,char) (fo) (go QUOTE))
			    (t (fo) (return-from read-quoted-char t)))
			QUOTE
			  (if (end-of-buffer-p scan)
			      (return-from read-quoted-char nil)
			      (let ((o (object-after scan)))
				(cond
				  ((eql o ,char) (fo) (return-from read-quoted-char t))
				  (t (return-from read-quoted-char nil)))))
			ESCAPE
			  (if (end-of-buffer-p scan)
			      (return (make-instance 'error-lexeme))
			      (let ((o (object-after scan)))
				(cond
				  ;; meta (6.5.5)
				  ((position o "\\'\"`") (fo) (return-from read-quoted-char t))
				  ;; symbolic (6.4.2.1)
				  ((position o "abfnrtv") (fo) (return-from read-quoted-char t))
				  ;; octal
				  ((digit-char-p o 8) (fo)
				   (tagbody
				    LOOP
				      (when (end-of-buffer-p scan)
					(return (make-instance 'error-lexeme)))
				      (let ((o (object-after scan)))
					(cond
					  ((eql o #\\) (fo) (return-from read-quoted-char t))
					  ((digit-char-p o 8) (fo) (go LOOP))
					  (t (return (make-instance 'error-lexeme)))))))
				  ((eql o #\x) (fo)
				   (if (or (end-of-buffer-p scan)
					   (not (digit-char-p (object-after scan) 16)))
				       (return (make-instance 'error-lexeme))
				       (progn 
					 (fo)
					 (tagbody
					  LOOP
					    (when (end-of-buffer-p scan)
					      (return (make-instance 'error-lexeme)))
					    (let ((o (object-after scan)))
					      (cond
						((eql o #\\) (fo) (return-from read-quoted-char t))
						((digit-char-p o 16) (fo) (go LOOP))
						(t (return (make-instance 'error-lexeme)))))))))
				  (t (return (make-instance 'error-lexeme)))))))))))
      (let ((object (object-after scan)))
	(block nil
	  (tagbody
	   START
	     (cond
	       ((lower-case-p object) (fo) (go IDENTIFIER))
               ((eql object #\/) (fo) (go COMMENT-OR-GRAPHIC))
               ((eql object #\%) (fo) (go LINE-COMMENT))
	       ((position object "#$&*+-./:<=>?@^~\\") (fo) (go GRAPHIC-TOKEN))
	       ((eql object #\') (fo) (go QUOTED-TOKEN))
	       ((eql object #\;)
		(fo) (return (make-instance 'semicolon-lexeme)))
	       ((eql object #\!)
		(fo) (return (make-instance 'cut-lexeme)))
	       ((eql object #\_) (fo) (go VARIABLE))
	       ((upper-case-p object) (fo) (go NAMED-VARIABLE))
	       ((eql object #\0) (fo) (go NUMBER-OR-INTEGER))
	       ((digit-char-p object) (fo) (go NUMBER))
	       ((eql object #\") (fo) (go CHAR-CODE-LIST))
	       ((eql object #\()
		(if (or (beginning-of-buffer-p scan)
			(not (member (object-before scan) '(#\Space #\Tab #\Newline))))
		    (progn (fo) (return (make-instance 'open-ct-lexeme)))
		    (progn (fo) (return (make-instance 'open-lexeme)))))
	       ((eql object #\)) (fo) (return (make-instance 'close-lexeme)))
	       ((eql object #\[) (fo) (return (make-instance 'open-list-lexeme)))
	       ((eql object #\]) (fo) (return (make-instance 'close-list-lexeme)))
	       ((eql object #\{) (fo) (return (make-instance 'open-curly-lexeme)))
	       ((eql object #\}) (fo) (return (make-instance 'close-curly-lexeme)))
	       ((eql object #\|)
		(fo) (return (make-instance 'head-tail-separator-lexeme)))
	       ((eql object #\,) (fo) (return (make-instance 'comma-lexeme)))
	       ((eql object #\.) (error "shouldn't get here"))
	       (t (fo) (return (make-instance 'error-lexeme))))
	   IDENTIFIER
	     (loop until (end-of-buffer-p scan)
		   while (let ((object (object-after scan)))
			   (or (alphanumericp object)
			       (eql object #\_)))
		   do (fo))
	     (return (make-instance 'identifier-lexeme))
           LINE-COMMENT
             (loop until (end-of-buffer-p scan)
                   until (eql (object-after scan) #\Newline)
                   do (fo))
             (if (end-of-buffer-p scan)
                 (return (make-instance 'error-lexeme))
                 (return (make-instance 'single-line-comment-lexeme)))
           COMMENT-OR-GRAPHIC
             (if (end-of-buffer-p scan)
                 (return (make-instance 'graphic-lexeme))
                 (let ((object (object-after scan)))
                   (cond
                     ((eql object #\*) (fo) (go COMMENT))
                     ((not (position object "#$&*+-./:<=>?@^~\\"))
                      (return (make-instance 'graphic-lexeme)))
                     (t (fo) (go GRAPHIC-TOKEN)))))
           COMMENT
             (cond
               ((end-of-buffer-p scan)
                (return (make-instance 'error-lexeme)))
               ((eql (object-after scan) #\*)
                (fo)
                (cond
                  ((end-of-buffer-p scan)
                   (return (make-instance 'error-lexeme)))
                  ((eql (object-after scan) #\/)
                   (fo)
                   (return (make-instance 'bracketed-comment-lexeme)))
                  (t (fo) (go COMMENT))))
               (t (fo) (go COMMENT)))
           GRAPHIC-TOKEN
	     (loop until (end-of-buffer-p scan)
		while (position (object-after scan) "#$&*+-./:<=>?@^~\\")
		do (fo))
	     (cond
	       ((end-of-buffer-p scan)
		(cond
		  ((string= string ".")
		   (return (make-instance 'end-lexeme)))
		  (t (return (make-instance 'graphic-lexeme)))))
	       (t
		(cond
		  ((and (string= string ".") 
                        (or (whitespacep (syntax lexer)
                                         (object-after scan))
                            (eql (object-after scan) #\%)))
		   (return (make-instance 'end-lexeme)))
		  (t (return (make-instance 'graphic-lexeme))))))
	   QUOTED-TOKEN
	     (loop named #:mu
		   until (end-of-buffer-p scan)
		   while (read-quoted-char #\'))
	     (return (make-instance 'quoted-lexeme))
	   VARIABLE
	     (if (or (end-of-buffer-p scan)
                     (let ((object (object-after scan)))
                       (not (or (alphanumericp object)
                                (eql object #\_)))))
		 (return (make-instance 'anonymous-lexeme))
		 (go NAMED-VARIABLE))
	   NAMED-VARIABLE
	     (loop until (end-of-buffer-p scan)
		while (let ((object (object-after scan)))
                        (or (alphanumericp object)
                            (eql object #\_)))
		do (fo))
	     (return (make-instance 'named-lexeme))
	   NUMBER-OR-INTEGER
	     (if (end-of-buffer-p scan)
		 (return (make-instance 'integer-lexeme))
		 (let ((object (object-after scan)))
		   (cond
		     ((eql object #\') (fo) (go CHARACTER-CODE-CONSTANT))
		     ((eql object #\b) (fo) (go BINARY-CONSTANT))
		     ((eql object #\o) (fo) (go OCTAL-CONSTANT))
		     ((eql object #\x) (fo) (go HEXADECIMAL-CONSTANT))
		     ((digit-char-p object) (fo) (go NUMBER))
                     ((eql object #\.) (fo) (go INTEGER-AND-END-OR-FLOAT))
		     (t (return (make-instance 'integer-lexeme))))))
	   CHARACTER-CODE-CONSTANT
	     (if (read-quoted-char #\')
		 (return (make-instance 'character-code-constant-lexeme))
		 (return (make-instance 'error-lexeme)))
	   BINARY-CONSTANT
	     (loop until (end-of-buffer-p scan)
		   while (digit-char-p (object-after scan) 2)
		   do (fo))
	     (return (make-instance 'binary-constant-lexeme))
	   OCTAL-CONSTANT
	     (loop until (end-of-buffer-p scan)
		   while (digit-char-p (object-after scan) 8)
		   do (fo))
	     (return (make-instance 'octal-constant-lexeme))
	   HEXADECIMAL-CONSTANT
	     (loop until (end-of-buffer-p scan)
		   while (digit-char-p (object-after scan) 16)
		   do (fo))
	     (return (make-instance 'hexadecimal-constant-lexeme))
	   NUMBER
	     (loop until (end-of-buffer-p scan)
                   when (eql (object-after scan) #\.) 
                     do (fo) and do (go INTEGER-AND-END-OR-FLOAT)
		   while (digit-char-p (object-after scan))
		   do (fo))
	     (return (make-instance 'integer-constant-lexeme))
	   CHAR-CODE-LIST
	     (loop named #:mu
		   until (end-of-buffer-p scan)
		   while (read-quoted-char #\"))
	     (return (make-instance 'char-code-list-lexeme))
           INTEGER-AND-END-OR-FLOAT
             (when (or (end-of-buffer-p scan)
                       (let ((object (object-after scan)))
                         (or (eql object #\%)
                             (whitespacep (syntax lexer)
                                          object))))
               (bo)
               (return (make-instance 'integer-lexeme)))
             (loop until (end-of-buffer-p scan)
                   while (digit-char-p (object-after scan))
                   do (fo))
             (when (or (end-of-buffer-p scan)
                       (not (member (object-after scan) '(#\e #\E))))
               (return (make-instance 'float-number-lexeme)))
             (fo)
             (when (end-of-buffer-p scan)
               (return (make-instance 'error-lexeme)))
             (when (member (object-after scan) '(#\+ #\-))
               (fo)
               (when (end-of-buffer-p scan)
                 (return (make-instance 'error-lexeme))))
             (loop until (end-of-buffer-p scan)
                   while (digit-char-p (object-after scan))
                   do (fo))
             (return (make-instance 'float-number-lexeme)))))))))

;;; parser

(defclass prolog-text (prolog-nonterminal)
  ())
(defclass empty-prolog-text (prolog-text)
  ())
(defclass clause-prolog-text (prolog-text)
  ((clause :initarg :clause :accessor clause)
   (text-rest :initarg :text-rest :accessor text-rest)))
(defclass directive-prolog-text (prolog-text)
  ((directive :initarg :directive :accessor directive)
   (text-rest :initarg :text-rest :accessor text-rest)))

(defmethod display-parse-tree
    ((entity empty-prolog-text) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (declare (ignore stream drei))
  nil)
(defmethod display-parse-tree
    ((entity clause-prolog-text) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (text-rest entity) syntax stream drei)
  (display-parse-tree (clause entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity directive-prolog-text) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (text-rest entity) syntax stream drei)
  (with-text-face (stream :italic)
    (display-parse-tree (directive entity) syntax stream drei)))

(defclass directive (prolog-nonterminal)
  ((directive-term :initarg :directive-term :accessor directive-term)
   (end :initarg :end :accessor end)))
(defclass directive-term (prolog-nonterminal)
  ((term :initarg :term :accessor term)))
(defclass clause (prolog-nonterminal)
  ((clause-term :initarg :clause-term :accessor clause-term)
   (end :initarg :end :accessor end)))
(defclass clause-term (prolog-nonterminal)
  ((term :initarg :term :accessor term)))

(defmethod display-parse-tree ((entity directive) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (with-text-face (stream :italic)
    (display-parse-tree (directive-term entity) syntax stream drei))
  (display-parse-tree (end entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity directive-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (term entity) syntax stream drei))
(defmethod display-parse-tree ((entity clause) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (clause-term entity) syntax stream drei)
  (display-parse-tree (end entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity clause-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (term entity) syntax stream drei))

(defgeneric functor (term))
(defgeneric arity (term))
(defclass term (prolog-nonterminal)
  ((priority :initarg :priority :accessor priority)))
(defclass constant-term (term)
  ((value :initarg :value :accessor value)))
(defclass variable-term (term)
  ((name :initarg :name :accessor name)))
(defclass compound-term (term)
  ())
(defgeneric compound-term-p (term))
(defmethod compound-term-p ((term term))
  nil)
(defmethod compound-term-p ((c compound-term))
  t)
(defclass functional-compound-term (compound-term)
  ((functor :initarg :functor :accessor functor)
   (open-ct :initarg :open-ct :accessor open-ct)
   (arg-list :initarg :arg-list :accessor arg-list)
   (close :initarg :close :accessor close)))
(defmethod arity ((f functional-compound-term))
  (arg-list-length (arg-list f)))
(defclass bracketed-term (term)
  ((open :initarg :open :accessor open)
   (term :initarg :term :accessor term)
   (close :initarg :close :accessor close)))
(defclass operator-compound-term (compound-term)
  ((operator :initarg :operator :accessor operator)))
(defmethod functor ((o operator-compound-term))
  (operator o))
(defclass binary-operator-compound-term (operator-compound-term)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))
(defmethod arity ((b binary-operator-compound-term))
  2)
(defclass prefix-operator-compound-term (operator-compound-term)
  ((right :initarg :right :accessor right)))
(defmethod arity ((p prefix-operator-compound-term))
  1)
(defclass postfix-operator-compound-term (operator-compound-term)
  ((left :initarg :left :accessor left)))
(defmethod arity ((p postfix-operator-compound-term))
  1)
(defclass list-compound-term (compound-term)
  (([ :initarg :[ :accessor [)
   (items :initarg :items :accessor items)
   (] :initarg :] :accessor ])))
(defmethod functor ((l list-compound-term))
  ".")
(defmethod arity ((l list-compound-term))
  2)
(defclass curly-compound-term (compound-term)
  (({ :initarg :{ :accessor {)
   (term :initarg :term :accessor term)
   (} :initarg :} :accessor })))
(defmethod functor ((c curly-compound-term))
  "{}")
(defmethod arity ((c curly-compound-term))
  1)
(defclass char-code-list-compound-term (compound-term)
  ((ccl :initarg :ccl :accessor ccl)))
(defmethod functor ((c char-code-list-compound-term))
  ".")
(defmethod arity ((l char-code-list-compound-term))
  2)

(defmethod display-parse-tree
    ((entity constant-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  ;; FIXME: this is so not the right thing.
  (cond
    ((consp (value entity))
     (display-parse-tree (first (value entity)) syntax stream drei)
     (display-parse-tree (second (value entity)) syntax stream drei))
    (t (display-parse-tree (value entity) syntax stream drei))))
(defmethod display-parse-tree 
    ((entity variable-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (with-drawing-options (stream :ink (make-rgb-color 0.7 0.7 0.0))
    (display-parse-tree (name entity) syntax stream drei)))
(defmethod display-parse-tree 
    ((entity functional-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (with-drawing-options (stream :ink (make-rgb-color 0.9 0 0.9))
    (display-parse-tree (functor entity) syntax stream drei))
  (display-parse-tree (open-ct entity) syntax stream drei)
  (display-parse-tree (arg-list entity) syntax stream drei)
  (display-parse-tree (close entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity bracketed-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (open entity) syntax stream drei)
  (display-parse-tree (term entity) syntax stream drei)
  (display-parse-tree (close entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity binary-operator-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (left entity) syntax stream drei)
  (display-parse-tree (operator entity) syntax stream drei)
  (display-parse-tree (right entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity prefix-operator-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (operator entity) syntax stream drei)
  (display-parse-tree (right entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity postfix-operator-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (left entity) syntax stream drei)
  (display-parse-tree (operator entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity list-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (with-drawing-options (stream :ink (make-rgb-color 0.0 0.0 0.8))
    (display-parse-tree ([ entity) syntax stream drei)
    (display-parse-tree (items entity) syntax stream drei)
    (display-parse-tree (] entity) syntax stream drei)))
(defmethod display-parse-tree
    ((entity curly-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree ({ entity) syntax stream drei)
  (display-parse-tree (term entity) syntax stream drei)
  (display-parse-tree (} entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity char-code-list-compound-term) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (with-drawing-options (stream :ink (make-rgb-color 0.0 0.6 0.0))
    (display-parse-tree (ccl entity) syntax stream drei)))

(defclass atom (prolog-nonterminal)
  ((value :initarg :value :accessor value)))
(defgeneric canonical-name (thing)
  ;; FIXME: is this actually necessary?  There is confusion over the
  ;; FUNCTOR of lists, curly lists and char-code lists.
  (:method ((thing string)) thing))
(defmethod canonical-name ((thing atom))
  (canonical-name (value thing)))
(defmethod canonical-name ((thing name))
  ;; FIXME: should canonize
  (lexeme-string (syntactic-lexeme thing)))
(defmethod canonical-name ((thing comma))
  ",")
(defclass empty-list (prolog-nonterminal)
  (([ :initarg :[ :accessor [)
   (] :initarg :] :accessor ])))
(defmethod canonical-name ((thing empty-list))
  ;; FIXME: this clashes with the canonical name for the atom '[]'
  "[]")
(defclass curly-brackets (prolog-nonterminal)
  (({ :initarg :{ :accessor {)
   (} :initarg :} :accessor })))
(defmethod canonical-name ((thing curly-brackets))
  ;; FIXME: see comment in CANONICAL-NAME (EMPTY-LIST)
  "{}")
(defmethod display-parse-tree ((entity atom) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (value entity) syntax stream drei))
(defmethod display-parse-tree ((entity empty-list) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree ([ entity) syntax stream drei)
  (display-parse-tree (] entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity curly-brackets) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree ({ entity) syntax stream drei)
  (display-parse-tree (} entity) syntax stream drei))

(defclass arg-list (prolog-nonterminal)
  ((exp :initarg :exp :accessor exp)))
(defclass arg-list-pair (arg-list)
  ((comma :initarg :comma :accessor comma)
   (arg-list :initarg :arg-list :accessor arg-list)))
(defmethod arg-list-length ((a arg-list))
  1)
(defmethod arg-list-length ((a arg-list-pair))
  ;; Hoho.  See also Felleisen (ECOOP 2004) about TRE and OO.
  (1+ (arg-list-length (arg-list a))))

(defmethod arg-list-nth (n (a arg-list))
  (if (= n 0)
      (exp a)
      (arg-list-nth (1- n) (arg-list a))))

(defmethod display-parse-tree ((entity arg-list) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (exp entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity arg-list-pair) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (exp entity) syntax stream drei)
  (display-parse-tree (comma entity) syntax stream drei)
  (display-parse-tree (arg-list entity) syntax stream drei))

(defclass exp (prolog-nonterminal) ())
(defclass exp-atom (exp)
  ((atom :initarg :atom :accessor atom)))
(defclass exp-term (exp)
  ((term :initarg :term :accessor term)))

(defmethod display-parse-tree ((entity exp-atom) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (atom entity) syntax stream drei))
(defmethod display-parse-tree ((entity exp-term) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (term entity) syntax stream drei))

(defclass lterm (term)
  ((term :initarg :term :accessor term)))
(defmethod compound-term-p ((l lterm))
  (compound-term-p (term l)))
(defmethod functor ((l lterm))
  (functor (term l)))
(defmethod arity ((l lterm))
  (arity (term l)))

(defmethod display-parse-tree ((entity lterm) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (term entity) syntax stream drei))

;;; FIXME: the need for these is because it is a protocol violation to
;;; create nested nonterminals from one rule.
(defclass operator-compound-lterm (lterm)
  ((operator :initarg :operator :accessor operator)))
(defmethod compound-term-p ((l operator-compound-lterm))
  t)
(defmethod functor ((l operator-compound-lterm))
  (operator l))
(defclass binary-operator-compound-lterm (operator-compound-lterm)
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))
(defmethod arity ((l binary-operator-compound-lterm))
  2)
(defclass prefix-operator-compound-lterm (operator-compound-lterm)
  ((right :initarg :right :accessor right)))
(defmethod arity ((l prefix-operator-compound-lterm))
  1)
(defclass postfix-operator-compound-lterm (operator-compound-lterm)
  ((left :initarg :left :accessor left)))
(defmethod arity ((l postfix-operator-compound-lterm))
  1)

(defmethod display-parse-tree
    ((entity binary-operator-compound-lterm) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (left entity) syntax stream drei)
  (display-parse-tree (operator entity) syntax stream drei)
  (display-parse-tree (right entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity prefix-operator-compound-lterm) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (operator entity) syntax stream drei)
  (display-parse-tree (right entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity postfix-operator-compound-lterm) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (left entity) syntax stream drei)
  (display-parse-tree (operator entity) syntax stream drei))

(defclass op (prolog-nonterminal)
  ((name :initarg :name :accessor name)
   (priority :initarg :priority :accessor priority)
   (specifier :initarg :specifier :accessor specifier)))
(defmethod canonical-name ((thing op))
  (canonical-name (name thing)))
(defclass prefix-op (op) ())
(defclass binary-op (op) ())
(defclass postfix-op (op) ())

(defmethod display-parse-tree ((entity op) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (name entity) syntax stream drei))

(defclass items (prolog-nonterminal)
  ((exp :initarg :exp :accessor exp)))
(defclass items-pair (items)
  ((htsep :initarg :htsep :accessor htsep)
   (texp :initarg :texp :accessor texp)))
(defclass items-list (items)
  ((comma :initarg :comma :accessor comma)
   (tlist :initarg :tlist :accessor tlist)))

(defmethod display-parse-tree ((entity items) (syntax prolog-syntax)
                               (stream extended-output-stream) (drei drei))
  (display-parse-tree (exp entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity items-pair) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (exp entity) syntax stream drei)
  (display-parse-tree (htsep entity) syntax stream drei)
  (display-parse-tree (texp entity) syntax stream drei))
(defmethod display-parse-tree
    ((entity items-list) (syntax prolog-syntax)
     (stream extended-output-stream) (drei drei))
  (display-parse-tree (exp entity) syntax stream drei)
  (display-parse-tree (comma entity) syntax stream drei)
  (display-parse-tree (tlist entity) syntax stream drei))

;;; FIXME FIXME FIXME!!!
;;;
;;; This is a band-aid for not having taken the time to sort out an
;;; LTERM "protocol".  I think the proper solution is to
;;;
;;; * make an "encapsulating-lterm" subclass of lterm, and use it in
;;; the lterm -> term rule;
;;;
;;; * for all the relevant questions we can ask of terms
;;; (COMPOUND-TERM-P, ARITY, FUNCTOR, NUMERIC-CONSTANT-P, and so on)
;;; implement methods which do the right thing for this
;;; encapsulating-lterm class, and also for bracketed-term.
;;;
;;; this SLOT-MISSING hack will cause pain later.  Please FIXME.
;;;
;;; CSR, 2005-05-26.
(defmethod slot-missing (class (lterm lterm) name operation &optional value)
  (case operation
    (slot-value (slot-value (term lterm) name))))

;;; 6.2.1
(defun op/3-directive-p (directive)
  (with-slots (directive-term) directive
    (with-slots (term) directive-term
      (with-slots (right) term
	(and (compound-term-p right)
	     (string= (canonical-name (functor right)) "op")
	     (= (arity right) 3))))))

(defun op/3-directive-priority (directive)
  (with-slots (directive-term) directive
    (with-slots (term) directive-term
      (with-slots (right) term
	(let* ((a (arg-list right))
	       ;; FIXME: error-checking
	       (exp (arg-list-nth 0 a))
	       (term (term exp)))
	  (when (numeric-constant-p term)
	    (let ((value (numeric-constant-value term)))
	      (and (<= 0 value 1200) value))))))))

(defun op/3-directive-specifier (directive)
  (with-slots (directive-term) directive
    (with-slots (term) directive-term
      (with-slots (right) term
	(let* ((a (arg-list right))
	       (exp (arg-list-nth 1 a))
	       (term (term exp)))
	  (let ((string (coerce (buffer-sequence (buffer term)
						 (start-offset term)
						 (end-offset term))
				'string)))
	    (cdr (assoc string '(("fx" . :fx) ("fy" . :fy)
				 ("xfx" . :xfx) ("xfy" . :xfy) ("yfx" . :yfx)
				 ("xf" . :xf) ("yf" . :yf))
			:test #'string=))))))))

(defun op/3-directive-operator (directive)
  (with-slots (directive-term) directive
    (with-slots (term) directive-term
      (with-slots (right) term
	(let* ((a (arg-list right))
	       (exp (arg-list-nth 2 a)))
	  (etypecase exp
	    (exp-atom (canonical-name (atom exp)))
	    (exp-term (let* ((term (term exp))
			     (value (slot-value term 'value)))
			  (when (typep value 'atom)
			    (canonical-name value))))))))))
	   
(define-prolog-rule (prolog-text -> (prolog-text directive))
  (when (and (op/3-directive-p directive)
	     (op/3-directive-priority directive)
	     (op/3-directive-specifier directive)
	     (op/3-directive-operator directive))
    ;; FIXME: argh.
    (push directive (operator-directives *this-syntax*)))
  (make-instance 'directive-prolog-text :directive directive
		 :text-rest prolog-text))
(define-prolog-rule (prolog-text -> (prolog-text clause))
  (make-instance 'clause-prolog-text :clause clause :text-rest prolog-text))
(define-prolog-rule (prolog-text -> ())
  (make-instance 'empty-prolog-text))

;;; 6.2.1.1
(defun term-directive-p (term)
  (and (compound-term-p term)
       (string= (canonical-name (functor term)) ":-")
       (= (arity term) 1)))

(define-prolog-rule (directive -> (directive-term end))
  (make-instance 'directive :directive-term directive-term :end end))
(define-prolog-rule (directive-term -> ((term (term-directive-p term))))
  (make-instance 'directive-term :term term))

;;; 6.2.1.2
(define-prolog-rule (clause -> (clause-term end))
  (make-instance 'clause :clause-term clause-term :end end))
(define-prolog-rule (clause-term -> ((term (not (term-directive-p term)))))
  (make-instance 'clause-term :term term))

;;; 6.3.1.1
(define-prolog-rule (term -> (integer))
  (make-instance 'constant-term :priority 0 :value integer))
(define-prolog-rule (term -> (float-number))
  (make-instance 'constant-term :priority 0 :value float-number))

;;; 6.3.1.2
(define-prolog-rule (term -> ((atom
                               (string= (canonical-name atom) "-"))
                              integer))
  ;; FIXME: this doesn't really look right.
  (make-instance 'constant-term :priority 0 :value (list atom integer)))
(define-prolog-rule (term -> ((atom
                               (string= (canonical-name atom) "-"))
                              float-number))
  ;; FIXME: this doesn't really look right.
  (make-instance 'constant-term :priority 0 :value (list atom float-number)))

;;; 6.3.1.3
(define-prolog-rule (term -> ((atom (not (operatorp atom)))))
  (make-instance 'constant-term :priority 0 :value atom))
(define-prolog-rule (term -> ((atom (operatorp atom))))
  (make-instance 'constant-term :priority 1201 :value atom))

(define-prolog-rule (atom -> (name))
  (make-instance 'atom :value name))
(define-prolog-rule (atom -> (empty-list))
  (make-instance 'atom :value empty-list))
(define-prolog-rule (atom -> (curly-brackets))
  (make-instance 'atom :value curly-brackets))
(define-prolog-rule (empty-list -> (open-list close-list))
  (make-instance 'empty-list :[ open-list :] close-list))
(define-prolog-rule (curly-brackets -> (open-curly close-curly))
  (make-instance 'curly-brackets :{ open-curly :} close-curly))

;;; 6.3.2
(define-prolog-rule (term -> (variable))
  (make-instance 'variable-term :priority 0 :name variable))

;;; 6.3.3
(define-prolog-rule (term -> (atom open-ct arg-list close))
  (make-instance 'functional-compound-term :priority 0 :functor atom
                 :arg-list arg-list :open-ct open-ct :close close))
(define-prolog-rule (arg-list -> (exp))
  (make-instance 'arg-list :exp exp))
(define-prolog-rule (arg-list -> (exp comma arg-list))
  (make-instance 'arg-list-pair :exp exp :comma comma :arg-list arg-list))

;;; 6.3.3.1
(define-prolog-rule (exp -> ((atom (and (operatorp atom)
                                        (not (typep (value atom) 'comma))))))
  (make-instance 'exp-atom :atom atom))
(define-prolog-rule (exp -> ((term (<= (priority term) 999))))
  (make-instance 'exp-term :term term))

;;; 6.3.4.1

;;; NOTE NOTE NOTE
;;;
;;; Handling the production rules
;;;
;;;   term  -> lterm
;;;   n        n
;;;
;;; and
;;;
;;;   lterm -> term
;;;   n        n-1
;;;
;;; is done by making LTERM a subclass of TERM (for the first) so that
;;; any LTERM produced by operator rules is acceptable where a regular
;;; term would be, by explicitly writing the second production rule
;;; out here, and by using inegality tests rather than equalities for
;;; priorities elsewhere.  LTERMs act as containers for terms.
;;;
;;; FIXME: why on earth doesn't this cause infinite recursion?  If
;;; LTERM is a subtype of TERM, as it is, this rule should surely be
;;; always applicable.
(define-prolog-rule (lterm -> (term))
  (make-instance 'lterm :term term :priority (1+ (priority term))))

(define-prolog-rule (term -> (open (term (<= (priority term) 1201)) close))
  (make-instance 'bracketed-term :priority 0
                 :open open :term term :close close))
(define-prolog-rule (term -> (open-ct
                              (term (<= (priority term) 1201))
                              close))
  (make-instance 'bracketed-term :priority 0
                 :open open-ct :term term :close close))

;;; 6.3.4.2
;;;
;;; NOTE NOTE NOTE
;;;
;;; We rely here on the (undocumented?) fact that returning NIL from
;;; the body of these rules implies a failure.
(define-prolog-rule (lterm -> ((left term)
                               (op (eql (specifier op) :xfx))
                               (right term)))
  (when (and (< (priority left) (priority op))
             (< (priority right) (priority op)))
    (make-instance 'binary-operator-compound-lterm :priority (priority op)
		   :left left :operator op :right right)))
(define-prolog-rule (lterm -> ((left lterm)
                               (op (eql (specifier op) :yfx))
                               (right term)))
  (when (and (<= (priority left) (priority op))
             (< (priority right) (priority op)))
    (make-instance 'binary-operator-compound-lterm :priority (priority op)
		   :left left :operator op :right right)))
(define-prolog-rule (term -> ((left term)
                              (op (eql (specifier op) :xfy))
                              (right term)))
  (when (and (< (priority left) (priority op))
             (<= (priority right) (priority op)))
    (make-instance 'binary-operator-compound-term :priority (priority op)
                   :left left :operator op :right right)))
(define-prolog-rule (lterm -> (lterm (op (eql (specifier op) :yf))))
  (when (<= (priority lterm) (priority op))
    (make-instance 'postfix-operator-compound-lterm :priority (priority op)
		   :left lterm :operator op)))
(define-prolog-rule (lterm -> (term (op (eql (specifier op) :xf))))
  (when (< (priority term) (priority op))
    (make-instance 'postfix-operator-compound-lterm :priority (priority op)
		   :left term :operator op)))
(define-prolog-rule (term -> ((op (eql (specifier op) :fy)) term))
  (when (and (or (not (string= (canonical-name op) "-"))
                 (not (numeric-constant-p term)))
             (not (typep (first-lexeme term) 'open-ct-lexeme))
             (<= (priority term) (priority op)))
    (make-instance 'prefix-operator-compound-term
                   :right term :operator op :priority (priority op))))
(define-prolog-rule (lterm -> ((op (eql (specifier op) :fx)) term))
  (when (and (or (not (string= (canonical-name op) "-"))
                 (not (numeric-constant-p term)))
             (not (typep (first-lexeme term) 'open-ct-lexeme))
             (< (priority term) (priority op)))
    (make-instance 'prefix-operator-compound-lterm :priority (priority op)
		   :right term :operator op)))

;;; 6.3.4.3
(macrolet ((def (class &rest specifiers)
             `(progn
               (define-prolog-rule (,class -> (name))
                 (let ((opspec (find-predefined-operator name ',specifiers)))
                   (when opspec
                     (make-instance ',class :name name
                                    :priority (opspec-priority opspec)
                                    :specifier (opspec-specifier opspec)))))
               (define-prolog-rule (,class -> (name))
                 (let ((opspec (find-defined-operator name ',specifiers)))
                   (when opspec
                     (make-instance ',class :name name
                                    :priority (opspec-priority opspec)
                                    :specifier (opspec-specifier opspec))))))))
  (def prefix-op :fx :fy)
  (def binary-op :xfx :xfy :yfx)
  (def postfix-op :xf :yf))
(define-prolog-rule (op -> (comma))
  (make-instance 'op :name comma :priority 1000 :specifier :xfy))

;;; 6.3.5
(define-prolog-rule (term -> (open-list items close-list))
  (make-instance 'list-compound-term :priority 0
                 :[ open-list :items items :] close-list))
(define-prolog-rule (items -> (exp comma items))
  (make-instance 'items-list :exp exp :comma comma :tlist items))
(define-prolog-rule (items -> ((left exp) head-tail-separator (right exp)))
  (make-instance 'items-pair :exp left
                 :htsep head-tail-separator :texp right))
(define-prolog-rule (items -> (exp))
  (make-instance 'items :exp exp))

;;; 6.3.6
(define-prolog-rule (term -> (open-curly term close-curly))
  (make-instance 'curly-compound-term :priority 0
                 :{ open-curly :term term :} close-curly))

;;; 6.3.7
(define-prolog-rule (term -> (char-code-list))
  (make-instance 'char-code-list-compound-term
                 :priority 0 :ccl char-code-list))

(defparameter *predefined-operators* nil)
(defstruct (opspec (:type list))
  name
  priority
  specifier)
(macrolet ((def (priority specifier &rest names)
               (let (result)
                 (dolist (name names `(progn ,@(nreverse result)))
                   (push
                    `(push (make-opspec :name ',name :priority ,priority
                            :specifier ,specifier)
                      *predefined-operators*)
                    result)))))
  ;; Table 5 -- The predefined operators
  (def 1200 :xfx ":-" "-->")
  (def 1200 :fx ":-" "?-")
  (def 1100 :xfy ";")
  (def 1050 :xfy "->")
  #+nil ; handled specially (FIXME: is this the right way?)
  (def 1000 :xfy ",")
  (def 700 :xfx "=" "\\=")
  (def 700 :xfx "==" "\\==" "@<" "@=<" "@>" "@>=")
  (def 700 :xfx "=..")
  (def 700 :xfx "is" "=:=" "=\\=" "<" "=<" ">" ">=")
  (def 500 :yfx "+" "-" "/\\" "\\/")
  (def 400 :yfx "*" "/" "//" "rem" "mod" "<<" ">>")
  (def 200 :xfx "**")
  (def 200 :xfy "^")
  (def 200 :fy "-" "\\")
  (def 100 :xfx "@")
  (def 50 :xfx ":"))

(defun find-predefined-operator (name specifiers)
  (find (canonical-name name)
        (remove-if-not (lambda (x) (member (opspec-specifier x) specifiers))
                       *predefined-operators*)
        :key #'opspec-name :test #'string=))
(defun find-defined-operator (name specifiers)
  (let ((operator-directives (operator-directives *this-syntax*)))
    (dolist (d operator-directives)
      (when (> (start-offset name) (end-offset d))
	(when (string= (canonical-name name) (op/3-directive-operator d))
	  (when (member (op/3-directive-specifier d) specifiers)
	    (return (make-opspec :name (op/3-directive-operator d)
				 :priority (op/3-directive-priority d)
				 :specifier (op/3-directive-specifier d)))))))))
		       
(defun operatorp (name)
  (or (find-predefined-operator name '(:xf :yf :fx :fx :xfx :xfy :yfx))
      (find-defined-operator name '(:xf :yf :fx :fx :xfx :xfy :yfx))))

(defun lexeme-string (thing)
  (check-type thing prolog-lexeme)
  (coerce
   (buffer-sequence (buffer thing)
                    (start-offset thing)
                    (end-offset thing))
   'string))

(defun numeric-constant-p (thing)
  (if (typep thing 'lterm)
      (numeric-constant-p (term thing))
      (and (typep thing 'constant-term)
	   (let ((value (value thing)))
	     (or (typep value 'integer)
		 (and (consp value)
		      (typep (car value) 'atom)
		      (typep (cadr value) 'integer)))))))

(defun numeric-constant-value (thing)
  (parse-integer
   (coerce
    (buffer-sequence (buffer thing) (start-offset thing) (end-offset thing))
    'string)))

(defun first-lexeme (thing)
  ;; KLUDGE: it might be "cleaner" to walk the various parsing
  ;; structures, but this will do.
  (let* ((syntax *this-syntax*)
         (lexer (slot-value syntax 'lexer)))
    (do ((i 0 (+ i 1)))
        ((= i (nb-lexemes lexer)) (error "foo"))
      (let ((lexeme (lexeme lexer i)))
        (when (= (start-offset thing) (start-offset lexeme))
          (return lexeme))))))

;;; update syntax

(defmethod inter-lexeme-object-p ((lexer prolog-lexer) object)
  (member object '(#\Space #\Newline #\Tab)))

(defmethod update-syntax esa-utils:values-max-min ((syntax prolog-syntax) prefix-size suffix-size &optional begin end)
  (declare (ignore begin))
  ;; FIXME: this isn't quite right; it's possible that an edit has
  ;; occurred out of view, destroying our parse-up-to-end-lexeme
  ;; invariant.  Actually it also seems to be wrong, maybe because
  ;; there's something weird in views.lisp?  Dunno.
  #+nil
  (when (< end prefix-size)
    (return-from update-syntax (values 0 prefix-size)))
  (with-slots (lexer valid-parse) syntax
    (let* ((low-mark (make-buffer-mark (buffer syntax) prefix-size :left))
           (high-mark (make-buffer-mark
                       (buffer syntax) (- (size (buffer syntax)) suffix-size) :left)))
      ;; this bit really belongs in a method on a superclass --
      ;; something like incremental-lexer.
      (when (mark<= low-mark high-mark)
	(with-slots (drei-syntax::lexemes valid-lex)
	    lexer
	  (let ((start 1)
		(end (nb-elements drei-syntax::lexemes)))
	    (loop while (< start end)
		  do (let ((middle (floor (+ start end) 2)))
		       (if (mark< (end-offset (element* drei-syntax::lexemes middle))
				  low-mark)
			   (setf start (1+ middle))
			   (setf end middle))))
	    (setf valid-lex start)
	    (setf valid-parse start))))
      ;; this bit is truly prolog-syntax specific.
      (when (mark<= low-mark high-mark)
	(with-slots (operator-directives) syntax
	  (do ((directives operator-directives (cdr directives)))
	      ((null directives) (setf operator-directives nil))
	    (when (< (end-offset (car directives))
		     (offset low-mark))
	      (setf operator-directives directives)
	      (return nil)))))))
  ;; old update-syntax-for-display
  (with-slots (parser lexer valid-parse) syntax
    (with-slots (drei-syntax::lexemes valid-lex) lexer
      (let ((scan (make-buffer-mark (buffer syntax) prefix-size :left))
	    (high-mark (make-buffer-mark (buffer syntax) (- (size (buffer syntax)) suffix-size) :left)))
        (setf (offset scan)
              (end-offset (lexeme lexer (1- valid-lex))))
	;; this magic belongs in a superclass' method.  (It's not the
	;; same as HTML/Common Lisp relexing, though)
        (loop named relex
	      do (skip-inter-lexeme-objects lexer scan)
              until (end-of-buffer-p scan)
	      until (and (<= end (start-offset (lexeme lexer (1- valid-lex))))
			 (typep (lexeme lexer (1- valid-lex)) 'end-lexeme))
	      do (when (mark> scan high-mark)
		   (do ()
		       ((= (nb-lexemes lexer) valid-lex))
		     (let ((l (lexeme lexer valid-lex)))
		       (cond
			 ((mark< scan (start-offset l))
			  (return nil))
			 ((mark= scan (start-offset l))
			  (setf valid-lex (nb-lexemes lexer))
			  (return-from relex))
			 (t
			  (delete* drei-syntax::lexemes valid-lex))))))
	      do (let* ((start-mark (clone-mark scan))
			(lexeme (next-lexeme lexer scan))
			(size (- (offset scan) (offset start-mark))))
		   (setf (slot-value lexeme 'drei-syntax::start-mark) start-mark
			 (slot-value lexeme 'drei-syntax::size) size)
		   (insert-lexeme lexer valid-lex lexeme)
		   (incf valid-lex)))
	;; remove lexemes which we know to be invalid.
	;;
	;; If we wanted some additional complexity, we could maintain
	;; the possibility of not matching a start lexeme in the
	;; visible region, but possibly elsewhere instead; however,
	;; for now, simply assume that the VALID-LEX from above is
	;; definitive.
	(loop until (= (nb-lexemes lexer) valid-lex)
	      do (delete* drei-syntax::lexemes valid-lex)))
      ;; parse up to the limit of validity imposed by the lexer, or
      ;; the bottom of the visible area, whichever comes sooner
      ;;
      ;; This is ugly, but apparently necessary to be able to refer to
      ;; the syntax in question: (syntax (buffer thing)) doesn't work,
      ;; because SYNTAX isn't part of the buffer protocol, and (buffer
      ;; thing) can return a delegating buffer.
      (let ((*this-syntax* syntax))
	(loop until (= valid-parse valid-lex)
	      until (and (<= end (start-offset (lexeme lexer (1- valid-parse))))
			 (typep (lexeme lexer (1- valid-parse)) 'end-lexeme))
	      do (let ((current-token (lexeme lexer (1- valid-parse)))
		       (next-lexeme (lexeme lexer valid-parse)))
		   (setf (slot-value next-lexeme 'state)
			 (advance-parse parser (list next-lexeme) 
					(slot-value current-token 'state)))
		   (incf valid-parse))))
      (let ((scan (make-buffer-mark (buffer syntax) 0 :left)))
	(setf (offset scan) (end-offset (lexeme lexer (1- valid-parse))))
	(skip-inter-lexeme-objects lexer scan)
	(values 0 (offset scan))))))

;;; display
(defclass pump-state ()
  ((drawing-options :initarg :drawing-options :accessor drawing-options)
   (lexeme-index :initarg :lexeme-index :accessor lexeme-index)
   (offset :initarg :offset :accessor pump-state-offset)))
(defmethod print-object ((o pump-state) s)
  (print-unreadable-object (o s :type t)
    (with-slots (lexeme-index offset) o
      (format s "~S ~S" lexeme-index offset))))

(defun make-pump-state (drawing-options lexeme-index offset)
  (make-instance 'pump-state :drawing-options drawing-options 
		 :lexeme-index lexeme-index :offset offset))

(defun %lexeme-index-before-offset (syntax offset)
  (update-parse syntax 0 offset)
  (with-slots (drei-syntax::lexemes valid-lex)
      (lexer syntax)
    ;; FIXME: speed this up.
    (do* ((i (1- valid-lex) (1- i))
	  (lexeme #1=(element* drei-syntax::lexemes i) #1#)
	  (start #2=(start-offset lexeme) #2#))
	 ((<= start offset) i))))

(defun %drawing-options-for-lexeme-index (syntax index)
  (with-slots (drei-syntax::lexemes)
      (lexer syntax)
    (typecase (element* drei-syntax::lexemes index)
      (comment-lexeme *comment-drawing-options*)
      (char-code-list-lexeme *string-drawing-options*)
      (variable-lexeme *special-variable-drawing-options*)
      (t +default-drawing-options+))))

(defmethod pump-state-for-offset-with-syntax 
    ((view textual-drei-syntax-view) (syntax prolog-syntax) (offset cl:integer))
  (let ((index (%lexeme-index-before-offset syntax offset)))
    (make-pump-state (%drawing-options-for-lexeme-index syntax index) index offset)))

(defmethod stroke-pump-with-syntax 
    ((view textual-drei-syntax-view) (syntax prolog-syntax)
     stroke (pump-state pump-state))
  (with-slots (drei-syntax::lexemes) (lexer syntax)
    (let* ((index (lexeme-index pump-state))
	   (offset (pump-state-offset pump-state))
	   (line (line-containing-offset view offset))
           (line-start-offset (start-offset line))
           (line-end-offset (end-offset line))
	   (lexeme (and index (element* drei-syntax::lexemes index))))
      (cond
	((or 
	  ;; in theory, if INDEX is null
	  (null index)
          ;; or if we're actually past the lexeme
          (> line-start-offset (end-offset lexeme))
	  ;; or somehow before the lexeme 
	  (< line-end-offset (start-offset lexeme)))
         ;; then we have blank space
	 (setf (stroke-start-offset stroke) offset
	       (stroke-end-offset stroke) line-end-offset
	       (stroke-drawing-options stroke) +default-drawing-options+)
	 (setf (pump-state-offset pump-state) (1+ line-end-offset))
	 pump-state)
	((< line-end-offset (end-offset lexeme))
	 (setf (stroke-start-offset stroke) offset
	       (stroke-end-offset stroke) line-end-offset
	       (stroke-drawing-options stroke) (drawing-options pump-state))
	 (setf (pump-state-offset pump-state) (1+ line-end-offset))
	 pump-state)
	(t
	 ;; before deciding what happens next, we need to ensure that
	 ;; we have given the parser a chance to lex and parse beyond
	 ;; the last lexeme.
	 (when (= (1+ index) (slot-value (lexer syntax) 'valid-lex))
	   (let ((next (min (size (buffer syntax))
			    (1+ (drei::prefix-size view)))))
	     (update-parse syntax 0 next)))
	 (cond
	   ((< (1+ index) (nb-lexemes (lexer syntax)))
	    (let* ((new-index (1+ index))
		   (new-lexeme (lexeme (lexer syntax) new-index))
		   (end-offset (min (start-offset new-lexeme) 
				    line-end-offset)))
	      (setf (stroke-start-offset stroke) offset
		    (stroke-end-offset stroke) end-offset
		    (stroke-drawing-options stroke) (drawing-options pump-state))
	      (setf (pump-state-offset pump-state) (if (= end-offset line-end-offset)
						       (1+ end-offset)
						       end-offset)
		    (drawing-options pump-state) (%drawing-options-for-lexeme-index syntax new-index)
		    (lexeme-index pump-state) new-index))
	    pump-state)
	   (t 
	    (let ((end-offset (end-offset lexeme)))
	      (setf (stroke-start-offset stroke) offset
		    (stroke-end-offset stroke) end-offset
		    (stroke-drawing-options stroke) (drawing-options pump-state))
	      (setf (pump-state-offset pump-state) (if (= end-offset line-end-offset)
						       (1+ end-offset)
						       end-offset)
		    (drawing-options pump-state) +default-drawing-options+
		    (lexeme-index pump-state) nil)
	      pump-state))))))))

#|
(climacs-gui::define-named-command com-inspect-lex ()
  (with-slots (lexer) (slot-value (buffer (climacs-gui::current-window)) 'drei-syntax::syntax)
    (let ((*standard-input* *query-io*)
	  (*standard-output* *query-io*))
      (inspect lexer))))

(climacs-gui::define-named-command com-inspect-parse ()
  (with-slots (parser) (slot-value (buffer (climacs-gui::current-window)) 'drei-syntax::syntax)
    (let ((*standard-input* *query-io*)
	  (*standard-output* *query-io*))
      (inspect parser))))
|#
