;;; -*- Mode: Lisp; Package: CLIMACS-HTML-SYNTAX -*-

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

(in-package "CLIMACS-PROLOG-SYNTAX")

(defclass prolog-parse-tree (parse-tree)
  ())

(define-syntax prolog-syntax ("Prolog" (basic-syntax))
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser)))

(defparameter *prolog-grammar* (grammar))

(defmacro define-prolog-rule ((&rest rule) &body body)
  `(add-rule (grammar-rule (,@rule ,@body)) *prolog-grammar*))

(defmethod initialize-instance :after ((syntax prolog-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser lexer buffer) syntax
     (setf parser (make-instance 'parser
		     :grammar *prolog-grammar*
		     :target 'prolog-text))
     (setf lexer (make-instance 'prolog-lexer :buffer (buffer syntax)))
     (let ((m (clone-mark (low-mark buffer) :left))
	   (lexeme (make-instance 'start-lexeme :state (initial-state parser))))
       (setf (offset m) 0)
       (setf (start-offset lexeme) m
	     (end-offset lexeme) 0)
       (insert-lexeme lexer 0 lexeme))))

;;; grammar

(defclass prolog-nonterminal (prolog-parse-tree)
  ())

(defclass prolog-token (prolog-parse-tree)
  ())

(defclass prolog-operator (prolog-token)
  ())

;;; lexer

(defclass prolog-lexeme (prolog-token)
  ((state :initarg :state)))

(defclass start-lexeme (prolog-lexeme) ())

(defgeneric display-parse-tree (entity syntax pane))

(defclass layout-text (prolog-nonterminal)
  ((comment :initarg :comment :accessor comment :initform nil)
   (cont :initarg :cont :accessor cont)))
(defmethod display-parse-tree
    ((entity layout-text) (syntax prolog-syntax) pane)
  (when (comment entity)
    (with-drawing-options (pane :ink (make-rgb-color 0.7 0.0 0.0))
      (display-parse-tree (comment entity) syntax pane)))
  (when (cont entity)
    (display-parse-tree (cont entity) syntax pane)))

(defgeneric syntactic-lexeme (thing))
(defmethod syntactic-lexeme ((lexeme prolog-lexeme))
  lexeme)
(macrolet ((def ((name &optional tokenp) &rest subs)
	     (flet ((f (x) (intern (format nil "~A-LEXEME" x))))
	       `(progn
		  (defclass ,(f name) (prolog-lexeme) ())

                  ,@(when tokenp
                      `((defclass ,name (prolog-nonterminal)
                          ((layout-text :initarg :layout-text :accessor layout-text :initform nil)
                           (syntactic-lexeme :initarg :syntactic-lexeme :accessor syntactic-lexeme)))
                        (defmethod display-parse-tree
                            ((entity ,name) (syntax prolog-syntax) pane)
                          (when (layout-text entity)
                            (display-parse-tree
                             (layout-text entity) syntax pane))
                          (display-parse-tree
                           (syntactic-lexeme entity) syntax pane))
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
  (def (integer t))
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

;;; 6.4.1
(define-prolog-rule (layout-text -> (comment-lexeme layout-text))
  (make-instance 'layout-text :comment comment-lexeme :cont layout-text))
(define-prolog-rule (layout-text -> ())
  (make-instance 'layout-text :cont nil))

(defclass prolog-lexer (incremental-lexer) ())

(defmethod next-lexeme ((lexer prolog-lexer) scan)
  (let ((string (make-array 0 :element-type 'character
			    :fill-pointer 0 :adjustable t)))
    (flet ((fo ()
	     (vector-push-extend (object-after scan) string)
	     (forward-object scan))
           #+nil ; we might need this later for float-number tokens
	   (bo ()
	     (vector-pop string)
	     (backward-object scan)))
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
	       ((digit-char-p object) (fo) (go NUMBER))
	       ((eql object #\") (fo) (go CHAR-CODE-LIST))
	       ((eql object #\()
		(if (or (beginning-of-buffer-p scan)
			(not (member (object-before scan) '(#\Space #\Newline))))
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
		  ((and (string= string ".") (whitespacep (object-after scan)))
		   (return (make-instance 'end-lexeme)))
		  (t (return (make-instance 'graphic-lexeme))))))
	   QUOTED-TOKEN
	     (loop until (end-of-buffer-p scan)
		;; FIXME
		until (eql (object-after scan) #\')
		do (fo))
	     (if (end-of-buffer-p scan)
		 (return (make-instance 'error-lexeme))
                 (progn (fo)
                        (return (make-instance 'quoted-lexeme))))
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
	   NUMBER
	     (loop until (end-of-buffer-p scan)
		while (digit-char-p (object-after scan))
		do (fo))
	     (return (make-instance 'integer-lexeme))
	   CHAR-CODE-LIST
	     (loop until (end-of-buffer-p scan)
		;; FIXME
		until (eql (object-after scan) #\")
		do (fo))
	     (if (end-of-buffer-p scan)
		 (return (make-instance 'error-lexeme))
		 (return (make-instance 'char-code-list-lexeme)))))))))

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
    ((entity empty-prolog-text) (syntax prolog-syntax) pane)
  (declare (ignore pane))
  nil)
(defmethod display-parse-tree
    ((entity clause-prolog-text) (syntax prolog-syntax) pane)
  (display-parse-tree (clause entity) syntax pane)
  (display-parse-tree (text-rest entity) syntax pane))
(defmethod display-parse-tree
    ((entity directive-prolog-text) (syntax prolog-syntax) pane)
  (with-text-face (pane :italic)
    (display-parse-tree (directive entity) syntax pane))
  (display-parse-tree (text-rest entity) syntax pane))

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

(defmethod display-parse-tree ((entity directive) (syntax prolog-syntax) pane)
  (with-text-face (pane :italic)
    (display-parse-tree (directive-term entity) syntax pane))
  (display-parse-tree (end entity) syntax pane))
(defmethod display-parse-tree
    ((entity directive-term) (syntax prolog-syntax) pane)
  (display-parse-tree (term entity) syntax pane))
(defmethod display-parse-tree ((entity clause) (syntax prolog-syntax) pane)
  (display-parse-tree (clause-term entity) syntax pane)
  (display-parse-tree (end entity) syntax pane))
(defmethod display-parse-tree
    ((entity clause-term) (syntax prolog-syntax) pane)
  (display-parse-tree (term entity) syntax pane))

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
    ((entity constant-term) (syntax prolog-syntax) pane)
  (display-parse-tree (value entity) syntax pane))
(defmethod display-parse-tree 
    ((entity variable-term) (syntax prolog-syntax) pane)
  (display-parse-tree (name entity) syntax pane))
(defmethod display-parse-tree 
    ((entity functional-compound-term) (syntax prolog-syntax) pane)
  (with-drawing-options (pane :ink (make-rgb-color 0.9 0 0.9))
    (display-parse-tree (functor entity) syntax pane))
  (display-parse-tree (open-ct entity) syntax pane)
  (display-parse-tree (arg-list entity) syntax pane)
  (display-parse-tree (close entity) syntax pane))
(defmethod display-parse-tree
    ((entity bracketed-term) (syntax prolog-syntax) pane)
  (display-parse-tree (open entity) syntax pane)
  (display-parse-tree (term entity) syntax pane)
  (display-parse-tree (close entity) syntax pane))
(defmethod display-parse-tree
    ((entity binary-operator-compound-term) (syntax prolog-syntax) pane)
  (display-parse-tree (left entity) syntax pane)
  (display-parse-tree (operator entity) syntax pane)
  (display-parse-tree (right entity) syntax pane))
(defmethod display-parse-tree
    ((entity prefix-operator-compound-term) (syntax prolog-syntax) pane)
  (display-parse-tree (operator entity) syntax pane)
  (display-parse-tree (right entity) syntax pane))
(defmethod display-parse-tree
    ((entity postfix-operator-compound-term) (syntax prolog-syntax) pane)
  (display-parse-tree (left entity) syntax pane)
  (display-parse-tree (operator entity) syntax pane))
(defmethod display-parse-tree
    ((entity list-compound-term) (syntax prolog-syntax) pane)
  (with-drawing-options (pane :ink (make-rgb-color 0.0 0.0 0.8))
    (display-parse-tree ([ entity) syntax pane)
    (display-parse-tree (items entity) syntax pane)
    (display-parse-tree (] entity) syntax pane)))
(defmethod display-parse-tree
    ((entity curly-compound-term) (syntax prolog-syntax) pane)
  (display-parse-tree ({ entity) syntax pane)
  (display-parse-tree (term entity) syntax pane)
  (display-parse-tree (} entity) syntax pane))

(defclass atom (prolog-nonterminal)
  ((value :initarg :value :accessor value)))
(defmethod syntactic-lexeme ((thing atom))
  ;; FIXME: wrong for empty-list atom and curly-brackets atom
  (syntactic-lexeme (value thing)))
(defclass empty-list (prolog-nonterminal)
  (([ :initarg :[ :accessor [)
   (] :initarg :] :accessor ])))
(defclass curly-brackets (prolog-nonterminal)
  (({ :initarg :{ :accessor {)
   (} :initarg :} :accessor })))
(defmethod display-parse-tree ((entity atom) (syntax prolog-syntax) pane)
  (display-parse-tree (value entity) syntax pane))
(defmethod display-parse-tree ((entity empty-list) (syntax prolog-syntax) pane)
  (display-parse-tree ([ entity) syntax pane)
  (display-parse-tree (] entity) syntax pane))
(defmethod display-parse-tree
    ((entity curly-brackets) (syntax prolog-syntax) pane)
  (display-parse-tree ({ entity) syntax pane)
  (display-parse-tree (} entity) syntax pane))

(defclass arg-list (prolog-nonterminal)
  ((exp :initarg :exp :accessor exp)))
(defclass arg-list-pair (arg-list)
  ((comma :initarg :comma :accessor comma)
   (arg-list :initarg :arg-list :accessor arg-list)))

(defmethod display-parse-tree ((entity arg-list) (syntax prolog-syntax) pane)
  (display-parse-tree (exp entity) syntax pane))
(defmethod display-parse-tree
    ((entity arg-list-pair) (syntax prolog-syntax) pane)
  (display-parse-tree (exp entity) syntax pane)
  (display-parse-tree (comma entity) syntax pane)
  (display-parse-tree (arg-list entity) syntax pane))

(defclass exp (prolog-nonterminal) ())
(defclass exp-atom (exp)
  ((atom :initarg :atom :accessor atom)))
(defclass exp-term (exp)
  ((term :initarg :term :accessor term)))

(defmethod display-parse-tree ((entity exp-atom) (syntax prolog-syntax) pane)
  (display-parse-tree (atom entity) syntax pane))
(defmethod display-parse-tree ((entity exp-term) (syntax prolog-syntax) pane)
  (display-parse-tree (term entity) syntax pane))

(defclass lterm (term)
  ((term :initarg :term :accessor term)))
(defmethod compound-term-p ((l lterm))
  (compound-term-p (term l)))
(defmethod functor ((l lterm))
  (functor (term l)))
(defmethod arity ((l lterm))
  (arity (term l)))

(defmethod display-parse-tree ((entity lterm) (syntax prolog-syntax) pane)
  (display-parse-tree (term entity) syntax pane))

(defclass op (prolog-nonterminal)
  ((name :initarg :name :accessor name)
   (priority :initarg :priority :accessor priority)
   (specifier :initarg :specifier :accessor specifier)))
(defmethod syntactic-lexeme ((thing op))
  (syntactic-lexeme (name thing)))
(defclass prefix-op (op) ())
(defclass binary-op (op) ())
(defclass postfix-op (op) ())

(defmethod display-parse-tree ((entity op) (syntax prolog-syntax) pane)
  (display-parse-tree (name entity) syntax pane))

(defclass items (prolog-nonterminal)
  ((exp :initarg :exp :accessor exp)))
(defclass items-pair (items)
  ((htsep :initarg :htsep :accessor htsep)
   (texp :initarg :texp :accessor texp)))
(defclass items-list (items)
  ((comma :initarg :comma :accessor comma)
   (tlist :initarg :tlist :accessor tlist)))

(defmethod display-parse-tree ((entity items) (syntax prolog-syntax) pane)
  (display-parse-tree (exp entity) syntax pane))
(defmethod display-parse-tree
    ((entity items-pair) (syntax prolog-syntax) pane)
  (display-parse-tree (exp entity) syntax pane)
  (display-parse-tree (htsep entity) syntax pane)
  (display-parse-tree (texp entity) syntax pane))
(defmethod display-parse-tree
    ((entity items-list) (syntax prolog-syntax) pane)
  (display-parse-tree (exp entity) syntax pane)
  (display-parse-tree (comma entity) syntax pane)
  (display-parse-tree (tlist entity) syntax pane))

;;; 6.2.1
(define-prolog-rule (prolog-text -> (directive prolog-text))
  (make-instance 'directive-prolog-text :directive directive
                 :text-rest prolog-text))
(define-prolog-rule (prolog-text -> (clause prolog-text))
  (make-instance 'clause-prolog-text :clause clause :text-rest prolog-text))
(define-prolog-rule (prolog-text -> ())
  (make-instance 'empty-prolog-text))

;;; 6.2.1.1
(defun term-directive-p (term)
  (and (compound-term-p term)
       (string= (lexeme-string (syntactic-lexeme (functor term))) ":-")
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

;;; 6.3.1.2
(define-prolog-rule (term -> ((atom
                               (string= (lexeme-string (syntactic-lexeme atom))
                                        "-"))
                              integer))
  ;; FIXME: this doesn't really look right.
  (make-instance 'constant-term :priority 0 :value (list atom integer)))

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
(define-prolog-rule (term -> (atom open-ct-lexeme arg-list close))
  (make-instance 'functional-compound-term :priority 0 :functor atom
                 :arg-list arg-list :open-ct open-ct-lexeme :close close))
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
(define-prolog-rule (lterm -> (term))
  (make-instance 'lterm :term term :priority (1+ (priority term))))

(define-prolog-rule (term -> (open (term (<= (priority term) 1201)) close))
  (make-instance 'bracketed-term :priority 0
                 :open open :term term :close close))
(define-prolog-rule (term -> (open-ct-lexeme
                              (term (<= (priority term) 1201))
                              close))
  (make-instance 'bracketed-term :priority 0
                 :open open-ct-lexeme :term term :close close))

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
    (make-instance 'lterm :priority (priority op) :term
                   (make-instance 'binary-operator-compound-term
                                  :left left :operator op :right right))))
(define-prolog-rule (lterm -> ((left lterm)
                               (op (eql (specifier op) :yfx))
                               (right term)))
  (when (and (<= (priority left) (priority op))
             (< (priority right) (priority op)))
    (make-instance 'lterm :priority (priority op) :term
                   (make-instance 'binary-operator-compound-term
                                  :left left :operator op :right right))))
(define-prolog-rule (term -> ((left term)
                              (op (eql (specifier op) :xfy))
                              (right term)))
  (when (and (< (priority left) (priority op))
             (<= (priority right) (priority op)))
    (make-instance 'binary-operator-compound-term :priority (priority op)
                   :left left :operator op :right right)))
(define-prolog-rule (lterm -> (lterm (op (eql (specifier op) :yf))))
  (when (<= (priority lterm) (priority op))
    (make-instance 'lterm :priority (priority op) :term
                   (make-instance 'postfix-operator-compound-term
                                  :left lterm :operator op))))
(define-prolog-rule (lterm -> (term (op (eql (specifier op) :xf))))
  (when (< (priority term) (priority op))
    (make-instance 'lterm :priority (priority op) :term
                   (make-instance 'postfix-operator-compound-term
                                  :left term :operator op))))
(define-prolog-rule (term -> ((op (eql (specifier op) :fy)) term))
  (when (and (or (not (string= (lexeme-string (syntactic-lexeme op)) "-"))
                 (not (numeric-constant-p term)))
             (not (typep (first-lexeme term) 'open-ct-lexeme))
             (<= (priority term) (priority op)))
    (make-instance 'prefix-operator-compound-term
                   :right term :operator op :priority (priority op))))
(define-prolog-rule (lterm -> ((op (eql (specifier op) :fx)) term))
  (when (and (or (not (string= (lexeme-string (syntactic-lexeme op)) "-"))
                 (not (numeric-constant-p term)))
             (not (typep (first-lexeme term) 'open-ct-lexeme))
             (< (priority term) (priority op)))
    (make-instance 'lterm :priority (priority op) :term
                   (make-instance 'prefix-operator-compound-term
                                  :right term :operator op))))

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
  (find (lexeme-string (syntactic-lexeme name))
        (remove-if-not (lambda (x) (member (opspec-specifier x) specifiers))
                       *predefined-operators*)
        :key #'opspec-name :test #'string=))
(defun find-defined-operator (name specifiers)
  (declare (ignore name specifiers))
  nil)
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
  (and (typep thing 'constant-term)
       (let ((value (value thing)))
         (or (typep value 'integer)
             (and (consp value)
                  (typep (car value) 'atom)
                  (typep (cadr value) 'integer))))))
  
(defun first-lexeme (thing)
  ;; FIXME: we'll need to implement this.
  (declare (ignore thing))
  nil)
  
;;; update syntax

(defmethod update-syntax-for-display (buffer (syntax prolog-syntax) top bot)
  (with-slots (parser lexer valid-parse) syntax
     (loop until (= valid-parse (nb-lexemes lexer))
	   while (mark<= (end-offset (lexeme lexer valid-parse)) bot)
	   do (let ((current-token (lexeme lexer (1- valid-parse)))
		    (next-lexeme (lexeme lexer valid-parse)))
		(setf (slot-value next-lexeme 'state)
		      (advance-parse parser (list next-lexeme) (slot-value current-token 'state))))
	      (incf valid-parse))))

(defmethod inter-lexeme-object-p ((lexer prolog-lexer) object)
  (member object '(#\Space #\Newline)))

(defmethod update-syntax (buffer (syntax prolog-syntax))
  (with-slots (lexer valid-parse) syntax
     (let* ((low-mark (low-mark buffer))
	    (high-mark (high-mark buffer)))
       (when (mark<= low-mark high-mark)
	 (let ((first-invalid-position (delete-invalid-lexemes lexer low-mark high-mark)))
	   (setf valid-parse first-invalid-position)
	   (update-lex lexer first-invalid-position high-mark))))))

;;; display

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

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
	       (#\Space (stream-increment-cursor-position
			 pane space-width 0))
	       (#\Tab (let ((x (stream-cursor-position pane)))
			(stream-increment-cursor-position
			 pane (- tab-width (mod x tab-width)) 0))))
	     (incf start))))		    

#+nil
(defmethod display-parse-tree :around ((entity prolog-parse-tree) syntax pane)
  (with-slots (top bot) pane
     (when (and (end-offset entity) (mark> (end-offset entity) top))
       (call-next-method))))

(defmethod display-parse-tree ((entity prolog-token) (syntax prolog-syntax) pane)
  (flet ((cache-test (t1 t2)
	   (and (eq t1 t2)
		#+nil
		(eq (slot-value t1 'ink)
		    (medium-ink (sheet-medium pane)))
		#+nil
		(eq (slot-value t1 'face)
		    (text-style-face (medium-text-style (sheet-medium pane)))))))
#|    (updating-output (pane :unique-id entity
			   :id-test #'eq
			   :cache-value entity
			   :cache-test #'cache-test)|#
      (with-slots (#|ink face|#) entity
	 #+nil
	 (setf ink (medium-ink (sheet-medium pane))
	       face (text-style-face (medium-text-style (sheet-medium pane))))
	 (present (coerce (buffer-sequence (buffer syntax)
					   (start-offset entity)
					   (end-offset entity))
			  'string)
		  'string
		  :stream pane))))

;;; KLUDGE: below this line, this is just s/html/prolog/ on the
;;; definitions in html-syntax.lisp

(defmethod display-parse-tree :before ((entity prolog-token) (syntax prolog-syntax) pane)
  (handle-whitespace pane (buffer pane) *white-space-start* (start-offset entity))
  (setf *white-space-start* (end-offset entity)))

(defgeneric display-parse-stack (symbol stack syntax pane))

(defmethod display-parse-stack (symbol stack (syntax prolog-syntax) pane)
  (let ((next (parse-stack-next stack)))
    (unless (null next)
      (display-parse-stack (parse-stack-symbol next) next syntax pane))
    (loop for parse-tree in (reverse (parse-stack-parse-trees stack))
	  do (display-parse-tree parse-tree syntax pane))))  

(defun display-parse-state (state syntax pane)
  (let ((top (parse-stack-top state)))
    (if (not (null top))
	(display-parse-stack (parse-stack-symbol top) top syntax pane)
	(display-parse-tree (target-parse-tree state) syntax pane))))

(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax prolog-syntax) current-p)
  (with-slots (top bot) pane
     (setf *cursor-positions* (make-array (1+ (number-of-lines-in-region top bot)))
	   *current-line* 0
	   (aref *cursor-positions* 0) (stream-cursor-position pane))
     (with-slots (lexer) syntax
	(let ((average-token-size (max (float (/ (size (buffer pane)) (nb-lexemes lexer)))
				       1.0)))
	  ;; find the last token before bot
	  (let ((end-token-index (max (floor (/ (offset bot) average-token-size)) 1)))
	    ;; go back to a token before bot
	    (loop until (mark<= (end-offset (lexeme lexer (1- end-token-index))) bot)
		  do (decf end-token-index))
	    ;; go forward to the last token before bot
	    (loop until (or (= end-token-index (nb-lexemes lexer))
			    (mark> (start-offset (lexeme lexer end-token-index)) bot))
		  do (incf end-token-index))
	    (let ((start-token-index end-token-index))
	      ;; go back to the first token after top, or until the previous token
	      ;; contains a valid parser state
	      (loop until (or (mark<= (end-offset (lexeme lexer (1- start-token-index))) top)
			      (not (parse-state-empty-p 
				    (slot-value (lexeme lexer (1- start-token-index)) 'state))))
		    do (decf start-token-index))
	      (let ((*white-space-start* (offset top)))
		;; display the parse tree if any
		(unless (parse-state-empty-p (slot-value (lexeme lexer (1- start-token-index)) 'state))
		  (display-parse-state (slot-value (lexeme lexer (1- start-token-index)) 'state)
				       syntax
				       pane))
		;; display the lexemes
		(with-drawing-options (pane :ink +red+)
		  (loop while (< start-token-index end-token-index)
			do (let ((token (lexeme lexer start-token-index)))
			     (display-parse-tree token syntax pane))
			   (incf start-token-index))))))))
     (let* ((cursor-line (number-of-lines-in-region top (point pane)))
	    (height (text-style-height (medium-text-style pane) pane))
	    (cursor-y (+ (* cursor-line (+ height (stream-vertical-spacing pane)))))
	    (cursor-column (column-number (point pane)))
	    (cursor-x (* cursor-column (text-style-width (medium-text-style pane) pane))))
       (updating-output (pane :unique-id -1)
	 (draw-rectangle* pane
			  (1- cursor-x) (- cursor-y (* 0.2 height))
			  (+ cursor-x 2) (+ cursor-y (* 0.8 height))
			  :ink (if current-p +red+ +blue+))))))
