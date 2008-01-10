;;; -*- Mode: Lisp; Package: CLIMACS-PROLOG-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Marcus Pearce (m.pearce@gold.ac.uk)

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

(in-package #:climacs-prolog-syntax)

#+nil
(progn
(defclass prolog-buffer (standard-buffer) 
  ((filepath :initform nil :accessor filepath)
   (syntax :accessor syntax)))

(defmethod initialize-instance :after ((buffer prolog-buffer) &rest args)
  (declare (ignore args))
  (with-slots (syntax) buffer
    (setf syntax (make-instance 'prolog-syntax :buffer buffer))))
)

(defvar *loaded-files* nil "List of files loaded by ensure_loaded directive.")

#+nil
(progn
(defun eval-prolog-file (filepath) 
  (setf *loaded-files* nil)
  (let ((*package* 
         (or (find-package :paiprolog) (error "Paiprolog not loaded."))))
    (dolist (e (buffer->paiprolog (find-prolog-file filepath)))
      (when e
        (pprint e)
        (eval e)))))

(defun find-prolog-file (filepath) 
  (let ((buffer (make-instance 'prolog-buffer)))
    (when (probe-file filepath)
      (with-open-file (stream filepath :direction :input)
        (save-buffer-to-stream stream buffer)))
    (setf (filepath buffer) filepath
          (offset (low-mark buffer)) 0
          (offset (high-mark buffer)) (size buffer))
    (update-syntax-for-display buffer (syntax buffer) (low-mark buffer) 
                               (high-mark buffer))
    buffer))
)

(defun view->paiprolog (view)
  (let ((lexemes (drei-syntax::lexemes (lexer (syntax view))))
        (expressions '()))
    (update-parse (syntax view))
    (dotimes (i (flexichain:nb-elements lexemes) (nreverse expressions))
      (let ((lexeme (flexichain:element* lexemes i)))
        (when (typep lexeme 'end-lexeme)
          (with-hash-table-iterator 
              (next-entry (drei-syntax::parse-trees (slot-value lexeme 'state)))
            (loop
             (multiple-value-bind (more from-state items) 
                 (next-entry)
               (declare (ignore from-state))
               (cond ((null more) (return))
                     ((typep (car items) 'clause-prolog-text)
                      (push (prolog->paiprolog (car items)) expressions)
                      (return))
                     ((typep (car items) 'directive-prolog-text)
                      ;; TODO: handle other directives 
                      (let* ((dexpr (cadr (prolog->paiprolog (car items))))
                             (dsym  (car dexpr)))
                        (case (intern (symbol-name dsym) :climacs-prolog-syntax)
                          (ensure-loaded 
                           (unless (member (cadr dexpr) *loaded-files* 
                                           :test #'string=)
                             (dolist (e (view->paiprolog 
                                         (find-prolog-file (cadr dexpr))))
                               (push e expressions))
                             (push (cadr dexpr) *loaded-files*)))
                          (include 
                           (dolist (e (view->paiprolog 
                                       (find-prolog-file (cadr dexpr))))
                             (push e expressions)))))
                      (return))
                     (t nil))))))))))

;;===========================================================================
;; ISO DIRECTIVES 
;;
;; Properties of procedures
;;   :- dynamic(PI).  
;;     PI is a predicate indicator, predicate indicator sequence, or
;;     predicate indicator list. Each procedure identified by PI is
;;     dynamic. The predicates abolish/1, clause/2, asserta/1,
;;     assertz/1 and retract/1 may be applied to these predicates
;;     without raising a permission_error.
;;   :- multifile(PI).
;;     PI is a predicate indicator, predicate indicator sequence, or
;;     predicate indicator list. Each procedure identified by PI may
;;     be defined by clauses that are contained in more than one
;;     Prolog text.
;;   :- discontiguous(PI).
;;     PI is a predicate indicator, predicate indicator sequence, or
;;     predicate indicator list. Each procedure identified by PI may
;;     be defined by clauses which are not consecutive in the Prolog
;;     text.
;;   :- set_prolog_flag(Flag, Value)
;;     The Prolog flag Flag shall have its value set to Value. 
;; 
;; Format and syntax of read-terms
;;   :- op(Priority, OpSpecifier, Operator).
;;     The arguments Priority, OpSpecifier, and Operator are as the
;;     corresponding arguments of the builtin predicate op/3. The effect
;;     on the operator table is to be the same.
;;   :- char_conversion(InChar,OutChar).  The arguments InChar and
;;     OutChar are as for the builtin predicate char_conversion/2,
;;     the effect on the character conversion table is the same.
;;     char_conversion(In_char, Out_char) is true, with the side
;;     effect of adding the pair (In_char, Out_char) to the
;;     character conversion table if In_char is not equal to
;;     Out_char and removing any pair (In_char, _) from the table if
;;     In_char is equal to Out_char. When the flag char_conversion
;;     has the value true, the In_char will be replaced by Out_char
;;     when a term is read using read_term/3.
;; 
;; A goal to be executed after the Prolog text has been prepared for execution
;;   :- initialization(Goal).  
;;     The goal Goal is to be executed immediately after the Prolog
;;     text has been prepared for execution. If there are several
;;     such directives the order in which the goals is executed is
;;     implementation defined.
;; 
;; Another unit of Prolog text to be prepared for execution.
;;   :- include(PrologText).
;;     The Prolog text identified by PrologText is to be textually
;;     included at this point of the current Prolog text.
;;   :- ensure_loaded(PrologText) 
;;     The Prolog text identified by PrologText must be prepared for
;;     execution (exactly once) with the current Prolog text. This
;;     directive is idempotent.
;;
;;   NB PrologText is implementation dependent. 
;;
;;===========================================================================

(defgeneric prolog->paiprolog (prolog-parse-tree))

;;; PROLOG-NONTERMINALs

(defmethod prolog->paiprolog ((n null))
  nil)

(defmethod prolog->paiprolog ((n layout-text))
  nil)

(defmethod prolog->paiprolog ((n empty-prolog-text))
  nil)

(defmethod prolog->paiprolog ((n directive-prolog-text))
  (prolog->paiprolog (directive n)))

(defmethod prolog->paiprolog ((n clause-prolog-text))
  (let ((expr (prolog->paiprolog (clause n))))
    (cond ((null expr) 
           nil)
          ((cl:atom expr)
           (cons (intern-paiprolog "<-") (list expr)))
          ((eq (car expr) (intern-paiprolog "<-"))
           expr)
          (t (list (intern-paiprolog "<-") expr)))))

(defmethod prolog->paiprolog ((n directive))
  (prolog->paiprolog (directive-term n)))

(defmethod prolog->paiprolog ((n directive-term))
  (prolog->paiprolog (term n)))

(defmethod prolog->paiprolog ((n clause))
  (prolog->paiprolog (clause-term n)))

(defmethod prolog->paiprolog ((n clause-term))
  (prolog->paiprolog (term n)))

(defmethod prolog->paiprolog ((n lterm))
  (prolog->paiprolog (term n)))

(defmethod prolog->paiprolog ((n bracketed-term))
  (prolog->paiprolog (term n)))

(defmethod prolog->paiprolog ((n functional-compound-term))
  `(,(functor->paiprolog (prolog->paiprolog (functor n)))
    ,@(prolog->paiprolog (arg-list n))))

(defun functor->paiprolog (functor) 
  "Consumes a string or symbol designating a prolog functor and
returns a symbol designating a paiprolog functor."
  (etypecase functor
    (symbol functor) 
    (string 
     (let ((id (identifier->paiprolog functor)))
       (if id id (read-from-string functor))))))
  
(defmethod prolog->paiprolog ((n atom))
  (prolog->paiprolog (value n)))

(defmethod prolog->paiprolog ((n arg-list))
  (list (prolog->paiprolog (exp n))))

(defmethod prolog->paiprolog ((n arg-list-pair))
  (cons (prolog->paiprolog (exp n)) (prolog->paiprolog (arg-list n))))

(defmethod prolog->paiprolog ((n exp-term))
  (prolog->paiprolog (term n)))

(defmethod prolog->paiprolog ((n exp-atom))
  (prolog->paiprolog (atom n)))

(defmethod prolog->paiprolog ((n variable-term))
  (prolog->paiprolog (name n)))

(defmethod prolog->paiprolog ((n constant-term))
  (let ((value (value n)))
    (typecase value
      (cons (- (prolog->paiprolog (cadr value))))
      (t (prolog->paiprolog value)))))

(defmethod prolog->paiprolog ((n list-compound-term))
  (prolog->paiprolog (items n)))

(defmethod prolog->paiprolog ((n items))
  (list (prolog->paiprolog (exp n))))

(defmethod prolog->paiprolog ((n items-pair))
  (cons (prolog->paiprolog (exp n))
        (prolog->paiprolog (texp n))))

(defmethod prolog->paiprolog ((n items-list))
  (cons (prolog->paiprolog (exp n)) 
        (prolog->paiprolog (tlist n))))

(defmethod prolog->paiprolog ((n binary-operator-compound-lterm))
  (list (prolog->paiprolog (operator n)) 
        (prolog->paiprolog (left n))
        (prolog->paiprolog (right n))))

(defmethod prolog->paiprolog ((n prefix-operator-compound-lterm))
  (list (prolog->paiprolog (operator n)) 
        (prolog->paiprolog (right n))))

(defmethod prolog->paiprolog ((n postfix-operator-compound-lterm))
  (list (prolog->paiprolog (operator n)) 
        (prolog->paiprolog (left n))))

(defmethod prolog->paiprolog ((n binary-operator-compound-term))
  ;; TODO: special-case AND 
  (list (prolog->paiprolog (operator n)) 
        (prolog->paiprolog (left n)) 
        (prolog->paiprolog (right n))))

(defmethod prolog->paiprolog ((n prefix-operator-compound-term))
  (list (prolog->paiprolog (operator n)) 
        (prolog->paiprolog (right n))))

(defmethod prolog->paiprolog ((n postfix-operator-compound-term))
  (list (prolog->paiprolog (operator n)) 
        (prolog->paiprolog (left n))))
  
(defmethod prolog->paiprolog ((n op))
  (prolog->paiprolog (name n))) 

(defmethod prolog->paiprolog ((n empty-list))
  '())

(defmethod prolog->paiprolog ((n char-code-list-compound-term))
  (prolog->paiprolog (ccl n)))

(defmethod prolog->paiprolog ((n curly-compound-term))
  ;; TODO: what is a curly-compound-term? 
  (list (prolog->paiprolog (term n))))

(defmethod prolog->paiprolog ((n curly-brackets))
  ;; TODO: what are curly brackets?  
  (intern-paiprolog "{}"))


(defmethod prolog->paiprolog ((n char-code-list))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n float-number))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n integer))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n name))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n head-tail-separator))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n open-list))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n close-list))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n variable))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n open-ct))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n open))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n close))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n comma))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n end))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n open-curly))
  (prolog->paiprolog (syntactic-lexeme n)))

(defmethod prolog->paiprolog ((n close-curly))
  (prolog->paiprolog (syntactic-lexeme n)))


;;; PROLOG-LEXEMEs 

(defmethod prolog->paiprolog ((l prolog-lexeme))
  ;; {start,end}-lexeme
  ;; comment-lexeme
  ;; error-lexeme
  ;; {open,open-ct,close}-lexeme
  ;; {open,close}-curly-lexeme
  ;; {open,close}-list-lexeme 
  ;; head-tail-separator-lexeme
  nil)

(defmethod prolog->paiprolog ((l char-code-list-lexeme))
  (read-from-string (lexeme-string l)))

;;; Numbers 

(defmethod prolog->paiprolog ((l integer-lexeme)) 
  (read-from-string (lexeme-string l)))

(defmethod prolog->paiprolog ((l float-number-lexeme)) 
  (read-from-string (lexeme-string l)))

;;; VARIABLE-LEXEMEs 

(defmethod prolog->paiprolog ((l named-lexeme)) 
  (intern-paiprolog (concatenate 'string "?" (lexeme-string l))))

(defmethod prolog->paiprolog ((l anonymous-lexeme)) 
  (intern-paiprolog "?"))

;;; NAME-LEXEMEs 

(defmethod prolog->paiprolog ((l comma-lexeme)) 
  (intern-paiprolog "and"))

(defmethod prolog->paiprolog ((l semicolon-lexeme)) 
  (intern-paiprolog "or"))

(defmethod prolog->paiprolog ((l cut-lexeme)) 
  (intern-paiprolog "!"))

(defmethod prolog->paiprolog ((l quoted-lexeme)) 
  (let ((s (lexeme-string l)))
    (subseq s 1 (1- (length s)))))

(defmethod prolog->paiprolog ((l identifier-lexeme)) 
  (intern-paiprolog (substitute #\- #\_ (lexeme-string l))))

(defmethod prolog->paiprolog ((l graphic-lexeme)) 
  (let* ((s (lexeme-string l))
         (id (identifier->paiprolog s)))
    (if id id (intern-paiprolog s))))
    
(defun identifier->paiprolog (id-string)
  (cond ((string= id-string ":-")
         (intern-paiprolog "<-"))
        ((string= id-string ",")
         (intern-paiprolog "and"))
        ((string= id-string ";")
         (intern-paiprolog "or"))
        ((string= id-string "->")
         (intern-paiprolog "if"))
        ((string= id-string "=:=")
         (intern-paiprolog "=:="))
        ((string= id-string "\\+")
         (intern-paiprolog "fail-if"))
        (t nil)))

(defun intern-paiprolog (name)
  (intern (string-upcase name) :paiprolog))

(define-command (com-export-paiprolog :name t :command-table prolog-table) 
    ((pathname 'pathname))
  (let ((expressions (view->paiprolog (current-view))))
    (with-open-file (s pathname :direction :output :if-exists :supersede)
      (dolist (e expressions)
	(prin1 e s)))))
