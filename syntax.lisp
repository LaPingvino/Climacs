;;; -*- Mode: Lisp; Package: CLIMACS-BUFFER -*-

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

(in-package :climacs-syntax)

(defclass syntax (name-mixin)
  ((buffer :initarg :buffer)))

(defgeneric update-syntax (buffer syntax))

(defgeneric syntax-line-indentation (mark tab-width syntax)
  (:documentation "Return the correct indentation for the line containing
the mark, according to the specified syntax."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax completion

(defparameter *syntaxes* '())

(defmacro define-syntax (class-name (name superclasses) &body body)
  `(progn (push '(,name . ,class-name) *syntaxes*)
	  (defclass ,class-name ,superclasses
	       ,@body
	    (:default-initargs :name ,name))))

(define-presentation-method accept
    ((type syntax) stream (view textual-view) &key)
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far *syntaxes* '() :action action
			 :name-key #'car
			 :value-key #'cdr))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success string))
    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic syntax

(define-syntax basic-syntax ("Basic" (syntax))
  ())

(defmethod update-syntax (buffer (syntax basic-syntax))
  (declare (ignore buffer))
  nil)

(defmethod syntax-line-indentation (mark tab-width (syntax basic-syntax))
  (declare (ignore mark tab-width))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Incremental Earley parser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar

(defclass rule ()
  ((left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (right-hand-side :initarg :right-hand-side :reader right-hand-side)
   (symbols :initarg :symbols :reader symbols)))

(defclass grammar ()
  ((rules :initarg :rules :reader rules)))

(defmacro grammar (&body body)
  (labels ((var-of (arg)
	     (if (symbolp arg)
		 arg
		 (car arg)))
	   (sym-of (arg)
	     (cond ((symbolp arg) arg)
		   ((= (length arg) 3) (cadr arg))
		   ((symbolp (cadr arg)) (cadr arg))
		   (t (car arg))))
	   (test-of (arg)
	     (cond ((symbolp arg) t)
		   ((= (length arg) 3) (caddr arg))
		   ((symbolp (cadr arg)) t)
		   (t (cadr arg))))
	   (build-rule (arglist body)
	       (if (null arglist)
		   body
		   (let ((arg (car arglist)))
		     `(lambda (,(var-of arg))
			(when (and (typep ,(var-of arg) ',(sym-of arg))
				   ,(test-of arg))
			  ,(build-rule (cdr arglist) body))))))
	   (make-rule (rule)
	     `(make-instance 'rule
		 :left-hand-side ',(car rule)
		 :right-hand-side
		 ,(build-rule (caddr rule)
			      (if (or (= (length rule) 3)
				      (symbolp (cadddr rule)))
				  `(make-instance ',(car rule) ,@(cdddr rule))
				  `(progn ,@(cdddr rule))))
		 :symbols ,(coerce (mapcar #'sym-of (caddr rule)) 'vector))))
    `(make-instance 'grammar
	:rules (list ,@(mapcar #'make-rule body)))))					 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defclass parser ()
  ((grammar :initarg :grammar)
   (target :initarg :target :reader target)
   (initial-state :reader initial-state)
   (lexer :initarg :lexer)))

(defclass rule-item () ())

(defclass incomplete-item (rule-item)
  ((rule :initarg :rule :reader rule)
   (dot-position :initarg :dot-position :reader dot-position)
   (parse-trees :initarg :parse-trees :reader parse-trees)
   (suffix :initarg :suffix :reader suffix)))

(defmethod print-object ((item incomplete-item) stream)
  (format stream "[~a ->" (left-hand-side (rule item)))
  (loop for i from 0 below (dot-position item)
	do (format stream " ~a" (aref (symbols (rule item)) i)))
  (format stream " *")
  (loop for i from (dot-position item) below (length (symbols (rule item)))
	do (format stream " ~a" (aref (symbols (rule item)) i)))
  (format stream "]"))	  

(defclass complete-item (rule-item)
  ((parse-tree :initarg :parse-tree :reader parse-tree)))

(defmethod print-object ((item complete-item) stream)
  (format stream "[~a]" (parse-tree item)))

(defgeneric derive-item (prev-item parse-tree))

(defmethod derive-item ((prev-item incomplete-item) parse-tree)
  (let ((remaining (funcall (suffix prev-item) parse-tree)))
    (cond ((null remaining)
	   nil)
	  ((functionp remaining)
	   (make-instance 'incomplete-item
	      :rule (rule prev-item)
	      :dot-position (1+ (dot-position prev-item))
	      :parse-trees (cons parse-tree (parse-trees prev-item))
	      :suffix remaining))
	  (t
	   (make-instance 'complete-item
	      :parse-tree remaining)))))

(defgeneric item-equal (item1 item2))

(defgeneric parse-tree-equal (tree1 tree2))

(defmethod item-equal ((item1 rule-item) (item2 rule-item))
  nil)

(defmethod item-equal ((item1 incomplete-item) (item2 incomplete-item))
  (and (eq (rule item1) (rule item2))
       (eq (length (parse-trees item1)) (length (parse-trees item2)))
       (every #'parse-tree-equal (parse-trees item1) (parse-trees item2))))

(defmethod parse-tree-equal (tree1 tree2)
  (eq (class-of tree1) (class-of tree2)))

(defgeneric parse-tree-better (tree1 tree2))

(defmethod parse-tree-better (tree1 tree2)
  nil)

(defclass parser-state ()
  ((grammar :initarg :grammar :reader state-grammar)
   (incomplete-items :initform (make-hash-table :test #'eq)
		     :reader incomplete-items)
   (parse-trees :initform (make-hash-table :test #'eq)
		:reader parse-trees)))

(defun map-over-incomplete-items (state fun)
  (maphash (lambda (key incomplete-items)
	     (loop for incomplete-item in incomplete-items
		   do (funcall fun key incomplete-item)))
	   (incomplete-items state)))

(defgeneric handle-item (item orig-state to-state))

(defun potentially-handle-parse-tree (parse-tree from-state to-state)
  (let ((parse-trees (parse-trees to-state)))
    (flet ((handle-parse-tree ()
	     (map-over-incomplete-items from-state
	       (lambda (orig-state incomplete-item)
		 (handle-item (derive-item incomplete-item parse-tree)
			      orig-state to-state)))))
      (cond ((find parse-tree (gethash from-state parse-trees)
		   :test #'parse-tree-better)
	     (setf (gethash from-state parse-trees)
		   (cons parse-tree
			 (remove parse-tree (gethash from-state parse-trees)
				 :test #'parse-tree-better)))
	     (handle-parse-tree))
	    ((find parse-tree (gethash from-state parse-trees)
		   :test (lambda (x y) (or (parse-tree-better y x) (parse-tree-equal y x))))
	     nil)
	    (t (push parse-tree (gethash from-state parse-trees))
	       (handle-parse-tree))))))

(defmethod handle-item ((item (eql nil)) orig-state to-state)
  nil)

(defmethod handle-item ((item incomplete-item) orig-state to-state)
   (cond ((find item (gethash orig-state (incomplete-items to-state))
 	       :test #'item-equal)
	  nil)
 	(t
 	 (push item (gethash orig-state (incomplete-items to-state)))
 	 (loop for rule in (rules (state-grammar to-state))
 	       do (when (let ((sym1 (aref (symbols (rule item)) (dot-position item)))
			      (sym2 (left-hand-side rule)))
			  (or (subtypep sym1 sym2) (subtypep sym2 sym1)))
		    (handle-item (if (functionp (right-hand-side rule))
				     (make-instance 'incomplete-item
					:rule rule
					:dot-position 0
					:parse-trees '()
					:suffix (right-hand-side rule))
				     (make-instance 'complete-item
					:parse-tree (right-hand-side rule)))
				 to-state to-state)))
	 (loop for parse-tree in (gethash to-state (parse-trees to-state))
 	       do (handle-item (derive-item item parse-tree)
			       to-state to-state)))))

(defmethod handle-item ((item complete-item) orig-state to-state)
  (potentially-handle-parse-tree (parse-tree item) orig-state to-state))
	   
(defmethod initialize-instance :after ((parser parser) &rest args)
  (declare (ignore args))
  (with-slots (grammar initial-state) parser
     (setf initial-state (make-instance 'parser-state :grammar grammar))
     (loop for rule in (rules grammar)
	   do (when (let ((sym (left-hand-side rule)))
		      (or (subtypep (target parser) sym)
			  (subtypep sym (target parser))))
		(handle-item (if (functionp (right-hand-side rule))
				 (make-instance 'incomplete-item
				    :rule rule
				    :dot-position 0
				    :parse-trees '()
				    :suffix (right-hand-side rule))
				 (make-instance 'complete-item
				    :parse-tree (right-hand-side rule)))
			     initial-state initial-state)))))

(defun advance-parse (parser tokens state)
  (with-slots (grammar) parser
     (let ((new-state (make-instance 'parser-state :grammar grammar)))
       (loop for token in tokens 
	     do (potentially-handle-parse-tree token state new-state))
       new-state)))

(defclass lexer () ())

(defgeneric lex (lexer))
