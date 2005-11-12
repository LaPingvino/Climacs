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
  ((buffer :initarg :buffer :reader buffer)))

(define-condition no-such-operation (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Operation unavailable for this syntax")))
  (:documentation "This condition is signaled whenever an attempt is
made to execute an operation that is unavailable for the particular syntax" ))

(define-condition no-expression (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "No expression at point")))
  (:documentation "This condition is signaled whenever an attempt is
made to execute a by-experssion motion command and no expression is available." ))

(defgeneric update-syntax (buffer syntax))

(defgeneric update-syntax-for-display (buffer syntax from to))

(defgeneric syntax-line-indentation (mark tab-width syntax)
  (:documentation "Return the correct indentation for the line containing
the mark, according to the specified syntax."))

(defgeneric forward-expression (mark syntax))

(defgeneric backward-expression (mark syntax))

(defgeneric eval-defun (mark syntax))

(defgeneric beginning-of-definition (mark syntax))

(defgeneric end-of-definition (mark syntax))

(defgeneric backward-paragraph (mark syntax))

(defgeneric forward-paragraph (mark syntax))

(defgeneric backward-sentence (mark syntax))

(defgeneric forward-sentence (mark syntax))

(defgeneric forward-list (mark syntax)
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric backward-list (mark syntax)
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric down-list (mark syntax)
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric backward-down-list (mark syntax)
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric backward-up-list (mark syntax)
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric up-list (mark syntax)
  (:method (mark syntax)
    (error 'no-such-operation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commenting

(defgeneric syntax-line-comment-string (syntax)
  (:documentation "string to use at the beginning of a line to 
indicate a line comment"))

(defgeneric line-comment-region (syntax mark1 mark2)
  (:documentation "inset a line comment string at the beginning of 
every line in the region"))

(defmethod line-comment-region (syntax mark1 mark2)
  (when (mark< mark2 mark1)
    (rotatef mark1 mark2))
  (let ((mark (clone-mark mark1)))
    (unless (beginning-of-line-p mark)
      (end-of-line mark)
      (unless (end-of-buffer-p mark)
	(forward-object mark)))
    (loop while (mark< mark mark2)
	  do (insert-sequence mark (syntax-line-comment-string syntax))
	     (end-of-line mark)
	     (unless (end-of-buffer-p mark)
	       (forward-object mark)))))	  

(defgeneric line-uncomment-region (syntax mark1 mark2)
  (:documentation "inset a line comment string at the beginning of 
every line in the region"))

(defmethod line-uncomment-region (syntax mark1 mark2)
  (when (mark< mark2 mark1)
    (rotatef mark1 mark2))
  (let ((mark (clone-mark mark1)))
    (unless (beginning-of-line-p mark)
      (end-of-line mark)
      (unless (end-of-buffer-p mark)
	(forward-object mark)))
    (loop while (mark< mark mark2)
	  do (when (looking-at mark (syntax-line-comment-string syntax))
	       (delete-range mark (length (syntax-line-comment-string syntax))))
	     (end-of-line mark)
	     (unless (end-of-buffer-p mark)
	       (forward-object mark)))))

(defgeneric comment-region (syntax mark1 mark2)
  (:documentation "turn the region between the two marks into a comment
in the specific syntax.")
  (:method (syntax mark1 mark2) nil))

(defgeneric uncomment-region (syntax mark1 mark2)
  (:documentation "remove comment around region")
  (:method (syntax mark1 mark2) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Name for info-pane

(defgeneric name-for-info-pane (syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax completion

(defparameter *syntaxes* '())

(defstruct (syntax-description (:type list))
  (name (error "required argument") :type string)
  (class-name (error "required argument") :type symbol)
  (pathname-types nil :type list))

(defmacro define-syntax (class-name superclasses slots &rest options)
  (let ((defclass-options nil)
	(default-initargs nil)
	(name nil)
        (command-table nil)
	(pathname-types nil))
    (dolist (option options)
      (case (car option)
	((:name)
	 (if name
	     (error "More than one ~S option provided to ~S"
		    ':name 'define-syntax)
	     (setf name (cadr option))))
	((:pathname-types)
	 (if pathname-types
	     (error "More than one ~S option provided to ~S"
		    ':pathname-types 'define-syntax)
	     (setf pathname-types (cdr option))))
        ((:command-table)
         (if command-table
             (error "More than one ~S option provided to ~S"
                    ':command-table 'define-syntax)
             (setf command-table (cadr option))))
	((:default-initargs)
	 (if default-initargs
	     (error "More than one ~S option provided to ~S"
		    ':default-initargs 'define-syntax)
	     (setf default-initargs (cdr option))))
	(t (push (cdr option) defclass-options))))
    (unless name
      (error "~S not supplied to ~S" ':name 'define-syntax))
    ;; FIXME: the :NAME initarg looks, well, a bit generic, and could
    ;; collide with user-defined syntax initargs.  Use
    ;; CLIMACS-SYNTAX::%NAME instead.
    (setf default-initargs (list* :name name default-initargs))
    `(progn
      (push (make-syntax-description
	     :name ,name :class-name ',class-name
	     :pathname-types ',pathname-types)
       *syntaxes*)
      (defclass ,class-name ,superclasses ,slots
	(:default-initargs ,@default-initargs)
	,@defclass-options))))

#+nil
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
			 :name-key #'syntax-description-name
			 :value-key #'syntax-description-class-name))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success string))
    object))

(defun syntax-from-name (syntax)
  (let ((description (find syntax *syntaxes*
			   :key #'syntax-description-name
			   :test #'string-equal)))
    (when description
      (find-class (syntax-description-class-name description)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic syntax

;;; FIXME: this is a really bad name.  It's even worse if it's
;;; case-insensitive.  Emacs' "Fundamental" isn't too bad.
(define-syntax basic-syntax (syntax)
  ()
  (:name "Basic"))

(defmethod update-syntax (buffer (syntax basic-syntax))
  (declare (ignore buffer))
  nil)

(defmethod update-syntax-for-display (buffer (syntax basic-syntax) from to)
  (declare (ignore buffer from to))
  nil)

(defmethod name-for-info-pane ((syntax basic-syntax))
  (name syntax))

(defmethod syntax-line-indentation (mark tab-width (syntax basic-syntax))
  (declare (ignore mark tab-width))
  0)

(defmethod forward-expression (mark syntax)
  (error 'no-such-operation))

(defmethod backward-expression (mark syntax)
  (error 'no-such-operation))

(defmethod eval-defun (mark syntax)
  (error 'no-such-operation))

(defmethod beginning-of-defintion (mark syntax)
  (error 'no-such-operation))

(defmethod end-of-definition (mark syntax)
  (error 'no-such-operation))

(defmethod backward-paragraph (mark syntax)
  (error 'no-such-operation))

(defmethod forward-paragraph (mark syntax)
  (error 'no-such-operation))

(defmethod backward-sentence (mark syntax)
  (error 'no-such-operation))

(defmethod forward-sentence (mark syntax)
  (error 'no-such-operation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Incremental Earley parser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parse tree

(defclass parse-tree ()
  ((start-mark :initform nil :initarg :start-mark :reader start-mark)
   (size :initform nil :initarg :size)))

(defgeneric start-offset (parse-tree))

(defmethod start-offset ((tree parse-tree))
  (let ((mark (start-mark tree)))
    (when mark
      (offset mark))))

(defmethod (setf start-offset) ((offset number) (tree parse-tree))
  (let ((mark (start-mark tree)))
    (assert (not (null mark)))
    (setf (offset mark) offset)))

(defmethod (setf start-offset) ((offset mark) (tree parse-tree))
  (with-slots (start-mark) tree
     (if (null start-mark)
	 (setf start-mark (clone-mark offset))
	 (setf (offset start-mark) (offset offset)))))

(defgeneric end-offset (parse-tree))

(defmethod end-offset ((tree parse-tree))
  (with-slots (start-mark size) tree
     (when start-mark
       (+ (offset start-mark) size))))

(defmethod (setf end-offset) ((offset number) (tree parse-tree))
  (with-slots (start-mark size) tree
     (assert (not (null start-mark)))
     (setf size (- offset (offset start-mark)))))

(defmethod (setf end-offset) ((offset mark) (tree parse-tree))
  (with-slots (start-mark size) tree
     (assert (not (null start-mark)))
     (setf size (- (offset offset) (offset start-mark)))))

(defmethod buffer ((tree parse-tree))
  (let ((start-mark (start-mark tree)))
    (when start-mark
      (buffer start-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass lexer ()
  ((buffer :initarg :buffer :reader buffer)))

(defgeneric nb-lexemes (lexer))
(defgeneric lexeme (lexer pos))
(defgeneric insert-lexeme (lexer pos lexeme))
(defgeneric delete-invalid-lexemes (lexer from to))
(defgeneric inter-lexeme-object-p (lexer object))
(defgeneric skip-inter-lexeme-objects (lexer scan))
(defgeneric update-lex (lexer start-pos end))
(defgeneric next-lexeme (lexer scan))

(defclass incremental-lexer (lexer)
  ((lexemes :initform (make-instance 'standard-flexichain) :reader lexemes)))

(defmethod nb-lexemes ((lexer incremental-lexer))
  (nb-elements (lexemes lexer)))

(defmethod lexeme ((lexer incremental-lexer) pos)
  (element* (lexemes lexer) pos))

(defmethod insert-lexeme ((lexer incremental-lexer) pos lexeme)
  (insert* (lexemes lexer) pos lexeme))

(defmethod delete-invalid-lexemes ((lexer incremental-lexer) from to)
  "delete all lexemes between FROM and TO and return the first invalid 
position in the lexemes of LEXER"
  (with-slots (lexemes) lexer
     (let ((start 1)
	   (end (nb-elements lexemes)))
       ;; use binary search to find the first lexeme to delete
       (loop while (< start end)
	     do (let ((middle (floor (+ start end) 2)))
		  (if (mark< (end-offset (element* lexemes middle)) from)
		      (setf start (1+ middle))
		      (setf end middle))))
       ;; delete lexemes
       (loop until (or (= start (nb-elements lexemes))
		       (mark> (start-mark (element* lexemes start)) to))
	     do (delete* lexemes start))
       start)))
	       
(defmethod skip-inter-lexeme-objects ((lexer incremental-lexer) scan)
  (loop until (end-of-buffer-p scan)
	while (inter-lexeme-object-p lexer (object-after scan))
	do (forward-object scan)))

(defmethod update-lex ((lexer incremental-lexer) start-pos end)
  (let ((scan (clone-mark (low-mark (buffer lexer)) :left)))
    (setf (offset scan)
	  (end-offset (lexeme lexer (1- start-pos))))
    (loop do (skip-inter-lexeme-objects lexer scan)
	  until (if (end-of-buffer-p end)
		    (end-of-buffer-p scan)
		    (mark> scan end))
	  do (let* ((start-mark (clone-mark scan))
		    (lexeme (next-lexeme lexer scan))
		    (size (- (offset scan) (offset start-mark))))
	       (setf (slot-value lexeme 'start-mark) start-mark
		     (slot-value lexeme 'size) size)
	       (insert-lexeme lexer start-pos lexeme))
	     (incf start-pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar

(defclass rule ()
  ((left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (right-hand-side :initarg :right-hand-side :reader right-hand-side)
   (symbols :initarg :symbols :reader symbols)
   (predict-test :initarg :predict-test :reader predict-test)
   (number)))

(defclass grammar ()
  ((rules :initform nil :accessor rules)
   (hash :initform (make-hash-table) :accessor hash)
   (number-of-rules :initform 0)))

(defmacro grammar-rule ((left-hand-side arrow arglist &body body) &key predict-test)
  (declare (ignore arrow))
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
			,(build-rule (cdr arglist) body)))))))
    `(make-instance 'rule
	:left-hand-side ',left-hand-side
	:right-hand-side
	,(build-rule arglist
		     (if (or (null body)
			     (symbolp (car body)))
			 `(make-instance ',left-hand-side ,@body)
			 `(progn ,@body)))
	:symbols ,(coerce (mapcar #'sym-of arglist) 'vector)
	:predict-test ,predict-test)))


(defmacro grammar (&body body)
  (let ((rule (gensym "RULE"))
	(rules (gensym "RULES"))
	(result (gensym "RESULT")))
    `(let* ((,rules (list ,@(loop for rule in body 
				  collect `(grammar-rule ,rule))))
	    (,result (make-instance 'grammar)))
       (dolist (,rule ,rules ,result)
	 (add-rule ,rule ,result)))))

(defgeneric add-rule (rule grammar))

(defmethod add-rule (rule (grammar grammar))
  (push rule (rules grammar))
  (setf (slot-value rule 'number) (slot-value grammar 'number-of-rules))
  (incf (slot-value grammar 'number-of-rules))
  (clrhash (hash grammar))
  (let (rhs-symbols)
    (dolist (rule (rules grammar))
      (setf rhs-symbols (union rhs-symbols (coerce (symbols rule) 'list))))
    (dolist (rule (rules grammar))
      (let ((lhs-symbol (left-hand-side rule)))
	(dolist (rhs-symbol rhs-symbols)
	  (when (or (subtypep lhs-symbol rhs-symbol)
		    (subtypep rhs-symbol lhs-symbol))
	    (pushnew rule (gethash rhs-symbol (hash grammar)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defclass parser ()
  ((grammar :initarg :grammar :reader parser-grammar)
   (target :initarg :target :reader target)
   (initial-state :reader initial-state)))

(defclass rule-item ()
  ((parse-trees :initform '() :initarg :parse-trees :reader parse-trees)))


(defclass incomplete-item (rule-item)
  ((orig-state :initarg :orig-state :reader orig-state)
   (predicted-from :initarg :predicted-from :reader predicted-from)
   (rule :initarg :rule :reader rule)
   (dot-position :initarg :dot-position :reader dot-position)
   (suffix :initarg :suffix :reader suffix)))

(defmethod print-object ((item incomplete-item) stream)
  (format stream "[~a ->" (left-hand-side (rule item)))
  (loop for i from 0 below (dot-position item)
	do (format stream " ~a" (aref (symbols (rule item)) i)))
  (format stream " *")
  (loop for i from (dot-position item) below (length (symbols (rule item)))
	do (format stream " ~a" (aref (symbols (rule item)) i)))
  (format stream "]"))	  

(defun derive-and-handle-item (prev-item parse-tree orig-state to-state)
  (let ((remaining (funcall (suffix prev-item) parse-tree)))
    (cond ((null remaining)
	   nil)
	  ((functionp remaining)
	   (handle-incomplete-item
	    (make-instance 'incomplete-item
	       :orig-state (orig-state prev-item)
	       :predicted-from (predicted-from prev-item)
	       :rule (rule prev-item)
	       :dot-position (1+ (dot-position prev-item))
	       :parse-trees (cons parse-tree (parse-trees prev-item))
	       :suffix remaining)
	    orig-state to-state))
	  (t
	   (let* ((parse-trees (cons parse-tree (parse-trees prev-item)))
		  (start (find-if-not #'null parse-trees
				      :from-end t :key #'start-offset))
		  (end (find-if-not #'null parse-trees :key #'end-offset)))
	     (with-slots (start-mark size) remaining
		(when start
		  (setf start-mark (start-mark start)
			size (- (end-offset end) (start-offset start))))
		(potentially-handle-parse-tree remaining orig-state to-state)))))))

(defun item-equal (item1 item2)
  (declare (optimize speed))
  (and (eq (rule item1) (rule item2))
       (do ((trees1 (parse-trees item1) (cdr trees1))
	    (trees2 (parse-trees item2) (cdr trees2)))
	   ((and (null trees1) (null trees2)) t)
	 (when (or (null trees1) (null trees2))
	   (return nil))
	 (when (not (parse-tree-equal (car trees1) (car trees2)))
	   (return nil)))))

(defun parse-tree-equal (tree1 tree2)
  (eq (class-of tree1) (class-of tree2)))

(defgeneric parse-tree-better (tree1 tree2))

(defmethod parse-tree-better (tree1 tree2)
  nil)

(defclass parser-state ()
  ((parser :initarg :parser :reader parser)
   (incomplete-items :initform (make-hash-table :test #'eq)
		     :reader incomplete-items)
   (parse-trees :initform (make-hash-table :test #'eq)
		:reader parse-trees)
   (last-nonempty-state :initarg :last-nonempty-state :accessor last-nonempty-state)
   (predicted-rules)))

(defmethod initialize-instance :after ((state parser-state) &rest args)
  (declare (ignore args))
  (with-slots (predicted-rules) state
     (setf predicted-rules
	   (make-array (slot-value (parser-grammar (parser state))
				   'number-of-rules)
		       :element-type 'bit
		       :initial-element 0))))

(defun map-over-incomplete-items (state fun)
  (maphash (lambda (key incomplete-items)
	     (loop for incomplete-item in incomplete-items
		   do (funcall fun key incomplete-item)))
	   (incomplete-items state)))

(defun potentially-handle-parse-tree (parse-tree from-state to-state)
  (let ((parse-trees (parse-trees to-state)))
    (flet ((handle-parse-tree ()
	     (map-over-incomplete-items from-state
	       (lambda (orig-state incomplete-item)
		 (derive-and-handle-item incomplete-item parse-tree orig-state to-state)))))
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

(defun predict (item state tokens)
  (dolist (rule (gethash (aref (symbols (rule item)) (dot-position item))
			 (hash (parser-grammar (parser state)))))
    (if (functionp (right-hand-side rule))
	(let ((predicted-rules (slot-value state 'predicted-rules))
	      (rule-number (slot-value rule 'number))
	      (predict-test (predict-test rule)))
	  (when (zerop (sbit predicted-rules rule-number))
	    (setf (sbit predicted-rules rule-number) 1)
	    (when (or (null predict-test)
		      (some predict-test tokens))
	      (handle-and-predict-incomplete-item
	       (make-instance 'incomplete-item
		  :orig-state state
		  :predicted-from item
		  :rule rule
		  :dot-position 0
		  :suffix (right-hand-side rule))
	       state tokens))))
	(potentially-handle-parse-tree (right-hand-side rule) state state)))
  (loop for parse-tree in (gethash state (parse-trees state))
	do (derive-and-handle-item item parse-tree state state)))

(defun handle-incomplete-item (item orig-state to-state)
  (declare (optimize speed))
  (cond ((find item (the list (gethash orig-state (incomplete-items to-state)))
 	       :test #'item-equal)
	  nil)
 	(t
 	 (push item (gethash orig-state (incomplete-items to-state))))))

(defun handle-and-predict-incomplete-item (item state tokens)
  (declare (optimize speed))
  (cond ((find item (the list (gethash state (incomplete-items state)))
 	       :test #'item-equal)
	  nil)
 	(t
 	 (push item (gethash state (incomplete-items state)))
	 (predict item state tokens))))

(defmethod initialize-instance :after ((parser parser) &rest args)
  (declare (ignore args))
  (with-slots (grammar initial-state) parser
     (setf initial-state (make-instance 'parser-state :parser parser))
     (setf (last-nonempty-state initial-state) initial-state)
     (loop for rule in (rules grammar)
	   do (when (let ((sym (left-hand-side rule)))
		      (or (subtypep (target parser) sym)
			  (subtypep sym (target parser))))
		(if (functionp (right-hand-side rule))
		    (let ((predicted-rules (slot-value initial-state 'predicted-rules))
			  (rule-number (slot-value rule 'number))
			  (predict-test (predict-test rule)))
		      (when (zerop (sbit predicted-rules rule-number))
			(setf (sbit predicted-rules rule-number) 1)
			(when (null predict-test)
			  (handle-and-predict-incomplete-item
			   (make-instance 'incomplete-item
					  :orig-state initial-state
					  :predicted-from nil
					  :rule rule
					  :dot-position 0
					  :suffix (right-hand-side rule))
			   initial-state nil))))
		    (potentially-handle-parse-tree
		     (right-hand-side rule) initial-state initial-state))))))

(defun state-contains-target-p (state)
  (loop with target = (target (parser state))
	for parse-tree in (gethash (initial-state (parser state))
				   (parse-trees state))
	when (typep parse-tree target)
	  do (return parse-tree)))

(defun advance-parse (parser tokens state)
  (maphash (lambda (from-state items)
	     (declare (ignore from-state))
	     (dolist (item items)
	       (predict item state tokens)))
	   (incomplete-items state))
  (let ((new-state (make-instance 'parser-state :parser parser)))
    (loop for token in tokens 
	  do (potentially-handle-parse-tree token state new-state))
    (setf (last-nonempty-state new-state)
	  (if (or (plusp (hash-table-count (incomplete-items new-state)))
		  (state-contains-target-p new-state))
	      new-state
	      (last-nonempty-state state)))
    new-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code for analysing parse stack

(defun parse-stack-top (state)
  "for a given state, return the top of the parse stack, or NIL if the parse stack
is empty in that state."
  (when (plusp (hash-table-count (incomplete-items state)))
    (maphash (lambda (state items)
	       (declare (ignore state))
	       (return-from parse-stack-top (car items)))
	     (incomplete-items state))))

(defun target-parse-tree (state)
  "for a given state, return a target parse tree, or NIL if this state does not
represent a complete parse of the target."
  (state-contains-target-p state))

(defun parse-state-empty-p (state)
  (and (null (parse-stack-top state))
       (null (target-parse-tree state))))

(defun parse-stack-next (parse-stack)
  "given a parse stack frame, return the next frame in the stack."
  (assert (not (null parse-stack)))
  (predicted-from parse-stack))

(defun parse-stack-symbol (parse-stack)
  "given a parse stack frame, return the target symbol of the frame."
  (assert (not (null parse-stack)))
  (left-hand-side (rule parse-stack)))

(defun parse-stack-parse-trees (parse-stack)
  "given a parse stack frame, return a list (in the reverse order of
analysis) of the parse trees recognized.  The return value reveals 
internal state of the parser.  Do not alter it!"
  (assert (not (null parse-stack)))
  (parse-trees parse-stack))

(defun map-over-parse-trees (function state)
  (labels ((map-incomplete-item (item)
	     (unless (null (predicted-from item))
	       (map-incomplete-item (predicted-from item)))
	     (loop for parse-tree in (reverse (parse-trees item))
		   do (funcall function parse-tree))))
    (let ((state (last-nonempty-state state)))
      (if (plusp (hash-table-count (incomplete-items state)))
	  (maphash (lambda (state items)
		     (declare (ignore state))
		     (map-incomplete-item (car items))
		     (return-from map-over-parse-trees nil))
		   (incomplete-items state))
	  (funcall function (state-contains-target-p state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display

(defgeneric redisplay-pane-with-syntax (pane syntax current-p))

