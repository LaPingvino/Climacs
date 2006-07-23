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
;;; Convenience functions and macros:

(defun unlisted (obj)
  (if (listp obj)
      (first obj)
      obj))

(defun listed (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun usable-package (package-designator)
  "Return a usable package based on `package-designator'."
  (or (find-package package-designator)
      *package*))

(defmacro evaluating-interactively (&body body)
  `(handler-case (progn ,@body)
     (end-of-file ()
       (esa:display-message "Unbalanced parentheses in form."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The command table.

(make-command-table 'lisp-table
                    :errorp nil
                    :inherit-from '(climacs-gui::global-climacs-table))

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
   (package-list :accessor package-list
                 :documentation "An alist mapping the end offset
                 of (in-package) forms to a string of the package
                 designator in the form. The list is sorted with
                 the earliest (in-package) forms last (descending
                 offset).")
   (base :accessor base
         :initform 10
         :documentation "The base which numbers in the buffer are
         expected to be in.")
   (option-specified-package :accessor option-specified-package
                             :initform nil
                             :documentation "The package
                             specified in the attribute
                             line (may be overridden
                             by (in-package) forms).")
   (image :accessor image
          :initform nil
          :documentation "An image object (or NIL) that
          determines where and how Lisp code in the buffer of the
          syntax should be run."))
  (:name "Lisp")
  (:pathname-types "lisp" "lsp" "cl")
  (:command-table lisp-table))

(define-option-for-syntax lisp-syntax "Package" (syntax package-name)
  (let ((specified-package (find-package package-name)))
    (setf (option-specified-package syntax) (or specified-package package-name))))

(define-option-for-syntax lisp-syntax "Base" (syntax base)
  (let ((integer-base (parse-integer base :junk-allowed t)))
    (when integer-base
      (setf (base syntax) integer-base))))

(defmethod initialize-instance :after ((syntax lisp-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

(defmethod name-for-info-pane ((syntax lisp-syntax) &key pane)
  (format nil "Lisp~@[:~(~A~)~]"
          (package-name (package-at-mark syntax (point pane)))))

(defgeneric default-image ()
  (:documentation "The default image for when the current syntax
  does not mandate anything itself (for example if it is not a
  Lisp syntax).")
  (:method ()
    t))

(defgeneric get-usable-image (syntax)
  (:documentation "Get usable image object from `syntax'.")
  (:method (syntax)
    (default-image))
  (:method ((syntax lisp-syntax))
    (or (image syntax)
        (default-image))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Swank interface functions:

(defgeneric eval-string-for-climacs (image string package)
  (:documentation "Evaluate `string' in `package'. A single value
is returned: The result of evaluating `string'.")
  (:method (image string package)
    (let ((*package* package))
      (eval-form-for-climacs image (read-from-string string)))))

(defgeneric eval-form-for-climacs (image form)
  (:documentation "Evaluate `string' in `package'. A single value
is returned: The result of evaluating `string'.")
  (:method (image form)
    (declare (ignore image))
    (eval form)))

(defgeneric compile-string-for-climacs (image string package buffer buffer-mark)
  (:documentation "Compile and evaluate `string' in
`package'. Two values are returned: The result of evaluating
`string' and a list of compiler notes. `Buffer' and `buffer-mark'
will be used for hyperlinking the compiler notes to the source
code.")
  (:method (image string package buffer buffer-mark)
    (declare (ignore image string package buffer buffer-mark))
    (error "Backend insufficient for this operation")))

(defgeneric compile-form-for-climacs (image form buffer buffer-mark)
  (:documentation "Compile and evaluate `form', which must be a
valid Lisp form. Two values are returned: The result of
evaluating `string' and a list of compiler notes. `Buffer' and
`buffer-mark' will be used for hyperlinking the compiler notes to
the source code.")
  (:method (image form buffer buffer-mark)
    (compile-string-for-climacs image
                                (write-to-string form)
                                *package* buffer buffer-mark)))

(defgeneric compile-file-for-climacs (image filepath package &optional load-p)
  (:documentation "Compile the file at `filepath' in
`package'. If `load-p' is non-NIL, also load the file at
`filepath'. Two values will be returned: the result of compiling
the file and a list of compiler notes.")
  (:method (image filepath package &optional load-p)
    (declare (ignore image filepath package load-p))
    (error "Backend insufficient for this operation")))

(defgeneric macroexpand-for-climacs (image form &optional full-p)
  (:documentation "Macroexpand `form' and return result.")
  (:method (image form &optional full-p)
    (declare (ignore image))
    (funcall (if full-p
                 #'macroexpand
                 #'macroexpand-1)
             form)))

(defgeneric find-definitions-for-climacs (image symbol)
  (:documentation "Return list of definitions for `symbol'.")
  (:method (image symbol)
    (declare (ignore image symbol))))

(defgeneric get-class-keyword-parameters (image class)
  (:documentation "Get a list of keyword parameters (possibly
along with any default values) that can be used in a
`make-instance' form for `class'.")
  (:method (image class)
    (declare (ignore image class))))

(defgeneric arglist (image symbol)
  (:documentation "Get plain arglist for symbol.")
  (:method (image symbol)
    (declare (ignore image symbol))))

(defgeneric simple-completions (image string default-package)
  (:documentation "Return a list of simple symbol-completions for
`string' in `default-package'.")
  (:method (image string default-package)
    (declare (ignore image string default-package))))

(defgeneric fuzzy-completions (image symbol-name default-package &optional limit)
  (:documentation "Return a list of fuzzy completions for `symbol-name'.")
  (:method (image symbol-name default-package &optional limit)
    (declare (ignore image symbol-name default-package limit))))

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
(defclass dot-lexeme (lisp-lexeme) ())
(defclass form-lexeme (form lisp-lexeme) ())
(defclass incomplete-character-lexeme (form-lexeme incomplete-form-mixin) ())
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
(defclass number-lexeme (form-lexeme) ())
(defclass token-mixin () ())
(defclass complete-token-lexeme (token-mixin form-lexeme) ())
(defclass multiple-escape-start-lexeme (lisp-lexeme) ())
(defclass multiple-escape-end-lexeme (lisp-lexeme) ())
(defclass incomplete-lexeme (lisp-lexeme) ())
(defclass unmatched-right-parenthesis-lexeme (lisp-lexeme) ())

(defmethod skip-inter ((syntax lisp-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
	    do (return nil)
	  until (not (whitespacep syntax (object-after scan)))
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
	(#\) (fo) (make-instance 'unmatched-right-parenthesis-lexeme))
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
		    (make-instance 'incomplete-lexeme))
		   (t
		    (case (object-after scan)
		      (#\@ (fo) (make-instance 'comma-at-lexeme))
		      (#\. (fo) (make-instance 'comma-dot-lexeme))
		      (t (make-instance 'comma-lexeme))))))
	(#\# (fo)
	     (cond ((end-of-buffer-p scan)
		    (make-instance 'incomplete-lexeme))
		   (t 
		    (let ((prefix 0))
		      (loop until (end-of-buffer-p scan)
			    while (digit-char-p (object-after scan))
			    do (setf prefix
				     (+ (* 10 prefix)
					(digit-char-p (object-after scan))))
			       (fo))
		    (if (end-of-buffer-p scan)
			(make-instance 'incomplete-lexeme)
			(case (object-after scan)
			  ((#\Backspace #\Tab #\Newline #\Linefeed 
			    #\Page #\Return #\Space #\))
			   (fo)
			   (make-instance 'error-lexeme))
			  (#\\ (fo)
			       (cond ((end-of-buffer-p scan)
				      (make-instance 'incomplete-character-lexeme))
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
			  ((#\B #\b #\O #\o #\X #\x)
			   (let ((radix
				  (ecase (object-after scan)
				    ((#\B #\b) 2)
				    ((#\O #\o) 8)
				    ((#\X #\x) 16))))
			     (fo)
			     (loop until (end-of-buffer-p scan)
				   while (digit-char-p (object-after scan) radix)
				   do (fo)))
			   (if (and (not (end-of-buffer-p scan))
				    (constituentp (object-after scan)))
			       (make-instance 'error-lexeme)
			       (make-instance 'number-lexeme)))
			  ((#\R #\r)
			   (fo)
			   (cond
			     ((<= 2 prefix 36)
			      (loop until (end-of-buffer-p scan)
				    while (digit-char-p (object-after scan) prefix)
				    do (fo))
			      (if (and (not (end-of-buffer-p scan))
				       (constituentp (object-after scan)))
				  (make-instance 'error-lexeme)
				  (make-instance 'number-lexeme)))
			     (t (make-instance 'error-lexeme))))
			  ;((#\C #\c) )
			  ((#\A #\a) (fo)
			   (make-instance 'array-start-lexeme))
			  ((#\S #\s) (fo)
			   (cond ((and (not (end-of-buffer-p scan))
				       (eql (object-after scan) #\())
				  (fo)
				  (make-instance 'structure-start-lexeme))
				 ((end-of-buffer-p scan)
				  (make-instance 'incomplete-lexeme))
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
			  (t (fo) (make-instance 'undefined-reader-macro-lexeme))))))))
	(#\| (fo) (make-instance 'multiple-escape-start-lexeme))
	(t (cond ((or (constituentp object)
		      (eql object #\\))
                  (lex-token syntax scan))
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
		    (not (whitespacep syntax (object-after scan))))
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

(defun lex-token (syntax scan)
  ;; May need more work. Can recognize symbols and numbers.
  (flet ((fo () (forward-object scan)))
    (let ((could-be-number t)
          sign-seen dot-seen slash-seen nondot-seen)
      (flet ((return-token-or-number-lexeme ()
               (return-from lex-token
                 (if could-be-number
                     (if nondot-seen
                         (make-instance 'number-lexeme)
                         (make-instance 'dot-lexeme))
                     (make-instance 'complete-token-lexeme))))
             (this-object ()
               (object-after scan)))
        (tagbody
         START
           (when (end-of-buffer-p scan)
             (return-token-or-number-lexeme))
           (when (constituentp (object-after scan))
             (when (not (eql (this-object) #\.))
               (setf nondot-seen t))
             (cond ((or (eql (this-object) #\+)
                        (eql (this-object) #\-))
                    (when sign-seen
                      (setf could-be-number nil))
                    (setf sign-seen t))
                   ((eql (this-object) #\.)
                    (when dot-seen
                      (setf could-be-number nil))
                    (setf dot-seen t))
                   ((eql (this-object) #\/)
                    (when slash-seen
                      (setf could-be-number nil))
                    (setf slash-seen t))
                   ;; We obey the base specified in the file when
                   ;; determining whether or not this character is an
                   ;; integer.
                   ((not (digit-char-p (this-object)
                                       (base syntax)))
                    (setf could-be-number nil)))
             (fo)
             (go START))
           (when (eql (object-after scan) #\\)
             (fo)
             (when (end-of-buffer-p scan)
               (return-from lex-token (make-instance 'incomplete-lexeme)))
             (fo)
             (go START))
           (when (eql (object-after scan) #\|)
             (fo)
             (return-from lex-token (make-instance 'multiple-escape-start-lexeme)))
           (return-token-or-number-lexeme))))))

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
	     (return-from lex (make-instance 'incomplete-lexeme)))
	   (fo)
	   (go start))
	 (when (eql (object-after scan) #\|)
	   (incf bars-seen)
	   (fo)
	   (go start))
         (if (evenp bars-seen)
             (unless (whitespacep syntax (object-after scan))
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
;; skip over unmatched right parentheses
(define-new-lisp-state (|initial-state | unmatched-right-parenthesis-lexeme) |initial-state |)

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

;;;;;;;;;;;;;;;; Cons cell
;; Also (foo bar baz . quux) constructs.
;; (foo bar . baz quux) flagged as an error (too aggressively?).

;;; parse trees
(defclass cons-cell-form (form) ())
(defclass complete-cons-cell-form (cons-cell-form complete-list-form) ())
(defclass incomplete-cons-cell-form (cons-cell-form incomplete-list-form) ())

(define-parser-state |( form* dot-lexeme |
    (lexer-list-state form-may-follow) ())
(define-parser-state |( form* dot-lexeme form |
    (lexer-list-state form-may-follow) ())
(define-parser-state |( form* dot-lexeme form ) |
    (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (|( form* | dot-lexeme)
  |( form* dot-lexeme |)
(define-new-lisp-state (|( form* dot-lexeme | form)
  |( form* dot-lexeme form |)
(define-new-lisp-state (|( form* dot-lexeme | comment)
  |( form* dot-lexeme |)
(define-new-lisp-state (|( form* dot-lexeme form | right-parenthesis-lexeme)
  |( form* dot-lexeme form ) |)
(define-new-lisp-state (|( form* dot-lexeme form | comment)
  |( form* dot-lexeme form |)

(define-lisp-action (|( form* dot-lexeme form ) | t)
  (reduce-until-type complete-cons-cell-form left-parenthesis-lexeme))

;;; Reduce at end of buffer.
(define-lisp-action (|( form* dot-lexeme | (eql nil))
  (reduce-until-type incomplete-cons-cell-form left-parenthesis-lexeme))
(define-lisp-action (|( form* dot-lexeme form | (eql nil))
  (reduce-until-type incomplete-cons-cell-form left-parenthesis-lexeme))

;;;;;;;;;;;;;;;; Simple Vector

;;; parse trees
(defclass simple-vector-form (list-form) ())
(defclass complete-simple-vector-form (complete-list-form simple-vector-form) ())
(defclass incomplete-simple-vector-form (incomplete-list-form simple-vector-form) ())

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
(defclass reader-conditional-form (form) ())
(defclass reader-conditional-positive-form (reader-conditional-form) ())
(defclass reader-conditional-negative-form (reader-conditional-form) ())

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
  (reduce-until-type array-form array-start-lexeme))

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
	((<= (start-offset (car parse-trees)) offset)
	 (loop with tree = (next-tree (car parse-trees))
	       until (or (null tree) (> (start-offset tree) offset))
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

(defun package-at-mark (syntax mark-or-offset)
  "Get the specified Lisp package for the syntax. First, an
attempt will be made to find the package specified in
the (in-package) preceding `mark-or-offset'. If none can be
found, return the package specified in the attribute list. If no
package can be found at all, or the otherwise found packages are
invalid, return the CLIM-USER package."
  (as-offsets ((mark-or-offset offset))
   (let* ((designator (rest (find offset (package-list syntax)
                                  :key #'first
                                  :test #'>=))))
     (or (handler-case (find-package designator)
           (type-error ()
             nil))
         (find-package (option-specified-package syntax))
         (find-package :clim-user)))))

(defmacro with-syntax-package (syntax offset (package-sym) &body
                               body)
  "Evaluate `body' with `package-sym' bound to a valid package,
  preferably taken from `syntax' based on `offset'.."
  `(let ((,package-sym (package-at-mark ,syntax ,offset)))
     ,@body))

(defun need-to-update-package-list-p (buffer syntax)
  (let ((low-mark-offset (offset (low-mark buffer)))
        (high-mark-offset (offset (high-mark buffer))))
    (flet ((test (x)
             (let ((start-offset (start-offset x))
                   (end-offset (end-offset x)))
              (when (and (or (<= start-offset
                                 low-mark-offset
                                 end-offset
                                 high-mark-offset)
                             (<= low-mark-offset
                                 start-offset
                                 high-mark-offset
                                 end-offset)
                             (<= low-mark-offset
                                 start-offset
                                 end-offset
                                 high-mark-offset)
                             (<= start-offset
                                 low-mark-offset
                                 high-mark-offset
                                 end-offset))
                         (typep x 'complete-list-form))
                (let ((candidate (first-form (children x))))
                  (and (typep candidate 'token-mixin)
                       (eq (token-to-object syntax candidate
                                            :no-error t)
                           'cl:in-package)))))))
      (with-slots (stack-top) syntax
        (or (not (slot-boundp syntax 'package-list))
            (loop for child in (children stack-top)
               when (test child)
               do (return t)))))))

(defun update-package-list (buffer syntax)
  (declare (ignore buffer))
  (setf (package-list syntax) nil)
  (flet ((test (x)
           (when (typep x 'complete-list-form)
             (let ((candidate (first-form (children x))))
               (and (typep candidate 'token-mixin)
                    (eq (token-to-object syntax candidate
                                         :no-error t)
                        'cl:in-package)))))
         (extract (x)
           (let ((designator (second-form (children x))))
             (token-to-object syntax designator
                              :no-error t))))
    (with-slots (stack-top) syntax
      (loop for child in (children stack-top)
         when (test child)
         do (push (cons (end-offset child)
                        (extract child))
                  (package-list syntax))))))

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
  (when (need-to-update-package-list-p buffer syntax)
    (update-package-list buffer syntax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; accessing parser forms

(defun first-noncomment (list)
  "Returns the first non-comment in list."
  (find-if-not #'(lambda (item) (typep item 'comment)) list))

(defun rest-noncomments (list)
  "Returns the remainder of the list after the first non-comment,
stripping leading comments."
  (loop for rest on list
	count (not (typep (car rest) 'comment))
	  into forms
	until (= forms 2)
	finally (return rest)))

(defun nth-noncomment (n list)
  "Returns the nth non-comment in list."
  (loop for item in list
	count (not (typep item 'comment))
	  into forms
	until (> forms n)
	finally (return item)))

(defun elt-noncomment (list n)
  "Returns the nth non-comment in list."
  (nth-noncomment n list))

(defun second-noncomment (list)
  "Returns the second non-comment in list."
  (nth-noncomment 1 list))

(defun third-noncomment (list)
  "Returns the third non-comment in list."
  (nth-noncomment 2 list))

(defun rest-forms (list)
  "Returns the remainder of the list after the first form,
stripping leading non-forms."
  (loop for rest on list
     count (typep (car rest) 'form)
       into forms
     until (= forms 2)
     finally (return rest)))

(defun nth-form (n list)
  "Returns the nth form in list or `nil'."
  (loop for item in list
     count (typep item 'form)
       into forms
     until (> forms n)
     finally (when (> forms n)
               (return item))))

(defun elt-form (list n)
  "Returns the nth form in list or `nil'."
  (nth-form n list))

(defun first-form (list)
  "Returns the first form in list."
  (nth-form 0 list))

(defun second-form (list)
  "Returns the second form in list."
  (nth-form 1 list))

(defun third-form (list)
  "Returns the third formw in list."
  (nth-form 2 list))

(defgeneric form-operator (form syntax)
  (:documentation "Return the operator of `form' as a Lisp
object. Returns nil if none can be found.")
  (:method (form syntax) nil))

(defmethod form-operator ((form list-form) syntax)
  (let* ((operator-token (first-form (rest (children form))))
         (operator-symbol (when operator-token
                            (token-to-object syntax operator-token :no-error t))))
    operator-symbol))

(defgeneric form-operands (form syntax)
  (:documentation "Returns the operands of `form' as a list of
  Lisp objects. Returns nil if none can be found.")
  (:method (form syntax) nil))

(defmethod form-operands ((form list-form) syntax)
  (mapcar #'(lambda (operand)
              (if (typep operand 'form)
                  (token-to-object syntax operand :no-error t)))
          (rest-forms (children form))))

(defun form-toplevel (form syntax)
  "Return the top-level form of `form'."
  (if (null (parent (parent form)))
      form
      (form-toplevel (parent form) syntax)))

(defgeneric operator-p (token syntax)
  (:documentation "Return true if `token' is the operator of its form. Otherwise,
  return nil.")
  (:method (token syntax)
    (with-accessors ((pre-token preceding-parse-tree)) token
      (cond ((typep pre-token 'left-parenthesis-lexeme)
             t)
            ((typep pre-token 'comment)
             (operator-p pre-token syntax))
            (t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful functions for selecting forms based on the mark.

(defun expression-at-mark (mark-or-offset syntax)
  "Return the form at `mark-or-offset'. If `mark-or-offset' is just after,
or inside, a top-level-form, or if there are no forms after
`mark-or-offset', the form preceding `mark-or-offset' is
returned. Otherwise, the form following `mark-or-offset' is
returned."
  (as-offsets ((mark-or-offset offset))
   (or (form-around syntax offset)
       (form-after syntax offset)
       (form-before syntax offset))))

(defun definition-at-mark (mark-or-offset syntax)
  "Return the top-level form at `mark-or-offset'. If `mark-or-offset' is just after,
or inside, a top-level-form, or if there are no forms after
`mark-or-offset', the top-level-form preceding `mark-or-offset'
is returned. Otherwise, the top-level-form following
`mark-or-offset' is returned."
  (form-toplevel (expression-at-mark mark-or-offset syntax) syntax))

(defun symbol-at-mark (mark-or-offset syntax)
  "Return a symbol token at `mark-or-offset'. This function will
  \"unwrap\" quote-forms in order to return the symbol token. If
  no symbol token can be found, NIL will be returned."
  (labels ((unwrap-form (form)
             (cond ((typep form 'quote-form)
                    (unwrap-form (first-form (children form))))
                   ((typep form 'complete-token-lexeme)
                    form))))
    (unwrap-form (expression-at-mark mark-or-offset syntax))))

(defun this-form (mark-or-offset syntax)
  "Return a form at `mark-or-offset'. This function defines which
  forms the COM-FOO-this commands affect."
  (as-offsets ((mark-or-offset offset))
    (or (form-around syntax offset)
        (form-before syntax offset))))

(defun preceding-form (mark-or-offset syntax)
  "Return a form at `mark-or-offset'."
  (as-offsets ((mark-or-offset offset))
   (or (form-before syntax offset)
       (form-around syntax offset))))

(defun text-of-definition-at-mark (mark syntax)
  "Return the text of the definition at mark."
  (let ((definition (definition-at-mark mark syntax)))
    (buffer-substring (buffer mark)
                      (start-offset definition)           
                      (end-offset definition))))
                      
(defun text-of-expression-at-mark (mark-or-offset syntax)
  "Return the text of the expression at `mark-or-offset'."
  (let ((expression (expression-at-mark mark-or-offset syntax)))
    (token-string syntax expression)))

(defun symbol-name-at-mark (mark-or-offset syntax)
  "Return the text of the symbol at `mark-or-offset'."
  (let ((token (symbol-at-mark mark-or-offset syntax)))
    (when token (token-string syntax token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful functions for modifying forms based on the mark.

(defun replace-symbol-at-mark (mark syntax string)
  "Replace the symbol at `mark' with `string' and move `mark' to
after `string'."
  (let ((token (symbol-at-mark mark syntax)))
    (setf (offset mark) (start-offset token))
    (forward-kill-expression mark syntax)
    (insert-sequence mark string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

(defparameter *standard-faces*
	      `((:error ,+red+ nil)
		(:string ,+rosy-brown+ ,(make-text-style nil :italic nil))
		(:keyword ,+orchid+ nil)
                (:macro ,+purple+ nil)
                (:special-form ,+purple+ nil)
		(:lambda-list-keyword ,+dark-green+ nil)
		(:comment ,+maroon+ nil)
		(:reader-conditional ,+gray50+ nil)))

(defparameter *reader-conditional-faces*
	      `((:error ,+red+ nil)
		(:string ,+gray50+ ,(make-text-style nil :italic nil))
		(:keyword ,+gray50+ nil)
                (:macro ,+gray50+ nil)
                (:special-form ,+gray50+ nil)
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
       do (case (buffer-object buffer start)
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
  (with-slots (top bot) pane
    (loop for child in (children parse-symbol)
       when (and (start-offset child) 
                 (mark< (start-offset child) bot)
                 (mark> (end-offset child) top))
       do (display-parse-tree child syntax pane))))

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

(defmethod display-parse-tree ((parse-symbol unmatched-right-parenthesis-lexeme)
			       (syntax lisp-syntax) pane)
  (with-face (:error)
    (call-next-method)))

(define-presentation-type unknown-symbol () :inherit-from 'symbol
                          :description "unknown symbol")

(define-presentation-method presentation-typep (object (type unknown-symbol))
  (or (symbolp object) (stringp object)))

(defmethod display-parse-tree ((parse-symbol token-mixin) (syntax lisp-syntax) pane)
  (if (> (the fixnum (end-offset parse-symbol)) (the fixnum (start-offset parse-symbol)))
      (let ((string (token-string syntax parse-symbol)))
        (multiple-value-bind (symbol status)
            (token-to-object syntax parse-symbol :no-error t)
          (with-output-as-presentation
              (pane (if status symbol string) (if status 'symbol 'unknown-symbol)
               :single-box :highlighting)
            (cond ((eql (buffer-object (buffer syntax) (start-offset parse-symbol)) #\:)
                   (with-face (:keyword)
                     (call-next-method)))
                  ((eql (buffer-object (buffer syntax) (start-offset parse-symbol)) #\&)
                   (with-face (:lambda-list-keyword)
                     (call-next-method)))
                  ((and (macro-function symbol)
                        (operator-p parse-symbol syntax))
                   (with-face (:macro)
                     (call-next-method)))
                  ((and (special-operator-p symbol)
                        (operator-p parse-symbol syntax))
                   (with-face (:special-form)
                     (call-next-method)))
                  (t (call-next-method))))))
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
	(let ((string (token-string syntax parser-symbol)))
          (present string 'string :stream pane))))))
          
(defmethod display-parse-tree :before ((parse-symbol lisp-lexeme) (syntax lisp-syntax) pane)
  (handle-whitespace pane (buffer pane) *white-space-start* (start-offset parse-symbol))
  (setf *white-space-start* (end-offset parse-symbol)))

(define-presentation-type lisp-string () 
                          :description "lisp string")

(defmethod display-parse-tree ((parse-symbol complete-string-form) (syntax lisp-syntax) pane)
  (let ((children (children parse-symbol)))
    (if (third children)
        (let ((string (buffer-substring (buffer syntax)
                                        (start-offset (second children))
                                        (end-offset (car (last children 2))))))
          (with-output-as-presentation (pane string 'lisp-string
                                             :single-box :highlighting)
            (with-face (:string)
              (display-parse-tree (pop children) syntax pane)
	      (loop until (null (cdr children))
                 do (display-parse-tree (pop children) syntax pane))
              (display-parse-tree (pop children) syntax pane))))
        (with-face (:string)
         (progn (display-parse-tree (pop children) syntax pane)
                (display-parse-tree (pop children) syntax pane))))))

(defmethod display-parse-tree ((parse-symbol incomplete-string-form) (syntax lisp-syntax) pane)
  (let ((children (children parse-symbol)))
    (if (second children)
        (let ((string (buffer-substring (buffer syntax)
                                        (start-offset (second children))
                                        (end-offset (car (last children))))))
          (with-output-as-presentation (pane string 'lisp-string
                                             :single-box :highlighting)
            (with-face (:string)
              (display-parse-tree (pop children) syntax pane)
              (loop until (null children)
                 do (display-parse-tree (pop children) syntax pane)))))
        (with-face (:string)
         (display-parse-tree (pop children) syntax pane)))))

(defmethod display-parse-tree ((parse-symbol line-comment-form) (syntax lisp-syntax) pane)
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol long-comment-form) (syntax lisp-syntax) pane)
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol reader-conditional-positive-form)
			       (syntax lisp-syntax) pane)
  (let ((conditional (second-noncomment (children parse-symbol))))
    (if (eval-feature-conditional conditional syntax)
	(call-next-method)
	(let ((*current-faces* *reader-conditional-faces*))
	  (with-face (:reader-conditional)
	    (call-next-method))))))

(defmethod display-parse-tree ((parse-symbol reader-conditional-negative-form)
				(syntax lisp-syntax) pane)
  (let ((conditional (second-noncomment (children parse-symbol))))
    (if (eval-feature-conditional conditional syntax)
	(let ((*current-faces* *reader-conditional-faces*))
	  (with-face (:reader-conditional)
	    (call-next-method)))
	(call-next-method))))

(defgeneric eval-feature-conditional (conditional-form syntax))

(defmethod eval-feature-conditional (conditional-form (syntax lisp-syntax))
  nil)

;; Adapted from slime.el

(defconstant +keyword-package+ (find-package :keyword)
  "The KEYWORD package.")

(defmethod eval-feature-conditional ((conditional token-mixin) (syntax lisp-syntax))
  (let* ((string (token-string syntax conditional))
	 (symbol (parse-symbol string :package +keyword-package+)))
    (member symbol *features*)))

(defmethod eval-feature-conditional ((conditional list-form) (syntax lisp-syntax))
  (let ((children (children conditional)))
    (when (third-noncomment children)
      (flet ((eval-fc (conditional)
	       (funcall #'eval-feature-conditional conditional syntax)))
	(let* ((type (second-noncomment children))
	       (conditionals  (butlast
			       (nthcdr
				2
				(remove-if
				 #'(lambda (child) (typep child 'comment))
				 children))))
	       (type-string (token-string syntax type))
	       (type-symbol (parse-symbol type-string :package +keyword-package+)))
	  (case type-symbol
	    (:and (funcall #'every #'eval-fc conditionals))
	    (:or (funcall #'some #'eval-fc conditionals))
	    (:not (when conditionals
		    (funcall #'(lambda (f l) (not (apply f l)))
			     #'eval-fc conditionals)))))))))
	  
(defmethod display-parse-tree ((parse-symbol complete-list-form) (syntax lisp-syntax) pane)
  (let* ((children (children parse-symbol))
         (point-offset (the fixnum (offset (point pane))))
         ;; The following is set to true if the location if the point
         ;; warrants highlighting of a set of matching parentheses.
         (should-highlight (or (= (the fixnum (end-offset parse-symbol)) point-offset)
                               (= (the fixnum (start-offset parse-symbol)) point-offset))))
    (if should-highlight
        (with-text-face (pane :bold)
          (display-parse-tree (car children) syntax pane))
        (display-parse-tree (car children) syntax pane))
    (loop for child-list on (cdr children)
       if (and should-highlight (null (cdr child-list))) do
       (with-text-face (pane :bold)
         (display-parse-tree (car child-list) syntax pane))
       else do
       (display-parse-tree (car child-list) syntax pane))))

(defmethod display-parse-tree ((parse-symbol incomplete-list-form) (syntax lisp-syntax) pane)
  (let* ((children (children parse-symbol))
         (point-offset (the fixnum (offset (point pane))))
         ;; The following is set to true if the location if the point
         ;; warrants highlighting of the beginning parenthesis
         (should-highlight (= (the fixnum (start-offset parse-symbol)) point-offset)))
    (with-face (:error)
      (if should-highlight
          (with-text-face (pane :bold)
            (display-parse-tree (car children) syntax pane))
          (display-parse-tree (car children) syntax pane)))
    (loop for child in (cdr children) do
      (display-parse-tree child syntax pane))))
    
(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax lisp-syntax) current-p)
  (with-slots (top bot) pane
     (setf *cursor-positions* (make-array (1+ (number-of-lines-in-region top bot)))
	   *current-line* 0
	   (aref *cursor-positions* 0) (stream-cursor-position pane))
     (setf *white-space-start* (offset top)))
  (let ((*current-faces* *standard-faces*))
    (with-slots (stack-top) syntax
      (display-parse-tree stack-top syntax pane)))
  (when (region-visible-p pane) (display-region pane syntax))
  (display-cursor pane syntax current-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

(defun form-before-in-children (children offset)
  (loop for (first . rest) on children
     if (typep first 'form)
     do
       (cond ((< (start-offset first) offset (end-offset first))
              (return (if (null (children first))
                          nil
                          (form-before-in-children (children first) offset))))
             ((and (>= offset (end-offset first))
                   (or (null (first-form rest))
                       (<= offset (start-offset (first-form rest)))))
              (return (let ((potential-form
                             (when (typep first 'list-form)
                               (form-before-in-children (children first) offset))))
                        (if (not (null potential-form))
                            (if (<= (end-offset first)
                                    (end-offset potential-form))
                                potential-form
                                first)
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
     if (typep child 'form)
     do (cond ((< (start-offset child) offset (end-offset child))
               (return (if (null (children child))
                           nil
                           (form-after-in-children (children child) offset))))
              ((<= offset (start-offset child))
               (return (let ((potential-form (form-after-in-children (children child) offset)))
                         (if (not (null potential-form))
                             (if (<= (start-offset child)
                                     (start-offset potential-form))
                                 child
                                 potential-form)
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
	if (typep child 'form)
	do (cond ((<= (start-offset child) offset (end-offset child))
		  (return (if (null (first-form (children child)))
			      (when (typep child 'form)
				child)
			      (or (form-around-in-children (children child) offset)
                                  (when (typep child 'form)
                                    child)))))
		 ((< offset (start-offset child))
		  (return nil))
		 (t nil))))

(defun form-around (syntax offset)
  (with-slots (stack-top) syntax
    (if (or (null (start-offset stack-top))
	    (>= offset (end-offset stack-top))
	    (<= offset (start-offset stack-top)))
	nil
	(form-around-in-children (children stack-top) offset))))

(defun find-list-parent-offset (form fn)
    "Find a list parent of `token' and return `fn' 
applied to this parent token. `Fn' should be a function 
that returns an offset when applied to a 
token (eg. `start-offset' or `end-offset'). If a list
parent cannot be found, return `fn' applied to `form'."
  (when (not (typep form 'form*))
    (let ((parent (parent form)))
      (typecase parent
        (form* (funcall fn form))
        (list-form (funcall fn form))
        (null (funcall fn form))
        (t (find-list-parent-offset parent fn))))))

(defun find-list-child-offset (form fn &optional (min-offset 0))
  "Find a list child of `token' with a minimum start 
offset of `min-offset' and return `fn' applied to this child token.
`Fn' should be a function that returns an offset when applied to a 
token (eg. `start-offset' or `end-offset'). If a list child cannot
be found, return nil."
  (labels ((has-list-child (form)
              (some #'(lambda (child)
                                   (if (and (typep child 'list-form)
                                            (>= (start-offset child)
                                                min-offset))
                                       child
                                       (has-list-child child)))
                               (children form))))
    (let ((list-child (has-list-child form)))
      (when (not (null list-child))
        (funcall fn list-child)))))

(defmethod backward-one-expression (mark (syntax lisp-syntax))
  (let ((potential-form (or (form-before syntax (offset mark))
			    (form-around syntax (offset mark)))))
    (when (and (not (null potential-form))
               (not (= (offset mark) (start-offset potential-form))))
	(setf (offset mark) (start-offset potential-form)))))

(defmethod forward-one-expression (mark (syntax lisp-syntax))
  (let ((potential-form (or (form-after syntax (offset mark))
			    (form-around syntax (offset mark)))))
    (when (and (not (null potential-form))
               (not (= (offset mark) (end-offset potential-form))))
	(setf (offset mark) (end-offset potential-form)))))

(defgeneric forward-one-list (mark syntax)
  (:documentation
   "Move `mark' forward by one list. 
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
     then (end-offset potential-form)
     for potential-form = (or (form-after syntax start)
                              (form-around syntax start))
     until (or (null potential-form)
               (and (= start
                       (end-offset potential-form))
                    (null (form-after syntax start))))
     when (typep potential-form 'list-form)
     do (setf (offset mark) (end-offset potential-form))
     (return t)))

(defgeneric backward-one-list (mark syntax)
  (:documentation
   "Move `mark' backward by one list.  Return T if successful, or
NIL if the buffer limit was reached."))

(defmethod backward-one-list (mark (syntax lisp-syntax))
  (loop for start = (offset mark)
     then (start-offset potential-form)
     for potential-form = (or (form-before syntax start)
                              (form-around syntax start))
     until (or (null potential-form)
               (and (= start
                       (start-offset potential-form))
                    (null (form-before syntax start))))
     when (typep potential-form 'list-form)
     do (setf (offset mark) (start-offset potential-form))
     (return t)))

(climacs-motion:define-motion-fns list)

(defun down-list-by-fn (mark syntax fn)
  (let* ((offset (offset mark))
         (potential-form (form-after syntax offset)))
    (let ((new-offset (typecase potential-form
                        (list-form (start-offset potential-form))
                        (null nil)
                        (t (find-list-child-offset
                            (parent potential-form) 
                            fn
                            offset)))))
      (when new-offset 
        (progn (setf (offset mark) (1+ new-offset)) t)))))

(defmethod forward-one-down (mark (syntax lisp-syntax))
  (down-list-by-fn mark syntax #'start-offset))

(defmethod backward-one-down (mark (syntax lisp-syntax))
  (down-list-by-fn mark syntax #'end-offset)
  (backward-object mark syntax))

(defun up-list-by-fn (mark syntax fn)
  (let ((form (or (form-before syntax (offset mark))
                  (form-after syntax (offset mark))
                  (form-around syntax (offset mark)))))
    (when form
        (let ((parent (parent form)))
          (when (not (null parent))
            (let ((new-offset (find-list-parent-offset parent fn)))
              (when new-offset
                (setf (offset mark) new-offset))))))))

(defmethod backward-one-up (mark (syntax lisp-syntax))
  (up-list-by-fn mark syntax #'start-offset))

(defmethod forward-one-up (mark (syntax lisp-syntax))
  (up-list-by-fn mark syntax #'end-offset))

(defmethod eval-defun (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
     (loop for form in (children stack-top)
	   when (and (mark<= (start-offset form) mark)
		     (mark<= mark (end-offset form)))
	     do (return (eval-form-for-climacs
                         (get-usable-image syntax)
                         (token-to-object syntax form :read t))))))

(defmethod backward-one-definition (mark (syntax lisp-syntax))
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
		       (setf (offset mark)
                             (start-offset last-toplevel-list))
                       (return t)))))

(defmethod forward-one-definition (mark (syntax lisp-syntax))
  (with-slots (stack-top) syntax
    (loop for form in (children stack-top)
	  when (and (typep form 'form)
		    (mark< mark (end-offset form)))
	  do (setf (offset mark) (end-offset form))
	     (loop-finish)
          finally (return t))))

(defun in-type-p-in-children (children offset type)
  (loop for child in children
	do (cond ((< (start-offset child) offset (end-offset child))
		  (return (if (typep child type)
			      child
			      (in-type-p-in-children (children child) offset type))))
		 ((<= offset (start-offset child))
		  (return nil))
		 (t nil))))

(defun in-type-p (mark syntax type)
  (let ((offset (offset mark)))
    (with-slots (stack-top) syntax
       (if (or (null (start-offset stack-top))
	       (>= offset (end-offset stack-top))
	       (<= offset (start-offset stack-top)))
	   nil)
       (in-type-p-in-children (children stack-top) offset type))))

(defun in-string-p (mark syntax)
  (in-type-p mark syntax 'string-form))

(defun in-comment-p (mark syntax)
  (in-type-p mark syntax 'comment))

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

(defun token-string (syntax token)
  "Return the string that specifies `token' in the buffer of
  `syntax'."
  (buffer-substring (buffer syntax)
                    (start-offset token)
                    (end-offset token)))

(defun parse-symbol (string &key (package *package*) (case (readtable-case *readtable*)))
  "Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbol was
found in the package. Note that a symbol may be returned even if
it was not found in a package."
  (multiple-value-bind (symbol-name package-name)
      (parse-token string case)
    (let ((package (cond ((string= package-name "") +keyword-package+)
                         (package-name              (find-package package-name))
                         (t                         package))))
      (multiple-value-bind (symbol status)
          (when package
            (find-symbol symbol-name package))
        (if (or symbol status)
            (values symbol status)
            (values (make-symbol symbol-name) nil))))))

(defun token-to-symbol (syntax token &optional (case (readtable-case *readtable*)))
  "Return the symbol `token' represents. If the symbol cannot be
found in a package, an uninterned symbol will be returned."
  (token-to-object syntax token
                   :case case
                   :no-error t))

(defgeneric token-to-object (syntax token &key)
  (:documentation "Return the Lisp object `token' would evaluate
to if read. An attempt will be made to construct objects from
incomplete tokens. This function may signal an error if
`no-error' is nil and `token' cannot be converted to a Lisp
object. Otherwise, nil will be returned.")
  (:method :around (syntax token &rest args &key no-error package quote read)
           ;; Ensure that every symbol that is READ will be looked up
           ;; in the correct package. Also handle quoting.
           (flet ((act ()
                    (with-syntax-package syntax (start-offset token)
                        (syntax-package)
                     (let ((*package* (or package syntax-package)))
                       (cond (read
                              (read-from-string (token-string syntax token)))
                             (quote
                              (setf (getf args :quote) nil)
                              `',(call-next-method))
                             (t
                              (call-next-method)))))))
             (if no-error 
                 (ignore-errors (act))
                 (act))))
  (:method (syntax (token t) &key no-error)
           (declare (ignore no-error))
           ;; We ignore `no-error' as it is truly a bug in Climacs if no
           ;; handler method is specialized on this form.
           (error "Cannot convert token to Lisp object: ~A"
                  token))
  (:method (syntax (token incomplete-form-mixin) &key no-error)
           (unless no-error
             (error "Cannot convert incomplete form to Lisp object: ~A"
                    token))))

(defmethod token-to-object (syntax (token complete-token-lexeme)
                            &key no-error
                            (case (readtable-case *readtable*)))
  (declare (ignore no-error))
  (parse-symbol (token-string syntax token) :case case))

(defmethod token-to-object (syntax (token complete-token-form)
                            &key no-error
                            (case (readtable-case *readtable*)))
  (declare (ignore no-error))
  (parse-symbol (token-string syntax token) :case case))

(defmethod token-to-object (syntax (token number-lexeme) &rest args)
  (declare (ignore args))
  (let ((*read-base* (base syntax)))
    (read-from-string (token-string syntax token))))

(defmethod token-to-object (syntax (token list-form) &rest args)
  (loop for child in (children token)
     if (typep child 'comma-at-form)
       ;; How should we handle this?
       collect (apply #'token-to-object syntax child args)
     else if (typep child 'form)
       collect (apply #'token-to-object syntax child args)))

(defmethod token-to-object (syntax (token simple-vector-form) &key)
  (apply #'vector
         (call-next-method)))

(defmethod token-to-object (syntax (token incomplete-string-form) &rest args)
  (declare (ignore args))
  (read-from-string (concatenate 'string
                                 (token-string syntax token)
                                 "\"")))

(defmethod token-to-object (syntax (token complete-string-form) &key no-error)
  (declare (ignore no-error))
  (read-from-string (token-string syntax token)))

(defmethod token-to-object (syntax (token quote-form) &rest args)
  (apply #'token-to-object syntax (second (children token)) :quote t args))

;; I'm not sure backquotes are handled correctly, but then again,
;; `token-to-object' is not meant to be a perfect Lisp reader, only a
;; convenience function.
(defmethod token-to-object (syntax (token backquote-form) &rest args)
  (let ((backquoted-form (first-form (children token))))
    (if (typep backquoted-form 'list-form)
        `'(,@(apply #'token-to-object syntax backquoted-form args))
        `',(apply #'token-to-object syntax backquoted-form args))))

(defmethod token-to-object (syntax (token comma-form) &rest args)
  (apply #'token-to-object syntax (first-form (children token)) args))

(defmethod token-to-object (syntax (token comma-at-form) &rest args)
  (apply #'token-to-object syntax (first-form (children token)) args))

(defmethod token-to-object (syntax (token function-form) &rest args)
  (list 'cl:function (apply #'token-to-object syntax (second (children token))
                                      args)))

(defmethod token-to-object (syntax (token character-lexeme) &key)
  (read-from-string (token-string syntax token)))

(defmethod token-to-object (syntax (token cons-cell-form) &key)
  (let ((components (remove-if #'(lambda (token)
                                   (not (typep token 'form)))
                               (children token))))
    (if (<= (length components) 2)
        (cons (token-to-object syntax (first components))
              (token-to-object syntax (second components)))
        (loop for (head . tail) on components
           if (rest tail)
           collect (token-to-object syntax head)
           else if (not (null tail))
           append (cons (token-to-object syntax head)
                        (token-to-object syntax (first tail)))))))

;; Perhaps just returning NIL for conditionals whose condition
;; evaluates to NIL isn't such a good idea? I don't think it's very
;; Intuitive.
(defmethod token-to-object (syntax (token reader-conditional-positive-form) &key)
  (let ((conditional (second-noncomment (children token))))
    (when (eval-feature-conditional conditional syntax)
      (token-to-object syntax (third-noncomment (children token))))))

(defmethod token-to-object (syntax (token reader-conditional-negative-form) &key)
  (let ((conditional (second-noncomment (children token))))
    (when (not (eval-feature-conditional conditional syntax))
      (token-to-object syntax (third-noncomment (children token))))))

(defmethod token-to-object (syntax (token undefined-reader-macro-form) &key)
  ;; ???
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; indentation

(defgeneric indent-form (syntax tree path))

(defmethod indent-form ((syntax lisp-syntax) (tree form*) path)
  (cond ((or (null path)
	     (and (null (cdr path)) (zerop (car path))))
	 (values tree 0))
	((null (cdr path))
	 (values (elt-noncomment (children tree) (1- (car path))) 0))
	(t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

(defmethod indent-form ((syntax lisp-syntax) (tree string-form) path)
  (values (form-toplevel tree syntax) 0))

(defmethod indent-form ((syntax lisp-syntax) (tree reader-conditional-form) path)
  (cond ((or (null path)
	     (and (null (cdr path)) (zerop (car path))))
	 (values tree 0))
	((null (cdr path))
	 (values (first-form (children tree)) 0))))

(defmethod indent-form ((syntax lisp-syntax) (tree readtime-evaluation-form) path)
  (if (null (cdr path))
      (values tree 0)
      (indent-form syntax (elt-form (children tree) 0) (cdr path))))

(defmethod indent-form ((syntax lisp-syntax) (tree list-form) path)
  (if (= (car path) 1)
      ;; before first element
      (values tree 1)
      (let ((first-child (elt-noncomment (children tree) 1)))
	(cond ((and (typep first-child 'token-mixin)
		    (token-to-object syntax first-child))
	       (compute-list-indentation syntax (token-to-object syntax first-child) tree path))
	      ((null (cdr path))
	       ;; top level
	       (if (= (car path) 2)
		   ;; indent like first element
		   (values (elt-noncomment (children tree) 1) 0)
		   ;; indent like second element
		   (values (elt-noncomment (children tree) 2) 0)))
	      (t
	       ;; inside a subexpression
	       (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))))	    

(defmethod indent-form ((syntax lisp-syntax) (tree token-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree error-symbol) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree long-comment-form) path)
  (values tree 0))

(defmethod indent-form ((syntax lisp-syntax) (tree quote-form) path)
  (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree backquote-form) path)
  (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree comma-form) path)
  (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))

(defmethod indent-form ((syntax lisp-syntax) (tree function-form) path)
  (if (null (cdr path))
      (values tree 0)
      (indent-form syntax (elt-form (children tree) 0) (cdr path))))

(defmethod indent-binding ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; top level
      (cond ((= (car path) 1)
	     ;; before variable, indent 1
	     (values tree 1))
	    ((= (car path) 2)
	     ;; between variable and value
	     (values (elt-noncomment (children tree) 1) 0))
	    (t
	     ;; after value
	     (values (elt-noncomment (children tree) 2) 0)))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-bindings ((syntax lisp-syntax) tree path)
  (if (null (cdr path))
      ;; entire bind form
      (if (= (car path) 1)
	  ;; before first binding, indent 1
	  (values tree 1)
	  ;; after some bindings, align with first binding
	  (values (elt-noncomment (children tree) 1) 0))
      ;; inside a bind form
      (indent-binding syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation ((syntax lisp-syntax) symbol tree path)
  (if (null (cdr path))
      ;; top level
      (let* ((arglist (when (fboundp symbol)
                        (arglist-for-form symbol)))
             (body-or-rest-pos (or (position '&body arglist)
                                   (position '&rest arglist))))
        (if (and (or (macro-function symbol)
                     (special-operator-p symbol))
                 (and (not (null body-or-rest-pos))
                      (plusp body-or-rest-pos)))
            ;; macro-form with "interesting" arguments.
            (if (>= (- (car path) 2) body-or-rest-pos)
                ;; &body arg.
                (values (elt-noncomment (children tree) 1) 1)
                ;; non-&body-arg.
                (values (elt-noncomment (children tree) 1) 3))
            ;; normal form.
            (if (= (car path) 2)
                ;; indent like first child
                (values (elt-noncomment (children tree) 1) 0)
                ;; indent like second child
                (values (elt-noncomment (children tree) 2) 0))))
      ;; inside a subexpression
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmacro define-list-indentor (name element-indentor)
  `(defun ,name (syntax tree path)
     (if (null (cdr path))
	 ;; top level
	 (if (= (car path) 1)
	     ;; indent one more than the list
	     (values tree 1)
	     ;; indent like the first element
	     (values (elt-noncomment (children tree) 1) 0))
	 ;; inside an element
	 (,element-indentor syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

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
		  collect `((= (car path) ,i)
			    (,fun syntax (elt-noncomment (children tree) ,i) (cdr path))))
	   (t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

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
(define-simple-indentor (lambda indent-ordinary-lambda-list))
(define-simple-indentor (pprint-logical-block indent-list))

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
	 (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
	(4
	 ;; in the slot specs 
	 (indent-slot-specs syntax (elt-noncomment (children tree) 4) (cdr path)))
	(t
	 ;; this is an approximation, might want to do better
	 (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defgeneric)) tree path)
  (if (null (cdr path))
      ;; top level
      (values tree (if (<= (car path) 3) 4 2))
      (case (car path)
	(2
	 ;; in the function name
	 (indent-list syntax (elt-noncomment (children tree) 2) (cdr path)))
	(3
	 ;; in the lambda-list
	 (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) 3) (cdr path)))
	(t
	 ;; in the options or method specifications
	 (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'defmethod)) tree path)
  (let ((lambda-list-pos (position-if (lambda (x) (typep x 'list-form))
				      (remove-if
				       (lambda (x) (typep x 'comment)) (children tree)))))
    (cond ((null (cdr path))
	   ;; top level
	   (values tree (if (or (null lambda-list-pos)
				(<= (car path) lambda-list-pos))
			    4
			    2)))
	  ((or (null lambda-list-pos)
	       (< (car path) lambda-list-pos))
	   (indent-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
	  ((= (car path) lambda-list-pos)
	   (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) (car path)) (cdr path)))
	  (t
	   (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))))

(defun indent-clause (syntax tree path)
  (if (null (cdr path))
      ;; top level
      (case (car path)
        (1 (values tree 1))
        (2 (values tree 1))
        (t (values (elt-noncomment (children tree) 2) 0)))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod compute-list-indentation
    ((syntax lisp-syntax) (symbol (eql 'cond)) tree path)
  (if (null (cdr path))
      ;; top level
      (if (= (car path) 2)
	  ;; after `cond' 
	  (values tree 2)
	  ;; indent like the first clause
	  (values (elt-noncomment (children tree) 2) 0))
      ;; inside a clause
      (indent-clause syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(macrolet ((def (symbol)
               `(defmethod compute-list-indentation
                 ((syntax lisp-syntax) (symbol (eql ',symbol)) tree path)
                 (if (null (cdr path))
                     (case (car path)
                       (2 (values tree 4))
                       (3 (values tree 2))
                       (t (values (elt-noncomment (children tree) 3) 0)))
                     (indent-clause syntax (elt-noncomment (children tree) (car path)) (cdr path))))))
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
      (if (typep (elt-noncomment (children tree) (car path)) 'token-mixin) 
          (values tree 2)
          (values tree 4))
      (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path))))

(defmethod indent-local-function-definition ((syntax lisp-syntax) tree path)
  (cond ((null (cdr path))
	 ;; top level
	 (cond ((= (car path) 1)
		;; before name, indent 1
		(values tree 1))
	       ((= (car path) 2)
		;; between name and lambda list, indent 4
		(values (elt-noncomment (children tree) 1) 4))
	       (t
		;; after lambda list, indent 2
		(values (elt-noncomment (children tree) 1) 2))))
	((= (car path) 1)
	 ;; inside lambda list
	 (indent-ordinary-lambda-list syntax (elt-noncomment (children tree) 1) (cdr path)))
	(t (indent-form syntax (elt-noncomment (children tree) (car path)) (cdr path)))))

(define-list-indentor indent-local-function-definitions indent-local-function-definition)

(define-simple-indentor (flet indent-local-function-definitions))
(define-simple-indentor (labels indent-local-function-definitions))
(define-simple-indentor (with-open-file indent-list))

;;; CLIM indentation 

(define-simple-indentor (clim:with-output-as-presentation indent-list))
(define-simple-indentor (clim:vertically indent-list))
(define-simple-indentor (clim:horizontally indent-list))
(define-simple-indentor (clim:scrolling indent-list))
(define-simple-indentor (clim:with-drawing-options indent-list))
(define-simple-indentor (clim:define-command-table indent-list))
(define-simple-indentor (clim:define-command indent-list indent-list))
(define-simple-indentor (clim:define-application-frame indent-list indent-list))

(defun compute-path-in-trees (trees n offset)
  (cond ((or (null trees)
	     (>= (start-offset (first-noncomment trees)) offset))    
	 (list n))
	((or (< (start-offset (first-noncomment trees)) offset (end-offset (first-noncomment trees)))
	     (typep (first-noncomment trees) 'incomplete-form-mixin))
	 (cons n (compute-path-in-tree (first-noncomment trees) offset)))
	(t (compute-path-in-trees (rest-noncomments trees) (1+ n) offset))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Swine

;;; Compiler note hyperlinking code

(defun make-compiler-note (note-list)
 (let ((severity (getf note-list :severity))
       (message (getf note-list :message))
       (location (getf note-list :location))
       (references (getf note-list :references))
       (short-message (getf note-list :short-message)))
   (make-instance
    (ecase severity
      (:error 'error-compiler-note)
      (:read-error 'read-error-compiler-note)
      (:warning 'warning-compiler-note)
      (:style-warning 'style-warning-compiler-note)
      (:note 'note-compiler-note))
      :message message :location location
      :references references :short-message short-message)))

(defclass compiler-note ()
    ((message :initarg :message :initform nil :accessor message)
     (location :initarg :location :initform nil :accessor location)
     (references :initarg :references :initform nil :accessor references)
     (short-message :initarg :short-message :initform nil :accessor short-message))
 (:documentation "The base for all compiler-notes."))

(defclass error-compiler-note (compiler-note) ())

(defclass read-error-compiler-note (compiler-note) ())

(defclass warning-compiler-note (compiler-note) ())

(defclass style-warning-compiler-note (compiler-note) ())

(defclass note-compiler-note (compiler-note) ())

(defclass location ()()
 (:documentation "The base for all locations."))

(defclass error-location (location)
    ((error-message :initarg :error-message :accessor error-message)))

(defclass actual-location (location)
    ((source-position :initarg :position :accessor source-position)
     (snippet :initarg :snippet :accessor snippet :initform nil))
 (:documentation "The base for all non-error locations."))

(defclass buffer-location (actual-location)
    ((buffer-name :initarg :buffer :accessor buffer-name)))

(defclass file-location (actual-location)
    ((file-name :initarg :file :accessor file-name)))

(defclass source-location (actual-location)
    ((source-form :initarg :source-form :accessor source-form)))

(defclass basic-position () ()
 (:documentation "The base for all positions."))

(defclass char-position (basic-position)
    ((char-position :initarg :position :accessor char-position)
     (align-p :initarg :align-p :initform nil :accessor align-p)))

(defun make-char-position (position-list)
 (make-instance 'char-position :position (second position-list)
                :align-p (third position-list)))

(defclass line-position (basic-position)
    ((start-line :initarg :line :accessor start-line)
     (end-line :initarg :end-line :initform nil :accessor end-line)))

(defun make-line-position (position-list)
 (make-instance 'line-position :line (second position-list)
                :end-line (third position-list)))

(defclass function-name-position (basic-position)
    ((function-name :initarg :function-name)))

(defun make-function-name-position (position-list)
 (make-instance 'function-name-position :function-name (second position-list)))

(defclass source-path-position (basic-position)
    ((path :initarg :source-path :accessor path)
     (start-position :initarg :start-position :accessor start-position)))

(defun make-source-path-position (position-list)
 (make-instance 'source-path-position :source-path (second position-list)
                :start-position (third position-list)))

(defclass text-anchored-position (basic-position)
    ((start :initarg :text-anchored :accessor start)
     (text :initarg :text :accessor text)
     (delta :initarg :delta :accessor delta)))

(defun make-text-anchored-position (position-list)
 (make-instance 'text-anchored-position :text-anchored (second position-list)
                :text (third position-list)
                :delta (fourth position-list)))

(defclass method-position (basic-position)
    ((name :initarg :method :accessor name)
     (specializers :initarg :specializers :accessor specializers)
     (qualifiers :initarg :qualifiers :accessor qualifiers)))

(defun make-method-position (position-list)
 (make-instance 'method-position :method (second position-list)
                :specializers (third position-list)
                :qualifiers (last position-list)))

(defun make-location (location-list)
 (ecase (first location-list)
   (:error (make-instance 'error-location :error-message (second location-list)))
   (:location
    (destructuring-bind (l buf pos hints) location-list
      (declare (ignore l))
      (let ((location
             (apply #'make-instance
                    (ecase (first buf)
                      (:file 'file-location)
                      (:buffer 'buffer-location)
                      (:source-form 'source-location))
                    buf))
            (position
             (funcall
              (ecase (first pos)
                (:position #'make-char-position)
                (:line #'make-line-position)
                (:function-name #'make-function-name-position)
                (:source-path #'make-source-path-position)
                (:text-anchored #'make-text-anchored-position)
                (:method #'make-method-position))
              pos)))
        (setf (source-position location) position)
        (when hints
          (setf (snippet location) (rest hints)))
        location)))))

(defmethod initialize-instance :after ((note compiler-note) &rest args)
 (declare (ignore args))
 (setf (location note) (make-location (location note))))

(defun show-note-counts (notes &optional seconds)
 (loop with nerrors = 0
       with nwarnings = 0
       with nstyle-warnings = 0
       with nnotes = 0
       for note in notes
       do (etypecase note
            (error-compiler-note (incf nerrors))
            (read-error-compiler-note (incf nerrors))
            (warning-compiler-note (incf nwarnings))
            (style-warning-compiler-note (incf nstyle-warnings))
            (note-compiler-note (incf nnotes)))
       finally
    (esa:display-message "Compilation finished: ~D error~:P ~
                            ~D warning~:P ~D style-warning~:P ~D note~:P ~
                            ~@[[~D secs]~]"
            nerrors nwarnings nstyle-warnings nnotes seconds)))

(defun one-line-ify (string)
  "Return a single-line version of STRING.
Each newline and following whitespace is replaced by a single space."
  (loop with count = 0
     while (< count (length string))
     with new-string = (make-array 0 :element-type 'character :adjustable t
                                   :fill-pointer 0)
     when (char= (char string count) #\Newline)
     do (loop while (and (< count (length string))
                         (whitespacep nil (char string count)))
           do (incf count)
           ;; Just ignore whitespace if it is last in the
           ;; string.
           finally (when (< count (length string))
                     (vector-push-extend #\Space new-string)))
     else
     do (vector-push-extend (char string count) new-string)
     (incf count)
     finally (return new-string)))

(defgeneric print-for-menu (object stream))

(defun print-note-for-menu (note stream severity ink)
 (with-accessors ((message message) (short-message short-message)) note
   (with-drawing-options (stream :ink ink
                                 :text-style (make-text-style :sans-serif :italic nil))
     (princ severity stream)
     (princ " " stream))
   (princ (if short-message
              (one-line-ify short-message)
              (one-line-ify message))
          stream)))

(defmacro def-print-for-menu (class name colour)
 `(defmethod print-for-menu ((object ,class) stream)
    (print-note-for-menu object stream ,name ,colour)))

(def-print-for-menu error-compiler-note "Error" +red+)
(def-print-for-menu read-error-compiler-note "Read Error" +red+)
(def-print-for-menu warning-compiler-note "Warning" +dark-red+)
(def-print-for-menu style-warning-compiler-note "Style Warning" +brown+)
(def-print-for-menu note-compiler-note "Note" +brown+)

(defun show-notes (notes buffer-name definition)
 (let ((stream (climacs-gui::typeout-window
                (format nil "~10TCompiler Notes: ~A  ~A" buffer-name definition))))
   (loop for note in notes
         do (with-output-as-presentation (stream note 'compiler-note)
              (print-for-menu note stream))
            (terpri stream)
         count note into length
         finally (change-space-requirements stream
                        :height (* length (stream-line-height stream)))
                 (scroll-extent stream 0 0))))

(defgeneric goto-location (location))

(defmethod goto-location ((location error-location))
 (esa:display-message (error-message location)))

(defmethod goto-location ((location buffer-location))
 (let ((buffer (find (buffer-name location)
                     (climacs-gui::buffers *application-frame*)
                     :test #'string= :key #'name)))
   (unless buffer
     (esa:display-message "No buffer ~A" (buffer-name location))
     (beep)
     (return-from goto-location))
   (climacs-gui::switch-to-buffer buffer)
   (goto-position (source-position location))))

(defmethod goto-location ((location file-location))
 (let ((buffer (find (file-name location)
                     (climacs-gui::buffers *application-frame*)
                     :test #'string= :key #'(lambda (buffer)
                                              (let ((path (filepath buffer)))
                                                (when path
                                                  (namestring path)))))))
   (if buffer
       (climacs-gui::switch-to-buffer buffer)
       (climacs-gui::find-file (file-name location)))
   (goto-position (source-position location))))

(defgeneric goto-position (position))

(defmethod goto-position ((position char-position))
 (climacs-gui::goto-position (climacs-gui::point (climacs-gui::current-window))
                             (char-position position)))

;;; Macroexpansion and evaluation

(defun macroexpand-token (syntax token &optional (all nil))
  (with-syntax-package syntax (start-offset token) (package)
    (let ((*package* package))
      (let* ((string (token-string syntax token))
             (expression (read-from-string string))
             (expansion (macroexpand-for-climacs (get-usable-image syntax)
                                                 expression
                                                 all))
             (expansion-string (with-output-to-string (s)
                                 (pprint expansion s))))
        (let ((buffer (climacs-gui::switch-to-buffer "*Macroexpansion*")))
          (climacs-gui::set-syntax buffer "Lisp"))
        (let ((point (point (climacs-gui::current-window)))
              (header-string (one-line-ify (subseq string 0
                                                   (min 40 (length string))))))
          (climacs-gui::end-of-buffer point)
          (unless (beginning-of-buffer-p point)
            (insert-object point #\Newline))
          (insert-sequence point
                           (format nil ";;; Macroexpand-~:[1~;all~] ~A...~%"
                                   all header-string))
          (insert-sequence point expansion-string)
          (insert-object point #\Newline))))))

(defun eval-string (syntax string)
  "Evaluate all expressions in STRING and return a list of
results."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil stream)
       while (not (eq form stream))
       collecting (multiple-value-list
                   (eval-form-for-climacs (get-usable-image syntax)
                                          form)))))

(defun eval-region (start end syntax)
  ;; Must be (mark>= end start).
  (with-slots (package) syntax
    (let* ((string (buffer-substring (buffer start)
                                     (offset start)
                                     (offset end)))
           (values (multiple-value-list
                    (eval-string syntax string)))
           ;; Enclose each set of values in {}.
           (result (apply #'format nil "~{{~:[No values~;~:*~{~S~^,~}~]}~}"
                          values)))
      (esa:display-message result))))

(defun compile-definition-interactively (mark syntax)
  (with-syntax-package syntax mark (package)
    (let* ((token (definition-at-mark mark syntax))
           (string (token-string syntax token))
           (m (clone-mark mark))
           (buffer-name (name (buffer syntax))))
      (forward-definition m syntax)
      (backward-definition m syntax)
      (multiple-value-bind (result notes)
          (compile-form-for-climacs (get-usable-image syntax)
                                    (token-to-object syntax token
                                                     :read t
                                                     :package package)
                                    (buffer syntax)
                                    m)
        (show-note-counts notes (second result))
        (when (not (null notes))
          (show-notes notes buffer-name
                      (one-line-ify (subseq string 0 (min (length string) 20)))))))))

(defun compile-file-interactively (buffer &optional load-p)
  (when (and (needs-saving buffer)
             (accept 'boolean :prompt (format nil "Save buffer ~A ?" (name buffer))))
    (climacs-gui::save-buffer buffer))
  (with-syntax-package (syntax buffer) 0 (package)
    (multiple-value-bind (result notes)
        (compile-file-for-climacs (get-usable-image (syntax buffer))
                                  (filepath buffer)
                                  package load-p)
      (show-note-counts notes (second result))
      (when notes (show-notes notes (name buffer) "")))))

;;; Parameter hinting

(defparameter +cl-arglist-keywords+
  lambda-list-keywords)

(defparameter +cl-garbage-keywords+
  '(&whole &environment))

(defun arglist-keyword-p (arg)
  "Return T if `arg' is an arglist keyword. NIL otherwise."
  (member arg +cl-arglist-keywords+))

(defun split-arglist-on-keywords (arglist)
  "Return an alist keying lambda list keywords of `arglist'
to the symbols affected by the keywords."
  (let ((sing-result '())
        (env (position '&environment arglist)))
    (when env
      (push (list '&environment (elt arglist (1+ env))) sing-result)
      (setf arglist (remove-if (constantly t) arglist :start env :end (+ env 2))))
    (when (eq '&whole (first arglist))
      (push (subseq arglist 0 2) sing-result)
      (setf arglist (cddr arglist)))
    (do ((llk '(&mandatory &optional &key &allow-other-keys &aux &rest &body))
         (args (if (arglist-keyword-p (first arglist))
                   arglist
                   (cons '&mandatory arglist))
               (cdr args))
         (chunk '())
         (result '()))
        ((null args)
         (when chunk (push (nreverse chunk) result))
         (nreverse (nconc sing-result result)))
      (if (member (car args) llk)
          (progn
            (when chunk (push (nreverse chunk) result))
            (setf chunk (list (car args))))
          (push (car args) chunk)))))

(defun find-optional-argument-values (arglist provided-args &optional
                                      (split-arglist
                                       (split-arglist-on-keywords
                                        arglist)))
  "Return an association list mapping symbols of optional or
  keyword arguments from `arglist' to the specified values in
  `provided-args'. `Split-arglist' should be either a split
  arglist or nil, in which case it will be calculated from
  `arglist'."
  ;; First we check whether any optional arguments have even been
  ;; provided.
  (flet ((get-args (keyword)
           (rest (assoc keyword split-arglist))))
    (let* ((mandatory-args-count (length (get-args '&mandatory)))
           (optional-args-count (length (get-args '&optional)))
           (keyword-args-count (length (get-args '&key)))
           (provided-args-count (length provided-args))
           (nonmandatory-args-count (+ keyword-args-count
                                       optional-args-count)))
      (when (> provided-args-count
               mandatory-args-count)
        ;; We have optional arguments.
        (let (
              ;; Find the part of the provided arguments that concern
              ;; optional arguments.
              (opt-args-values (subseq provided-args
                                       mandatory-args-count
                                       (min provided-args-count
                                            nonmandatory-args-count)))
              ;; Find the part of the provided arguments that concern
              ;; keyword arguments.
              (keyword-args-values (subseq provided-args
                                           (min (+ mandatory-args-count
                                                   optional-args-count)
                                                provided-args-count))))
          (append (mapcar #'cons
                          (mapcar #'unlisted (get-args '&optional))
                          opt-args-values)

                  (loop
                     ;; Loop over the provided keyword symbols and
                     ;; values in the argument list. Note that
                     ;; little checking is done to ensure that the
                     ;; given symbols are valid - this is not a
                     ;; compiler, so extra mappings do not
                     ;; matter.
                     for (keyword value) on keyword-args-values by #'cddr
                     if (keywordp keyword)
                     collect (let ((argument-symbol
                                    (unlisted (find (symbol-name keyword)
                                                    (get-args '&key)
                                                    :key #'(lambda (arg)
                                                             (symbol-name (unlisted arg)))
                                                    :test #'string=))))
                               ;; We have to find the associated
                               ;; symbol in the argument list... ugly.
                               (cons argument-symbol
                                     value)))))))))

(defun find-affected-simple-arguments (arglist current-arg-index preceding-arg
                                       &optional (split-arglist (split-arglist-on-keywords arglist)))
  "Find the simple arguments of `arglist' that would be affected
  if an argument was intered at index `current-arg-index' in the
  arglist. If `current-arg-index' is nil, no calculation will be
  done (this function will just return nil). `Preceding-arg'
  should either be nil or the argument directly preceding
  point. `Split-arglist' should either be a split arglist or nil,
  in which case `split-arglist' will be computed from
  `arglist'. This function returns two values: The primary value
  is a list of symbols that should be emphasized, the secondary
  value is a list of symbols that should be highlighted."
  (when current-arg-index
    (flet ((get-args (keyword)
             (rest (assoc keyword split-arglist))))
      (let ((mandatory-argument-count (length (get-args '&mandatory))))
        (cond ((> mandatory-argument-count
                  current-arg-index)
               ;; We are in the main, mandatory, positional arguments.
               (let ((relevant-arg (elt (get-args '&mandatory)
                                        current-arg-index)))
                 ;; We do not handle complex argument lists here, only
                 ;; pure standard arguments.
                 (unless (and (listp relevant-arg)
                              (< current-arg-index mandatory-argument-count))
                   (values nil (list (unlisted relevant-arg))))))
              ((> (+ (length (get-args '&optional))
                     (length (get-args '&mandatory)))
                  current-arg-index)
               ;; We are in the &optional arguments.
               (values nil
                       (list (unlisted (elt (get-args '&optional)
                                            (- current-arg-index
                                               (length (get-args '&mandatory))))))))
              (t
               (let ((body-or-rest-args (or (get-args '&rest)
                                            (get-args '&body)))
                     (key-arg (find (format nil "~A" preceding-arg)
                                    (get-args '&key)
                                    :test #'string=
                                    :key #'(lambda (arg)
                                             (symbol-name (unlisted arg))))))
                 ;; We are in the &body, &rest or &key arguments.
                 (values
                  ;; Only emphasize the &key
                  ;; symbol if we are in a position to add a new
                  ;; keyword-value pair, and not just in a position to
                  ;; specify a value for a keyword.
                  (when (and (null key-arg)
                             (get-args '&key))
                    '(&key))
                  (append (when key-arg
                            (list (unlisted key-arg)))
                          body-or-rest-args)))))))))

(defun analyze-arglist-impl (arglist current-arg-indices preceding-arg provided-args)
  "The implementation for `analyze-arglist'."
  (let* ((split-arglist (split-arglist-on-keywords arglist))
         (user-supplied-arg-values (find-optional-argument-values
                                    arglist
                                    provided-args
                                    split-arglist))
         (mandatory-argument-count
          (length (rest (assoc '&mandatory split-arglist))))
         
         (current-arg-index (or (first current-arg-indices)
                                0))
         ret-arglist
         emphasized-symbols
         highlighted-symbols)
    ;; First, we find any standard arguments that should be
    ;; highlighted or emphasized, more complex, destructuring
    ;; arguments will be handled specially.
    (multiple-value-bind (es hs)
        (find-affected-simple-arguments arglist
                                        ;; if `current-arg-indices' is
                                        ;; nil, that means that we do
                                        ;; not have enough information
                                        ;; to properly highlight
                                        ;; symbols in the arglist.
                                        (and current-arg-indices
                                             current-arg-index)
                                        preceding-arg
                                        split-arglist)
      (setf emphasized-symbols es)
      (setf highlighted-symbols hs))
    ;; We loop over the arglist and build a new list, and if we have a
    ;; default value for a given argument, we insert it into the
    ;; list. Also, whenever we encounter a list in a mandatory
    ;; argument position, we assume that it is a destructuring arglist
    ;; and recursively call `analyze-arglist' on it to find the
    ;; arglist and emphasized and highlighted symbols for it.
    (labels ((generate-arglist (arglist)
               (loop
                  for arg-element in arglist
                  for arg-name = (unlisted arg-element)
                  for index from 0
                    
                  if (and (listp arg-element)
                          (> mandatory-argument-count
                             index))
                  collect (multiple-value-bind (arglist
                                                sublist-emphasized-symbols
                                                sublist-highlighted-symbols)
                              (analyze-arglist arg-element
                                               (rest current-arg-indices)
                                               preceding-arg
                                               (when (< index (length provided-args))
                                                 (listed (elt provided-args index))))
                            ;; Unless our `current-arg-index'
                            ;; actually refers to this sublist, its
                            ;; highlighted and emphasized symbols
                            ;; are ignored. Also, if
                            ;; `current-arg-indices' is nil, we do
                            ;; not have enough information to
                            ;; properly highlight symbols in the
                            ;; arglist.
                            (when (and current-arg-indices
                                       (= index current-arg-index))
                              (if (and (rest current-arg-indices))
                                  (setf emphasized-symbols
                                        (union (mapcar #'unlisted
                                                       sublist-emphasized-symbols)
                                               emphasized-symbols)
                                        highlighted-symbols
                                        (union sublist-highlighted-symbols
                                               highlighted-symbols))
                                  (setf emphasized-symbols
                                        (union (mapcar #'unlisted
                                                       arg-element)
                                               emphasized-symbols))))
                            arglist)
                  else if (assoc arg-name user-supplied-arg-values)
                  collect (list arg-name
                                (rest (assoc
                                       arg-name
                                       user-supplied-arg-values)))
                  else
                  collect arg-element)))
      (setf ret-arglist (generate-arglist arglist)))
    (list ret-arglist emphasized-symbols highlighted-symbols)))

(defun analyze-arglist (arglist current-arg-indices
                        preceding-arg provided-args)
  "Analyze argument list and provide information for highlighting
it. `Arglist' is the argument list that is to be analyzed,
`current-arg-index' is the index where the next argument would be
written (0 is just after the operator), `preceding-arg' is the
written argument preceding point and `provided-args' is a list of
the args already written.

Three values are returned: 

* An argument list with values for &optional and &key arguments
inserted from `provided-args'.

* A list of symbols that should be emphasized.

* A list of symbols that should be highlighted."
  (apply #'values (analyze-arglist-impl
                   arglist
                   current-arg-indices
                   preceding-arg
                   provided-args)))

(defun cleanup-arglist (arglist)
  "Remove elements of `arglist' that we are not interested in."
  (loop
     for arg in arglist
     with in-&aux                       ; If non-NIL, we are in the
                                        ; &aux parameters that should
                                        ; not be displayed.
                    
     with in-garbage                    ; If non-NIL, the next
                                        ; argument is a garbage
                                        ; parameter that should not be
                                        ; displayed.
     if in-garbage
     do (setf in-garbage nil)
     else if (not in-&aux)
     if (eq arg '&aux)
     do (setf in-&aux t)
     else if (member arg +cl-garbage-keywords+ :test #'eq)
     do (setf in-garbage t)
     else
     collect arg))

(defgeneric arglist-for-form (operator &optional arguments)
  (:documentation
   "Return an arglist for `operator'")
  (:method (operator &optional arguments)
    (declare (ignore arguments))
    (cleanup-arglist
     (arglist (get-usable-image (syntax (current-buffer))) operator))))

;; Proof of concept, just to make sure it can be done. Also, we need a
;; more elegant interface. Perhaps it could be integrated with the
;; indentation definition macros, in order to create some sort of
;; `define-form-traits'-supermacro. That could be cool. Also, that way
;; various libraries could trivially create a Climacs-extension-file
;; containing calls to this super-macro that would make Climacs aware
;; of the libraries indentation- and completion-needs.
(defmethod arglist-for-form ((operator (eql 'cl:make-instance)) &optional arguments)
  (let ((arglist (call-next-method)))
    (if (and (plusp (length arguments))
             (listp (first arguments))
             (> (length (first arguments)) 1)
             (eq (caar arguments) 'cl:quote))
        (append arglist
                (cons '&key (get-class-keyword-parameters
                             (get-usable-image (syntax (current-buffer)))
                             (first arguments))))
        arglist)))

(defmethod arglist-for-form ((operator list) &optional arguments)
  (declare (ignore arguments))
  (case (first operator)
    ('cl:lambda (cleanup-arglist (second operator)))))

(defgeneric operator-for-display (operator)
  (:documentation "Return what should be displayed whenever
  `operator' is displayed as an operator.")
  (:method (operator)
    operator))

(defmethod operator-for-display ((operator list))
  (case (first operator)
    ('cl:lambda '|Lambda-Expression|)))

(defun display-arglist-to-stream (stream operator arglist
                                  &optional emphasized-symbols
                                  highlighted-symbols)
  "Display the operator and arglist to stream, format as
  appropriate."
  ;; FIXME: This is fairly ugly.
  (labels ((display-symbol (symbol)
             (with-text-style
                 (stream
                  `(nil
                    ,(cond ((member symbol
                                    highlighted-symbols)
                            :bold)
                           ((member symbol
                                    emphasized-symbols)
                            :italic))
                    nil))
               (format stream "~A" symbol)))
           (display-list (list)
             (if (and (eq (first list) 'quote)
                      (= (length list) 2))
                 (progn
                   (format stream "'")
                   (display-argument (second list)))
                 (progn
                   (format stream "(")
                   (display-argument (first list))
                   (dolist (arg (rest list))
                     (format stream " ")
                     (display-argument arg))
                   (format stream ")"))))
           (display-argument (arg)
             (if (and (listp arg)
                      (not (null arg)))
                 (display-list arg)
                 (display-symbol arg))))
    (display-argument (cons (operator-for-display operator)
                            arglist))))

(defun show-arglist-silent (operator &optional
                            current-arg-indices
                            preceding-arg arguments)
  "Display the arglist for `operator' in the minibuffer, do not
complain if `operator' is not bound to, or is not, a function.

`Current-arg-index' and `preceding-arg' are used to add extra
information to the arglist display. `Arguments' should be either
nil or a list of provided arguments in the form housing symbol.

Returns NIL if an arglist cannot be displayed."
  (multiple-value-bind (arglist emphasized-symbols highlighted-symbols)
      (analyze-arglist
       (arglist-for-form operator arguments)
       current-arg-indices
       preceding-arg
       arguments)
    (esa:with-minibuffer-stream (minibuffer)
      (display-arglist-to-stream minibuffer operator
                                 arglist emphasized-symbols
                                 highlighted-symbols))))

(defun show-arglist (symbol)
  (unless (and (fboundp symbol)
               (show-arglist-silent symbol))
    (esa:display-message "Function ~a not found." symbol)))

(defun find-argument-indices-for-operand (syntax operand-form operator-form)
  "Return a list of argument indices for `argument-form' relative
  to `operator-form'. These lists take the form of (n m p), which
  means (aref form-operand-list n m p). A list of
  argument indices can have arbitrary length (but they are
  practically always at most 2 elements long). "
  (declare (ignore syntax))
  (let ((operator (first-form (children operator-form))))
    (labels ((worker (operand-form &optional the-first)
               ;; Cannot find index for top-level-form.
               (when (parent operand-form)
                 (let ((form-operand-list
                        (remove-if #'(lambda (form)
                                       (or (not (typep form 'form))
                                           (eq form operator)))
                                   (children (parent operand-form)))))

                   (let ((operand-position (position operand-form form-operand-list))
                         (go-on (not (eq operator-form (parent operand-form)))))
                     ;; If we find anything, we have to increment the
                     ;; position by 1, since we consider the existance
                     ;; of a first operand to mean point is at operand
                     ;; 2. Likewise, a position of nil is interpreted
                     ;; as 0.
                     (cons (if operand-position
                               (if (or the-first)
                                   (1+ operand-position)
                                   operand-position)
                               0)
                           (when go-on
                             (worker (parent operand-form)))))))))
      (nreverse (worker operand-form t)))))

(defun find-operand-info (mark-or-offset syntax operator-form)
  "Returns two values: The operand preceding `mark-or-offset' and
  the path from `operator-form' to the operand."
  (as-offsets ((mark-or-offset offset))
    (let* ((preceding-arg-token (form-before syntax offset))
           (indexing-start-arg
            (let* ((candidate-before preceding-arg-token)
                   (candidate-after (when (null candidate-before)
                                      (let ((after (form-after syntax offset)))
                                        (when after
                                          (parent after)))))
                   (candidate-around (when (null candidate-after)
                                       (form-around syntax offset)))
                   (candidate (or candidate-before
                                  candidate-after
                                  candidate-around)))
              (if (or (and candidate-before
                           (typep candidate-before 'incomplete-list-form))
                      (and (null candidate-before)
                           (typep (or candidate-after candidate-around)
                                  'list-form)))
                  ;; HACK: We should not attempt to find the location of
                  ;; the list form itself, so we create a new parser
                  ;; symbol, attach the list form as a parent and try to
                  ;; find the new symbol. That way we can get a list of
                  ;; argument-indices to the first element of the list
                  ;; form, even if it is empty or incomplete.
                  (let ((obj (make-instance 'parser-symbol)))
                    (setf (parent obj) candidate)
                    obj)
                  candidate)))
           (argument-indices (find-argument-indices-for-operand
                              syntax
                              indexing-start-arg
                              operator-form))
           (preceding-arg-obj (when preceding-arg-token
                                (token-to-object syntax preceding-arg-token
                                                 :no-error t))))
      (values preceding-arg-obj argument-indices))))

(defun valid-operator-p (operator)
  "Check whether or not `operator' is a valid
  operator. `Operator' is considered a valid operator if it is a
  symbol bound to a function, or if it is a lambda expression."
  (cond ((symbolp operator)
         (or (fboundp operator)
             (macro-function operator)
             (special-operator-p operator)))
        ((listp operator)
         (eq (first operator) 'cl:lambda))))

(defun indices-match-arglist (arglist arg-indices)
  "Check whether the argument indices `arg-indices' could refer
  to a direct argument for the operator with the argument list
  `arglist'. Returns T if they could, NIL otherwise. This
  functions does not care about the argument quantity, only their
  structure."
  (let* ((index (first arg-indices))
         (pure-arglist (remove-if #'arglist-keyword-p arglist))
         (arg (when (< index (length pure-arglist))
                (elt pure-arglist index))))
    (cond ((and (> index (or (position #'arglist-keyword-p arglist) 0))
                (not (null (rest arg-indices))))
           nil)
          ((and (not (null arg))
                (listp arg)
                (rest arg-indices))
           (indices-match-arglist arg (rest arg-indices)))
          (t t))))

(defun direct-arg-p (form syntax)
  "Check whether `form' is a direct argument to one of its
   parents."
  (labels ((recurse (parent)
             (let ((operator (form-operator
                              parent
                              syntax)))
               (or (and
                    ;; An operator is not an argument to itself...
                    (not (= (start-offset form)
                            (start-offset (first-form (children parent)))))
                    (valid-operator-p operator)
                    (indices-match-arglist
                     (arglist (image syntax)
                              operator)
                     (second
                      (multiple-value-list
                       (find-operand-info
                        (start-offset form)
                        syntax
                        parent)))))
                   (when (parent parent)
                     (recurse (parent parent)))))))
    (when (parent form)
      (recurse (parent form)))))

(defmacro with-code-insight (mark-or-offset syntax (&key operator preceding-operand
                                                         form preceding-operand-indices
                                                         operands)
                             &body body)
  "Evaluate `body' with the provided symbols lexically bound to
  interesting details about the code at `mark'. If `mark' is not
  within a form, everything will be bound to nil."
  (let ((operator-sym (or operator (gensym)))
        (preceding-operand-sym (or preceding-operand (gensym)))
        (operands-sym (or operands (gensym)))
        (form-sym (or form (gensym)))
        (operand-indices-sym (or preceding-operand-indices (gensym)))
        ;; My kingdom for with-gensyms (or once-only)!
        (mark-value-sym (gensym))
        (syntax-value-sym (gensym)))
    `(let* ((,mark-value-sym ,mark-or-offset)
            (,syntax-value-sym ,syntax)
            (,form-sym
             ;; Find a form with a valid (fboundp) operator.
             (let ((immediate-form
                    (preceding-form ,mark-value-sym ,syntax-value-sym)))
               ;; Recurse upwards until we find a form with a valid
               ;; operator. This could be improved a lot, as we could
               ;; inspect the lambda list of the found operator and
               ;; check if the position of mark makes sense with
               ;; regard to the structure of the lambda list. If we
               ;; cannot find a form with a valid operator, just
               ;; return the form `mark' is in.
               (unless (null immediate-form)
                 (labels ((recurse (form)
                            (unless (null (parent form))
                              (or (unless (eq (first-form (children (parent form)))
                                              form)
                                    (recurse (parent form)))
                                  (and (valid-operator-p (form-operator
                                                          form
                                                          ,syntax-value-sym))
                                       (indices-match-arglist
                                        (arglist-for-form
                                         (form-operator
                                          form
                                          ,syntax-value-sym)
                                         (form-operands
                                          form
                                          ,syntax-value-sym))
                                        (second
                                         (multiple-value-list
                                          (find-operand-info ,mark-value-sym ,syntax-value-sym form))))
                                       (not (direct-arg-p form ,syntax-value-sym))
                                       form)))))
                   (or (recurse (parent immediate-form))
                       (parent immediate-form))))))
            ;; If we cannot find a form, there's no point in looking
            ;; up any of this stuff.
            (,operator-sym (when ,form-sym (form-operator ,form-sym ,syntax-value-sym)))
            (,operands-sym (when ,form-sym (form-operands ,form-sym ,syntax-value-sym))))
       (declare (ignorable ,mark-value-sym ,syntax-value-sym ,form-sym
                           ,operator-sym ,operands-sym))
       (multiple-value-bind (,preceding-operand-sym ,operand-indices-sym)
           (when ,form-sym (find-operand-info ,mark-value-sym ,syntax-value-sym ,form-sym))
         (declare (ignorable ,preceding-operand-sym ,operand-indices-sym))
         ,@body))))

(defun show-arglist-for-form-at-mark (mark syntax)
  "Display the argument list for the operator of `form'. The
list need not be complete. If an argument list cannot be
retrieved for the operator, nothing will be displayed."
  (with-code-insight mark syntax (:operator operator
                                            :preceding-operand preceding-operand
                                            :preceding-operand-indices preceding-operand-indices
                                            :operands operands)
    (when (valid-operator-p operator) 
      (show-arglist-silent operator preceding-operand-indices preceding-operand operands))))

;;; Definition editing

(defparameter *find-definition-stack* '())

(defun pop-find-definition-stack ()
 (unless (null *find-definition-stack*)
   (let* ((offset+buffer (pop *find-definition-stack*))
          (offset (first offset+buffer))
          (buffer (second offset+buffer)))
     (if (find buffer (climacs-gui::buffers *application-frame*))
         (progn (climacs-gui::switch-to-buffer buffer)
                (climacs-gui::goto-position (point (climacs-gui::current-window)) offset))
         (pop-find-definition-stack)))))

;; KLUDGE: We need to put more info in the definition objects to begin
;; with.
(defun definition-type (definition)
  (let ((data (read-from-string (first definition))))
     (case (first data)
      ((or cl:defclass)
       'cl:class)
      ((or cl:defgeneric
           cl:defmethod
           cl:defun
           cl:defmacro)
       'cl:function)
      (t t))))

(defun edit-definition (symbol &optional type)
  (let ((all-definitions (find-definitions-for-climacs
                          (get-usable-image (syntax (current-buffer)))
                          symbol)))
    (let ((definitions (if (not type)
                           all-definitions
                           (remove-if-not #'(lambda (definition)
                                              (eq (definition-type definition) type))
                                          all-definitions))))
      (cond ((null definitions)
             (esa:display-message "No known definitions for: ~A" symbol)
             (beep))
            (t
             (goto-definition symbol definitions))))))

(defun goto-definition (name definitions)
 (let* ((pane (climacs-gui:current-window))
        (buffer (buffer pane))
        (point (point pane))
        (offset (offset point)))
   (push (list offset buffer) *find-definition-stack*))
 (cond ((null (cdr definitions))
        (let* ((def (car definitions))
               (xref (make-xref def)))
          (goto-location xref)))
       (t
        (let ((xref (show-definitions name definitions)))
          (when xref (goto-location xref))))))

(defclass xref ()
  ((dspec :initarg :dspec :accessor dspec)
   (location :initarg :location :accessor location)))

(defun make-xref (xref-list)
 (destructuring-bind (dspec location) xref-list
     (make-instance 'xref
        :dspec dspec
        :location (make-location location))))

(defmethod goto-location ((xref xref))
 (goto-location (location xref)))

(defun show-definitions (name definitions)
 (show-xrefs (loop for xref-list in definitions
                   collect (make-xref xref-list))
             'definition name))

(defun show-xrefs (xrefs type symbol)
 (cond ((null xrefs)
        (esa:display-message "No references found for ~A." symbol)
        (beep))
       (t
        (flet ((printer (item stream)
                 (with-drawing-options (stream :ink +dark-blue+
                                               :text-style (make-text-style :fixed nil nil))
                   (princ (dspec item) stream))))
         (let ((stream (climacs-gui::typeout-window
                        (format nil "~10T~A ~A" type symbol))))
             (loop for xref in xrefs
                   do (with-output-as-presentation (stream xref 'xref)
                        (printer xref stream))
                      (terpri stream)
                   count xref into length
                   finally (change-space-requirements stream
                                  :height (* length (stream-line-height stream)))
                           (scroll-extent stream 0 0)))))))

;;; Symbol completion

(defvar *completion-pane* nil)

(defun relevant-keywords (arglist arg-indices)
  "Return a list of the keyword arguments that it would make
  sense to use at the position `arg-indices' relative to the
  operator that has the argument list `arglist'."
  (let* ((key-position (position '&key arglist))
         (cleaned-arglist (remove-if #'arglist-keyword-p
                                     arglist))
         (index (first arg-indices))
         (difference (- (length arglist)
                        (length cleaned-arglist))))
    (cond ((and (null key-position)
                (rest arg-indices)
                (> (length cleaned-arglist)
                   index)
                (listp (elt cleaned-arglist index)))
           ;; Look in a nested argument list.
           (relevant-keywords (elt cleaned-arglist index)
                              (rest arg-indices)))
          ((and (not (null key-position))
                (>= (+ index
                       difference) 
                    key-position)
                (not (evenp (- index key-position difference))))
           (mapcar #'unlisted (subseq cleaned-arglist
                                      (- key-position
                                         difference
                                         -1)))))))

(defun completions-from-keywords (syntax token)
  "Assume that `token' is a (partial) keyword argument
keyword. Find out which operator it is applicable to, and return
a completion list based on the valid keywords, or NIL, if no
keyword arguments would be valid (for example, if the operator
doesn't take keyword arguments)."
  (with-code-insight (start-offset token) syntax
      (:preceding-operand-indices poi
                                  :operator operator)
    (when (valid-operator-p operator)
      (let* ((relevant-keywords
              (relevant-keywords (arglist-for-form operator)
                                 poi))
             (completions (simple-completions
                           (get-usable-image syntax)
                           (token-string syntax token)
                           +keyword-package+))
             (relevant-completions
              (remove-if-not #'(lambda (compl)
                                 (member compl relevant-keywords
                                         :test #'(lambda (a b)
                                                   (string-equal a b
                                                                 :start1 1))
                                         :key #'symbol-name))
                             (mapcar #'string-downcase (first completions)))))
        (list relevant-completions
              (longest-completion relevant-completions))))))

;; The following stuff is from Swank.

(defun longest-completion (completions)
  "Return the longest completion of `completions', which must be a
list of sequences."
  (untokenize-completion
   (mapcar #'longest-common-prefix
           (transpose-lists (mapcar #'tokenize-completion completions)))))

(defun tokenize-completion (string)
  "Return all substrings of STRING delimited by #\-."
  (loop with end
        for start = 0 then (1+ end)
        until (> start (length string))
        do (setq end (or (position #\- string :start start) (length string)))
        collect (subseq string start end)))

(defun untokenize-completion (tokens)
  (format nil "~{~A~^-~}" tokens))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun transpose-lists (lists)
  "Turn a list-of-lists on its side.
If the rows are of unequal length, truncate uniformly to the shortest.

For example:
\(transpose-lists '((ONE TWO THREE) (1 2)))
  => ((ONE 1) (TWO 2))"
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (transpose-lists (mapcar #'cdr lists))))))

(defun clear-completions ()
  (when *completion-pane*
    (climacs-gui::delete-window *completion-pane*)
    (setf *completion-pane* nil)))

(defun show-completions-by-fn (fn symbol package)
  (esa:display-message (format nil "~a completions" symbol))
  (let* ((result (funcall fn symbol (package-name package)))
         (set (first result))
         (longest (second result)))
    (cond ((<=(length set) 1)
           (clear-completions))
          (t (let ((stream (or *completion-pane*
                               (climacs-gui::typeout-window "Simple Completions"))))
               (setf *completion-pane* stream)
               (window-clear stream)
               (format stream "~{~A~%~}" set))))
       (if (not (null longest))
           (esa:display-message (format nil "Longest is ~a|" longest))
           (esa:display-message "No completions found"))
    longest))

(defun show-completions (syntax token package)
  (let ((symbol-name (token-string syntax token)))
    (show-completions-by-fn
     #'(lambda (&rest args)
         (find-if #'identity
                  (list
                   ;; If we are in a position where only keyword parameters
                   ;; make sense, only complete from the available keyword
                   ;; parameters.
                   (completions-from-keywords syntax token)
                   ;; Plain, complete symbol completion.
                   (apply #'simple-completions
                          (get-usable-image syntax)
                          args))
                  :key #'first))
     symbol-name package)))

(defun show-fuzzy-completions (syntax symbol-name package)
  (esa:display-message (format nil "~a completions" symbol-name))
  (let* ((set (fuzzy-completions (get-usable-image syntax) symbol-name package 10))
         (best (caar set)))
    (cond ((<= (length set) 1)
           (clear-completions))
          (t (let ((stream (or *completion-pane*
                               (climacs-gui::typeout-window "Simple Completions"))))
               (setf *completion-pane* stream)
               (window-clear stream)
               (loop for completed-string in set
                  do (format stream "~{~A  ~}~%" completed-string)))))
    (esa:display-message (if (not (null best))
                             (format nil "Best is ~a|" best)
                             "No fuzzy completions found"))        
    best))
