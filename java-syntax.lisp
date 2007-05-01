;; -*- Mode: Lisp; Package: CLIMACS-JAVA-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2007 by
;;;           John Q Splittist (splittist@gmail.com)
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

;;;# Syntax module for analysing Java(TM)

(in-package :climacs-java-syntax)

;;;# The command table.

(define-syntax-command-table java-table
    :errorp nil)

;;;# The syntax object.
;;;
;;; We could add options here.

(define-syntax java-syntax (lr-syntax-mixin fundamental-syntax)
  ((package :accessor package-of
	    :documentation "A list of strings being the components of
the `package' definition, if any."))
  (:name "Java")
  (:pathname-types "java" "jav")
  (:command-table java-table)
  (:default-initargs :initial-state |initial-state |))

;;; Now some ways to indicate what the syntax is. Extra details could be
;;; added. For now we'll show the package, if any.

(defmethod name-for-info-pane ((syntax java-syntax) &key pane)
  (declare (ignore pane))
  (format nil "Java~@[:~{~A~^.~}~]"
	  (package-of syntax)))

;;;# Lexing.
;;;
;;; First we define the different states the lexer can be in (as triggered
;;; by the parser.)

(define-lexer-state lexer-string-state ()
  ()
  (:documentation "In this state, the lexer is working inside a string
    delimited by double quote characters."))

(define-lexer-state lexer-line-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a line
    comment starting with //."))

(define-lexer-state lexer-long-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a long
    comment delimited by /* and */."))

;;; And then we define the various elements of the language.
;;;
;;; First, some high-level concepts:

(defclass java-nonterminal (nonterminal) ())

(defclass form (java-nonterminal) ())

;;; Since we're dealing with things that might not be finished,
;;; we allow for incomplete forms at the end of the buffer.

(defclass complete-form-mixin () ())
(defclass incomplete-form-mixin () ())

(defclass comment (java-nonterminal) ())
(defclass line-comment (java-comment) ())
(defclass long-comment (java-comment) ())

;;; Of course, sometimes people type things that don't (yet) comply
;;; with the language specification.

(defclass error-symbol (java-nonterminal) ())

;;; Finally, we define the relevant lexeme. We will check the `ink' and
;;; and the `face' later during redisplay.

(defclass java-lexeme (lexeme)
  ((ink)
   (face)))

(defclass form-lexeme (form java-lexeme) ())

;;; Keywords come in various flavours.

(defclass keyword-lexeme (form-lexeme) ())

(defclass basic-type () ())
(defclass modifier () ())
(defclass operator () ())

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun spelling-to-symbol (name)
  (intern (concatenate 'string name "-LEXEME") #.*package*)))

(defmacro define-keywords (&rest keyword-names)
  `(progn
     ,@(loop for (name . supers) in keyword-names
	     for real-name = (spelling-to-symbol name)
	     collecting `(defclass ,real-name (,@ supers keyword-lexeme) ())
	       into defclasses
	     collecting name into names
	     finally (return (cons `(defparameter *keyword-spellings* ',names)
				   defclasses)))))

(define-keywords 
    ("abstract" modifier)
    ("assert" operator)
    ("boolean" basic-type)
    ("break" operator)
    ("byte" basic-type)
    ("case" operator)
    ("catch" operator)
    ("char" basic-type)
    ("class" operator)
    ("const") 				; reserved but not used
    ("continue" operator)
    ("default" operator)
    ("do" operator)
    ("double" basic-type)
    ("else" operator)
    ("enum" operator)
    ("extends" operator)
    ("final" modifier)
    ("finally" operator)
    ("float" basic-type)
    ("for" operator)
    ("if" operator)
    ("int" basic-type)
    ("goto")				; reserved but not used
    ("implements" operator)
    ("import" operator)
    ("instanceof" operator)
    ("interface" operator)
    ("long" basic-type)
    ("native" basic-type)
    ("new" operator)
    ("package" operator)
    ("private" operator)
    ("package" operator)
    ("private" modifier)
    ("protected" modifier)
    ("public" modifier)
    ("return" operator)
    ("short" basic-type)
    ("static" modifier)
    ("striftfp" modifier)
    ("super" operator)
    ("switch" operator)
    ("synchronized" modifier)
    ("this" operator)
    ("throw" operator)
    ("throws" operator)
    ("transient" modifier)
    ("try" operator)
    ("void" operator)
    ("volatile" modifier)
    ("while" operator))

(defclass identifier-lexeme (form-lexeme) ())
(defclass literal-lexeme (form-lexeme) ())
(defclass integer-literal-lexeme (literal-lexeme) ())
(defclass decimal-integer-literal-lexeme (integer-literal-lexeme) ())
(defclass octal-integer-literal-lexeme (integer-literal-lexeme) ())
(defclass hex-integer-literal-lexeme (integer-literal-lexeme) ())
(defclass floating-point-literal-lexeme (literal-lexeme) ())
(defclass decimal-floating-point-literal-lexeme (floating-point-literal-lexeme) ())
(defclass hexidecimal-floating-point-literal-lexeme (floating-point-literal-lexeme) ())
;;; A badly formed, or perhaps unfinished, number.
(defclass bad-number-literal-lexeme (literal-lexeme) ())
(defclass boolean-literal-lexeme (literal-lexeme) ())
(defclass character-literal-lexeme (literal-lexeme) ())
(defclass incomplete-character-literal-lexeme (literal-lexeme incomplete-form-mixin) ())
(defclass string-literal-lexeme (literal-lexeme) ())
(defclass null-literal-lexeme (literal-lexeme) ())
(defclass separator-lexeme (form-lexeme) ())
(defclass punctuator-lexeme (form-lexeme) ())

;;; Separators: ( ) { } [ ] ; , .

(defclass semi-colon-lexeme (separator-lexeme) ())
(defclass comma-lexeme (separator-lexeme) ())
(defclass dot-lexeme (separator-lexeme) ())
(defclass delimiter-mixin () ())
(defclass opening-delimiter-mixin (delimiter-mixin) ())
(defclass closing-delimiter-mixin (delimiter-mixin) ())

(defclass left-bracket-lexeme (separator-lexeme opening-delimiter-mixin) ())
(defclass right-bracket-lexeme (separator-lexeme closing-delimiter-mixin) ())
(defclass left-parenthesis-lexeme (separator-lexeme opening-delimiter-mixin) ())
(defclass right-parenthesis-lexeme (separator-lexeme closing-delimiter-mixin) ())
(defclass left-brace-lexeme (separator-lexeme opening-delimiter-mixin) ())
(defclass right-brace-lexeme (separator-lexeme closing-delimiter-mixin) ())

;;; Operators:
;;; = < > ! ~ ? :
;;; == <= >= != && || ++ --
;;; +  -  *  /  &  |  ^  %  <<  >>  >>>
;;; += -= *= /= &= |= ^= %= <<= >>= >>>=

(defmacro define-operators (&rest punctuator-names)
  `(progn
     ,@(loop for name in punctuator-names
	     for real-name = (intern (concatenate 'string 
						 (string name) "-LEXEME")
				   #.*package*)
	     collecting `(defclass ,real-name (punctuator-lexeme) ()))))

(define-operators 
    equal left-angle-bracket right-angle-bracket exclamation tilde question 
    colon
    eq leq geq neq and-and or-or increment decrement
    plus minus asterisk slash ampersand pipe circumflex percent
    left-shift right-shift unsigned-right-shift
    plus-equal minus-equal asterisk-equal slash-equal ampersand-equal pipe-equal
    circumflex-equal percent-equal left-shift-equal right-shift-equal
    unsigned-right-shift-equal)

;;; This for annotated interfaces.
(defclass ampersand-lexeme (punctuator-lexeme) ())

;;; And something for when we come across something completely wrong.

(defclass error-lexeme (java-lexeme) ())

;;; Some lexemes that will drive the parser and lexer.

(defclass line-comment-start-lexeme (java-lexeme) ())
(defclass long-comment-start-lexeme (java-lexeme) ())
(defclass comment-end-lexeme (java-lexeme) ())
(defclass string-start-lexeme (java-lexeme) ())
(defclass string-end-lexeme (java-lexeme) ())

;;; And some lexemes used inside strings and comments.

(defclass word-lexeme (java-lexeme) ())
(defclass delimiter-lexeme (java-lexeme) ())
(defclass text-lexeme (java-lexeme) ())

;;; Some predicates for recognizing the constituents of identifiers.
;;; "The $ character should be used only in mechanically generated
;;;  source code or, rarely, to access preexisting names on legacy
;;;  systems."

(defun java-letter-p (ch)
  (and (characterp ch)
       (or (alpha-char-p ch)
	   (char= ch #\_)
	   (char= ch #\$))))

(defun java-letter-or-digit-p (ch)
  (and (characterp ch)
       (or (alphanumericp ch)
	   (char= ch #\_)
	   (char= ch #\$))))

;;; Something to recognise escapes, including unicode escapes (which may
;;; have multiple #\u characters).

(defun eat-escape (scan)
  "Advance over an escape (after the #\\), returning T if valid so far, or NIL."
  (macrolet ((fo () `(forward-object scan)))
    (case (object-after scan)
      ((#\b #\t #\n #\f #\r #\" #\' #\\)
       (fo) t)
      (#\u
       (loop until (end-of-buffer-p scan)
	     while (eql (object-after scan) #\u)
	     do (fo))
       (loop until (end-of-buffer-p scan)
	     for char = (object-after scan)
	     with count = 0
	     while (and (characterp char)
			(digit-char-p char 16))
	     do (fo) (incf count)
	     finally (return (or (and (end-of-buffer-p scan)
				      (< count 4))
				 (= count 4)))))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (loop repeat 3
	     until (end-of-buffer-p scan)
	     for char = (object-after scan)
	     while (and (characterp char)
			(digit-char-p char 8))
	     do (fo))
       t)
      (t nil))))

;;; The default method for skipping whitespace.

(defmethod skip-inter ((syntax java-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
	    do (return nil)
	  until (not (whitespacep syntax (object-after scan)))
	  do (fo)
	  finally (return t))))

;;; The lexing procedure used at the toplevel. Dispatches to lex-token
;;; at the appropriate time - except for standalone dots (where the lexer
;;; doesn't know whether it's looking at a potential number or the
;;; separator in a QualifiedIdentifier).

(defmethod lex ((syntax java-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\" (fo) (make-instance 'string-start-lexeme))
	(#\' (fo)
	     (cond ((end-of-buffer-p scan)
		    (make-instance 'incomplete-character-literal-lexeme))
		   (t (cond ((eql (object-after scan) #\\)
			     (fo)
			     (if (not (end-of-buffer-p scan))
				 (unless (eat-escape scan)
				   (return-from lex 
				     (make-instance 'error-lexeme)))))
			    (t (fo)))
		      (cond ((end-of-buffer-p scan)
			     (make-instance 'incomplete-character-literal-lexeme))
			    ((eql (object-after scan) #\')
			     (fo)
			     (make-instance 'character-literal-lexeme))
			    (t (make-instance 'error-lexeme))))))
	(#\[ (fo) (make-instance 'left-bracket-lexeme))
	(#\] (fo) (make-instance 'right-bracket-lexeme))
	(#\( (fo) (make-instance 'left-parenthesis-lexeme))
	(#\) (fo) (make-instance 'right-parenthesis-lexeme))
	(#\{ (fo) (make-instance 'left-brace-lexeme))
	(#\} (fo) (make-instance 'right-brace-lexeme))
	(#\@ (fo) (make-instance 'ampersand-lexeme))
	(#\. (fo) (if (end-of-buffer-p scan)
		      (make-instance 'dot-lexeme)
		      (cond ((and (characterp (object-after scan))
				  (digit-char-p (object-after scan)))
			     (backward-object scan)
			     (lex-token syntax scan))
			    (t (make-instance 'dot-lexeme)))))
	(#\- (fo) (if (end-of-buffer-p scan)
		      (make-instance 'minus-lexeme)
		      (case (object-after scan)
			(#\- (fo) (make-instance 'decrement-lexeme))
			(#\= (fo) (make-instance 'minus-equal-lexeme)) 
			(t (make-instance 'minus-lexeme)))))
	(#\+ (fo) (if (end-of-buffer-p scan)
		      (make-instance 'plus-lexeme)
		      (case (object-after scan)
			(#\+ (fo) (make-instance 'increment-lexeme))
			(#\= (fo) (make-instance 'plus-equal-lexeme))
			(t (make-instance 'plus-lexeme)))))
	(#\& (fo) (if (end-of-buffer-p scan)
		      (make-instance 'ampersand-lexeme)
		      (case (object-after scan)
			(#\& (fo) (make-instance 'and-and-lexeme))
			(#\= (fo) (make-instance 'ampersand-equal-lexeme))
			(t (make-instance 'ampersand-lexeme)))))
	(#\* (fo) (if (end-of-buffer-p scan)
		      (make-instance 'asterisk-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'asterisk-equal-lexeme))
			    (t (make-instance 'asterisk-lexeme)))))
	(#\~ (fo) (make-instance 'tilde-lexeme))
	(#\! (fo) (if (end-of-buffer-p scan)
		      (make-instance 'exclamation-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'neq-lexeme))
			    (t (make-instance 'exclamation-lexeme)))))
	(#\/ (fo) (if (end-of-buffer-p scan)
		      (make-instance 'slash-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'slash-equal-lexeme))
			(#\* (fo) (make-instance 'long-comment-start-lexeme))
			(#\/ (fo) (make-instance 'line-comment-start-lexeme))
			(t (make-instance 'slash-lexeme)))))
	(#\% (fo) (if (end-of-buffer-p scan)
		      (make-instance 'percent-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'percent-equal-lexeme)) 
			(t (make-instance 'percent-lexeme)))))
	(#\< (fo) (if (end-of-buffer-p scan)
		      (make-instance 'left-angle-bracket-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'leq-lexeme))
			(#\< (fo) 
			     (cond ((eql (object-after scan) #\=)
				    (fo)
				    (make-instance 'left-shift-equal-lexeme))
				   (t (make-instance 'left-shift-lexeme))))
			(t (make-instance 'left-angle-bracket-lexeme)))))
	(#\> (fo) (if (end-of-buffer-p scan)
		      (make-instance 'right-angle-bracket-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'geq-lexeme))
			(#\> (fo) 
			     (cond ((eql (object-after scan) #\=)
				    (fo)
				    (make-instance 'right-shift-equal-lexeme))
				   ((eql (object-after scan) #\>)
				    (fo)
				    (cond ((eql (object-after scan) #\=)
					   (fo)
					   (make-instance 'unsigned-right-shift-equal-lexeme))
					  (t (make-instance 'unsigned-right-shift-lexeme))))
				   (t (make-instance 'right-shift-lexeme))))
			(t (make-instance 'right-angle-bracket-lexeme)))))
	(#\= (fo) (if (end-of-buffer-p scan)
		      (make-instance 'equal-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'eq-lexeme))
			    (t (make-instance 'equal-lexeme)))))
	(#\^ (fo) (if (end-of-buffer-p scan)
		      (make-instance 'circumflex-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'circumflex-equal-lexeme))
			    (t (make-instance 'circumflex-lexeme)))))
	(#\| (fo) (if (end-of-buffer-p scan)
		      (make-instance 'pipe-lexeme)
		      (case (object-after scan)
			(#\| (fo) (make-instance 'or-or-lexeme))
			(#\= (fo) (make-instance 'pipe-equal-lexeme)) 
			(t (make-instance 'pipe-lexeme)))))
	(#\? (fo) (make-instance 'question-lexeme))
	(#\; (fo) (make-instance 'semi-colon-lexeme))
	(#\, (fo) (make-instance 'comma-lexeme))
	(t (cond ((or (java-letter-or-digit-p object)
		      (eql object #\\))
		  (lex-token syntax scan))
                 (t (fo) (make-instance 'error-lexeme))))))))

;;; Lexing in strings is essentially splitting the input into words,
;;; delimters and whitespace.

(defmethod lex ((syntax java-syntax) (state lexer-string-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\") (fo) (make-instance 'string-end-lexeme))
	    ((eql object #\\)
	     (fo)
	     (eat-escape scan)
	     (make-instance 'delimiter-lexeme))
	    ((java-letter-or-digit-p object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (java-letter-or-digit-p (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

;;; Lexing in comments is similar to strings, but in long comments we
;;; need to detect the comment end.

(defmethod lex ((syntax java-syntax) (state lexer-long-comment-state) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\*)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\/)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'comment-end-lexeme))))
	    ((java-letter-or-digit-p object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (java-letter-or-digit-p (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod skip-inter ((syntax java-syntax) 
		       (state lexer-line-comment-state) 
		       scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (or (end-of-line-p scan)
		    (not (whitespacep syntax (object-after scan))))
	  do (fo)
	  finally (return t))))

(defmethod lex ((syntax java-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((end-of-line-p scan)
	   (make-instance 'comment-end-lexeme))
	  ((java-letter-or-digit-p (object-after scan))
	   (loop until (or (end-of-buffer-p scan)
			   (not (java-letter-or-digit-p (object-after scan))))
		 do (fo))
	   (make-instance 'word-lexeme))
	  (t (fo) (make-instance 'delimiter-lexeme)))))

;;; Recognise the various types of numbers, returning the appropriate
;;; class name. We return `'bad-number-lexeme' in some circumstances where
;;; the author might just not have finished typing in. The logic detects
;;; 'long' versions separately, although the same result as the non-long
;;; version is returned for now.

(defun lex-number (scan)
  (let (hex oct dot exp float-suffix)
    (labels ((fo () (forward-object scan))
	     (eat-digits (&optional (radix 10))
	       (loop until (end-of-buffer-p scan)
		     while (and (characterp (object-after scan))
				(digit-char-p (object-after scan) radix))
		     do (fo))))
      (when (eql (object-after scan) #\0)
	(fo)
	(cond ((end-of-buffer-p scan)
	       (return-from lex-number 'decimal-integer-literal-lexeme))
	      ((equalp (object-after scan) #\X)
	       (fo)
	       (setf hex t))
	      ((eql (object-after scan) #\.)
	       (fo)
	       (setf dot t))
	      ((and (characterp (object-after scan))
		    (digit-char-p (object-after scan) 8))
	       (setf oct t))
	      ((equalp (object-after scan) #\L)
	       (fo)
	       (return-from lex-number 'decimal-integer-literal-lexeme))))
      (eat-digits (cond (hex 16) (oct 8) (t 10)))
      (when (end-of-buffer-p scan)
	(return-from lex-number
	  (cond (hex 'hex-integer-literal-lexeme)
		(oct 'octal-integer-literal-lexeme)
		(t 'decimal-integer-literal-lexeme))))
      (cond ((equalp (object-after scan) #\L)
	     (fo)
	     (return-from lex-number
	       (cond (hex 'hex-integer-literal-lexeme)
		(oct 'octal-integer-literal-lexeme)
		(t 'decimal-integer-literal-lexeme))))
	    (oct (return-from lex-number 'octal-integer-literal-lexeme))
	    ((eql (object-after scan) #\.)
	     (when dot
	       (return-from lex-number 'bad-number-literal-lexeme))
	     (setf dot t)
	     (fo)
	     (eat-digits (cond (hex 16) (oct 8) (t 10)))))
      (when (end-of-buffer-p scan)
	(return-from lex-number
	  (if (or dot exp float-suffix)
	      (if hex
		  'hexidecimal-floating-point-literal-lexeme
		  'decimal-floating-point-literal-lexeme)
	      (if hex
		  'hex-integer-literal-lexeme
		  'decimal-integer-literal-lexeme))))
      (when (equalp (object-after scan) (if hex #\P #\E))
	(setf exp t)
	(fo)
	(when (end-of-buffer-p scan)
	  (return-from lex-number 'bad-number-literal-lexeme))
	(if (member (object-after scan) '(#\+ #\-))
	    (fo))
	(when (end-of-buffer-p scan)
	  (return-from lex-number 'bad-number-literal-lexeme))
	(eat-digits))
      (unless (end-of-buffer-p scan)
	  (when (member (object-after scan) '(#\f #\F #\d #\D))
	    (setf float-suffix t)
	    (fo)))
      (return-from lex-number
	(if (or dot exp float-suffix)
	    (if hex
		'hexidecimal-floating-point-literal-lexeme
		'decimal-floating-point-literal-lexeme)
	    (if hex
		'hex-integer-literal-lexeme
		'decimal-integer-literal-lexeme))))))

;;; Decide whether we're lexing an identifier (or one of the textual literals)
;;; or a number.

(defun lex-token (syntax scan)
  (declare (ignore syntax))
  (labels ((fo () (forward-object scan)))
    (cond ((java-letter-p (object-after scan))
	   (let ((token (make-array 32 :element-type 'character
				    :adjustable t :fill-pointer 0)))
	     (loop until (or (end-of-buffer-p scan)
			     (not (or (java-letter-or-digit-p 
				       (object-after scan)))))
		   do (vector-push-extend (object-after scan) token)
		      (fo))
	     (cond ((find token *keyword-spellings* :test #'string=)
		    (make-instance (spelling-to-symbol token)))
		   ((string= token "null")
		    (make-instance 'null-literal-lexeme))
		   ((or (string= token "true")
			(string= token "false"))
		    (make-instance 'boolean-literal-lexeme))
		   (t (make-instance 'identifier-lexeme)))))
	  (t
	   (make-instance (lex-number scan))))))

;;; In the error state, just slurp full lines.

(defmethod lex ((syntax java-syntax) (state lexer-error-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (end-of-line-p scan)
	  do (fo))
    (make-instance 'error-lexeme)))

;;;# Parsing

(defmacro define-java-action ((state lexeme) &body body)
  `(defmethod action ((syntax java-syntax) (state ,state) (lexeme ,lexeme))
     ,@body))

(defmacro define-new-java-state ((state parser-symbol) &body body)
  `(defmethod new-state ((syntax java-syntax) 
			 (state ,state) 
			 (tree ,parser-symbol))
     ,@body))

(define-java-action (error-reduce-state (eql nil))
  (throw 'done nil))

;;; The default action for any lexeme is shift.
(define-java-action (t java-lexeme)
  lexeme)

;;; The action on end-of-buffer is to reduce to the error symbol.
(define-java-action (t (eql nil))
  (reduce-all error-symbol))

;;; The default new state is the error state.
(define-new-java-state (t parser-symbol) error-state)

;;; The new state when an error-state
(define-new-java-state (t error-symbol) error-reduce-state)

;;;;;;;;;;;;;;;; Top-level

#| rules
   form* ->
   form* -> form* form
|#

;;; parse trees
(defclass form* (java-nonterminal) ())

(define-parser-state |form* | (lexer-toplevel-state parser-state) ())
(define-parser-state form-may-follow (lexer-toplevel-state parser-state) ())
(define-parser-state |initial-state | (form-may-follow) ())

(define-new-java-state (|initial-state | form) |initial-state |)
(define-new-java-state (|initial-state | comment) |initial-state |)

(define-java-action (|initial-state | (eql nil))
  (reduce-all form*))

(define-new-java-state (|initial-state | form*) |form* | )

(define-java-action (|form* | (eql nil))
  (throw 'done nil))

;;;;;;;;;;;;;;;; String

;;; parse trees
(defclass string-form (form) ())
(defclass complete-string-form (string-form complete-form-mixin) ())
(defclass incomplete-string-form (string-form incomplete-form-mixin) ())

(define-parser-state |" word* | (lexer-string-state parser-state) ())
(define-parser-state |" word* " | (lexer-toplevel-state parser-state) ())

(define-new-java-state (|" word* | word-lexeme) |" word* |)
(define-new-java-state (|" word* | delimiter-lexeme) |" word* |)
(define-new-java-state (form-may-follow string-start-lexeme) |" word* |)
(define-new-java-state (|" word* | string-end-lexeme) |" word* " |)

;;; reduce according to the rule form -> " word* "
(define-java-action (|" word* " | t)
  (reduce-until-type complete-string-form string-start-lexeme))

;;; reduce at the end of the buffer
(define-java-action (|" word* | (eql nil))
  (reduce-until-type incomplete-string-form string-start-lexeme))

;;;;;;;;;;;;;;;; Line comment

;;; parse trees
(defclass line-comment-form (comment) ())

(define-parser-state |// word* | (lexer-line-comment-state parser-state) ())
(define-parser-state |// word* NL | (lexer-toplevel-state parser-state) ())

(define-new-java-state (form-may-follow line-comment-start-lexeme) |// word* |)
(define-new-java-state (|// word* | word-lexeme) |// word* |)
(define-new-java-state (|// word* | delimiter-lexeme) |// word* |)
(define-new-java-state (|// word* | comment-end-lexeme) |// word* NL |)

;;; reduce according to the rule form -> // word* NL
(define-java-action (|// word* NL | t)
  (reduce-until-type line-comment-form line-comment-start-lexeme))

;;;;;;;;;;;;;;;; Long comment

;;; parse trees
(defclass long-comment-form (comment) ())
(defclass complete-long-comment-form (long-comment-form complete-form-mixin) ())
(defclass incomplete-long-comment-form (long-comment-form incomplete-form-mixin) ())

(define-parser-state |/* word* | (lexer-long-comment-state parser-state) ())
(define-parser-state |/* word* */ | (lexer-toplevel-state parser-state) ())

(define-new-java-state (|/* word* | word-lexeme) |/* word* |)
(define-new-java-state (|/* word* | delimiter-lexeme) |/* word* |)
(define-new-java-state (|/* word* | long-comment-start-lexeme) |/* word* |)
(define-new-java-state (|/* word* | long-comment-form) |/* word* |)
(define-new-java-state (form-may-follow long-comment-start-lexeme) |/* word* |)
(define-new-java-state (|/* word* | comment-end-lexeme) |/* word* */ |)

;;; reduce according to the rule form -> /* word* */
(define-java-action (|/* word* */ | t)
  (reduce-until-type complete-long-comment-form long-comment-start-lexeme))

;;; reduce at the end of the buffer
(define-java-action (|/* word* | (eql nil))
  (reduce-until-type incomplete-long-comment-form long-comment-start-lexeme))

;;; Here we search for the package name.

(defun update-package-name (buffer syntax)
  (declare (ignore buffer))
  (setf (package-of syntax) nil)
  (with-slots (stack-top) syntax
     (loop for (token . rest) on (children stack-top)
	   when (typep token '|package|-LEXEME)
	     do (loop for component in rest
		      until (typep component 'semi-colon-lexeme)
		      while (or (typep component 'dot-lexeme)
				(typep component 'identifier-lexeme)
				(typep component 'comment))
		      when (typep component 'identifier-lexeme)
			collect (form-string syntax component) into components
		      finally (setf (package-of syntax) components)))))

;;; TODO: conditionalise this
(defmethod update-syntax :after (buffer (syntax java-syntax))
  (update-package-name buffer syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defun form-string (syntax form)
  "Return the string that correspond to `form' in the buffer of
`syntax'."
  (buffer-substring (buffer syntax) (start-offset form) (end-offset form)))

(defvar *white-space-start* nil)

(defvar *current-line* 0)

(defparameter *current-faces*
  `((:error ,+red+ nil)
    (:string ,+rosy-brown+ ,(make-text-style nil :italic nil))
    (:operator ,+orchid+ nil)
    (:basic-type ,+dark-blue+ nil)
    (:modifier ,+dark-green+ nil)
    (:comment ,+maroon+ nil)
    (:number ,+gray50+ nil)))

(defun face-colour (type)
  (first (cdr (assoc type *current-faces*))))

(defun face-style (type)
  (second (cdr (assoc type *current-faces*))))

(defmacro with-face ((face &optional (stream-symbol 'stream)) &body body)
  `(with-drawing-options (,stream-symbol :ink (face-colour ,face)
                                         :text-style (face-style ,face))
     ,@body))

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
        (tab-width (tab-width pane)))
    (with-sheet-medium (medium pane)
      (with-accessors ((cursor-positions cursor-positions)) (syntax buffer)
        (loop while (< start end)
           do (case (buffer-object buffer start)
                (#\Newline (record-line-vertical-offset pane (syntax buffer) (incf *current-line*))
                           (terpri pane)
                           (stream-increment-cursor-position
                            pane (first (aref cursor-positions 0)) 0))
                ((#\Page #\Return #\Space) (stream-increment-cursor-position
                                            pane space-width 0))
                (#\Tab (when (plusp tab-width)
                         (let ((x (stream-cursor-position pane)))
                           (stream-increment-cursor-position
                            pane (- tab-width (mod x tab-width)) 0)))))
           (incf start))))))

(defgeneric display-parse-tree (parse-symbol stream drei syntax)
  (:documentation "Display the given parse-symbol on the supplied
  stream, assuming `drei' to be the relevant Drei instance and
  `syntax' being the syntax object responsible for the parse
  symbol."))

(defmethod display-parse-tree ((parse-symbol (eql nil)) stream (drei drei)
                               (syntax java-syntax))
  nil)

(defmethod display-parse-tree :around (parse-symbol stream (drei drei)
                                                    (syntax java-syntax))
  (with-slots (top bot) drei
     (when (and (start-offset parse-symbol)
                (mark< (start-offset parse-symbol) bot)
                (mark> (end-offset parse-symbol) top))
       (call-next-method))))

(defmethod display-parse-tree (parse-symbol stream (drei drei)
                               (syntax java-syntax))
  (with-slots (top bot) drei
    (loop for child in (children parse-symbol)
       when (and (start-offset child)
                 (mark> (end-offset child) top))
         do (if (mark< (start-offset child) bot)
                (display-parse-tree child stream drei syntax)
                (return)))))

(defmethod display-parse-tree ((parse-symbol error-symbol) stream (drei drei)
                               (syntax java-syntax))
  (let ((children (children parse-symbol)))
    (loop until (or (null (cdr children))
		    (typep (parser-state (cadr children)) 'error-state))
	  do (display-parse-tree (pop children) stream drei syntax))
    (if (and (null (cdr children))
	     (not (typep (parser-state parse-symbol) 'error-state)))
	(display-parse-tree (car children) stream drei syntax)
	(with-face (:error)
	  (loop for child in children
		do (display-parse-tree child stream drei syntax))))))

(defmethod display-parse-tree ((parse-symbol error-lexeme) stream (drei drei) (syntax java-syntax))
  (with-face (:error)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol integer-literal-lexeme)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:number)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol floating-point-literal-lexeme)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:number)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol basic-type)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:basic-type)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol modifier)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:modifier)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol operator)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:operator)
    (call-next-method)))

(defmethod display-parse-tree ((parser-symbol java-lexeme) stream (drei drei)
                               (syntax java-syntax))
  (flet ((cache-test (t1 t2)
           (and (eq t1 t2)
                (eq (slot-value t1 'ink)
                    (medium-ink (sheet-medium stream)))
                (eq (slot-value t1 'face)
                    (text-style-face 
		     (medium-text-style (sheet-medium stream)))))))
    (updating-output
        (stream :unique-id (list drei parser-symbol)
                :id-test #'equal
                :cache-value parser-symbol
                :cache-test #'cache-test)
      (with-slots (ink face) parser-symbol
        (setf ink (medium-ink (sheet-medium stream))
              face (text-style-face (medium-text-style (sheet-medium stream))))
        (write-string (form-string syntax parser-symbol) stream)))))

(defmethod display-parse-tree :before ((parse-symbol java-lexeme) 
				       stream 
				       (drei drei)
                                       (syntax java-syntax))
  (handle-whitespace stream (buffer drei) 
		     *white-space-start* (start-offset parse-symbol))
  (setf *white-space-start* (end-offset parse-symbol)))

(defmethod display-parse-tree ((parse-symbol character-literal-lexeme)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:string)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol 
				incomplete-character-literal-lexeme)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:string)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol boolean-literal-lexeme)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:operator)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol null-literal-lexeme)
			       stream
			       (drei drei)
			       (syntax java-syntax))
  (with-face (:operator)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol complete-string-form) 
			       stream 
			       (drei drei) 
			       (syntax java-syntax))
  (let ((children (children parse-symbol)))
    (if (third children)
        (with-face (:string)
	  (display-parse-tree (pop children) stream drei syntax)
	  (loop until (null (cdr children))
		do (display-parse-tree (pop children) stream drei syntax))
	  (display-parse-tree (pop children) stream drei syntax))
        (with-face (:string)
	  (display-parse-tree (pop children) stream drei syntax)
	  (display-parse-tree (pop children) stream drei syntax)))))

(defmethod display-parse-tree ((parse-symbol incomplete-string-form) 
			       stream 
			       (drei drei) 
			       (syntax java-syntax))
  (let ((children (children parse-symbol)))
    (if (second children)
        (with-face (:string)
	  (display-parse-tree (pop children) stream drei syntax)
	  (loop until (null children)
		do (display-parse-tree (pop children) stream drei syntax)))
        (with-face (:string)
	  (display-parse-tree (pop children) stream drei syntax)))))

(defmethod display-parse-tree ((parse-symbol line-comment-form) 
			       stream 
			       (drei drei) 
			       (syntax java-syntax))
  (with-face (:comment)
    (call-next-method)))

(defmethod display-parse-tree ((parse-symbol long-comment-form) 
			       stream 
			       (drei drei) 
			       (syntax java-syntax))
  (with-face (:comment)
    (call-next-method)))

(defmethod display-drei-contents ((stream clim-stream-pane) 
				  (drei drei) 
				  (syntax java-syntax))
  (with-slots (top bot) drei
     (with-accessors ((cursor-positions cursor-positions)) syntax
       ;; There must always be room for at least one element of line
       ;; information.
       (setf cursor-positions (make-array (1+ 
					   (number-of-lines-in-region top bot))
					  :initial-element nil)
	     *current-line* 0
	     (aref cursor-positions 0) (multiple-value-list
					(stream-cursor-position stream))))
     (setf *white-space-start* (offset top)))
  (with-slots (stack-top) syntax
     (display-parse-tree stack-top stream drei syntax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse

(defun form-string-p (form)
  (typep form 'string-form))

(defun commentp (form)
  (typep form 'comment))

(defun top-level-vector (syntax)
  (coerce (children (slot-value syntax 'stack-top)) 'simple-vector))

(defun top-level-form-before-in-vector (tlv 
					offset 
					&optional ignore-comments-p)
  "Return top-level form in top-level-vector `tlv' around or before `offset'
together with index of form in `tlv', or nil. If `ignore-comments-p', don't 
treat comments as forms."
  (loop for count from (1- (length tlv)) downto 0
	for tlf = (aref tlv count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (< (start-offset tlf) offset (end-offset tlf)))
	  return (values tlf count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (<= (end-offset tlf) offset))
	  return (values tlf count)
	finally (return nil)))

(defun top-level-form-after-in-vector (tlv 
				       offset
				       &optional ignore-comments-p)
  "Return top-level form in top-level-vector `tlv' around or after `offset'
together with index of form in `tlv', or nil. If `ignore-comments-p', don't
treat comments as forms."
  (loop for tlf across tlv
	for count from 0
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (< (start-offset tlf) offset (end-offset tlf)))
	  return (values tlf count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (>= (start-offset tlf) offset))
	  return (values tlf count)
	finally (return nil)))

(defun top-level-form-around-in-vector (tlv 
					offset
					&optional ignore-comments-p)
  "Return top-level form in top-level-vector `tlv' around `offset'
together with index of form in `tlv', or nil. If `ignore-comments-p', don't
treat comments as forms."
  (loop for tlf across tlv
	for count from 0
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (< (start-offset tlf) offset (end-offset tlf)))
	  return (values tlf count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (>= (start-offset tlf) offset))
	  return nil
	finally (return nil)))

(defun form-around (syntax offset &optional ignore-comments-p)
  (top-level-form-around-in-vector
   (top-level-vector syntax)
   offset
   ignore-comments-p))

(defgeneric opening-delimiter-p (token)
  (:documentation "Is `token' an opening delimiter."))

(defmethod opening-delimiter-p (token)
  nil)

(defmethod opening-delimiter-p ((token opening-delimiter-mixin))
  t)

(defgeneric closing-delimiter-p (token)
  (:documentation "Is `token' a closing delimiter."))

(defmethod closing-delimiter-p (token)
  nil)

(defmethod closing-delimiter-p ((token closing-delimiter-mixin))
  t)

(defgeneric matching-delimiter-p (token match)
  (:documentation "Is `match' a matching delimiter of `token'."))

(defmethod matching-delimiter-p (token match)
  nil)

(defmethod matching-delimiter-p ((token closing-delimiter-mixin)
				 (match opening-delimiter-mixin))
  (matching-delimiter-p match token))

(defmethod matching-delimiter-p ((token left-parenthesis-lexeme)
				(match right-parenthesis-lexeme))
  t)

(defmethod matching-delimiter-p ((token left-bracket-lexeme)
				 (match right-bracket-lexeme))
  t)

(defmethod matching-delimiter-p ((token left-brace-lexeme)
				 (match right-brace-lexeme))
  t)

(defmethod backward-one-expression (mark (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark) t)
      (when form
	(if (closing-delimiter-p form)
	    (loop for index from count downto 0
		  for match = (aref tlv index)
		  with delims = 0
		  when (eql (class-of match)
			    (class-of form))
		    do (incf delims)
		  when (matching-delimiter-p form match)
		    do (decf delims)
		  until (zerop delims)
		  finally (cond ((zerop delims)
				 (setf (offset mark) (start-offset match))
				 (return t))
				(t (return nil))))
	    (setf (offset mark) (start-offset form)))))))

(defmethod forward-one-expression (mark (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark) t)
      (when form
	(if (opening-delimiter-p form)
	    (loop for index from count below (length tlv)
		  for match = (aref tlv index)
		  with delims = 0
		  when (eql (class-of match)
			    (class-of form))
		    do (incf delims)
		  when (matching-delimiter-p form match)
		    do (decf delims)
		  until (zerop delims)
		  finally (cond ((zerop delims)
				 (setf (offset mark) (end-offset match))
				 (return t))
				(t (return nil))))
	    (setf (offset mark) (end-offset form)))))))

(defgeneric forward-one-list (mark syntax)
  (:documentation "Move `mark' forward by one list.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-list (mark (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark))
      (when form
	(loop for index from count below (length tlv)
	      for match = (aref tlv index)
	      with delims = ()
	      when (opening-delimiter-p match)
		do (push match delims)
	      when (closing-delimiter-p match)
		do (cond ((null delims)
			  (return nil))
			 (t (cond ((matching-delimiter-p match 
							 (car delims))
				   (pop delims)
				   (when (null delims)
				     (setf (offset mark) (end-offset match))
				     (return t)))
				  (t (return nil)))))
	      finally (return nil))))))

(defgeneric backward-one-list (mark syntax)
  (:documentation "Move `mark' backward by one list.  Return T if
successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-list (mark (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark))
      (when form
	(loop for index from count downto 0
	      for match = (aref tlv index)
	      with delims = ()
	      when (closing-delimiter-p match)
		do (push match delims)
	      when (opening-delimiter-p match)
		do (cond 
		     ((null delims)
		      (return nil))
		     (t (cond ((matching-delimiter-p match 
						     (car delims))
			       (pop delims)
			       (when (null delims)
				 (setf (offset mark) (start-offset match))
				 (return t)))
			      (t (return nil))))) 
	      finally (return nil))))))

(drei-motion:define-motion-fns list)

(defmethod backward-one-down ((mark mark) (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark))
      (when form
	(loop for index from count downto 0
	      for match = (aref tlv index)
	      when (closing-delimiter-p match)
		do (setf (offset mark) (start-offset match))
		   (return t)
	      finally (return nil))))))

(defmethod backward-one-up (mark (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark))
      (when form
	(loop for index from count downto 0
	      for match = (aref tlv index)
	      with delims = ()
	      when (closing-delimiter-p match)
		do (push match delims)
	      when (opening-delimiter-p match)
		do (cond ((null delims)
			  (setf (offset mark) (start-offset match))
			  (return t))
			 ((matching-delimiter-p match 
						(car delims))
			  (pop delims))
			 (t (return nil)))
	      finally (return nil))))))

(defmethod forward-one-down ((mark mark) (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark))
      (when form
	(loop for index from count below (length tlv)
	      for match = (aref tlv index)
	      when (opening-delimiter-p match)
		do (setf (offset mark) (end-offset match))
		   (return t)
	      finally (return nil))))))

(defmethod forward-one-up (mark (syntax java-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark))
      (when form
	(loop for index from count below (length tlv)
	      for match = (aref tlv index)
	      with delims = ()
	      when (opening-delimiter-p match)
		do (push match delims)
	      when (closing-delimiter-p match)
		do (cond ((null delims)
			  (setf (offset mark) (end-offset match))
			  (return t))
			 ((matching-delimiter-p match 
						(car delims))
			  (pop delims))
			 (t (return nil)))
	      finally (return nil))))))

;; (defmethod backward-one-definition ((mark mark) (syntax java-syntax))
;;   )

;; (defmethod forward-one-definition ((mark mark) (syntax java-syntax))
;;   )

;;;# Indentation

(defun line-indentation (mark tab-width syntax)
  "Return the column of the first non-whitespace object, or nil."
  (setf mark (clone-mark mark))
  (beginning-of-line mark)
  (loop until (end-of-line-p mark)
	while (whitespacep syntax (object-after mark))
	with column = 0
	if (eql (object-after mark) #\Tab)
	  do (incf column (- tab-width (mod column tab-width)))
	else
	  do (incf column)
	do (forward-object mark)
	finally (return (if (end-of-line-p mark) nil column))))

(defmethod syntax-line-indentation (mark tab-width (syntax java-syntax))
  (setf mark (clone-mark mark))
  (let ((this-indentation (line-indentation mark tab-width syntax)))
    (beginning-of-line mark)
    (loop until (beginning-of-buffer-p mark)
	  do (previous-line mark 0)
	  when (line-indentation mark tab-width syntax)
	    return it
	  finally (return this-indentation))))

;;;# Commenting

(defmethod syntax-line-comment-string ((syntax java-syntax))
  "// ")

(defmethod comment-region ((syntax java-syntax) mark1 mark2)
  (line-comment-region syntax mark1 mark2))

(defmethod uncomment-region ((syntax java-syntax) mark1 mark2)
  (line-uncomment-region syntax mark1 mark2))

;; ;;; TESTING

;; (defun collect-forms (top)
;;   (loop for child in (children top)
;; 	collect (collect-forms child)
;; 	  into things
;; 	finally (return (cons top things))))

;; (define-command (com-dump-forms :name t :command-table java-table)
;;     ()
;;   "Dump the parse trees to trace output."
;;   (let* ((buffer (current-buffer))
;; 	 (syntax (syntax buffer)))
;;     (pprint (collect-forms (slot-value syntax 'stack-top)) *trace-output*)
;;     (terpri *trace-output*)
;;     (finish-output *trace-output*)))

;; (set-key 'com-dump-forms
;; 	 'java-table
;; 	 '((#\c :control) (#\c :control)))
