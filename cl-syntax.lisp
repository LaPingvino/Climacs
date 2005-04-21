;;; -*- Mode: Lisp; Package: COMMON-LISP-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;           Nada Ayad (nada.ayad@etu.u-bordeaux1.fr)
;;;           Julien Cazaban (bizounorc@hotmail.com)
;;;           Pascal Fong Kye (pfongkye@yahoo.com)
;;;           Bruno Mery (mery@member.fsf.org)
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

;;; Syntax for analysing Common Lisp

(in-package :climacs-cl-syntax)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar classes

(defclass cl-parse-tree (parse-tree) ())

(defclass cl-entry (cl-parse-tree)
  ((ink) (face)))  

(defclass cl-nonterminal (cl-entry) ())

(defclass cl-terminal (cl-entry) 
  ((item :initarg :item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass cl-lexeme (cl-entry)
  ((state :initarg :state)))
(defclass start-lexeme (cl-lexeme) ())
(defclass paren-open (cl-lexeme) ())
(defclass paren-close (cl-lexeme) ())
(defclass comma (cl-lexeme) ())
(defclass quote-symbol (cl-lexeme) ())
(defclass double-quote (cl-lexeme) ())
(defclass hex (cl-lexeme) ())
(defclass pipe (cl-lexeme) ())
(defclass semicolon (cl-lexeme) ())
(defclass backquote (cl-lexeme) ())
(defclass at (cl-lexeme) ())
(defclass default-item (cl-lexeme) ())


(defclass cl-lexer (incremental-lexer) ())

(defmethod next-lexeme ((lexer cl-lexer) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\( (fo) (make-instance 'paren-open))
	(#\) (fo) (make-instance 'paren-close))
	(#\, (fo) (make-instance 'comma))
	(#\" (fo) (make-instance 'double-quote))
	(#\' (fo) (make-instance 'quote-symbol))
	(#\# (fo) (make-instance 'hex))
	(#\| (fo) (make-instance 'pipe))
	(#\` (fo) (make-instance 'backquote))
	(#\@ (fo) (make-instance 'at))
	(#\; (fo) (make-instance 'semicolon))
	(t (cond ((numberp object) 
		  (loop until (end-of-buffer-p scan)
		     while (numberp (object-after scan))
		     do (fo))
		  (make-instance 'default-item))
		 ((neutralcharp object)
		  (loop until (end-of-buffer-p scan)
		     while (neutralcharp (object-after scan))
		     do (fo))
		  (make-instance 'default-item))
		 (t (fo) (make-instance 'default-item))))))))


(define-syntax cl-syntax ("Common-lisp" (basic-syntax))
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser)))



(defun neutralcharp (var)
  (and (characterp var)
       (not (member var '(#\( #\) #\, #\" #\' #\# #\| #\` #\@ #\; #\\
			  #\. #\+ #\-)
		    :test #'char=))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defparameter *cl-grammar* (grammar))

(defmacro add-cl-rule (rule)
  `(add-rule (grammar-rule ,rule) *cl-grammar*))

(defun default-item-is (default-item string)
  (string-equal (coerce (buffer-sequence (buffer default-item) (start-offset default-item) (end-offset default-item)) 'string)
		string))

(defmacro define-list (name empty-name nonempty-name item-name)
  `(progn
     (defclass ,name (cl-entry) ())
     (defclass ,empty-name (,name) ())
     
     (defclass ,nonempty-name (,name)
       ((items :initarg :items)
	(item :initarg :item)))
     
     (add-cl-rule (,name -> () (make-instance ',empty-name)))
     
     (add-cl-rule (,name -> (,name ,item-name)
			 (make-instance ',nonempty-name
					:items ,name :item ,item-name)))     
     (defmethod display-parse-tree ((entity ,empty-name) (syntax cl-syntax) pane)
       (declare (ignore pane))
       nil)
     
     (defmethod display-parse-tree ((entity ,nonempty-name) (syntax cl-syntax) pane)
       (with-slots (items item) entity
	  (display-parse-tree items syntax pane)
	  (display-parse-tree item syntax pane)))))


;;;;;; string-items

(defclass string-char (cl-entry)
  ((item :initarg :item)))

(add-cl-rule (string-char -> (default-item) :item default-item))
(add-cl-rule (string-char -> (paren-open) :item paren-open))
(add-cl-rule (string-char -> (paren-close) :item paren-close))
(add-cl-rule (string-char -> (comma) :item comma))
(add-cl-rule (string-char -> (semicolon) :item semicolon))
(add-cl-rule (string-char -> (backquote) :item backquote))
(add-cl-rule (string-char -> (at) :item at))

(defmethod display-parse-tree ((entity string-char) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))

(defclass string-part (cl-entry)
  ((item :initarg :item)
   (ch :initarg :ch)))

(add-cl-rule (string-part -> ((item string-part) (ch string-char (= (end-offset
								     item)
								    (start-offset
								     ch))))
			  :item item :ch ch))

(defmethod display-parse-tree ((entity string-part) (syntax cl-syntax) pane)
  (with-slots (item ch) entity
    (display-parse-tree item syntax pane)
    (display-parse-tree ch syntax pane)))

(defclass string-item (cl-entry)
  ((item :initarg :item)))

(add-cl-rule (string-item -> (string-char) :item string-char))
(add-cl-rule (string-item -> (string-part) :item string-part))

(defmethod display-parse-tree ((entity string-item) (syntax cl-syntax) pane)
  (with-slots (item) entity
     (display-parse-tree item syntax pane)))

(define-list string-items empty-string-items nonempty-string-items string-item)


(defclass identifier-item (cl-entry)
  ((item :initarg :item)))

(add-cl-rule (identifier-item -> (string-item) :item string-item))
(add-cl-rule (identifier-item -> (hex) :item hex)) 
(add-cl-rule (identifier-item -> (double-quote) :item double-quote))

(define-list identifier-items empty-identifier-items
  nonempty-identifier-items identifier-item)

(defmethod display-parse-tree ((entity identifier-item) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))

(defclass identifier-compound (cl-entry) 
  ((start :initarg :start)
   (items :initarg :items)
   (end :initarg :end)))

(add-cl-rule (identifier-compound -> ((start pipe) identifier-items
				      (end pipe))
				  :start start :items identifier-items
				  :end end))
(defmethod display-parse-tree ((entity identifier-compound) (syntax cl-syntax) pane)
  (with-slots (start items end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree items syntax pane)
    (display-parse-tree end syntax pane)))


(defclass identifier (cl-entry)
  ((item :initarg :item)))

(add-cl-rule (identifier -> (string-item) :item string-item)) 
(add-cl-rule (identifier -> (identifier-compound) :item identifier-compound))

(defmethod display-parse-tree ((entity identifier) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;line-comment

;;missing (cannot parse end of line)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;balanced-comment

(defclass balanced-comment (cl-entry)
  ((start-hex :initarg :start-hex)
   (items :initarg :items)
   (end-hex :initarg :end-hex)))

(add-cl-rule (balanced-comment -> ((start-hex hex)
				   (items identifier-compound)
				   (end-hex hex))
			       :start-hex start-hex
			       :items items
			       :end-hex end-hex))

(defmethod display-parse-tree ((entity balanced-comment) (syntax cl-syntax) pane)
  (with-slots (start-hex items end-hex) entity
    (with-drawing-options (pane :ink +blue+)
      (display-parse-tree start-hex syntax pane)
      (display-parse-tree items syntax pane)
      (display-parse-tree end-hex syntax pane)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;string

(defclass cl-string (cl-entry)
  ((string-start :initarg :string-start)
   (items :initarg :items)
   (string-end :initarg :string-end)))

(add-cl-rule (cl-string -> ((start double-quote) string-items (end double-quote))
			:string-start start :items string-items
			:string-end end))


(defmethod display-parse-tree ((entity cl-string) (syntax cl-syntax) pane)
  (with-slots (string-start items string-end) entity
    (with-drawing-options (pane :ink +orange+)
      (display-parse-tree string-start syntax pane)
      (display-parse-tree items syntax pane)
      (display-parse-tree string-end syntax pane))))

;;;;;;;;;;;;;;;;;;;;; #-type constants 

(defun radix-is (num-string radix)
  (values (parse-integer (coerce (buffer-sequence (buffer num-string)
						  (start-offset
						   num-string)
						  (end-offset
						   num-string)) 'string)
			 :radix radix :junk-allowed t)))

(defclass hexadecimal-expr (cl-entry)
  ((start :initarg :start)
   (header :initarg :header)
   (item :initarg :item)))

(add-cl-rule (hexadecimal-expr -> ((start hex)
				   (header default-item (default-item-is
							    header
							    #\x))
				   (item string-item (radix-is
						      item 16)))
			       :start start :header header :item
			       item))

(defmethod display-parse-tree ((entity hexadecimal-expr) (syntax cl-syntax) pane)
  (with-slots (start header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))

(defclass octal-expr (cl-entry)
  ((start :initarg :start)
   (header :initarg :header)
   (item :initarg :item)))

(add-cl-rule (octal-expr -> ((start hex)
			     (header default-item (default-item-is
						      header
						      #\o))
			     (item string-item (radix-is
						item 8)))
			 :start start :header header :item
			 item))

(defmethod display-parse-tree ((entity octal-expr) (syntax cl-syntax) pane)
  (with-slots (start header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))

(defclass start-number-expr (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))

(defclass binary-expr (cl-entry)
  ((start :initarg :start)
   (header :initarg :header)
   (item :initarg :item)))

(add-cl-rule (binary-expr -> ((start hex)
			      (header default-item (default-item-is
						       header
						       #\b))
			      (item string-item (radix-is
						 item 2)))
			  :start start :header header :item
			  item))

(defmethod display-parse-tree ((entity binary-expr) (syntax cl-syntax) pane)
  (with-slots (start header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))

(defclass radix-n-expr (cl-entry)
  ((start :initarg :start)
   (radix :initarg :radix)
   (header :initarg :header)
   (item :initarg :item)))

(add-cl-rule (radix-n-expr -> ((start hex)
			       (radix string-item (radix-is radix 10))
			       (header default-item (default-item-is header #\r))
			       (item string-item (radix-is item (second
								 (multiple-value-list
								  (parse-integer (coerce
										  (buffer-sequence (buffer radix)
												   (start-offset radix)
												   (end-offset radix))
										  'string)))))))
			   :start start :header header :item item))

(defmethod display-parse-tree ((entity radix-n-expr) (syntax cl-syntax) pane)
  (with-slots (start radix header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree radix syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))

(defclass simple-number (cl-entry)
  ((content :initarg :content)))

(add-cl-rule (simple-number -> ((content string-item (radix-is
						      content 10)))
			    :content content))

(defmethod display-parse-tree ((entity simple-number) (syntax cl-syntax) pane)
  (with-slots (content) entity
    (display-parse-tree content syntax pane)))

(defclass complex-number (cl-entry)
  ((start :initarg :start)
   (realpart :initarg :realpart)
   (imagpart :initarg :imagpart)
   (end :initarg :end)))

(add-cl-rule (complex-number -> ((start paren-open)
				 (realpart simple-number)
				 (imagpart simple-number (>
							  (end-offset
							   realpart)
							  (start-offset imagpart)))
				 (end paren-close))
			     :start start :realpart realpart :imagpart
			     imagpart :end end))

(defmethod display-parse-tree ((entity complex-number) (syntax cl-syntax) pane)
  (with-slots (start realpart imagpart end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree realpart syntax pane)
    (display-parse-tree imagpart syntax pane)
    (display-parse-tree end syntax pane)))

(defclass complex-expr (cl-entry)
  ((start :initarg :start)
   (header :initarg :header)
   (item :initarg :item)))

(add-cl-rule (complex-expr -> ((start hex)
			       (header default-item (default-item-is
							header
							#\c))
			       (item complex-number))
			   :start start :header header :item
			   item))

(defmethod display-parse-tree ((entity complex-expr) (syntax cl-syntax) pane)
  (with-slots (start header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))

(defclass number-expr (cl-entry)
  ((content :initarg :content)))

(add-cl-rule (number-expr -> ((item simple-number)) :content item))
(add-cl-rule (number-expr -> ((item binary-expr)) :content item))
(add-cl-rule (number-expr -> ((item octal-expr)) :content item))
(add-cl-rule (number-expr -> ((item hexadecimal-expr)) :content item))
(add-cl-rule (number-expr -> ((item radix-n-expr)) :content item))
(add-cl-rule (number-expr -> ((item complex-expr)) :content item))

(defmethod display-parse-tree ((entity number-expr) (syntax cl-syntax) pane)
  (with-slots (content) entity
    (with-drawing-options (pane :ink +blue+)
      (display-parse-tree content syntax pane))))

(defclass pathname-expr (cl-entry)
  ((start :initarg :start)
   (header :initarg :header)
   (item :initarg :item)))

(add-cl-rule (pathname-expr -> ((start hex)
				(header default-item (default-item-is header #\p))
				(item string-item))
			    :start start :header header :item item))

(defmethod display-parse-tree ((entity pathname-expr) (syntax cl-syntax) pane)
  (with-slots (start header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;characters

(defclass char-item (cl-entry)
  ((start :initarg :start)
   (backslash :initarg :backslash)
   (item :initarg :item)))

(add-cl-rule (char-item -> ((start hex)
			    (backslash default-item (and (= (end-offset start)    
							    (start-offset backslash)) 
							 (default-item-is backslash #\\))) 
			    (item cl-lexeme (= (end-offset backslash)
					       (start-offset item)))) 
			:start start :backslash backslash :item item))

(defmethod display-parse-tree ((entity char-item) (syntax cl-syntax) pane)
  (with-slots (start backslash item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree backslash syntax pane)
    (display-parse-tree item syntax pane))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;list-expression
(defclass list-expr (cl-entry)
  ((start :initarg :start)
   (items :initarg :items)
   (end :initarg :end)))

(add-cl-rule (list-expr -> ((start paren-open) cl-terminals (end paren-close))
			:start start :items cl-terminals
			:end end))

(defmethod display-parse-tree ((entity list-expr) (syntax cl-syntax) pane)
  (with-slots (start items end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree items syntax pane)
    (display-parse-tree end syntax pane)))


;;;;;;;;;;;;; read-time-point-attr

(defclass read-time-point-attr (cl-entry) 
  ((read-car :initarg :read-car)
   (read-expr :initarg :read-expr)))

(add-cl-rule (read-time-point-attr -> ((read-car default-item (default-item-is read-car #\.))
				       (read-expr identifier (= (end-offset read-car) (start-offset read-expr))))
				   :read-car read-car :read-expr read-expr))


(defmethod display-parse-tree ((entity read-time-point-attr) (syntax cl-syntax) pane)
  (with-slots (read-car read-expr) entity
    (display-parse-tree read-car syntax pane)
    (display-parse-tree read-expr syntax pane)))

;;;;;;;;;;;;; read-time-evaluation

(defclass read-time-evaluation (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))


(add-cl-rule (read-time-evaluation -> ((start hex) 
				       (item read-time-point-attr (= (end-offset start) (start-offset item))))
				   :start start :item item))

(defmethod display-parse-tree ((entity read-time-evaluation) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree item syntax pane)))


;;;;;;;;;;;;;;;;;;;;;;; read-time-attr

(defclass read-time-attr (cl-entry)
  ((read-car :initarg :read-car)
   (read-expr :initarg :read-expr)))

(defmethod display-parse-tree ((entity read-time-attr) (syntax cl-syntax) pane)
  (with-slots (read-car read-expr) entity
    (display-parse-tree read-car syntax pane)
    (display-parse-tree read-expr syntax pane)))


;;;;;;;;;;;;;; read-time-plus-attr

(defclass read-time-plus-attr (read-time-attr) ()) 

(add-cl-rule (read-time-plus-attr -> ((read-car default-item (default-item-is read-car #\+))
				      (read-expr read-time-expr (= (end-offset read-car) (start-offset read-expr))))
				  :read-car read-car :read-expr
				  read-expr))

;;;;;;;;;;;;;; read-time-minus-attr

(defclass read-time-minus-attr (read-time-attr) ()) 

(add-cl-rule (read-time-minus-attr -> ((read-car default-item (default-item-is read-car #\-))
				       (read-expr read-time-expr (= (end-offset read-car) (start-offset read-expr))))
				   :read-car read-car :read-expr
				   read-expr))

;;;;;;;;;;;;; read-time-expr

(defclass read-time-expr (cl-entry) 
  ((time-expr :initarg :time-expr)))

(add-cl-rule (read-time-expr -> (list-expr) :time-expr list-expr)) 

(add-cl-rule (read-time-expr -> (identifier) :time-expr identifier))


(defmethod display-parse-tree ((entity read-time-expr) (syntax cl-syntax) pane)
  (with-slots (time-expr) entity
    (display-parse-tree time-expr syntax pane)))


;;;;;;;;;;;;;;;;;;;;;;;;;; read-time-conditional
(defclass read-time-conditional (cl-entry)
  ((start :initarg :start)
   (test :initarg :test)
   (expr :initarg :expr)))


(defmethod display-parse-tree ((entity read-time-conditional) (syntax cl-syntax) pane)
  (with-slots (start test expr) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree test syntax pane)
    (display-parse-tree expr syntax pane)))


;;;;;;;;;;;;; read-time-conditional-plus

(defclass read-time-conditional-plus (read-time-conditional) ())
  

(add-cl-rule (read-time-conditional-plus -> ((start hex) 
					     (test read-time-plus-attr (= (end-offset start) (start-offset test)))
					     (expr cl-terminal (/= (end-offset test) (start-offset expr))))
					 :start start :test test :expr expr))

;;;;;;;;;;;;; read-time-conditional-minus

(defclass read-time-conditional-minus (read-time-conditional) ())

(add-cl-rule (read-time-conditional-minus -> ((start hex) 
					      (test read-time-minus-attr (= (end-offset start) (start-offset test)))
					      (expr cl-terminal (/= (end-offset test) (start-offset expr))))
					  :start start :test test :expr expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;function-expression

(defclass fun-expr (cl-entry) 
  ((start :initarg :start)
   (quoted-expr :initarg :quoted-expr)))

(add-cl-rule (fun-expr -> ((start hex)
			   (quoted-expr quoted-expr))
		       :start start :quoted-expr quoted-expr))

(defmethod display-parse-tree ((entity fun-expr) (syntax cl-syntax) pane)
  (with-slots (start quoted-expr) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree quoted-expr syntax pane)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;vector-expression

(defclass vect-expr (cl-entry)
  ((start :initarg :start)
   (list-expr :initarg :list-expr)))

(add-cl-rule (vect-expr -> ((start hex)
			    (list-expr list-expr))
			:start start :list-expr list-expr))

(defmethod display-parse-tree ((entity vect-expr) (syntax cl-syntax) pane)
  (with-slots (start list-expr) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree list-expr syntax pane)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;array-expression

(defclass array-expr (cl-entry) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bitvector-expression

(defclass bit-item (cl-entry)
  ((item :initarg :item)))

(add-cl-rule (bit-item -> ((item string-item (radix-is item 2)))
		       :item item))

(define-list bit-items empty-bit-items nonempty-bit-items bit-item)

(defclass bitvect-expr (cl-nonterminal)
  ((start :initarg :start)
   (asterisk :initarg :asterisk)
   (items :initarg :items)))

(add-cl-rule (bitvect-expr -> ((start hex)
			       (asterisk default-item (and (= (end-offset start)    
							      (start-offset asterisk)) 
							   (default-item-is asterisk #\*))) 
			       (items bit-items))
			   :start start :asterisk asterisk :items items))

(defmethod display-parse-tree ((entity bitvect-expr) (syntax cl-syntax) pane)
  (with-slots (start asterisk items) entity
    (with-drawing-options (pane :ink +brown+)
      (display-parse-tree start syntax pane)
      (display-parse-tree asterisk syntax pane)
      (display-parse-tree items syntax pane))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quote expr
(defclass quoted-expr  (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))

(add-cl-rule (quoted-expr -> ((start quote-symbol) 
			      (item cl-terminal))
			  :start start :item item))

(defmethod display-parse-tree ((entity quoted-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree item syntax pane))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Backquoted expr
(defclass backquoted-expr (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))

(add-cl-rule (backquoted-expr -> ((start backquote)
				  (item cl-terminal))
			      :start start :item item)) 
(add-cl-rule (backquoted-expr -> ((start backquote)
				  (item unquoted-expr))
			      :start start :item item)) 

(defmethod display-parse-tree ((entity backquoted-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree item syntax pane))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;unquoted expr


(defclass unquoted-item (cl-entry)
  ((start :initarg :start)
   (end :initarg :end)))

(add-cl-rule (unquoted-item -> ((start comma)
				(end at (= (end-offset start)
					   (start-offset end))))
			    :start start :end end))

(defmethod display-parse-tree ((entity unquoted-item) (syntax cl-syntax) pane)
  (with-slots (start end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree end syntax pane))) 


(defclass unquoted-expr (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))

(add-cl-rule (unquoted-expr -> ((start comma)
				(item identifier))
			    :start start :item item))
(add-cl-rule (unquoted-expr -> ((start comma)
				(item list-expr))
			    :start start :item item))

(add-cl-rule (unquoted-expr -> ((start unquoted-item)
				(item identifier))
			    :start start :item item))
(add-cl-rule (unquoted-expr -> ((start unquoted-item)
				(item list-expr))
			    :start start :item item))

(defmethod display-parse-tree ((entity unquoted-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree item syntax pane))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;cl-terminal

(add-cl-rule (cl-terminal -> (identifier) :item identifier))
(add-cl-rule (cl-terminal -> (balanced-comment) :item balanced-comment))
(add-cl-rule (cl-terminal -> (cl-string) :item cl-string))
(add-cl-rule (cl-terminal -> (quoted-expr) :item quoted-expr))
(add-cl-rule (cl-terminal -> (backquoted-expr) :item backquoted-expr))
(add-cl-rule (cl-terminal -> (char-item) :item char-item))
(add-cl-rule (cl-terminal -> (unquoted-expr) :item unquoted-expr))
(add-cl-rule (cl-terminal -> (list-expr) :item list-expr))
(add-cl-rule (cl-terminal -> (fun-expr) :item fun-expr))
(add-cl-rule (cl-terminal -> (vect-expr) :item vect-expr))
(add-cl-rule (cl-terminal -> (bitvect-expr) :item bitvect-expr))
(add-cl-rule (cl-terminal -> (number-expr) :item number-expr))
(add-cl-rule (cl-terminal -> (pathname-expr) :item pathname-expr))
(add-cl-rule (cl-terminal -> (read-time-conditional-plus) :item read-time-conditional-plus))
(add-cl-rule (cl-terminal -> (read-time-conditional-minus) :item read-time-conditional-minus))
(add-cl-rule (cl-terminal -> (read-time-evaluation) :item read-time-evaluation))

(define-list cl-terminals empty-cl-terminals
  nonempty-cl-terminals cl-terminal)

(defmethod display-parse-tree ((entity cl-terminal) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((syntax cl-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser lexer buffer) syntax
    (setf parser (make-instance 'parser
				:grammar *cl-grammar*
				:target 'cl-terminals))
    (setf lexer (make-instance 'cl-lexer :buffer (buffer syntax)))
    (let ((m (clone-mark (low-mark buffer) :left))
	   (lexeme (make-instance 'start-lexeme :state (initial-state parser))))
      (setf (offset m) 0)
      (setf (start-offset lexeme) m
	    (end-offset lexeme) 0)
      (insert-lexeme lexer 0 lexeme))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax


(defmethod update-syntax-for-display (buffer (syntax cl-syntax) top bot)
  (with-slots (parser lexer valid-parse) syntax
    (loop until (= valid-parse (nb-lexemes lexer))
       while (mark<= (end-offset (lexeme lexer valid-parse)) bot)
       do (let ((current-token (lexeme lexer (1- valid-parse)))
		(next-lexeme (lexeme lexer valid-parse)))
	    (setf (slot-value next-lexeme 'state)
		  (advance-parse parser (list next-lexeme) (slot-value current-token 'state))))
	 (incf valid-parse))))

(defmethod inter-lexeme-object-p ((lexer cl-lexer) object)
  (whitespacep object))

(defmethod update-syntax (buffer (syntax cl-syntax))
  (with-slots (lexer valid-parse) syntax
    (let* ((low-mark (low-mark buffer))
	   (high-mark (high-mark buffer)))
       (when (mark<= low-mark high-mark)
	 (let ((first-invalid-position (delete-invalid-lexemes lexer low-mark high-mark)))
	   (setf valid-parse first-invalid-position)
	   (update-lex lexer first-invalid-position high-mark))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
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

(defmethod display-parse-tree :around ((entity cl-parse-tree) syntax pane)
  (with-slots (top bot) pane
    (when (and (end-offset entity) (mark> (end-offset entity) top))
      (call-next-method))))

(defmethod display-parse-tree ((entity cl-entry) (syntax cl-syntax) pane)
  (flet ((cache-test (t1 t2)
	   (and (eq t1 t2)
		(eq (slot-value t1 'ink)
		    (medium-ink (sheet-medium pane)))
		(eq (slot-value t1 'face)
		    (text-style-face (medium-text-style (sheet-medium pane)))))))
    (updating-output (pane :unique-id entity
			   :id-test #'eq
			   :cache-value entity
			   :cache-test #'cache-test)
		     (with-slots (ink face) entity
		       (setf ink (medium-ink (sheet-medium pane))
			     face (text-style-face (medium-text-style (sheet-medium pane))))
		       (present (coerce (buffer-sequence (buffer syntax)
							 (start-offset entity)
							 (end-offset entity))
					'string)
				'string
				:stream pane)))))

(defmethod display-parse-tree :before ((entity cl-entry) (syntax cl-syntax) pane)
  (handle-whitespace pane (buffer pane) *white-space-start* (start-offset entity))
  (setf *white-space-start* (end-offset entity)))

(defgeneric display-parse-stack (symbol stack syntax pane))

(defmethod display-parse-stack (symbol stack (syntax cl-syntax) pane)
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


(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax cl-syntax) current-p)
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



