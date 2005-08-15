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
  ((ink) (face)
  (state :initarg :state)))

(defclass cl-nonterminal (cl-entry) ())

(defclass cl-terminal (cl-entry) 
  ((item :initarg :item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass cl-lexeme (cl-entry) ())

(defclass start-lexeme (cl-lexeme) ())
(defclass paren-open (cl-lexeme) ())
(defclass paren-close (cl-lexeme) ())
(defclass comma (cl-lexeme) ())
(defclass quote-symbol (cl-lexeme) ())
(defclass colon (cl-lexeme) ())
(defclass ampersand (cl-lexeme) ())
(defclass double-quote (cl-lexeme) ())
(defclass hex (cl-lexeme) ())
(defclass pipe (cl-lexeme) ())
(defclass line-comment-entry (cl-lexeme) ())
(defclass backquote (cl-lexeme) ())
(defclass at (cl-lexeme) ())
(defclass backslash (cl-lexeme) ())
(defclass slash (cl-lexeme) ())
(defclass dot (cl-lexeme) ())
(defclass plus-symbol (cl-lexeme) ())
(defclass minus-symbol (cl-lexeme) ())
(defclass default-item (cl-lexeme) ())
(defclass other-entry (cl-lexeme) ())

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
	(#\: (fo) (make-instance 'colon))
        (#\& (fo) (make-instance 'ampersand))
	(#\# (fo) (make-instance 'hex))
	(#\| (fo) (make-instance 'pipe))
	(#\` (fo) (make-instance 'backquote))
	(#\@ (fo) (make-instance 'at))
	(#\\ (fo) (make-instance 'backslash))
	(#\/ (fo) (make-instance 'slash))
	(#\. (fo) (make-instance 'dot))
	(#\+ (fo) (make-instance 'plus-symbol))
	(#\- (fo) (make-instance 'minus-symbol))
	(#\; (fo) (loop until (end-of-buffer-p scan)
		        until (eql (object-after scan) #\Newline)
		     do (fo))
	     (if (end-of-buffer-p scan)
		 (make-instance 'other-entry) 
		 (make-instance 'line-comment-entry)))
	(t (cond ((digit-char-p object) 
		  (loop until (end-of-buffer-p scan)
		     while (digit-char-p (object-after scan))
		     do (fo))
		  (make-instance 'default-item))
		 ((neutralcharp object)
		  (loop until (end-of-buffer-p scan)
		     while (neutralcharp (object-after scan))
		     do (fo))
		  (make-instance 'default-item))
		 (t (fo)
		    (make-instance 'other-entry))))))))


(define-syntax cl-syntax (basic-syntax)
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser))
  (:name "Common Lisp")
  (:pathname-types "lisp" "lsp" "cl"))

(defun neutralcharp (var)
  (and (characterp var)
       (not (member var '(#\( #\) #\, #\" #\' #\# #\| #\` #\@ #\; #\\
			  #\: #\/ #\Newline #\Space #\Tab)
		    :test #'char=))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parser

(defparameter *cl-grammar* (grammar))

(defmacro add-cl-rule (rule)
  `(add-rule (grammar-rule ,rule) *cl-grammar*))

(defun item-sequence (item)
  (buffer-sequence (buffer item) (start-offset item) (end-offset item)))

(defun default-item-is (default-item string)
  (string-equal (coerce (item-sequence default-item) 'string)
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


;;;;;;;;;;;;; token-items

(defclass empty-item (cl-entry) ())

(defmethod display-parse-tree ((entity empty-item) (syntax cl-syntax) pane)
  (declare (ignore pane))
  nil)

(defclass cl-item (cl-entry) 
  ((item :initarg :item))) 

(defclass token-char (cl-item) ())

(add-cl-rule (token-char -> (default-item) :item default-item))
(add-cl-rule (token-char -> (comma) :item comma))
(add-cl-rule (token-char -> (backquote) :item backquote))
(add-cl-rule (token-char -> (at) :item at))
(add-cl-rule (token-char -> (plus-symbol) :item plus-symbol))
(add-cl-rule (token-char -> (minus-symbol) :item minus-symbol))
(add-cl-rule (token-char -> (pipe) :item pipe))

(defmethod display-parse-tree ((entity token-char) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))

(defclass token-item (cl-entry)
  ((item :initarg :item)
   (ch :initarg :ch)))

(add-cl-rule (token-item -> ((ch token-char (or (alpha-char-p (coerce (item-head ch) 'character))
						(member (item-head ch) '(#\= #\* #\+ #\> #\<) :test #'string-equal)
						(member ch '(#\/ #\+ #\- #\*)
							:test #'default-item-is))))
			 :item (make-instance 'empty-item) :ch ch))

(add-cl-rule (token-item -> ((item token-item) (ch token-char (= (end-offset
								  item)
								 (start-offset
								  ch))))
			 :item item :ch ch))

(defmethod display-parse-tree ((entity token-item) (syntax cl-syntax) pane)
  (with-slots (item ch) entity
    (display-parse-tree item syntax pane)
    (display-parse-tree ch syntax pane)))

(define-list token-items empty-token-items nonempty-token-items token-item)


;;;;;;;;;;;;; string-items

(defclass string-item (cl-item) ())

(add-cl-rule (string-item -> (token-item) :item token-item))
(add-cl-rule (string-item -> (default-item) :item default-item))
(add-cl-rule (string-item -> (paren-open) :item paren-open))
(add-cl-rule (string-item -> (paren-close) :item paren-close))
(add-cl-rule (string-item -> (hex) :item hex))
(add-cl-rule (string-item -> (backslash) :item backslash))
(add-cl-rule (string-item -> (slash) :item slash))
(add-cl-rule (string-item -> (dot) :item dot))
(add-cl-rule (string-item -> (line-comment-entry) :item line-comment-entry))


(define-list string-items empty-string-items
  nonempty-string-items string-item)

(defmethod display-parse-tree ((entity string-item) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass identifier-item (cl-item) ())

(add-cl-rule (identifier-item -> (string-item) :item string-item))
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


(defclass identifier (cl-item) ())

(add-cl-rule (identifier -> ((item token-item))
			 :item item))

(add-cl-rule (identifier -> ((item slash))
			 :item item))

(add-cl-rule (identifier -> (identifier-compound) :item identifier-compound))

(defmethod display-parse-tree ((entity identifier) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))

;;;;;;;;;;;;; line-comment

(defclass line-comment (cl-item) ())

(add-cl-rule (line-comment -> ((item line-comment-entry)) :item item))

(defmethod display-parse-tree ((entity line-comment) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.6 0.16 0.3))
      (display-parse-tree item syntax pane))))

;;;;;;;;;;;;; balanced-comment

(defclass balanced-comment (cl-entry)
  ((start-hex :initarg :start-hex)
   (start-pipe :initarg :start-pipe)
   (item :initarg :item)
   (end-pipe :initarg :end-pipe)
   (end-hex :initarg :end-hex)))

(add-cl-rule (balanced-comment -> ((start-hex hex)
				   (start-pipe pipe (= (end-offset
							start-hex)
						       (start-offset start-pipe)))
				   (item identifier-items)
				   (end-pipe pipe)
				   (end-hex hex (= (end-offset end-pipe)
						   (start-offset end-hex))))
			       :start-hex start-hex
			       :start-pipe start-pipe
			       :item item
			       :end-pipe end-pipe
			       :end-hex end-hex))

(defmethod display-parse-tree ((entity balanced-comment) (syntax cl-syntax) pane)
  (with-slots (start-hex start-pipe item end-pipe end-hex) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.6 0.16 0.3))
      (display-parse-tree start-hex syntax pane)
      (display-parse-tree start-pipe syntax pane)
      (display-parse-tree item syntax pane)
      (display-parse-tree end-pipe syntax pane)
      (display-parse-tree end-hex syntax pane)))) 

;;;;;;;;;;;;; string

(defclass cl-string (cl-entry)
  ((string-start :initarg :string-start)
   (items :initarg :items)
   (string-end :initarg :string-end)))

(add-cl-rule (cl-string -> ((start double-quote) string-items (end double-quote))
			:string-start start :items string-items
			:string-end end))


(defmethod display-parse-tree ((entity cl-string) (syntax cl-syntax) pane)
  (with-slots (string-start items string-end) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.6 0.4 0.2))
      (display-parse-tree string-start syntax pane)
      (display-parse-tree items syntax pane)
      (display-parse-tree string-end syntax pane))))


;;;;;;;;;;;;;;;;;;;;; #-type constants 

(defun item-head (default-item)
  (coerce (buffer-sequence (buffer default-item) 
			   (start-offset default-item) 
			   (1+ (start-offset default-item))) 'string))

(defun item-tail (default-item)
  (coerce (buffer-sequence (buffer default-item) 
			   (1+ (start-offset default-item))
			   (end-offset default-item)) 'string))

(defun radix-is (num-string radix)
  (values (ignore-errors
	    (parse-integer num-string :radix radix :junk-allowed 'nil))))

(defclass radix-expr (cl-entry)
  ((start :initarg :start)
   (item :initarg :item))) 

(defmethod display-parse-tree ((entity radix-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree item syntax pane)))

(defclass hexadecimal-expr (radix-expr) ())

(add-cl-rule (hexadecimal-expr -> ((start hex)
				   (item token-item
					 (and (= (end-offset start)
						 (start-offset item))
					      (string-equal (item-head item) #\x)
					      (radix-is (item-tail item) 16))))
			       :start start :item item))

(defclass octal-expr (radix-expr) ())

(add-cl-rule (octal-expr -> ((start hex)
			     (item default-item
				   (and (= (end-offset start)
					   (start-offset item))
					(string-equal (item-head item) #\o)
					(radix-is (item-tail item) 8))))
			 :start start :item item))

(defclass binary-expr (radix-expr) ())

(add-cl-rule (binary-expr -> ((start hex)
			      (item default-item
				    (and (= (end-offset start)
					    (start-offset item))
					 (string-equal (item-head item) #\b)
					 (radix-is (item-tail
						    item) 2))))
			  :start start :item item))

(defclass radix-n-expr (cl-entry)
  ((start :initarg :start)
   (radix :initarg :radix)
   (item :initarg :item)))

(add-cl-rule (radix-n-expr -> ((start hex)
			       (radix simple-number (= (end-offset start)
						       (start-offset radix)))
			       (item default-item (and (= (end-offset radix)
							  (start-offset item))
						       (string-equal
							(item-head item) #\r)
						       (radix-is
							(item-tail item)
							(values (parse-integer (coerce
										(item-sequence radix) 'string)))))))
						       
			   :start start :radix radix :item item))

(defmethod display-parse-tree ((entity radix-n-expr) (syntax cl-syntax) pane)
  (with-slots (start radix item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree radix syntax pane)
    (display-parse-tree item syntax pane)))

(defclass simple-number (cl-item) ())

(add-cl-rule (simple-number -> ((item default-item (radix-is
						      (coerce
						       (item-sequence  item) 'string) 10)))
			    :item item))

(defmethod display-parse-tree ((entity simple-number) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (display-parse-tree item syntax pane)))


(defclass real-number (cl-entry)
  ((primary :initarg :primary)
   (separator :initarg :separator)
   (secondary :initarg :secondary)))

(add-cl-rule (real-number -> ((primary simple-number)
			      (separator slash (= (end-offset primary)
						  (start-offset separator)))
			      (secondary simple-number (= (end-offset
							   separator)
							  (start-offset secondary))))
			  :primary primary :separator separator
			  :secondary secondary))

(add-cl-rule (real-number -> ((primary simple-number)
			      (separator dot (= (end-offset primary)
						(start-offset separator)))
			      (secondary simple-number (= (end-offset
							   separator)
							  (start-offset secondary))))
			  :primary primary :separator separator
			  :secondary secondary))

(defmethod display-parse-tree ((entity real-number) (syntax cl-syntax) pane)
  (with-slots (primary secondary separator) entity
    (display-parse-tree primary syntax pane)
    (display-parse-tree separator syntax pane)
    (display-parse-tree secondary syntax pane)))


(defclass complex-number (cl-entry)
  ((start :initarg :start)
   (realpart :initarg :realpart)
   (imagpart :initarg :imagpart)
   (end :initarg :end)))

(add-cl-rule (complex-number -> ((start paren-open)
				 (realpart real-number)
				 (imagpart real-number (/=
							  (end-offset
							   realpart)
							  (start-offset imagpart)))
				 (end paren-close))
			     :start start :realpart realpart :imagpart
			     imagpart :end end))

(add-cl-rule (complex-number -> ((start paren-open)
				 (realpart simple-number)
				 (imagpart simple-number (/=
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
			       (header default-item (and (default-item-is
							     header #\c)
							 (= (end-offset start)
							    (start-offset header))))
			       (item complex-number (= (end-offset header)
						       (start-offset item))))
			   :start start :header header :item
			   item))

(defmethod display-parse-tree ((entity complex-expr) (syntax cl-syntax) pane)
  (with-slots (start header item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree header syntax pane)
    (display-parse-tree item syntax pane)))

(defclass number-expr (cl-item) ())

(add-cl-rule (number-expr -> ((item simple-number)) :item item))
(add-cl-rule (number-expr -> ((item real-number)) :item item))
(add-cl-rule (number-expr -> ((item binary-expr)) :item item))
(add-cl-rule (number-expr -> ((item octal-expr)) :item item))
(add-cl-rule (number-expr -> ((item hexadecimal-expr)) :item item))
(add-cl-rule (number-expr -> ((item radix-n-expr)) :item item))
(add-cl-rule (number-expr -> ((item complex-expr)) :item item))

(defmethod display-parse-tree ((entity number-expr) (syntax cl-syntax) pane)
  (with-slots (item) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.14 0.0 0.86))
      (display-parse-tree item syntax pane))))

(defclass pathname-expr (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))

(add-cl-rule (pathname-expr -> ((start hex)
				(item default-item (and (string-equal
							 (item-head item) #\p)
							(= (end-offset start)
							   (start-offset item)))))
			    :start start :item item))

(defmethod display-parse-tree ((entity pathname-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree item syntax pane)))


;;;;;;;;;;;;; characters

(defclass char-item (cl-entry)
  ((start :initarg :start)
   (separator :initarg :separator)
   (item :initarg :item)))

(add-cl-rule (char-item -> ((start hex)
			    (separator backslash (= (end-offset start)    
						    (start-offset separator))) 
			    (item cl-lexeme (and (= (end-offset separator)
						    (start-offset item))
						 (= (end-offset item)
						    (1+ (start-offset item)))))) 
			:start start :separator separator :item item))

(add-cl-rule (char-item -> ((start hex)
			    (separator backslash (= (end-offset start)    
						    (start-offset separator))) 
			    (item default-item (and (= (end-offset separator)
						       (start-offset item))
						    (member item
							    '("Newline" "Tab" "Space") :test #'default-item-is))))
			:start start :separator separator :item item))

(defmethod display-parse-tree ((entity char-item) (syntax cl-syntax) pane)
  (with-slots (start separator item) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.14 0.0 0.86))
      (display-parse-tree start syntax pane)
      (display-parse-tree separator syntax pane)
      (display-parse-tree item syntax pane))))


;;;;;;;;;;;;; list-expression

(defclass list-expr (cl-entry)
  ((start :initarg :start)
   (items :initarg :items)
   (end :initarg :end)))

(add-cl-rule (list-expr -> ((start paren-open)
			    (items cl-terminals)
			    (end paren-close))
			:start start :items items :end end))

(defmethod display-parse-tree ((entity list-expr) (syntax cl-syntax) pane)
  (with-slots (start items end) entity
    (with-text-face (pane :bold)
      (display-parse-tree start syntax pane))
      (display-parse-tree items syntax pane)
    (with-text-face (pane :bold)
      (display-parse-tree end syntax pane))))


;;;;;;;;;;;;; read-time-attr

(defclass read-time-attr (cl-entry)
  ((read-car :initarg :read-car)
   (read-expr :initarg :read-expr)))

(defmethod display-parse-tree ((entity read-time-attr) (syntax cl-syntax) pane)
  (with-slots (read-car read-expr) entity
    (display-parse-tree read-car syntax pane)
    (display-parse-tree read-expr syntax pane)))


;;;;;;;;;;;;; read-time-point-attr

(defclass read-time-point-attr (read-time-attr) ()) 
 
(add-cl-rule (read-time-point-attr -> ((read-car dot)
				       (read-expr identifier (= (end-offset read-car) (start-offset read-expr))))
				   :read-car read-car :read-expr read-expr))


;;;;;;;;;;;;; read-time-evaluation

(defclass read-time-evaluation (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))


(add-cl-rule (read-time-evaluation -> ((start hex) 
				       (item read-time-point-attr (= (end-offset start) (start-offset item))))
				   :start start :item item))

(defmethod display-parse-tree ((entity read-time-evaluation) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.0 0.42 0.42))
      (display-parse-tree start syntax pane)
      (display-parse-tree item syntax pane))))


;;;;;;;;;;;;; read-time-expr

(defclass read-time-expr (cl-entry) 
  ((time-expr :initarg :time-expr)))

(add-cl-rule (read-time-expr -> (list-expr) :time-expr list-expr)) 

(add-cl-rule (read-time-expr -> (identifier) :time-expr identifier))


(defmethod display-parse-tree ((entity read-time-expr) (syntax cl-syntax) pane)
  (with-slots (time-expr) entity
    (display-parse-tree time-expr syntax pane)))


;;;;;;;;;;;;;; read-time-plus-attr

(defclass read-time-plus-attr (read-time-attr) ()) 

(add-cl-rule (read-time-plus-attr -> ((read-car plus-symbol)
				      (read-expr read-time-expr (= (end-offset read-car) (start-offset read-expr))))
				  :read-car read-car :read-expr read-expr))


;;;;;;;;;;;;;; read-time-minus-attr

(defclass read-time-minus-attr (read-time-attr) ()) 

(add-cl-rule (read-time-minus-attr -> ((read-car minus-symbol)
				       (read-expr read-time-expr (= (end-offset read-car) (start-offset read-expr))))
				   :read-car read-car :read-expr read-expr))


;;;;;;;;;;;;; read-time-conditional

(defclass read-time-conditional (cl-entry)
  ((start :initarg :start)
   (test :initarg :test)
   (expr :initarg :expr)))


(defmethod display-parse-tree ((entity read-time-conditional) (syntax cl-syntax) pane)
  (with-slots (start test expr) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.0 0.42 0.42))
      (display-parse-tree start syntax pane)
      (display-parse-tree test syntax pane)
      (display-parse-tree expr syntax pane))))


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


;;;;;;;;;;;;; function-expression

(defclass fun-expr (cl-entry) 
  ((start :initarg :start)
   (quoted-expr :initarg :quoted-expr)))

(add-cl-rule (fun-expr -> ((start hex)
			   (quoted-expr quoted-expr (= (end-offset start)
						       (start-offset quoted-expr))))
		       :start start :quoted-expr quoted-expr))

(defmethod display-parse-tree ((entity fun-expr) (syntax cl-syntax) pane)
  (with-slots (start quoted-expr) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.4 0.0 0.4))
      (display-parse-tree start syntax pane)
      (display-parse-tree quoted-expr syntax pane))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;vector-expression

(defclass vect-expr (cl-entry)
  ((start :initarg :start)
   (list-expr :initarg :list-expr)))

(add-cl-rule (vect-expr -> ((start hex)
			    (list-expr list-expr (= (end-offset start)
						    (start-offset list-expr))))
			:start start :list-expr list-expr))

(defmethod display-parse-tree ((entity vect-expr) (syntax cl-syntax) pane)
  (with-slots (start list-expr) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.14 0.0 0.86))
      (display-parse-tree start syntax pane)
      (display-parse-tree list-expr syntax pane))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bitvector-expression

(defclass bitvect-expr (radix-expr) ())

(add-cl-rule (bitvect-expr -> ((start hex)
			       (item default-item
				     (and (= (end-offset start)
					     (start-offset item))
					  (string-equal (item-head item) #\*)
					  (radix-is (item-tail
						     item) 2))))
			   :start start :item item))

(defmethod display-parse-tree ((entity bitvect-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (with-drawing-options (pane :ink (make-rgb-color 0.14 0.0 0.86))
      (display-parse-tree start syntax pane)
      (display-parse-tree item syntax pane))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quoted expr

(defclass quoted-expr (cl-entry)
  ((start :initarg :start)
   (item :initarg :item)))

(add-cl-rule (quoted-expr -> ((start quote-symbol) 
			      (item cl-terminal))
			  :start start :item item))

(defmethod display-parse-tree ((entity quoted-expr) (syntax cl-syntax) pane)
  (with-slots (start item) entity
    (with-text-face (pane :bold)
      (display-parse-tree start syntax pane))
    (display-parse-tree item syntax pane))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Qualified symbols

;; XXX: There's a bit of duplication going on here. I'm not sure if
;; that could be reduced by clever inheritance. But then, it's only
;; OAOOM.

(defclass qualified-symbol (cl-entry)
     ((package-name :initarg :package-name)
      (colon1 :initarg :colon1)
      (colon2 :initarg :colon2)
      (symbol-name :initarg :symbol-name)))

(defclass qualified-exported-symbol (cl-entry)
     ((package-name :initarg :package-name)
      (colon :initarg :colon)
      (symbol-name :initarg :symbol-name)))

(add-cl-rule (qualified-symbol -> ((package-name default-item)
                                   (colon1 colon (= (end-offset package-name)
                                                    (start-offset colon1)))
                                   (colon2 colon (= (end-offset colon1)
                                                    (start-offset colon2)))
                                   (symbol-name default-item (= (end-offset colon2)
                                                                (start-offset symbol-name))))
                             :package-name package-name
                             :colon1 colon1
                             :colon2 colon2
                             :symbol-name symbol-name))

(add-cl-rule (qualified-exported-symbol -> ((package-name default-item)
                                            (colon colon (= (end-offset package-name)
                                                            (start-offset colon)))
                                            (symbol-name default-item (= (end-offset colon)
                                                                         (start-offset symbol-name))))
                                        :package-name package-name
                                        :colon colon
                                        :symbol-name symbol-name))

(defmethod display-parse-tree ((entity qualified-symbol) (syntax cl-syntax) pane)
  (with-slots (package-name colon1 colon2 symbol-name) entity
       (with-drawing-options (pane :text-style (make-text-style :fix :bold nil) :ink +purple+)
         (display-parse-tree package-name syntax pane)     
         (display-parse-tree colon1 syntax pane)
         (display-parse-tree colon2 syntax pane))
       (display-parse-tree symbol-name syntax pane)))

(defmethod display-parse-tree ((entity qualified-exported-symbol) (syntax cl-syntax) pane)
  (with-slots (package-name colon symbol-name) entity
     (display-parse-tree package-name syntax pane)
       (with-drawing-options (pane :ink (make-rgb-color 0.0 0.0 1.0))
         (display-parse-tree colon syntax pane))
       (display-parse-tree symbol-name syntax pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Keyword symbols

(defclass keyword-symbol (cl-entry)
     ((start :initarg :start)
      (item :initarg :item)))

(add-cl-rule (keyword-symbol -> ((start colon)
                                 (item identifier))
                             :start start :item item))

(defmethod display-parse-tree ((entity keyword-symbol) (syntax cl-syntax) pane)
  (with-slots (start item) entity
     (with-text-face (pane :bold)
       (display-parse-tree start syntax pane)
       (display-parse-tree item syntax pane))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Lambda list Keywords

(defclass lambda-list-keyword (cl-entry)
     ((start :initarg :start)
      (item :initarg :item)))

(add-cl-rule (lambda-list-keyword -> ((start ampersand)
                                      (item default-item (and
                                                          (= (end-offset start)
                                                             (start-offset item))
                                                          (member item
                                                                  '( ;; ordinary LLs
                                                                    "optional" "rest" "key" "aux" "allow-other-keys"
                                                                    ;; macro LLs
                                                                    "body" "whole" "environment")
                                                                  :test #'default-item-is))))
                                  :start start :item item))

(defmethod display-parse-tree ((entity lambda-list-keyword) (syntax cl-syntax) pane)
  (with-slots (start item) entity
     (with-drawing-options (pane :ink +blue+)
       (display-parse-tree start syntax pane)
       (display-parse-tree item syntax pane))))

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

(add-cl-rule (cl-terminal -> (number-expr) :item number-expr))
(add-cl-rule (cl-terminal -> (identifier) :item identifier))
(add-cl-rule (cl-terminal -> (balanced-comment) :item balanced-comment))
(add-cl-rule (cl-terminal -> (cl-string) :item cl-string))
(add-cl-rule (cl-terminal -> (quoted-expr) :item quoted-expr))
(add-cl-rule (cl-terminal -> (keyword-symbol) :item keyword-symbol))
(add-cl-rule (cl-terminal -> (lambda-list-keyword) :item lambda-list-keyword))
(add-cl-rule (cl-terminal -> (qualified-symbol) :item qualified-symbol))
(add-cl-rule (cl-terminal -> (qualified-exported-symbol) :item qualified-exported-symbol))
(add-cl-rule (cl-terminal -> (backquoted-expr) :item backquoted-expr))
(add-cl-rule (cl-terminal -> (char-item) :item char-item))
(add-cl-rule (cl-terminal -> (unquoted-expr) :item unquoted-expr))
(add-cl-rule (cl-terminal -> (list-expr) :item list-expr))
(add-cl-rule (cl-terminal -> (fun-expr) :item fun-expr))
(add-cl-rule (cl-terminal -> (vect-expr) :item vect-expr))
(add-cl-rule (cl-terminal -> (bitvect-expr) :item bitvect-expr))
(add-cl-rule (cl-terminal -> (pathname-expr) :item pathname-expr))
(add-cl-rule (cl-terminal -> (read-time-conditional-plus) :item read-time-conditional-plus))
(add-cl-rule (cl-terminal -> (read-time-conditional-minus) :item read-time-conditional-minus))
(add-cl-rule (cl-terminal -> (read-time-evaluation) :item read-time-evaluation))
(add-cl-rule (cl-terminal -> (line-comment) :item line-comment))

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
;;; display

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
	(tab-width (tab-width pane)))
    (loop while (and (< start end)
                     (whitespacep (buffer-object buffer start)))
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
                         pane (- tab-width (mod x tab-width)) 0)))
               (#\Page nil))
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
	      (with-drawing-options (pane :ink (make-rgb-color 0.7 0.7 0.7))
		(loop while (< start-token-index end-token-index)
		   do (let ((token (lexeme lexer start-token-index)))
			(display-parse-tree token syntax pane))
		     (incf start-token-index))))))))
    (when (mark-visible-p pane) (display-mark pane syntax))
    (display-cursor pane syntax current-p)))



