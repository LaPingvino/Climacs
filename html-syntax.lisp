;;; -*- Mode: Lisp; Package: CLIMACS-HTML-SYNTAX -*-

;;;  (c) copyright 2005 by
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

;;; Syntax for analysing HTML

(in-package :climacs-html-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar classes

(defclass html-parse-tree (parse-tree)
  ((badness :initform 0 :initarg :badness :reader badness)))

(defmethod parse-tree-better ((t1 html-parse-tree) (t2 html-parse-tree))
  (and (eq (class-of t1) (class-of t2))
       (< (badness t1) (badness t2))))

(defclass html-nonterminal (html-parse-tree) ())

(defclass html-token (html-parse-tree)
  ((ink) (face)))

(defclass html-tag (html-token) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass html-lexeme (html-token)
  ((state :initarg :state)))

(defclass start-lexeme (html-lexeme) ())
(defclass tag-start (html-lexeme) ())
(defclass tag-end (html-lexeme) ())
(defclass slash (html-lexeme) ())
(defclass word (html-lexeme) ())
(defclass delimiter (html-lexeme) ())

(defclass html-lexer (incremental-lexer) ())     

(defmethod next-lexeme ((lexer html-lexer) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\< (fo) (make-instance 'tag-start))
	(#\> (fo) (make-instance 'tag-end))
	(#\/ (fo) (make-instance 'slash))
	(t (cond ((alphanumericp object)
		  (loop until (end-of-buffer-p scan)
			while (alphanumericp (object-after scan))
			do (fo))
		  (make-instance 'word))
		 (t
		  (fo) (make-instance 'delimiter))))))))

(define-syntax html-syntax ("HTML" (basic-syntax))
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defparameter *html-grammar* (grammar))

(defun word-is (word string)
  (string-equal (coerce (buffer-sequence (buffer word) (start-offset word) (end-offset word)) 'string)
		string))

(defmacro define-start-tag (name string)
  `(progn
     (defclass ,name (html-tag) ())

     (add-rule (grammar-rule
		(,name -> (tag-start
			   (word (and (= (end-offset tag-start) (start-offset word))
				      (word-is word ,string)))
			   (tag-end (= (end-offset word) (start-offset tag-end))))))
	       *html-grammar*)))

(defmacro define-end-tag (name string)
  `(progn
     (defclass ,name (html-tag) ())

     (add-rule (grammar-rule
		(,name -> (tag-start
			   (slash (= (end-offset tag-start) (start-offset slash)))
			   (word (and (= (end-offset slash) (start-offset word))
				      (word-is word ,string)))
			   (tag-end (= (end-offset word) (start-offset tag-end))))))
	       *html-grammar*)))

(defmacro define-tag-pair (start-name end-name string)
  `(progn (define-start-tag ,start-name ,string)
	  (define-end-tag ,end-name ,string)))

(define-tag-pair <html> </html> "html")
(define-tag-pair <head> </head> "head")
(define-tag-pair <title> </title> "title")
(define-tag-pair <body> </body> "body")
(define-tag-pair <h1> </h1> "h1")
(define-tag-pair <h2> </h2> "h2")
(define-tag-pair <h3> </h3> "h3")
(define-tag-pair <p> </p> "p")
(define-tag-pair <ul> </ul> "ul")
(define-tag-pair <li> </li> "li")

(defmacro define-list (name empty-name nonempty-name item-name)
  `(progn
     (defclass ,name (html-nonterminal) ())
     (defclass ,empty-name (,name) ())

     (defclass ,nonempty-name (,name)
	  ((items :initarg :items)
	   (item :initarg :item)))

     (add-rule (grammar-rule (,name -> ()
				    (make-instance ',empty-name)))
	       *html-grammar*)

     (add-rule (grammar-rule (,name -> (,name ,item-name)
				    (make-instance ',nonempty-name
				       :items ,name :item ,item-name)))
	       *html-grammar*)
     
     (defmethod display-parse-tree ((entity ,empty-name) (syntax html-syntax) pane)
       (declare (ignore pane))
       nil)
     
     (defmethod display-parse-tree ((entity ,nonempty-name) (syntax html-syntax) pane)
       (with-slots (items item) entity
	  (display-parse-tree items syntax pane)
	  (display-parse-tree item syntax pane)))))     

;;;;;;;;;;;;;;; title-item, title-items

(defclass title-item (html-nonterminal)
  ((item :initarg :item)))

(add-rule (grammar-rule (title-item -> (word) :item word)) *html-grammar*)
(add-rule (grammar-rule (title-item -> (delimiter) :item delimiter)) *html-grammar*)

(defmethod display-parse-tree ((entity title-item) (syntax html-syntax) pane)
  (with-slots (item) entity
     (display-parse-tree item syntax pane)))

(define-list title-items empty-title-items nonempty-title-items title-item)

;;;;;;;;;;;;;;; title

(defclass title (html-nonterminal)
  ((<title> :initarg :<title>)
   (items :initarg :items)
   (</title> :initarg :</title>)))

(add-rule (grammar-rule (title -> (<title> title-items </title>)
			       :<title> <title> :items title-items :</title> </title>))
	  *html-grammar*)

(defmethod display-parse-tree ((entity title) (syntax html-syntax) pane)
  (with-slots (<title> items </title>) entity
     (display-parse-tree <title> syntax pane)
     (with-text-face (pane :bold)
       (display-parse-tree items syntax pane))
     (display-parse-tree </title> syntax pane)))

;;;;;;;;;;;;;;; body-item body-items

(defclass body-item (html-nonterminal)
  ((item :initarg :item)))

(add-rule (grammar-rule (body-item -> (word) :item word)) *html-grammar*)
(add-rule (grammar-rule (body-item -> (delimiter) :item delimiter)) *html-grammar*)
(add-rule (grammar-rule (body-item -> (a) :item a)) *html-grammar*)

(defmethod display-parse-tree ((entity body-item) (syntax html-syntax) pane)
  (with-slots (item) entity
     (display-parse-tree item syntax pane)))

(define-list body-items empty-body-items nonempty-body-items body-item)

;;;;;;;;;;;;;;; body

(defclass body (html-nonterminal)
  ((<body> :initarg :<body>)
   (items :initarg :items)
   (</body> :initarg :</body>)))

(add-rule (grammar-rule (body -> (<body> body-items </body>)
			      :<body> <body> :items body-items :</body> </body>))
	  *html-grammar*)

(defmethod display-parse-tree ((entity body) (syntax html-syntax) pane)
  (with-slots (<body> items </body>) entity
     (display-parse-tree <body> syntax pane)
     (display-parse-tree items syntax pane)     
     (display-parse-tree </body> syntax pane)))

;;;;;;;;;;;;;;; <a>-tag

(defclass a-tag-item (html-nonterminal)
  ((item :initarg :item)))

(add-rule (grammar-rule (a-tag-item -> (word) :item word)) *html-grammar*)
(add-rule (grammar-rule (a-tag-item -> (delimiter) :item delimiter)) *html-grammar*)

(defmethod display-parse-tree ((entity a-tag-item) (syntax html-syntax) pane)
  (with-slots (item) entity
     (display-parse-tree item syntax pane)))

(define-list a-tag-items empty-a-tag-items nonempty-a-tag-items a-tag-item)

(defclass <a> (html-tag)
  ((start :initarg :start)
   (name :initarg :name)
   (items :initarg :items)
   (end :initarg :end)))

(add-rule (grammar-rule (<a> -> (tag-start
				 (word (and (= (end-offset tag-start) (start-offset word))
					    (word-is word "a")))
				 a-tag-items
				 tag-end)
			     :start tag-start :name word :items a-tag-items :end tag-end))
	  *html-grammar*)

(defmethod display-parse-tree ((entity <a>) (syntax html-syntax) pane)
  (with-slots (start name items end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree name syntax pane)
    (display-parse-tree items syntax pane)
    (display-parse-tree end syntax pane)))

(define-end-tag </a> "a")

(defclass a (html-nonterminal)
  ((<a> :initarg :<a>)
   (items :initarg :items)
   (</a> :initarg :</a>)))

(add-rule (grammar-rule (a -> (<a> body-items </a>)
			   :<a> <a> :items body-items :</a> </a>))
	  *html-grammar*)

(defmethod display-parse-tree ((entity a) (syntax html-syntax) pane)
  (with-slots (<a> items </a>) entity
     (display-parse-tree <a> syntax pane)
     (display-parse-tree items syntax pane)     
     (display-parse-tree </a> syntax pane)))

;;;;;;;;;;;;;;; head

(defclass head (html-nonterminal)
  ((<head> :initarg :<head>)
   (title :initarg :title)
   (</head> :initarg :</head>)))

(add-rule (grammar-rule (head -> (<head> title </head>)
			      :<head> <head> :title title :</head> </head>))
	  *html-grammar*)

(defmethod display-parse-tree ((entity head) (syntax html-syntax) pane)
  (with-slots (<head> title </head>) entity
     (display-parse-tree <head> syntax pane)
     (display-parse-tree title syntax pane)     
     (display-parse-tree </head> syntax pane)))

;;;;;;;;;;;;;;; html

(defclass html (html-nonterminal)
  ((<html> :initarg :<html>)
   (head :initarg :head)
   (body :initarg :body)
   (</html> :initarg :</html>)))

(add-rule (grammar-rule (html -> (<html> head body </html>)
			      :<html> <html> :head head :body body :</html> </html>))
	  *html-grammar*)    

(defmethod display-parse-tree ((entity html) (syntax html-syntax) pane)
  (with-slots (<html> head body </html>) entity
     (display-parse-tree <html> syntax pane)
     (display-parse-tree head syntax pane)     
     (display-parse-tree body syntax pane)     
     (display-parse-tree </html> syntax pane)))

;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((syntax html-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser lexer buffer) syntax
     (setf parser (make-instance 'parser
		     :grammar *html-grammar*
		     :target 'html))
     (setf lexer (make-instance 'html-lexer :buffer (buffer syntax)))
     (let ((m (clone-mark (low-mark buffer) :left))
	   (lexeme (make-instance 'start-lexeme :state (initial-state parser))))
       (setf (offset m) 0)
       (setf (start-offset lexeme) m
	     (end-offset lexeme) 0)
       (insert-lexeme lexer 0 lexeme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax


(defmethod update-syntax-for-display (buffer (syntax html-syntax) top bot)
  (with-slots (parser lexer valid-parse) syntax
     (loop until (= valid-parse (nb-lexemes lexer))
	   while (mark<= (end-offset (lexeme lexer valid-parse)) bot)
	   do (let ((current-token (lexeme lexer (1- valid-parse)))
		    (next-lexeme (lexeme lexer valid-parse)))
		(setf (slot-value next-lexeme 'state)
		      (advance-parse parser (list next-lexeme) (slot-value current-token 'state))))
	      (incf valid-parse))))

(defmethod inter-lexeme-object-p ((lexer html-lexer) object)
  (whitespacep object))

(defmethod update-syntax (buffer (syntax html-syntax))
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

(defmethod display-parse-tree :around ((entity html-parse-tree) syntax pane)
  (with-slots (top bot) pane
     (when (and (end-offset entity) (mark> (end-offset entity) top))
       (call-next-method))))

(defmethod display-parse-tree ((entity html-token) (syntax html-syntax) pane)
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

(defmethod display-parse-tree :around ((entity html-tag) (syntax html-syntax) pane)
  (with-drawing-options (pane :ink +green+)
    (call-next-method)))

(defmethod display-parse-tree :before ((entity html-token) (syntax html-syntax) pane)
  (handle-whitespace pane (buffer pane) *white-space-start* (start-offset entity))
  (setf *white-space-start* (end-offset entity)))

(defgeneric display-parse-stack (symbol stack syntax pane))

(defmethod display-parse-stack (symbol stack (syntax html-syntax) pane)
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

(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax html-syntax) current-p)
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
	    
