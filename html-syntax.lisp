;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

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

(defclass html-sym (parse-tree)
  ((badness :initform 0 :initarg :badness :reader badness)))

(defmethod parse-tree-better ((t1 html-sym) (t2 html-sym))
  (and (eq (class-of t1) (class-of t2))
       (< (badness t1) (badness t2))))

(defclass html-nonterminal (html-sym) ())

(defclass words (html-nonterminal) ())

(defclass empty-words (words) ())

(defclass nonempty-words (words)
  ((words :initarg :words)
   (word :initarg :word)))

(defclass html-balanced (html-nonterminal)
  ((start :initarg :start)
   (end :initarg :end)))

(defclass html (html-balanced)
  ((head :initarg :head)
   (body :initarg :body)))

(defclass head (html-balanced)
  ((title :initarg :title)))

(defclass html-words (html-balanced)
  ((words :initarg :words)))

(defclass title (html-words) ())
(defclass body (html-words) ())
(defclass h1 (html-words) ())
(defclass h2 (html-words) ())
(defclass h3 (html-words) ())
(defclass a (html-words) ())
(defclass para (html-words) ())

(defclass html-token (html-sym)
  ((ink) (face)))

(defclass html-tag (html-token) ())

(defclass <html> (html-tag) () (:default-initargs :size 6))
(defclass </html> (html-tag) ()(:default-initargs :size 7))
(defclass <head> (html-tag) () (:default-initargs :size 6))
(defclass </head> (html-tag) () (:default-initargs :size 7))
(defclass <title> (html-tag) () (:default-initargs :size 7))
(defclass </title> (html-tag) () (:default-initargs :size 8))
(defclass <body> (html-tag) () (:default-initargs :size 6))
(defclass </body> (html-tag) () (:default-initargs :size 7))
(defclass <h1> (html-tag) () (:default-initargs :size 4))
(defclass </h1> (html-tag) () (:default-initargs :size 5))
(defclass <h2> (html-tag) () (:default-initargs :size 4))
(defclass </h2> (html-tag) () (:default-initargs :size 5))
(defclass <h3> (html-tag) () (:default-initargs :size 4))
(defclass </h3> (html-tag) () (:default-initargs :size 5))
(defclass <p> (html-tag) () (:default-initargs :size 3))
(defclass </p> (html-tag) () (:default-initargs :size 4))
(defclass <ul> (html-tag) () (:default-initargs :size 4))
(defclass </ul> (html-tag) () (:default-initargs :size 5))
(defclass <li> (html-tag) () (:default-initargs :size 4))
(defclass </li> (html-tag) () (:default-initargs :size 5))
(defclass <a> (html-tag)
  ((start :initarg :start)
   (word :initarg :word)
   (words :initarg :words)
   (end :initarg :end)))
(defclass </a> (html-tag) () (:default-initargs :size 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass html-element (html-token)
  ((state :initarg :state)))

(defclass start-element (html-element) ())
(defclass tag-start (html-element) ())
(defclass tag-end (html-element) ())
(defclass slash (html-element) ())
(defclass word (html-element) ())
(defclass delimiter (html-element) ())

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

(defclass html-lexer (incremental-lexer) ())     

(define-syntax html-syntax ("HTML" (basic-syntax))
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defun word-is (word string)
  (string-equal (coerce (region-to-sequence (start-mark word) (end-offset word)) 'string)
		string))

(defparameter *html-grammar*
  (grammar
    (<html> -> (tag-start
		(word (and (= (end-offset tag-start) (start-offset word))
			   (word-is word "html")))
		(tag-end (= (end-offset word) (start-offset tag-end))))
	    :start-mark (start-mark tag-start))
    (</html> -> (tag-start
		 (slash (= (end-offset tag-start) (start-offset slash)))
		 (word (and (= (end-offset slash) (start-offset word))
			    (word-is word "html")))
		 (tag-end (= (end-offset word) (start-offset tag-end))))
	     :start-mark (start-mark tag-start))
    (<head> -> (tag-start
		(word (and (= (end-offset tag-start) (start-offset word))
			   (word-is word "head")))
		(tag-end (= (end-offset word) (start-offset tag-end))))
	    :start-mark (start-mark tag-start))
    (</head> -> (tag-start
		 (slash (= (end-offset tag-start) (start-offset slash)))
		 (word (and (= (end-offset slash) (start-offset word))
			    (word-is word "head")))
		 (tag-end (= (end-offset word) (start-offset tag-end))))
	     :start-mark (start-mark tag-start))
    (<title> -> (tag-start
		 (word (and (= (end-offset tag-start) (start-offset word))
			    (word-is word "title")))
		 (tag-end (= (end-offset word) (start-offset tag-end))))
	     :start-mark (start-mark tag-start))
    (</title> -> (tag-start
		  (slash (= (end-offset tag-start) (start-offset slash)))
		  (word (and (= (end-offset slash) (start-offset word))
			     (word-is word "title")))
		  (tag-end (= (end-offset word) (start-offset tag-end))))
	      :start-mark (start-mark tag-start))
    (<body> -> (tag-start
		(word (and (= (end-offset tag-start) (start-offset word))
			   (word-is word "body")))
		(tag-end (= (end-offset word) (start-offset tag-end))))
	    :start-mark (start-mark tag-start))
    (</body> -> (tag-start
		 (slash (= (end-offset tag-start) (start-offset slash)))
		 (word (and (= (end-offset slash) (start-offset word))
			    (word-is word "body")))
		 (tag-end (= (end-offset word) (start-offset tag-end))))
	     :start-mark (start-mark tag-start))
    (<a> -> (tag-start
	     (word (and (= (end-offset tag-start) (start-offset word))
			(word-is word "a")))
	     words
	     tag-end)
	 :start-mark (start-mark tag-start)
	 :size (- (end-offset tag-end) (start-offset tag-start))
	 :start tag-start :word word :words words :end tag-end)
    (</a> -> (tag-start
	      (slash (= (end-offset tag-start) (start-offset slash)))
	      (word (and (= (end-offset slash) (start-offset word))
			 (word-is word "a")))
	      (tag-end (= (end-offset word) (start-offset tag-end))))
	  :start-mark (start-mark tag-start))
    (html -> (<html> head body </html>)
	  :start-mark (start-mark <html>)
	  :size (- (end-offset </html>) (start-offset <html>))
	  :start <html> :head head :body body :end </html>)
    (head -> (<head> title </head>)
	  :start-mark (start-mark <head>)
	  :size (- (end-offset </head>) (start-offset <head>))
	  :start <head> :title title :end </head>)
    (title -> (<title> words </title>)
	   :start-mark (start-mark <title>)
	   :size (- (end-offset </title>) (start-offset <title>))
	   :start <title> :words words :end </title>)
    (body -> (<body> words </body>)
	  :start-mark (start-mark <body>)
	  :size (- (end-offset </body>) (start-offset <body>))
	  :start <body> :words words :end </body>)
    (a -> (<a> words </a>)
       :start-mark (start-mark <a>)
       :size (- (end-offset </a>) (start-offset <a>))
       :start <a> :words words :end </a>)
    (words -> ()
	   (make-instance 'empty-words :start-mark nil))
    (words -> (words word)
	   (make-instance 'nonempty-words
	      :start-mark (or (start-mark words) (start-mark word))
	      :size (- (end-offset word) (offset (or (start-mark words) (start-mark word))))
	      :words words :word word))
    (word -> (a)
	  :start-mark (start-mark a)
	  :size (- (end-offset a) (start-offset a)))
    (word -> (delimiter)
	  :start-mark (start-mark delimiter)
	  :size (- (end-offset delimiter) (start-offset delimiter)))))
	  

(defmethod initialize-instance :after ((syntax html-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser lexer buffer) syntax
     (setf parser (make-instance 'parser
		     :grammar *html-grammar*
		     :target 'html))
     (setf lexer (make-instance 'html-lexer :buffer (buffer syntax)))
     (let ((m (clone-mark (low-mark buffer) :left)))
       (setf (offset m) 0)
       (insert-lexeme lexer 0 (make-instance 'start-element
				 :start-mark m
				 :size 0
				 :state (initial-state parser))))))

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
	    (high-mark (high-mark buffer))
	    (first-invalid-position (delete-invalid-lexemes lexer low-mark high-mark)))
       (setf valid-parse first-invalid-position)
       (update-lex lexer first-invalid-position high-mark))))

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

(defmethod display-parse-tree :around ((entity html-sym) syntax pane)
  (with-slots (top bot) pane
     (when (mark> (end-offset entity) top)
       (call-next-method))))

(defmethod display-parse-tree :around ((entity empty-words) syntax pane)
  (declare (ignore syntax pane))
  nil)

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
	 (present (coerce (region-to-sequence (start-mark entity)
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

(defmethod display-parse-tree :before ((entity html-balanced) (syntax html-syntax) pane)
  (with-slots (start) entity
     (display-parse-tree start syntax pane)))

(defmethod display-parse-tree :after ((entity html-balanced) (syntax html-syntax) pane)
  (with-slots (end) entity
     (display-parse-tree end syntax pane)))

(defmethod display-parse-tree :around ((entity title) (syntax html-syntax) pane)
  (with-text-face (pane :bold)
    (call-next-method)))

(defmethod display-parse-tree ((entity html-words) (syntax html-syntax) pane)
  (with-slots (words) entity
     (display-parse-tree words syntax pane)))

(defmethod display-parse-tree ((entity empty-words) (syntax html-syntax) pane)
  (declare (ignore pane))
  nil)

(defmethod display-parse-tree ((entity nonempty-words) (syntax html-syntax) pane)
  (with-slots (words word) entity
     (display-parse-tree words syntax pane)
     (display-parse-tree word syntax pane)))

(defmethod display-parse-tree ((entity html) (syntax html-syntax) pane)
  (with-slots (head body) entity
     (display-parse-tree head syntax pane)
     (display-parse-tree body syntax pane)))

(defmethod display-parse-tree ((entity head) (syntax html-syntax) pane)
  (with-slots (title) entity
     (display-parse-tree title syntax pane)))

(defmethod display-parse-tree ((entity <a>) (syntax html-syntax) pane)
  (with-slots (start word words end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree word syntax pane)
    (display-parse-tree words syntax pane)
    (display-parse-tree end syntax pane)))

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
	    
