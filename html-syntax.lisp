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

(define-syntax html-syntax (basic-syntax)
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser))
  (:name "HTML")
  (:pathname-types "html" "htm"))

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

(defclass html-start-tag (html-tag)
  ((start :initarg :start)
   (name :initarg :name)
   (attributes :initform nil :initarg :attributes)
   (end :initarg :end)))

(defmethod display-parse-tree ((entity html-start-tag) (syntax html-syntax) pane)
  (with-slots (start name attributes end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree name syntax pane)
    (unless (null attributes)
      (display-parse-tree attributes syntax pane))
    (display-parse-tree end syntax pane)))

(defclass html-end-tag (html-tag)
  ((start :initarg :start)
   (name :initarg :name)
   (end :initarg :end)))

(defmethod display-parse-tree ((entity html-end-tag) (syntax html-syntax) pane)
  (with-slots (start name attributes end) entity
    (display-parse-tree start syntax pane)
    (display-parse-tree name syntax pane)
    (display-parse-tree end syntax pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass html-lexeme (html-token)
  ((state :initarg :state)))

(defclass start-lexeme (html-lexeme) ())
(defclass start-tag-start (html-lexeme) ())
(defclass end-tag-start (html-lexeme) ())
(defclass tag-end (html-lexeme) ())
(defclass word (html-lexeme) ())
(defclass delimiter (html-lexeme) ())

(defclass html-lexer (incremental-lexer) ())     

(defmethod next-lexeme ((lexer html-lexer) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\< (fo) (cond ((or (end-of-buffer-p scan)
			     (not (eql (object-after scan) #\/)))
			 (make-instance 'start-tag-start))
			(t (fo)
			   (make-instance 'end-tag-start))))
	(#\> (fo) (make-instance 'tag-end))
	(t (cond ((alphanumericp object)
		  (loop until (end-of-buffer-p scan)
			while (alphanumericp (object-after scan))
			do (fo))
		  (make-instance 'word))
		 (t
		  (fo) (make-instance 'delimiter))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defparameter *html-grammar* (grammar))

(defmacro add-html-rule (rule &key predict-test)
  `(add-rule (grammar-rule ,rule :predict-test ,predict-test)
	     *html-grammar*))

(defun word-is (word string)
  (string-equal (coerce (buffer-sequence (buffer word) (start-offset word) (end-offset word)) 'string)
		string))

(defmacro define-start-tag (name string)
  `(progn
     (defclass ,name (html-start-tag) ())

     (add-html-rule
      (,name -> (start-tag-start
		 (word (and (= (end-offset start-tag-start) (start-offset word))
			    (word-is word ,string)))
		 (tag-end (= (end-offset word) (start-offset tag-end))))
	     :start start-tag-start :name word :end tag-end))))

(defmacro define-end-tag (name string)
  `(progn
     (defclass ,name (html-end-tag) ())

     (add-html-rule
      (,name -> (end-tag-start
		 (word (and (= (end-offset end-tag-start) (start-offset word))
			    (word-is word ,string)))
		 (tag-end (= (end-offset word) (start-offset tag-end))))
	     :start end-tag-start :name word :end tag-end)
      :predict-test (lambda (token)
		      (typep token 'end-tag-start)))))

(defmacro define-tag-pair (start-name end-name string)
  `(progn (define-start-tag ,start-name ,string)
	  (define-end-tag ,end-name ,string)))

(define-tag-pair <head> </head> "head")
(define-tag-pair <title> </title> "title")
(define-tag-pair <body> </body> "body")

(defmacro define-list (name item-name)
  (let ((empty-name (gensym))
	(nonempty-name (gensym)))
    `(progn
       (defclass ,name (html-nonterminal) ())
       (defclass ,empty-name (,name) ())
       
       (defclass ,nonempty-name (,name)
	 ((items :initarg :items)
	  (item :initarg :item)))
       
       (add-html-rule (,name -> ()
			     (make-instance ',empty-name)))
       
       (add-html-rule (,name -> (,name ,item-name)
			     (make-instance ',nonempty-name
				:items ,name :item ,item-name)))
       
       (defmethod display-parse-tree ((entity ,empty-name) (syntax html-syntax) pane)
	 (declare (ignore pane))
	 nil)
       
       (defmethod display-parse-tree ((entity ,nonempty-name) (syntax html-syntax) pane)
	 (with-slots (items item) entity
	   (display-parse-tree items syntax pane)
	   (display-parse-tree item syntax pane))))))

(defmacro define-nonempty-list (name item-name)
  (let ((empty-name (gensym))
	(nonempty-name (gensym)))
    `(progn
       (defclass ,name (html-nonterminal) ())
       (defclass ,empty-name (,name) ())
       
       (defclass ,nonempty-name (,name)
	 ((items :initarg :items)
	  (item :initarg :item)))
       
       (add-html-rule (,name -> (,item-name)
			     (make-instance ',nonempty-name
				:items (make-instance ',empty-name)
				:item ,item-name)))
       
       (add-html-rule (,name -> (,name ,item-name)
			     (make-instance ',nonempty-name
				:items ,name :item ,item-name)))
       
       (defmethod display-parse-tree ((entity ,empty-name) (syntax html-syntax) pane)
	 (declare (ignore pane))
	 nil)
       
       (defmethod display-parse-tree ((entity ,nonempty-name) (syntax html-syntax) pane)
	 (with-slots (items item) entity
	   (display-parse-tree items syntax pane)
	   (display-parse-tree item syntax pane))))))

;;;;;;;;;;;;;;; string

(defclass string-lexeme (html-lexeme) ())

(add-html-rule (string-lexeme -> ((html-lexeme (not (word-is html-lexeme "\""))))))

(defclass html-string (html-token)
  ((start :initarg :start)
   (lexemes :initarg :lexemes)
   (end :initarg :end)))

(define-list string-lexemes string-lexeme)

(add-html-rule (html-string -> ((start delimiter (word-is start "\""))
				string-lexemes
				(end delimiter (word-is end "\"")))
			    :start start :lexemes string-lexemes :end end))

(defmethod display-parse-tree ((entity html-string) (syntax html-syntax) pane)
  (with-slots (start lexemes end) entity
     (display-parse-tree start syntax pane)
     (with-text-face (pane :italic)
       (display-parse-tree lexemes syntax pane))
     (display-parse-tree end syntax pane)))

;;;;;;;;;;;;;;; attributes

(defclass html-attribute (html-nonterminal)
  ((name :initarg :name)
   (equals :initarg :equals)))

(defmethod display-parse-tree :before ((entity html-attribute) (syntax html-syntax) pane)
  (with-slots (name equals) entity
     (display-parse-tree name syntax pane)
     (display-parse-tree equals syntax pane)))

(defclass common-attribute (html-attribute) ())

(defclass core-attribute (common-attribute) ())
(defclass i18n-attribute (common-attribute) ())
(defclass scripting-event (common-attribute) ())

(define-list common-attributes common-attribute)

;;;;;;;;;;;;;;; lang attribute

(defclass lang-attr (i18n-attribute)
  ((lang :initarg :lang)))

(add-html-rule (lang-attr -> ((name word (word-is name "lang"))
			      (equals delimiter (and (= (end-offset name) (start-offset equals))
						     (word-is equals "=")))
			      (lang word (and (= (end-offset equals) (start-offset lang))
					      (= (- (end-offset lang) (start-offset lang))
						 2))))
			  :name name :equals equals :lang lang))

(defmethod display-parse-tree ((entity lang-attr) (syntax html-syntax) pane)
  (with-slots (lang) entity
     (display-parse-tree lang syntax pane)))

;;;;;;;;;;;;;;; dir attribute

(defclass dir-attr (i18n-attribute)
  ((dir :initarg :dir)))

(add-html-rule (dir-attr -> ((name word (word-is name "dir"))
			     (equals delimiter (and (= (end-offset name) (start-offset equals))
						    (word-is equals "=")))
			     (dir word (and (= (end-offset equals) (start-offset dir))
					    (or (word-is dir "rtl")
						(word-is dir "ltr")))))
			 :name name :equals equals :dir dir))

(defmethod display-parse-tree ((entity dir-attr) (syntax html-syntax) pane)
  (with-slots (dir) entity
     (display-parse-tree dir syntax pane)))


;;;;;;;;;;;;;;; href attribute

(defclass href-attr (html-attribute)
  ((href :initarg :href)))

(add-html-rule (href-attr -> ((name word (word-is name "href"))
			      (equals delimiter (and (= (end-offset name) (start-offset equals))
						     (word-is equals "=")))
			      (href html-string))
			  :name name :equals equals :href href))

(defmethod display-parse-tree ((entity href-attr) (syntax html-syntax) pane)
  (with-slots (href) entity
     (display-parse-tree href syntax pane)))


;;;;;;;;;;;;;;; title

(defclass title-item (html-nonterminal)
  ((item :initarg :item)))

(add-html-rule (title-item -> (word) :item word))
(add-html-rule (title-item -> (delimiter) :item delimiter))

(defmethod display-parse-tree ((entity title-item) (syntax html-syntax) pane)
  (with-slots (item) entity
     (display-parse-tree item syntax pane)))

(define-list title-items title-item)

(defclass title (html-nonterminal)
  ((<title> :initarg :<title>)
   (items :initarg :items)
   (</title> :initarg :</title>)))

(add-html-rule (title -> (<title> title-items </title>)
		      :<title> <title> :items title-items :</title> </title>))

(defmethod display-parse-tree ((entity title) (syntax html-syntax) pane)
  (with-slots (<title> items </title>) entity
     (display-parse-tree <title> syntax pane)
     (with-text-face (pane :bold)
       (display-parse-tree items syntax pane))
     (display-parse-tree </title> syntax pane)))

;;;;;;;;;;;;;;; inline-element, block-level-element

(defclass inline-element (html-nonterminal) ())
(defclass block-level-element (html-nonterminal) ())

;;;;;;;;;;;;;;; %inline

(defclass $inline (html-nonterminal)
  ((contents :initarg :contents)))
     
(add-html-rule ($inline -> (inline-element) :contents inline-element)
	       :predict-test (lambda (token)
			       (typep token 'start-tag-start)))
(add-html-rule ($inline -> (word) :contents word))
(add-html-rule ($inline -> (delimiter) :contents delimiter))

(defmethod display-parse-tree ((entity $inline) (syntax html-syntax) pane)
  (with-slots (contents) entity
     (display-parse-tree contents syntax pane)))

(define-list $inlines $inline)

;;;;;;;;;;;;;;; %flow

(defclass $flow (html-nonterminal)
  ((contents :initarg :contents)))
     
(add-html-rule ($flow -> ($inline) :contents $inline))
(add-html-rule ($flow -> (block-level-element) :contents block-level-element)
	       :predict-test (lambda (token)
			       (typep token 'start-tag-start)))

(defmethod display-parse-tree ((entity $flow) (syntax html-syntax) pane)
  (with-slots (contents) entity
     (display-parse-tree contents syntax pane)))

(define-list $flows $flow)

;;;;;;;;;;;;;;; headings

(defclass heading (block-level-element)
  ((start :initarg :start)
   (contents :initarg :contents)
   (end :initarg :end)))

(defmethod display-parse-tree ((entity heading) (syntax html-syntax) pane)
  (with-slots (start contents end) entity
     (display-parse-tree start syntax pane)
     (with-text-face (pane :bold)
       (display-parse-tree contents syntax pane))
     (display-parse-tree end syntax pane)))
	      
(defmacro define-heading (class-name tag-string start-tag-name end-tag-name)
  `(progn
     (define-tag-pair ,start-tag-name ,end-tag-name ,tag-string)

     (defclass ,class-name (heading) ())

     (add-html-rule
      (,class-name -> (,start-tag-name $inlines ,end-tag-name)
		   :start ,start-tag-name :contents $inlines :end ,end-tag-name))))


(define-heading h1 "h1" <h1> </h1>)
(define-heading h2 "h2" <h2> </h2>)
(define-heading h3 "h3" <h3> </h3>)
(define-heading h4 "h4" <h4> </h4>)
(define-heading h5 "h5" <h5> </h5>)
(define-heading h6 "h6" <h6> </h6>)

;;;;;;;;;;;;;;; a element

(defclass <a>-attribute (html-nonterminal)
  ((attribute :initarg :attribute)))

(add-html-rule (<a>-attribute -> (href-attr) :attribute href-attr))

(defmethod display-parse-tree ((entity <a>-attribute) (syntax html-syntax) pane)
  (with-slots (attribute) entity
     (display-parse-tree attribute syntax pane)))

(define-list <a>-attributes <a>-attribute)

(defclass <a> (html-start-tag) ())

(add-html-rule (<a> -> (start-tag-start
			(word (and (= (end-offset start-tag-start) (start-offset word))
				   (word-is word "a")))
			<a>-attributes
			tag-end)
		    :start start-tag-start :name word :attributes <a>-attributes :end tag-end))

(define-end-tag </a> "a")

(defclass a-element (inline-element)
  ((<a> :initarg :<a>)
   (items :initarg :items)
   (</a> :initarg :</a>)))

(add-html-rule (a-element -> (<a> $inlines </a>)
			  :<a> <a> :items $inlines :</a> </a>))

(defmethod display-parse-tree ((entity a-element) (syntax html-syntax) pane)
  (with-slots (<a> items </a>) entity
     (display-parse-tree <a> syntax pane)
     (with-text-face (pane :bold)
       (display-parse-tree items syntax pane))
     (display-parse-tree </a> syntax pane)))

;;;;;;;;;;;;;;; br element

(defclass br-element (inline-element)
  ((<br> :initarg :<br>)))
     
(define-start-tag <br> "br")

(add-html-rule (br-element -> (<br>) :<br> <br>))

(defmethod display-parse-tree ((entity br-element) (syntax html-syntax) pane)
  (with-slots (<br>) entity
     (display-parse-tree <br> syntax pane)))

;;;;;;;;;;;;;;; p element

(defclass <p> (html-start-tag) ())

(add-html-rule (<p> -> (start-tag-start
			(word (and (= (end-offset start-tag-start) (start-offset word))
				   (word-is word "p")))
			common-attributes
			tag-end)
		    :start start-tag-start :name word :attributes common-attributes :end tag-end))

(define-end-tag </p> "p")

(defclass p-element (block-level-element)
  ((<p> :initarg :<p>)
   (contents :initarg :contents)
   (</p> :initarg :</p>)))

(add-html-rule (p-element -> (<p> $inlines </p>)
			  :<p> <p> :contents $inlines :</p> </p>))

(defmethod display-parse-tree ((entity p-element) (syntax html-syntax) pane)
  (with-slots (<p> contents </p>) entity
    (display-parse-tree <p> syntax pane)
    (display-parse-tree contents syntax pane)
    (display-parse-tree </p> syntax pane)))

;;;;;;;;;;;;;;; li element

(defclass <li> (html-start-tag) ())

(add-html-rule (<li> -> (start-tag-start
			 (word (and (= (end-offset start-tag-start) (start-offset word))
				    (word-is word "li")))
			 common-attributes
			 tag-end)
		     :start start-tag-start
		     :name word
		     :attributes common-attributes
		     :end tag-end))

(define-end-tag </li> "li")

(defclass li-element (html-nonterminal)
  ((<li> :initarg :<li>)
   (items :initarg :items)
   (</li> :initarg :</li>)))

(add-html-rule (li-element -> (<li> $flows </li>)
			   :<li> <li> :items $flows :</li> </li>))
(add-html-rule (li-element -> (<li> $flows)
			   :<li> <li> :items $flows :</li> nil))

(defmethod display-parse-tree ((entity li-element) (syntax html-syntax) pane)
  (with-slots (<li> items </li>) entity
     (display-parse-tree <li> syntax pane)
     (display-parse-tree items syntax pane)     
     (when </li>
       (display-parse-tree </li> syntax pane))))

;;;;;;;;;;;;;;; ul element

(defclass <ul> (html-start-tag) ())

(add-html-rule (<ul> -> (start-tag-start
			 (word (and (= (end-offset start-tag-start) (start-offset word))
				    (word-is word "ul")))
			 common-attributes
			 tag-end)
		     :start start-tag-start
		     :name word
		     :attributes common-attributes
		     :end tag-end))

(define-end-tag </ul> "ul")

(define-nonempty-list li-elements li-element)

(defclass ul-element (block-level-element)
  ((<ul> :initarg :<ul>)
   (items :initarg :items)
   (</ul> :initarg :</ul>)))

(add-html-rule (ul-element -> (<ul> li-elements </ul>)
			   :<ul> <ul> :items li-elements :</ul> </ul>))

(defmethod display-parse-tree ((entity ul-element) (syntax html-syntax) pane)
  (with-slots (<ul> items </ul>) entity
     (display-parse-tree <ul> syntax pane)
     (display-parse-tree items syntax pane)     
     (display-parse-tree </ul> syntax pane)))

;;;;;;;;;;;;;;; hr element

(defclass hr-element (block-level-element)
  ((<hr> :initarg :<hr>)))

(define-start-tag <hr> "hr")

(add-html-rule (hr-element -> (<hr>) :<hr> <hr>))

(defmethod display-parse-tree ((entity hr-element) (syntax html-syntax) pane)
  (with-slots (<hr>) entity
     (display-parse-tree <hr> syntax pane)))

;;;;;;;;;;;;;;; body element

(defclass body-item (html-nonterminal)
  ((item :initarg :item)))

(add-html-rule (body-item -> ((element block-level-element)) :item element))

(defmethod display-parse-tree ((entity body-item) (syntax html-syntax) pane)
  (with-slots (item) entity
     (display-parse-tree item syntax pane)))

(define-list body-items body-item)

(defclass body (html-nonterminal)
  ((<body> :initarg :<body>)
   (items :initarg :items)
   (</body> :initarg :</body>)))

(add-html-rule (body -> (<body> body-items </body>)
		     :<body> <body> :items body-items :</body> </body>))

(defmethod display-parse-tree ((entity body) (syntax html-syntax) pane)
  (with-slots (<body> items </body>) entity
     (display-parse-tree <body> syntax pane)
     (display-parse-tree items syntax pane)     
     (display-parse-tree </body> syntax pane)))

;;;;;;;;;;;;;;; head

(defclass head (html-nonterminal)
  ((<head> :initarg :<head>)
   (title :initarg :title)
   (</head> :initarg :</head>)))

(add-html-rule (head -> (<head> title </head>)
		     :<head> <head> :title title :</head> </head>))

(defmethod display-parse-tree ((entity head) (syntax html-syntax) pane)
  (with-slots (<head> title </head>) entity
     (display-parse-tree <head> syntax pane)
     (display-parse-tree title syntax pane)     
     (display-parse-tree </head> syntax pane)))

;;;;;;;;;;;;;;; html

(defclass <html>-attribute (html-nonterminal)
  ((attribute :initarg :attribute)))

(defmethod display-parse-tree ((entity <html>-attribute) (syntax html-syntax) pane)
  (with-slots (attribute) entity
     (display-parse-tree attribute syntax pane)))

(add-html-rule (<html>-attribute -> (lang-attr) :attribute lang-attr))
(add-html-rule (<html>-attribute -> (dir-attr) :attribute dir-attr))

(define-list <html>-attributes <html>-attribute)

(defclass <html> (html-start-tag) ())

(add-html-rule (<html> -> (start-tag-start
			   (word (and (= (end-offset start-tag-start) (start-offset word))
				      (word-is word "html")))
			   <html>-attributes
			   tag-end)
		       :start start-tag-start :name word :attributes <html>-attributes :end tag-end))

(define-end-tag </html> "html")

(defclass html (html-nonterminal)
  ((<html> :initarg :<html>)
   (head :initarg :head)
   (body :initarg :body)
   (</html> :initarg :</html>)))

(add-html-rule (html -> (<html> head body </html>)
		     :<html> <html> :head head :body body :</html> </html>))

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

(defmethod display-parse-tree ((entity html-lexeme) (syntax html-syntax) pane)
  (flet ((cache-test (t1 t2)
	   (let ((result (and (eq t1 t2)
			      (eq (slot-value t1 'ink)
				  (medium-ink (sheet-medium pane)))
			      (eq (slot-value t1 'face)
				  (text-style-face (medium-text-style (sheet-medium pane)))))))
	     result)))
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
  (with-drawing-options (pane :ink +green4+)
    (call-next-method)))

(defmethod display-parse-tree :before ((entity html-lexeme) (syntax html-syntax) pane)
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
     (when (mark-visible-p pane) (display-mark pane syntax))
     (display-cursor pane syntax current-p)))
	    
