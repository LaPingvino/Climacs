;;; -*- Mode: Lisp -*-

;;;  (c) copyright 2005 by
;;;           Brian Mastenbrook (brian@mastenbrook.net)
;;;           Christophe Rhodes (c.rhodes@gold.ac.uk)
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

(defpackage :climacs-slidemacs-editor
  (:use :clim-lisp :clim :clim-extensions :climacs-buffer :climacs-base 
	:climacs-syntax :flexichain :climacs-pane :climacs-fundamental-syntax)
  (:export))

(in-package :climacs-slidemacs-editor)

(defgeneric display-parse-tree (entity syntax pane))

(defclass slidemacs-parse-tree (parse-tree) ())

(defclass slidemacs-entry (slidemacs-parse-tree)
  ((ink) (face) (state :initarg :state)))

(defclass slidemacs-nonterminal (slidemacs-entry) ())

(defclass slidemacs-terminal (slidemacs-entry)
  ((item :initarg :item)))

(defclass slidemacs-lexeme (slidemacs-entry) ())

(defgeneric lexeme-string (foo))

(defmethod lexeme-string ((thing slidemacs-entry))
  (coerce
   (buffer-sequence (buffer thing)
                    (start-offset thing)
                    (end-offset thing))
   'string))

(defmethod print-object ((o slidemacs-lexeme) s)
  (print-unreadable-object (o s :type t)
    (format s "~S" (lexeme-string o))))

(defmacro define-lexemes (superclass &body lexemes)
  `(progn
     ,@(loop for lexeme in lexemes
	    collect `(defclass ,lexeme (,superclass) ()))))

(define-lexemes slidemacs-lexeme start-lexeme slidemacs-keyword
                block-open block-close slidemacs-quoted-string slidemacs-italic-string bullet other-entry)

(defclass slidemacs-lexer (incremental-lexer) ())

(defun identifier-char-p (var &key start)
  (and (characterp var)
       (if start (alpha-char-p var) t)
       (or (alphanumericp var) (eql var #\_))))

(defmethod next-lexeme ((lexer slidemacs-lexer) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (macrolet ((dispatch-object (&body cases)
		   `(case object
		      ,@(loop for case in cases
			     collect `(,(first case)
					(fo)
					,@(if (and (eql (length case) 2)
						  (symbolp (second case)))
					     `((make-instance ',(second case)))
					     (cdr case)))))))
	(dispatch-object 
	 (#\{ block-open)
	 (#\} block-close)
         (#\" (loop until (end-of-buffer-p scan)
                    while (not (eql (object-after scan) #\"))
                    do (fo))
              (unless (end-of-buffer-p scan)
                (fo)) ; get the closing #\"
              (make-instance 'slidemacs-quoted-string))
         (#\/ (loop until (end-of-buffer-p scan)
                    while (not (eql (object-after scan) #\/))
                    do (fo))
              (unless (end-of-buffer-p scan)
                (fo)) ; get the closing #\/
              (make-instance 'slidemacs-italic-string))
         (#\* bullet)
	 (t
          (cond ((identifier-char-p object :start t)
                 (loop until (end-of-buffer-p scan)
                       while (identifier-char-p (object-after scan))
                       do (fo))
                 (make-instance 'slidemacs-keyword))
                (t (fo) (make-instance 'other-entry)))))))))

(define-syntax slidemacs-editor-syntax (fundamental-syntax)
  ((lexer :reader lexer)
   (valid-parse :initform 1) (parser))
  (:name "Slidemacs-Editor")
  (:pathname-types "slidemacs"))

(defparameter *slidemacs-grammar* (grammar))

(defmethod initialize-instance :after ((syntax slidemacs-editor-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser lexer buffer) syntax
    (setf parser (make-instance 'parser
				:grammar *slidemacs-grammar*
				:target 'slidemacs-terminals))
    (setf lexer (make-instance 'slidemacs-lexer :buffer (buffer syntax)))
    (let ((m (clone-mark (low-mark buffer) :left))
	   (lexeme (make-instance 'start-lexeme :state (initial-state parser))))
      (setf (offset m) 0)
      (setf (start-offset lexeme) m
	    (end-offset lexeme) 0)
      (insert-lexeme lexer 0 lexeme))))

(defmacro define-list (name empty-name nonempty-name item-name)
  `(progn
     (defclass ,name (slidemacs-entry) ())
     (defclass ,empty-name (,name) ())
     
     (defclass ,nonempty-name (,name)
       ((items :initarg :items)
	(item :initarg :item)))
     
     (add-rule (grammar-rule (,name -> () (make-instance ',empty-name))) *slidemacs-grammar*)
     
     (add-rule (grammar-rule
		(,name -> (,name ,item-name)
		       (make-instance ',nonempty-name
				      :items ,name :item ,item-name))) *slidemacs-grammar*)

     (defmethod display-parse-tree ((entity ,empty-name) (syntax slidemacs-editor-syntax) pane)
       (declare (ignore pane))
       nil)
     
     (defmethod display-parse-tree ((entity ,nonempty-name) (syntax slidemacs-editor-syntax) pane)
       (with-slots (items item) entity
	  (display-parse-tree items syntax pane)
	  (display-parse-tree item syntax pane)))))

(defmacro define-simple-list (name item-name)
  (let ((empty-name (gensym))
	(nonempty-name (gensym)))
    `(define-list ,name ,empty-name ,nonempty-name ,item-name)))

(defmacro define-simple-nonempty-list (nonempty-name item-name)
  (let ((empty-name (gensym))
	(name (gensym)))
    `(define-list ,name ,empty-name ,nonempty-name ,item-name)))

(defgeneric word-is (word string))

(defmethod word-is (word string)
  (string-equal (coerce (buffer-sequence (buffer word) (start-offset word) (end-offset word)) 'string)
		string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sort-definitions (forms)
    (loop for form in forms
	  for name = (and (consp form) (car form))
	  if (eq name 'defclass)
	  collect form into defclasses
	  else if (eq name 'define-simple-list)
	  collect form into simple-lists
	  else if (eq name 'define-simple-nonempty-list)
	  collect form into nonempty-lists
	  else collect form into others
	  end
	  finally (return `(,@defclasses
			    ,@simple-lists
			    ,@nonempty-lists
			    ,@others)))))

(defmacro define-parsing-rules ((grammar entry terminal syntax) &body rules)
  (let (already-processed-rules)
    (flet
	((process-rule (name rule-body start-p)
	   (assert (not (member name already-processed-rules)))
	   (push name already-processed-rules)
	   (cond
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'string))
	      `((defclass ,name (,entry) ((word :initarg :word)))
		(add-rule (grammar-rule (,name -> ((word slidemacs-keyword (word-is word ,(first rule-body)))) :word word))
                 ,grammar)
		,@(if start-p `((add-rule (grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))
		(defmethod display-parse-tree :around ((entity ,name) (syntax ,syntax) pane)
                           (with-drawing-options (pane :ink +blue-violet+)
                             (call-next-method)))))
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'cons)
		   (eq (first (first rule-body)) 'or))
	      `((defclass ,name (,entry) ((item :initarg :item)))
		,@(loop for alt in (cdr (first rule-body))
                        collect `(add-rule (grammar-rule (,name -> ((item ,alt)) :item item)) ,grammar))
		,@(if start-p `((add-rule (grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))
		(defmethod display-parse-tree ((entity ,name) (syntax ,syntax) pane)
		  (display-parse-tree (slot-value entity 'item) syntax pane))))
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'cons)
		   (eq (first (first rule-body)) 'nonempty-list-of))
	      `((define-simple-nonempty-list ,name ,(second (first rule-body)))
		,@(if start-p `((add-rule (grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))))
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'cons)
		   (eq (first (first rule-body)) 'list-of))
	      `((define-simple-list ,name ,(second (first rule-body)))
		,@(if start-p `((add-rule (grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))))
	     ((every #'symbolp rule-body)
	      `((defclass ,name (,entry)
		  (,@(loop for component in rule-body
                           collect `(,component :initarg ,(intern (symbol-name component) :keyword)))))
		(add-rule
		 (grammar-rule (,name ->
				      (,@(loop for component in rule-body
                                               collect `(,component ,component)))
				      ,@(loop for component in rule-body
                                              appending `(,(intern (symbol-name component) :keyword)
                                                          ,component)))) ,grammar)
		,@(if start-p `((add-rule (grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))
		(defmethod display-parse-tree ((entity ,name) (syntax ,syntax) pane)
		  (with-slots ,rule-body
                    entity
		    ,@(loop for component in rule-body collect
                            `(display-parse-tree ,component syntax pane))))))
	     (t (error "Unrecognized rule body ~S for rule ~S~%" rule-body
		       name)))))
      `(progn
        ,@(sort-definitions
           (loop for rule in rules
                 appending (destructuring-bind (=-thingy rule-name &body rule-body)
                               rule
                             (assert (member =-thingy '(:= :==)))
                             (process-rule rule-name rule-body (eq =-thingy :==)))))))))

(define-list slidemacs-terminals empty-slidemacs-terminals nonempty-slidemacs-terminals slidemacs-terminal)

(define-parsing-rules (*slidemacs-grammar* slidemacs-entry slidemacs-terminal slidemacs-editor-syntax)
  (:== slidemacs-slideset slidemacs-slideset-keyword slidemacs-slideset-name block-open
       slideset-info nonempty-list-of-slides block-close)
  (:= slidemacs-slideset-keyword "slideset")
  (:= slidemacs-string (or slidemacs-quoted-string slidemacs-italic-string))
  (:= slidemacs-slideset-name slidemacs-string)
  (:= slideset-info slideset-info-keyword block-open author-institution-pairs opt-slide-venue opt-slide-date block-close)
  (:= author-institution-pairs (list-of author-institution-pair))
  (:= author-institution-pair slide-author slide-institution)
  (:= slideset-info-keyword "info")
  (:= opt-slide-author (or slide-author empty-slidemacs-terminals))
  (:= slide-author author-keyword author)
  (:= author-keyword "author")
  (:= author slidemacs-string)
  (:= opt-slide-venue (or slide-venue empty-slidemacs-terminals))
  (:= slide-venue slide-venue-keyword venue)
  (:= slide-venue-keyword "venue")
  (:= venue slidemacs-string)
  (:= opt-slide-institution (or slide-institution empty-slidemacs-terminals))
  (:= slide-institution institution-keyword institution)
  (:= institution-keyword "institution")
  (:= institution slidemacs-string)
  (:= opt-slide-date (or slide-date empty-slidemacs-terminals))
  (:= slide-date date-keyword opt-date-string)
  (:= opt-date-string (or date-string empty-slidemacs-terminals))
  (:= date-keyword "date")
  (:= date-string slidemacs-string)
  (:= nonempty-list-of-slides
       (nonempty-list-of slidemacs-all-slide-types))
  (:= slidemacs-all-slide-types
      (or slidemacs-slide slidemacs-graph-slide))
  (:= slidemacs-graph-slide slidemacs-graph-slide-keyword slidemacs-slide-name block-open orientation list-of-roots list-of-edges block-close)
  (:= orientation (or horizontal-keyword vertical-keyword))
  (:= horizontal-keyword "horizontal")
  (:= vertical-keyword "vertical")
  (:= slidemacs-graph-slide-keyword "graph")
  (:= list-of-roots (list-of graph-root))
  (:= graph-root graph-root-keyword vertex-name)
  (:= graph-root-keyword "root")
  (:= list-of-edges (list-of graph-edge))
  (:= graph-edge graph-edge-keyword from-keyword from-vertex to-keyword to-vertex)
  (:= graph-edge-keyword "edge")
  (:= from-keyword "from")
  (:= to-keyword "to")
  (:= from-vertex vertex-name)
  (:= to-vertex vertex-name)
  (:= vertex-name slidemacs-string)
  (:= slidemacs-slide slidemacs-slide-keyword slidemacs-slide-name block-open
      nonempty-list-of-bullets block-close)
  (:= slidemacs-slide-keyword "slide")
  (:= slidemacs-slide-name slidemacs-string)
  (:= nonempty-list-of-bullets (nonempty-list-of slidemacs-bullet-types))
  (:= slidemacs-bullet-types (or slidemacs-bullet picture-node url-point reveal-button-point))
  (:= slidemacs-bullet bullet talking-point)
  (:= talking-point slidemacs-string)
  (:= picture-node picture-keyword picture-pathname)
  (:= picture-keyword "picture")
  (:= picture-pathname slidemacs-string)
  (:= url-point url-keyword url-string)
  (:= url-keyword "url")
  (:= url-string slidemacs-string)
  (:= reveal-button-point reveal-keyword button-label reveal-text)
  (:= reveal-keyword "reveal")
  (:= button-label slidemacs-string)
  (:= reveal-text slidemacs-string))

(defmethod display-parse-tree ((entity slidemacs-terminal) (syntax slidemacs-editor-syntax) pane)
  (with-slots (item) entity
      (display-parse-tree item syntax pane)))

(defmethod display-parse-tree ((entity slidemacs-italic-string) (syntax slidemacs-editor-syntax) pane)
  (with-text-face (pane :italic)
    (call-next-method)))

(defmethod display-parse-tree ((entity slidemacs-entry) (syntax slidemacs-editor-syntax) pane)
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

(defgeneric display-parse-stack (symbol stack syntax pane))

(defmethod display-parse-stack (symbol stack (syntax slidemacs-editor-syntax) pane)
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

(defmethod update-syntax-for-display (buffer (syntax slidemacs-editor-syntax) top bot)
  nil)

(defmethod inter-lexeme-object-p ((lexer slidemacs-lexer) object)
  (whitespacep (syntax (buffer lexer)) object))

(defmethod update-syntax (buffer (syntax slidemacs-editor-syntax))
  (with-slots (parser lexer valid-parse) syntax
    (let* ((low-mark (low-mark buffer))
	   (high-mark (high-mark buffer)))
       (when (mark<= low-mark high-mark)
	 (let ((first-invalid-position (delete-invalid-lexemes lexer low-mark high-mark)))
	   (setf valid-parse first-invalid-position)
	   (update-lex lexer first-invalid-position high-mark)
           (loop until (= valid-parse (nb-lexemes lexer))
                 do (let ((current-token (lexeme lexer (1- valid-parse)))
                          (next-lexeme (lexeme lexer valid-parse)))
                      (setf (slot-value next-lexeme 'state)
                            (advance-parse parser (list next-lexeme) (slot-value current-token 'state))))
                 (incf valid-parse)))))))

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
	(tab-width (tab-width pane)))
    (loop while (and (< start end)
                     (whitespacep (syntax buffer)
                                  (buffer-object buffer start)))
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

(defvar *handle-whitespace* t)

(defmethod display-parse-tree :before ((entity slidemacs-entry) (syntax slidemacs-editor-syntax) pane)
  (when *handle-whitespace*
    (handle-whitespace pane (buffer pane) *white-space-start* (start-offset entity))
    (setf *white-space-start* (end-offset entity))))

(defmethod display-parse-tree :around ((entity slidemacs-parse-tree) (syntax slidemacs-editor-syntax) pane)
  (if (not (typep syntax 'slidemacs-gui-syntax))
      (with-slots (top bot) pane
        (when (and (end-offset entity) (mark> (end-offset entity) top))
          (call-next-method)))
      (call-next-method)))

(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax slidemacs-editor-syntax) current-p)
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
    (when (region-visible-p pane) (display-region pane syntax))
    (display-cursor pane syntax current-p)))
