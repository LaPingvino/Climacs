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

(defclass html-sym ()
  ((badness :initform 0 :initarg :badness :reader badness)
   (message :initform "" :initarg :message :reader message)))

(defmethod parse-tree-better ((t1 html-sym) (t2 html-sym))
  (and (eq (class-of t1) (class-of t2))
       (< (badness t1) (badness t2))))

(defclass html-nonterminal (html-sym)
  ((start-offset :initarg :start-offset :reader start-offset)
   (end-offset :initarg :end-offset :reader end-offset)))

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
(defclass para (html-words) ())

(defclass html-token (html-sym)
  ((start-mark :initarg :start-mark :reader start-mark)
   (size :initarg :size)))

(defgeneric end-offset (html-token))

(defmethod end-offset ((token html-token))
  (with-slots (start-mark size) token
     (+ (offset start-mark) size)))

(defgeneric start-offset (html-token))

(defmethod start-offset ((token html-token))
  (offset (start-mark token)))

(defclass <html> (html-token) () (:default-initargs :size 6))
(defclass </html> (html-token) ()(:default-initargs :size 7))
(defclass <head> (html-token) () (:default-initargs :size 6))
(defclass </head> (html-token) () (:default-initargs :size 7))
(defclass <title> (html-token) () (:default-initargs :size 7))
(defclass </title> (html-token) () (:default-initargs :size 8))
(defclass <body> (html-token) () (:default-initargs :size 6))
(defclass </body> (html-token) () (:default-initargs :size 7))
(defclass <h1> (html-token) () (:default-initargs :size 4))
(defclass </h1> (html-token) () (:default-initargs :size 5))
(defclass <h2> (html-token) () (:default-initargs :size 4))
(defclass </h2> (html-token) () (:default-initargs :size 5))
(defclass <h3> (html-token) () (:default-initargs :size 4))
(defclass </h3> (html-token) () (:default-initargs :size 5))
(defclass <p> (html-token) () (:default-initargs :size 3))
(defclass </p> (html-token) () (:default-initargs :size 4))
(defclass <ul> (html-token) () (:default-initargs :size 4))
(defclass </ul> (html-token) () (:default-initargs :size 5))
(defclass <li> (html-token) () (:default-initargs :size 4))
(defclass </li> (html-token) () (:default-initargs :size 5))

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

(defun next-token (scan)
  (let ((start-mark (clone-mark scan)))
    (flet ((fo () (forward-object scan)))
      (macrolet ((make-entry (type)
		   `(return-from next-token
		      (make-instance ,type :start-mark start-mark
				     :size (- (offset scan) (offset start-mark))))))
	(loop with object = (object-after scan)
	      until (end-of-buffer-p scan)
	      do (case object
		   (#\< (fo) (make-entry 'tag-start))
		   (#\> (fo) (make-entry 'tag-end))
		   (#\/ (fo) (make-entry 'slash))
		   (t (cond ((alphanumericp object)
			     (loop until (end-of-buffer-p scan)
				   while (alphanumericp (object-after scan))
				   do (fo))
			     (make-entry 'word))
			    (t
			     (fo) (make-entry 'delimiter))))))))))

(define-syntax html-syntax ("HTML" (basic-syntax))
  ((tokens :initform (make-instance 'standard-flexichain))
   (guess-pos :initform 1)
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
    (html -> (<html> head body </html>)
	  :start-offset (start-offset <html>) :end-offset (end-offset </html>)
	  :start <html> :head head :body body :end </html>)
    (head -> (<head> title </head>)
	  :start-offset (start-offset <head>) :end-offset (end-offset </head>)
	  :start <head> :title title :end </head>)
    (title -> (<title> words </title>)
	   :start-offset (start-offset <title>) :end-offset (end-offset </title>)
	   :start <title> :words words :end </title>)
    (body -> (<body> words </body>)
	  :start-offset (start-offset <body>) :end-offset (end-offset </body>)
	  :start <body> :words words :end </body>)
    (words -> ()
	   (make-instance 'empty-words :start-offset nil))
    (words -> (words word)
	   (make-instance 'nonempty-words
	      :start-offset (or (start-offset words) (start-offset word))
	      :end-offset (end-offset word)
	      :words words :word word))))

(defmethod initialize-instance :after ((syntax html-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser tokens buffer) syntax
     (setf parser (make-instance 'parser
		     :grammar *html-grammar*
		     :target 'html))
     (insert* tokens 0 (make-instance 'start-element
			  :start-mark (make-instance 'standard-left-sticky-mark
					 :buffer buffer
					 :offset 0)
			  :size 0
			  :state (initial-state parser)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defmethod update-syntax-for-display (buffer (syntax html-syntax) top bot)
  (with-slots (parser tokens valid-parse) syntax
     (loop until (= valid-parse (nb-elements tokens))
	   while (mark<= (end-offset (element* tokens valid-parse)) bot)
	   do (let ((current-token (element* tokens (1- valid-parse)))
		    (next-token (element* tokens valid-parse)))
		(setf (slot-value next-token 'state)
		      (advance-parse parser (list next-token) (slot-value current-token 'state))))
	      (incf valid-parse))))

(defmethod update-syntax (buffer (syntax html-syntax))
  (let ((low-mark (low-mark buffer))
	(high-mark (high-mark buffer))
	(scan))
    (with-slots (tokens guess-pos valid-parse) syntax
       (when (mark<= low-mark high-mark)
	 ;; go back to a position before low-mark
	 (loop until (or (= guess-pos 1)
			 (mark< (end-offset (element* tokens (1- guess-pos))) low-mark))
	       do (decf guess-pos))
	 ;; go forward to the last position before low-mark
	 (loop with nb-elements = (nb-elements tokens)
	       until (or (= guess-pos nb-elements)
			 (mark>= (end-offset (element* tokens guess-pos)) low-mark))
	       do (incf guess-pos))
	 ;; mark valid parse
	 (setf valid-parse guess-pos)
	 ;; delete entries that must be reparsed
	 (loop until (or (= guess-pos (nb-elements tokens))
			 (mark> (start-mark (element* tokens guess-pos)) high-mark))
	       do (delete* tokens guess-pos))
	 (setf scan (make-instance 'standard-left-sticky-mark
		       :buffer buffer
		       :offset (if (zerop guess-pos)
				   0
				   (end-offset (element* tokens (1- guess-pos))))))
	 ;; scan
	 (loop with start-mark = nil
	       do (loop until (end-of-buffer-p scan)
			while (whitespacep (object-after scan))
			do (forward-object scan))
	       until (if (end-of-buffer-p high-mark)
			 (end-of-buffer-p scan)
			 (mark> scan high-mark))
	       do (setf start-mark (clone-mark scan))
		  (insert* tokens guess-pos (next-token scan))
		  (incf guess-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display



