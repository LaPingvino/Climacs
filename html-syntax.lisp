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

(in-package :climacs-syntax) ;;; Put this in a separate package once it works

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar classes

(defclass html-sym ()
  ((badness :initform 0 :initarg :badness :reader badness)
   (message :initform "" :initarg :message :reader message)))

(defmethod parse-tree-better ((t1 html-sym) (t2 html-sym))
  (and (eq (class-of t1) (class-of t2))
       (< (badness t1) (badness t2))))

(defclass html (html-sym) ())
(defclass head (html-sym) ())
(defclass title (html-sym) ())
(defclass body (html-sym) ())
(defclass h1 (html-sym) ())
(defclass h2 (html-sym) ())
(defclass h3 (html-sym) ())
(defclass para (html-sym) ())
(defclass ul (html-sym) ())
(defclass li (html-sym) ())
(defclass texts (html-sym) ())

(defclass error-token (html-sym) ())
(defclass text (html-sym) ())

(defclass <html> (html-sym) ())
(defclass </html> (html-sym) ())
(defclass <head> (html-sym) ())
(defclass </head> (html-sym) ())
(defclass <title> (html-sym) ())
(defclass </title> (html-sym) ())
(defclass <body> (html-sym) ())
(defclass </body> (html-sym) ())
(defclass <h1> (html-sym) ())
(defclass </h1> (html-sym) ())
(defclass <h2> (html-sym) ())
(defclass </h2> (html-sym) ())
(defclass <h3> (html-sym) ())
(defclass </h3> (html-sym) ())
(defclass <p> (html-sym) ())
(defclass </p> (html-sym) ())
(defclass <ul> (html-sym) ())
(defclass </ul> (html-sym) ())
(defclass <li> (html-sym) ())
(defclass </li> (html-sym) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defparameter *token-table*
	      '(("<html>" . <html>)
		("</html>" . </html>)
		("<head>" . <head>)
		("</head>" . </head>)
		("<title>" . <title>)
		("</title>" . </title>)
		("<body>" . <body>)
		("</body>" . </body>)
		("<h1>" . <h1>)
		("</h1>" . </h1>)
		("<h2>" . <h2>)
		("</h2>" . </h2>)
		("<h3>" . <h3>)
		("</h3>" . </h3>)
		("<p>" . <p>)
		("</p>" . </p>)
		("<ul>" . <ul>)
		("</ul>" . </ul>)
		("<li>" . <li>)
		("</li>" . </li>)))

(defclass html-lexer (lexer)
  ((mark :initarg :mark)))

(defmethod lex ((lexer html-lexer))
  (with-slots (mark) lexer
     (assert (not (end-of-buffer-p mark)))
     (cond ((or (end-of-line-p mark)
		(not (eql (object-after mark) #\<)))
	    (when (end-of-line-p mark)
	      (forward-object mark))
	    (loop until (or (end-of-line-p mark)
			    (eql (object-after mark) #\<))
		  do (forward-object mark))
	    (make-instance 'text))
	   (t
	    (let ((offset (offset mark)))
	      (forward-object mark)
	      (loop until (or (end-of-line-p mark)
			      (whitespacep (object-after mark))
			      (eql (object-before mark) #\>))
		    do (forward-object mark))
	      (let* ((word (region-to-sequence offset mark))
		     (class-name (cdr (assoc word *token-table* :test #'equalp))))
		(make-instance (or class-name 'error-token))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defparameter *html-grammar*
  (grammar
   (html -> (<html> head body </html>))
   (<html> -> (html-sym) :badness 5 :message "substituted <html>")
   (</html> -> (html-sym) :badness 5 :message "substituted </html>")
   (<html> -> () :badness 10 :message "missing <html> inserted")
   (</html> -> () :badness 10 :message "missing </html> inserted")
   (head -> (<head> title </head>))
   (<head> -> (html-sym) :badness 5 :message "substituted <head>")
   (</head> -> (html-sym) :badness 5 :message "substituted </head>")
   (<head> -> () :badness 10 :message "missing <head> inserted")
   (</head> -> () :badness 10 :message "missing </head> inserted")
   (title -> (<title> texts </title>))
   (<title> -> (html-sym) :badness 5 :message "substituted <title>")
   (</title> -> (html-sym) :badness 5 :message "substituted </title>")
   (<title> -> () :badness 10 :message "missing <title> inserted")
   (</title> -> () :badness 10 :message "missing </title> inserted")
   (body -> (<body> texts </body>))
   (<body> -> (html-sym) :badness 5 :message "substituted <body>")
   (</body> -> (html-sym) :badness 5 :message "substituted </body>")
   (<body> -> () :badness 10 :message "missing <body> inserted")
   (</body> -> () :badness 10 :message "missing </body> inserted")
   (texts -> ())
   (texts -> (texts text))))

(define-syntax html-syntax ("HTML" (basic-syntax))
  ((parser)
   (states)))

(defmethod initialize-instance :after ((syntax html-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser states buffer) syntax
     (setf parser (make-instance 'parser
		     :grammar *html-grammar*
		     :lexer (make-instance 'html-lexer
			       :mark (make-instance 'standard-left-sticky-mark :buffer buffer))
		     :target 'html))
     (setf states (list (cons (make-instance 'standard-left-sticky-mark :buffer buffer)
			      (initial-state parser))))))

(defmethod update-syntax (buffer (syntax html-syntax))
  (let ((low-mark (low-mark buffer)))
    (with-slots (parser states) syntax
       (with-slots (lexer) parser
	  (with-slots (mark) lexer
	     (loop until (or (null (cdr states))
			     (< (offset (caar states)) (offset low-mark)))
		   do (pop states))
	     (setf (offset mark) (offset (caar states)))
	     (loop until (end-of-buffer-p mark)
		   do (let ((token (lex lexer)))
			(push (cons (clone-mark mark)
				    (advance-parse parser (list token) (cdar states)))
			      states))))))))

(defgeneric forward-to-error (point syntax))
(defgeneric backward-to-error (point syntax))

(defun find-bad-parse-tree (state)
  (maphash (lambda (key parse-trees)
	     (declare (ignore key))
	     (let ((parse-tree (find-if (lambda (parse-tree)
					  (plusp (badness parse-tree)))
					parse-trees)))
	       (when parse-tree
		 (return-from find-bad-parse-tree parse-tree))))
	   (parse-trees state)))

(defmethod backward-to-error (point (syntax html-syntax))
  (let ((states (slot-value syntax 'states)))
    (loop until (or (null states)
		    (mark< (caar states) point))
	  do (pop states))
    (loop for (mark . state) in states
	  for tree = (find-bad-parse-tree state)
	  when tree
	    do (setf (offset point) (offset mark))
	       (return (message tree))
	  finally (return "no more errors"))))
