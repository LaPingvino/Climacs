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
   (head -> (<head> title </head>))
   (title -> (<title> texts </title>))
   (body -> (<body> texts </body>))
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
			      states)))))
       (print (find 'html (gethash (initial-state parser) (parse-trees (cdar states)))
		    :key #'type-of)
	      *query-io*)
       (finish-output *query-io*))))

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

(defmethod empty-state-p (state)
  (maphash (lambda (key val)
	     (declare (ignore key))
	     (loop for parse-tree in val
		   do (return-from empty-state-p nil)))
	   (parse-trees state))
  (maphash (lambda (key val)
	     (declare (ignore key))
	     (loop for parse-tree in val
		   do (return-from empty-state-p nil)))
	   (incomplete-items state)))

(defmethod backward-to-error (point (syntax html-syntax))
  (let ((states (slot-value syntax 'states)))
    ;; find the last state before point
    (loop until (or (null states)
		    (mark< (caar states) point))
	  do (pop states))
    (when (null states)
      (return-from backward-to-error "no more errors"))
    (when (empty-state-p (cdar states))
      (loop for ((m1 . s1) (m2 . s2)) on states
	    until (not (empty-state-p s2))
	    finally (setf (offset point) (offset m1)))
      (return-from backward-to-error "no valid parse from this point"))
    (loop for (mark . state) in states
	  for tree = (find-bad-parse-tree state)
	  when tree
	    do (setf (offset point) (offset mark))
	       (return (message tree))
	  finally (return "no more errors"))))
