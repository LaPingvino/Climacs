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

;;; Syntax for analysing Common Lisp

(in-package :climacs-cl-syntax)

(defclass stack-entry ()
  ((start-mark :initarg :start-mark :reader start-mark)
   (size :initarg :size))
  (:documentation "A stack entry corresponds to a syntactic category"))

(defgeneric end-offset (stack-entry))

(defmethod end-offset ((entry stack-entry))
  (with-slots (start-mark size) entry
     (+ (offset start-mark) size)))

(defclass error-entry (stack-entry) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Terminal entries.

(defclass terminal-entry (stack-entry)
  ((parse-tree))
  (:documentation "Used for tokens (numbers, symbols), but also for
macro characters that start more complex expressions."))

(defclass start-entry (terminal-entry)
  ()
  (:documentation "dummy entry before all the others."))

(defclass token-entry (terminal-entry)
  ()
  (:documentation "the syntactic class of tokens."))

(defclass character-entry (terminal-entry)
  ()
  (:documentation "the syntactic class of characters."))

(defclass double-quote-entry (terminal-entry)
  ())

(defclass quote-entry (terminal-entry)
  ()
  (:documentation "syntactic class of quote inidicators."))

(defclass backquote-entry (terminal-entry)
  ()
  (:documentation "syntactic class of backquote indicators. "))

(defclass unquote-entry (terminal-entry)
  ()
  (:documentation "syntactic class of unquote indicators. "))

(defclass comment-entry (terminal-entry)
  ()
  (:documentation "syntactic class of single-line comment indicators. "))

(defclass list-start-entry (terminal-entry)
  ()
  (:documentation "syntactic class of list start indicators."))

(defclass list-end-entry (terminal-entry)
  ()
  (:documentation "syntactic class of list end indicators."))

(defclass label-ref-entry (terminal-entry)
  ()
  (:documentation "syntactic class of label reference indicators."))

(defclass label-entry (terminal-entry)
  ()
  (:documentation "syntactic class of label indicators."))

(defclass function-entry (terminal-entry)
  ()
  (:documentation "syntactic class of function indicators."))

(defclass balanced-comment-entry (terminal-entry)
  ()
  (:documentation "syntactic class of balanced comment entry indicators. "))

(defclass read-time-conditional-plus-entry (terminal-entry)
  ()
  (:documentation "syntactic class of read-time conditional indicators. "))

(defclass read-time-conditional-minus-entry (terminal-entry)
  ()
  (:documentation "syntactic class of read-time conditional indicators. "))

(defclass vector-entry (terminal-entry)
  ()
  (:documentation "syntactic class of vector indicators."))

(defclass array-entry (terminal-entry)
  ()
  (:documentation "syntactic class of array indicators."))

(defclass bitvector-entry (terminal-entry)
  ()
  (:documentation "syntactic class of bit vector indicators. "))

(defclass uninterned-symbol-entry (terminal-entry)
  ()
  (:documentation "syntactic class of uninterned symbol indicators. "))

(defclass read-time-evaluation-entry (terminal-entry)
  ()
  (:documentation "syntactic class of read-time evaluation indicators. "))

(defclass complex-entry (terminal-entry)
  ()
  (:documentation "syntactic class of complex indicators."))

(defclass octal-entry (terminal-entry)
  ()
  (:documentation "syntactic class of octal rational indicators."))

(defclass hex-entry (terminal-entry)
  ()
  (:documentation "syntactic class of hex rational indicators."))

(defclass radix-n-entry (terminal-entry)
  ()
  (:documentation "syntactic class of radix-n rational indicators."))

(defclass pathname-entry (terminal-entry)
  ()
  (:documentation "syntactic class of pathname indicators."))

(defclass structure-entry (terminal-entry)
  ()
  (:documentation "syntactic class of structure indicators."))

(defclass binary-entry (terminal-entry)
  ()
  (:documentation "syntactic class of binary rational indicators."))

(defclass unknown-entry (terminal-entry)
  ()
  (:documentation "unknown (user-defined) syntactic classes."))

(define-syntax cl-syntax ("Common Lisp" (basic-syntax))
  ((elements :initform (make-instance 'standard-flexichain))
   (guess-pos :initform 1)))
  
(defmethod initialize-instance :after ((syntax cl-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer elements) syntax
     (let ((mark (clone-mark (low-mark buffer) :left)))
       (setf (offset mark) 0)
       (insert* elements 0 (make-instance 'start-entry
			      :start-mark mark :size 0)))))

(defun next-entry (scan)
  (let ((start-mark (clone-mark scan)))
    (flet ((fo () (forward-object scan)))
      (macrolet ((make-entry (type)
		   `(return-from next-entry
		      (make-instance ,type :start-mark start-mark
				     :size (- (offset scan) (offset start-mark))))))
	(loop with object = (object-after scan)
	      until (end-of-buffer-p scan)
	      do (case object
		   (#\( (fo) (make-entry 'list-start-entry))
		   (#\) (fo) (make-entry 'list-end-entry))
		   (#\; (loop do (fo)
			      until (end-of-line-p scan))
			(make-entry 'comment-entry))
		   (#\" (fo) (make-entry 'double-quote-entry))
		   (#\' (fo) (make-entry 'quote-entry))
		   (#\` (fo) (make-entry 'backquote-entry))
		   (#\, (fo) (make-entry 'unquote-entry))
		   (#\# (fo)
			(loop until (end-of-buffer-p scan)
			      while (member (object-after scan)
					    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
			      do (fo))
			(if (end-of-buffer-p scan)
			    (make-entry 'error-entry)
			    (case (object-after scan)
			      (#\# (fo) (make-entry 'label-ref-entry))
			      (#\= (fo) (make-entry 'label-entry))
			      (#\' (fo) (make-entry 'function-entry))
			      (#\| (fo) (make-entry 'balanced-comment-entry))
			      (#\+ (fo) (make-entry 'read-time-conditional-plus-entry))
			      (#\- (fo) (make-entry 'read-time-conditional-minus-entry))
			      (#\( (fo) (make-entry 'vector-entry))
			      (#\* (fo) (make-entry 'bitvector-entry))
			      (#\: (fo) (make-entry 'uninterned-symbol-entry))
			      (#\. (fo) (make-entry 'read-time-evaluation-entry))
			      ((#\A #\a) (fo) (make-entry 'array-entry))
			      ((#\B #\b) (fo) (make-entry 'binary-entry))
			      ((#\C #\c) (fo) (make-entry 'complex-entry))
			      ((#\O #\o) (fo) (make-entry 'octal-entry))
			      ((#\P #\p) (fo) (make-entry 'pathname-entry))
			      ((#\R #\r) (fo) (make-entry 'radix-n-entry))
			      ((#\S #\s) (fo) (make-entry 'structure-entry))
			      ((#\X #\x) (fo) (make-entry 'hex-entry))
			      (#\\ (fo)
				   (cond ((end-of-buffer-p scan)
					  (make-entry 'error-entry))
					 ((not (constituentp (object-after scan)))
					  (fo)
					  (make-entry 'character-entry))
					 (t
					  (fo)
					  (loop until (end-of-buffer-p scan)
						while (constituentp (object-after scan))
						do (fo))
					  (make-entry 'character-entry))))
			      (t (make-entry 'error-entry)))))
		   (t (cond ((constituentp object)
			     (loop until (end-of-buffer-p scan)
				   while (constituentp (object-after scan))
				   do (fo))
			     (make-entry 'token-entry))
			    (t
			     (fo) (make-entry 'error-entry))))))))))

(defmethod update-syntax (buffer (syntax cl-syntax))
  (let ((low-mark (low-mark buffer))
	(high-mark (high-mark buffer))
	(scan))
    (with-slots (elements guess-pos) syntax
       (when (mark<= low-mark high-mark)
	 ;; go back to a position before low-mark
	 (loop until (or (= guess-pos 1)
			 (mark< (end-offset (element* elements (1- guess-pos))) low-mark))
	       do (decf guess-pos))
	 ;; go forward to the last position before low-mark
	 (loop with nb-elements = (nb-elements elements)
	       until (or (= guess-pos nb-elements)
			 (mark>= (end-offset (element* elements guess-pos)) low-mark))
	       do (incf guess-pos))
	 ;; delete entries that must be reparsed
	 (loop until (or (= guess-pos (nb-elements elements))
			 (mark> (start-mark (element* elements guess-pos)) high-mark))
	       do (delete* elements guess-pos))
	 (let ((m (clone-mark (low-mark buffer) :left)))
	   (setf (offset m)
		 (if (zerop guess-pos)
		     0
		     (end-offset (element* elements (1- guess-pos)))))
	   (setf scan m))
	 ;; scan
	 (loop with start-mark = nil
	       do (loop until (end-of-buffer-p scan)
			while (whitespacep (object-after scan))
			do (forward-object scan))
	       until (if (end-of-buffer-p high-mark)
			 (end-of-buffer-p scan)
			 (mark> scan high-mark))
	       do (setf start-mark (clone-mark scan))
		  (insert* elements guess-pos (next-entry scan))
		  (incf guess-pos))))))
