;;; -*- Mode: Lisp; Package: CLIMACS-BUFFER -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

;;; A stupid implementation of the buffer protocol.  This
;;; implementation serves two purposes: First, so that higher-level
;;; functionality can be built on top of a working implementation of
;;; the buffer protocol, and second, to use as a comparison for
;;; testing a new, better implementation of the buffer protocol.

(in-package :climacs-buffer)

(defclass buffer () ())

(defclass standard-buffer (buffer)
  ((contents :initform (list nil) :initarg :contents)
   (marks :initform '())))

(defgeneric buffer (mark))  

(defclass mark () ())

(defclass left-sticky-mark (mark) ())

(defclass right-sticky-mark (mark) ())

(defgeneric offset (mark))
(defgeneric (setf offset) (new-offset mark))

(defclass mark-mixin ()
  ((buffer :initarg :buffer :reader buffer)
   (offset :initarg :offset :initform 0 :accessor offset)))

(defmethod (setf offset) :before (new-offset (mark mark-mixin))
  (assert (<= 0 new-offset (size (buffer mark))) ()
	  (make-condition 'no-such-offset :offset new-offset)))

(defmethod initialize-instance :after ((mark mark-mixin) &rest args)
  (declare (ignore args))
  (push mark (slot-value (buffer mark) 'marks)))

(defclass standard-left-sticky-mark (left-sticky-mark mark-mixin) ())

(defclass standard-right-sticky-mark (right-sticky-mark mark-mixin) ())

(defgeneric clone-mark (mark &optional type))

(defmethod clone-mark ((mark standard-left-sticky-mark) &optional type)
  (unless type
    (setf type 'standard-left-sticky-mark))
  (make-instance type :buffer (buffer mark) :offset (offset mark)))

(defmethod clone-mark ((mark standard-right-sticky-mark) &optional type)
  (unless type
    (setf type 'standard-right-sticky-mark))
  (make-instance type :buffer (buffer mark) :offset (offset mark)))

(define-condition no-such-offset (simple-error)
  ((offset :reader offset :initarg :offset))
  (:report (lambda (condition stream)
	     (format stream "No such offset: ~a" (offset condition)))))

(defgeneric size (buffer))

(defmethod size ((buffer standard-buffer))
  (1- (length (slot-value buffer 'contents))))


(defgeneric number-of-lines (buffer))

(defmethod number-of-lines ((buffer standard-buffer))
  (count #\Newline (cdr (slot-value buffer 'contents))))

(defgeneric mark< (mark1 mark2))

(defmethod mark< ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (< (offset mark1) (offset mark2)))

(defmethod mark< ((mark1 mark-mixin) (mark2 integer))
  (< (offset mark1) mark2))

(defmethod mark< ((mark1 integer) (mark2 mark-mixin))
  (< mark1 (offset mark2)))

(defgeneric mark<= (mark1 mark2))

(defmethod mark<= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (<= (offset mark1) (offset mark2)))

(defmethod mark<= ((mark1 mark-mixin) (mark2 integer))
  (<= (offset mark1) mark2))

(defmethod mark<= ((mark1 integer) (mark2 mark-mixin))
  (<= mark1 (offset mark2)))

(defgeneric mark= (mark1 mark2))

(defmethod mark= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (= (offset mark1) (offset mark2)))

(defmethod mark= ((mark1 mark-mixin) (mark2 integer))
  (= (offset mark1) mark2))

(defmethod mark= ((mark1 integer) (mark2 mark-mixin))
  (= mark1 (offset mark2)))

(defgeneric mark> (mark1 mark2))

(defmethod mark> ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (> (offset mark1) (offset mark2)))

(defmethod mark> ((mark1 mark-mixin) (mark2 integer))
  (> (offset mark1) mark2))

(defmethod mark> ((mark1 integer) (mark2 mark-mixin))
  (> mark1 (offset mark2)))

(defgeneric mark>= (mark1 mark2))

(defmethod mark>= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (>= (offset mark1) (offset mark2)))

(defmethod mark>= ((mark1 mark-mixin) (mark2 integer))
  (>= (offset mark1) mark2))

(defmethod mark>= ((mark1 integer) (mark2 mark-mixin))
  (>= mark1 (offset mark2)))


(defgeneric beginning-of-buffer (mark))

(defmethod beginning-of-buffer ((mark mark-mixin))
  (setf (offset mark) 0))

(defgeneric end-of-buffer (mark))

(defmethod end-of-buffer ((mark mark-mixin))
  (setf (offset mark) (size (buffer mark))))

(defgeneric beginning-of-buffer-p (mark))

(defmethod beginning-of-buffer-p ((mark mark-mixin))
  (zerop (offset mark)))

(defgeneric end-of-buffer-p (mark))

(defmethod end-of-buffer-p ((mark mark-mixin))
  (= (offset mark) (size (buffer mark))))

(defgeneric beginning-of-line (mark))

(defmethod beginning-of-line ((mark mark-mixin))
  (loop until (or (beginning-of-buffer-p mark)
		  (eql (object-before mark) #\Newline))
	do (decf (offset mark))))

(defgeneric end-of-line (mark))

(defmethod end-of-line ((mark mark-mixin))
  (loop until (or (end-of-buffer-p mark)
		  (eql (object-after mark) #\Newline))
	do (incf (offset mark))))

(defgeneric beginning-of-line-p (mark))

(defmethod beginning-of-line-p ((mark mark-mixin))
  (or (beginning-of-buffer-p mark)
      (eql (object-before mark) #\Newline)))

(defgeneric end-of-line-p (mark))

(defmethod end-of-line-p ((mark mark-mixin))
  (or (end-of-buffer-p mark)
      (eql (object-after mark) #\Newline)))

(defgeneric line-number (mark))

(defmethod line-number ((mark mark-mixin))
  (count #\Newline (cdr (slot-value (buffer mark) 'contents))
	 :end (offset mark)))

(defgeneric column-number (mark))

(defmethod column-number ((mark mark-mixin))
  (loop for offset downfrom (offset mark)
	while (> offset 0)
	until (eql (buffer-object (buffer mark) (1- offset)) #\Newline)
	count t))

(defgeneric insert-buffer-object (buffer offset object))

(defmethod insert-buffer-object ((buffer standard-buffer) offset object)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (push object (cdr (nthcdr offset (slot-value buffer 'contents))))
  (loop for mark in (slot-value buffer 'marks)
	when (or (> (offset mark) offset)
		 (and (= (offset mark) offset)
		      (typep mark 'right-sticky-mark)))
	  do (incf (offset mark))))

(defgeneric insert-buffer-sequence (buffer offset sequence))
      
(defmethod insert-buffer-sequence ((buffer standard-buffer) offset sequence)
  (loop for elem across sequence
	do (insert-buffer-object buffer offset elem)
	   (incf offset)))

(defgeneric insert-object (mark object))

(defmethod insert-object ((mark mark-mixin) object)
  (insert-buffer-object (buffer mark) (offset mark) object))

(defgeneric insert-sequence (mark sequence))

(defmethod insert-sequence ((mark mark-mixin) sequence)
  (insert-buffer-sequence (buffer mark) (offset mark) sequence))

(defgeneric delete-buffer-range (buffer offset n))

(defmethod delete-buffer-range ((buffer standard-buffer) offset n)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (with-slots (contents marks) buffer
     (setf (cdr (nthcdr offset contents)) (nthcdr (+ offset n 1) contents))
     (loop for mark in marks
	   when (> (offset mark) offset)
	     do (setf (offset mark) (max offset (- (offset mark) n))))))

(defgeneric delete-range (mark &optional n))

(defmethod delete-range ((mark mark-mixin) &optional (n 1))
  (cond ((plusp n) (delete-buffer-range (buffer mark) (offset mark) n))
	((minusp n) (delete-buffer-range (buffer mark) (+ (offset mark) n) (- n)))
	(t nil)))

(defgeneric delete-region (mark1 mark2))

(defmethod delete-region ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (when (> (offset mark1) (offset mark2))
    (delete-buffer-range (buffer mark1)
			 (offset mark1)
			 (- (offset mark2) (offset mark1)))))

(defmethod delete-region ((mark1 mark-mixin) offset2)
  (when (> offset2 (offset mark1))
    (delete-buffer-range (buffer mark1)
			 (offset mark1)
			 (- offset2 (offset mark1)))))

(defmethod delete-region (offset1 (mark2 mark-mixin))
  (when (> (offset mark2) offset1)
    (delete-buffer-range (buffer mark2)
			 offset1
			 (- (offset mark2) offset1))))

(defgeneric buffer-object (buffer offset))

(defmethod buffer-object ((buffer standard-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (nth (1+ offset) (slot-value buffer 'contents)))

(defgeneric buffer-sequence (buffer offset1 offset2))

(defmethod buffer-sequence ((buffer standard-buffer) offset1 offset2)
  (assert (<= 0 offset1 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset1))
  (assert (<= 0 offset2 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset2))
  (coerce (subseq (slot-value buffer 'contents) (1+ offset1) (1+ offset2)) 'vector))

(defgeneric object-before (mark))

(defmethod object-before ((mark mark-mixin))
  (buffer-object (buffer mark) (1- (offset mark))))

(defgeneric object-after (mark))

(defmethod object-after ((mark mark-mixin))
  (buffer-object (buffer mark) (offset mark)))

(defgeneric region-to-sequence (mark1 mark2))

(defmethod region-to-sequence ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (buffer-sequence (buffer mark1) (offset mark1) (offset mark2)))


