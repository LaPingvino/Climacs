;;; -*- Mode: Lisp; Package: CLIMACS-BUFFER -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)

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

;;; A not-so-stupid implementation of the buffer protocol.  This
;;; implementation serves two purposes: First, so that higher-level
;;; functionality can be built on top of a working implementation of
;;; the buffer protocol, and second, to use as a comparison for
;;; testing a new, better implementation of the buffer protocol.

(in-package :climacs-buffer)

(defclass buffer () ()
  (:documentation "A base class for all buffers. A buffer conceptually contains a
large array of arbitrary objects.  Lines of objects are separated by
newline characters.  The last object of the buffer is not
necessarily a newline character."))

(defgeneric low-mark (buffer))

(defgeneric high-mark (buffer))

(defgeneric modified-p (buffer))

(defclass standard-buffer (buffer)
  ((contents :initform (make-instance 'standard-cursorchain))
   (low-mark :reader low-mark)
   (high-mark :reader high-mark)
   (modified :initform nil :reader modified-p))
  (:documentation "The Climacs standard buffer [an instantable subclass of buffer]."))

(defgeneric buffer (mark)
  (:documentation "Return the buffer that the mark is positioned in."))  

(defclass mark () ()
  (:documentation "A base class for all marks."))

(defclass left-sticky-mark (mark) ()
  (:documentation "A subclass of mark.  A mark of this type will \"stick\" to the left of 
an object, i.e. when an object is inserted at this mark, the mark will 
be positioned to the left of the object"))

(defclass right-sticky-mark (mark) ()
  (:documentation "A subclass of mark.  A mark of this type will \"stick\" to the right of 
an object, i.e. when an object is inserted at this mark, the mark will 
be positioned to the right of the object."))

(defgeneric offset (mark)
  (:documentation "Return the offset of the mark into the buffer."))

(defgeneric (setf offset) (new-offset mark)
  (:documentation "Set the offset of the mark into the buffer.  A no-such-offset
condition is signaled if the offset is less than zero or greater than
the size of the buffer."))

(defclass mark-mixin ()
  ((buffer :initarg :buffer :reader buffer)
   (cursor :reader cursor))
  (:documentation "A mixin class used in the initialization of a mark."))

(defmethod offset ((mark mark-mixin))
  (cursor-pos (cursor mark)))

(defmethod (setf offset) (new-offset (mark mark-mixin))
  (assert (<= 0 new-offset (size (buffer mark))) ()
	  (make-condition 'no-such-offset :offset new-offset))
  (setf (cursor-pos (cursor mark)) new-offset))

(defgeneric backward-object (mark &optional count))

(defmethod backward-object ((mark mark) &optional (count 1))
  (decf (offset mark) count))

(defgeneric forward-object (mark &optional count))

(defmethod forward-object ((mark mark) &optional (count 1))
  (incf (offset mark) count))

(defclass standard-left-sticky-mark (left-sticky-mark mark-mixin) ()
  (:documentation "A left-sticky-mark subclass suitable for use in a standard-buffer"))
 
(defclass standard-right-sticky-mark (right-sticky-mark mark-mixin) ()
  (:documentation "A right-sticky-mark subclass suitable for use in a standard-buffer"))

(defmethod initialize-instance :after ((mark standard-left-sticky-mark) &rest args &key (offset 0))
  "Associates a created mark with the buffer it was created for."
  (declare (ignore args))
  (assert (<= 0 offset (size (buffer mark))) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'left-sticky-flexicursor
	   :chain (slot-value (buffer mark) 'contents)
	   :position offset)))

(defmethod initialize-instance :after ((mark standard-right-sticky-mark) &rest args &key (offset 0))
  "Associates a created mark with the buffer it was created for."
  (declare (ignore args))
  (assert (<= 0 offset (size (buffer mark))) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'right-sticky-flexicursor
	   :chain (slot-value (buffer mark) 'contents)
	   :position offset)))

(defmethod initialize-instance :after ((buffer standard-buffer) &rest args)
  "Create the low-mark and high-mark"
  (declare (ignore args))
  (with-slots (low-mark high-mark) buffer
     (setf low-mark (make-instance 'standard-left-sticky-mark :buffer buffer))
     (setf high-mark (make-instance 'standard-right-sticky-mark :buffer buffer))))

(defgeneric clone-mark (mark &optional type)
  (:documentation "Clone a mark.  By default (when type is NIL) the same type of mark is
returned.  Otherwise type is the name of a class (subclass of the mark
class) to be used as a class of the clone."))

(defmethod clone-mark ((mark mark) &optional type)
  (make-instance (or type (class-of mark))
                 :buffer (buffer mark) :offset (offset mark)))

(define-condition no-such-offset (simple-error)
  ((offset :reader condition-offset :initarg :offset))
  (:report (lambda (condition stream)
	     (format stream "No such offset: ~a" (condition-offset condition))))
  (:documentation "This condition is signaled whenever an attempt is made at an operation
that is before the beginning or after the end of the buffer."))

(defgeneric size (buffer)
  (:documentation "Return the number of objects in the buffer."))

(defmethod size ((buffer standard-buffer))
  (nb-elements (slot-value buffer 'contents)))

(defgeneric number-of-lines (buffer)
  (:documentation "Return the number of lines of the buffer, or really the number of
newline characters."))

(defmethod number-of-lines ((buffer standard-buffer))
  (loop for offset from 0 below (size buffer)
	count (eql (buffer-object buffer offset) #\Newline)))

(defgeneric mark< (mark1 mark2)
  (:documentation "Return t if the offset of mark1 is strictly less than that of mark2.
An error is signaled if the two marks are not positioned in the same
buffer.  It is acceptable to pass an offset in place of one of the
marks"))

(defmethod mark< ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (< (offset mark1) (offset mark2)))

(defmethod mark< ((mark1 mark-mixin) (mark2 integer))
  (< (offset mark1) mark2))

(defmethod mark< ((mark1 integer) (mark2 mark-mixin))
  (< mark1 (offset mark2)))

(defgeneric mark<= (mark1 mark2)
  (:documentation "Return t if the offset of mark1 is less than or equal to that of
mark2.  An error is signaled if the two marks are not positioned in
the same buffer.  It is acceptable to pass an offset in place of one
of the marks."))

(defmethod mark<= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (<= (offset mark1) (offset mark2)))

(defmethod mark<= ((mark1 mark-mixin) (mark2 integer))
  (<= (offset mark1) mark2))

(defmethod mark<= ((mark1 integer) (mark2 mark-mixin))
  (<= mark1 (offset mark2)))

(defgeneric mark= (mark1 mark2)
  (:documentation "Return t if the offset of mark1 is equal to that of mark2.  An error
 is signaled if the two marks are not positioned in the same buffer.
 It is acceptable to pass an offset in place of one of the marks."))

(defmethod mark= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (= (offset mark1) (offset mark2)))

(defmethod mark= ((mark1 mark-mixin) (mark2 integer))
  (= (offset mark1) mark2))

(defmethod mark= ((mark1 integer) (mark2 mark-mixin))
  (= mark1 (offset mark2)))

(defgeneric mark> (mark1 mark2)
  (:documentation "Return t if the offset of mark1 is strictly greater than that of
mark2.  An error is signaled if the two marks are not positioned in
the same buffer.  It is acceptable to pass an offset in place of one
of the marks."))

(defmethod mark> ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (> (offset mark1) (offset mark2)))

(defmethod mark> ((mark1 mark-mixin) (mark2 integer))
  (> (offset mark1) mark2))

(defmethod mark> ((mark1 integer) (mark2 mark-mixin))
  (> mark1 (offset mark2)))

(defgeneric mark>= (mark1 mark2)
  (:documentation "Return t if the offset of mark1 is greater than or equal to that of
mark2.  An error is signaled if the two marks are not positioned in
the same buffer.  It is acceptable to pass an offset in place of one
of the marks."))

(defmethod mark>= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (>= (offset mark1) (offset mark2)))

(defmethod mark>= ((mark1 mark-mixin) (mark2 integer))
  (>= (offset mark1) mark2))

(defmethod mark>= ((mark1 integer) (mark2 mark-mixin))
  (>= mark1 (offset mark2)))

(defgeneric beginning-of-buffer (mark)
  (:documentation "Move the mark to the beginning of the buffer.  This is equivalent to
 (setf (offset mark) 0)"))

(defmethod beginning-of-buffer ((mark mark-mixin))
  (setf (offset mark) 0))

(defgeneric end-of-buffer (mark)
  (:documentation "Move the mark to the end of the buffer."))

(defmethod end-of-buffer ((mark mark-mixin))
  (setf (offset mark) (size (buffer mark))))

(defgeneric beginning-of-buffer-p (mark)
  (:documentation "Return t if the mark is at the beginning of the buffer, nil
 otherwise."))

(defmethod beginning-of-buffer-p ((mark mark-mixin))
  (zerop (offset mark)))

(defgeneric end-of-buffer-p (mark)
  (:documentation "Return t if the mark is at the end of the buffer, nil otherwise."))

(defmethod end-of-buffer-p ((mark mark-mixin))
  (= (offset mark) (size (buffer mark))))

(defgeneric beginning-of-line-p (mark)
  (:documentation "Return t if the mark is at the beginning of the line (i.e., if the
character preceding the mark is a newline character or if the mark is
at the beginning of the buffer), nil otherwise."))

(defmethod beginning-of-line-p ((mark mark-mixin))
  (or (beginning-of-buffer-p mark)
      (eql (object-before mark) #\Newline)))

(defgeneric end-of-line-p (mark)
  (:documentation "Return t if the mark is at the end of the line (i.e., if the character
following the mark is a newline character, or if the mark is at the
end of the buffer), nil otherwise."))

(defmethod end-of-line-p ((mark mark-mixin))
  (or (end-of-buffer-p mark)
      (eql (object-after mark) #\Newline)))

(defgeneric beginning-of-line (mark)
  (:documentation "Move the mark to the beginning of the line.  The mark will be
 positioned either immediately after the closest preceding newline
 character, or at the beginning of the buffer if no preceding newline
 character exists."))

(defmethod beginning-of-line ((mark mark-mixin))
  (loop until (beginning-of-line-p mark)
	do (decf (offset mark))))

(defgeneric end-of-line (mark)
  (:documentation "Move the mark to the end of the line. The mark will be positioned
either immediately before the closest following newline character, or
at the end of the buffer if no following newline character exists."))

(defmethod end-of-line ((mark mark-mixin)) ;PB
  (let* ((offset (offset mark))
	 (buffer (buffer mark))
	 (chain (slot-value buffer 'contents))
	 (size (nb-elements chain)))
    (loop until (or (= offset size)
		    (eql (element* chain offset) #\Newline))
	  do (incf offset))
    (setf (offset mark) offset)))

(defgeneric buffer-line-number (buffer offset)
  (:documentation "Return the line number of the offset.  Lines are numbered from zero."))

(defmethod buffer-line-number ((buffer standard-buffer) (offset integer))
  (loop for i from 0 below offset
	count (eql (buffer-object buffer i) #\Newline)))

(defgeneric buffer-column-number (buffer offset)
  (:documentation "Return the column number of the offset. The column number of an offset is
 the number of objects between it and the preceding newline, or
 between it and the beginning of the buffer if the offset is on the
 first line of the buffer."))

(defmethod buffer-column-number ((buffer standard-buffer) (offset integer))
  (loop for i downfrom offset
	while (> i 0)
	until (eql (buffer-object buffer (1- i)) #\Newline)
	count t))

(defgeneric line-number (mark)
  (:documentation "Return the line number of the mark.  Lines are numbered from zero."))

(defmethod line-number ((mark mark-mixin))
  (buffer-line-number (buffer mark) (offset mark)))

(defgeneric column-number (mark)
  (:documentation "Return the column number of the mark. The column number of a mark is
 the number of objects between it and the preceding newline, or
 between it and the beginning of the buffer if the mark is on the
 first line of the buffer."))

(defmethod column-number ((mark mark-mixin))
  (buffer-column-number (buffer mark) (offset mark)))

(defgeneric insert-buffer-object (buffer offset object)
  (:documentation "Insert the object at the offset in the buffer.  Any left-sticky marks
 that are placed at the offset will remain positioned before the
 inserted object.  Any right-sticky marks that are placed at the
 offset will be positioned after the inserted object."))

(defmethod insert-buffer-object ((buffer standard-buffer) offset object)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (insert* (slot-value buffer 'contents) offset object))

(defgeneric insert-buffer-sequence (buffer offset sequence)
  (:documentation "Like calling insert-buffer-object on each of the objects in the
sequence."))
      
(defmethod insert-buffer-sequence ((buffer standard-buffer) offset sequence)
  (insert-vector* (slot-value buffer 'contents) offset sequence))

(defgeneric insert-object (mark object)
  (:documentation "Insert the object at the mark.  This function simply calls
insert-buffer-object with the buffer and the position of the mark."))

(defmethod insert-object ((mark mark-mixin) object)
  (insert-buffer-object (buffer mark) (offset mark) object))

(defgeneric insert-sequence (mark sequence)
  (:documentation "Insert the objects in the sequence at the mark. This function simply
calls insert-buffer-sequence with the buffer and the position of the
mark."))

(defmethod insert-sequence ((mark mark-mixin) sequence)
  (insert-buffer-sequence (buffer mark) (offset mark) sequence))

(defgeneric delete-buffer-range (buffer offset n)
  (:documentation "Delete n objects from the buffer starting at the offset.  If offset
 is negative or offset+n is greater than the size of the buffer, a
 no-such-offset condition is signaled."))

(defmethod delete-buffer-range ((buffer standard-buffer) offset n)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (loop repeat n
	do (delete* (slot-value buffer 'contents) offset)))

(defgeneric delete-range (mark &optional n)
  (:documentation "Delete n objects after (if n > 0) or before (if n < 0) the mark.
This function eventually calls delete-buffer-range, provided that n
is not zero."))

(defmethod delete-range ((mark mark-mixin) &optional (n 1))
  (cond ((plusp n) (delete-buffer-range (buffer mark) (offset mark) n))
	((minusp n) (delete-buffer-range (buffer mark) (+ (offset mark) n) (- n)))
	(t nil)))

(defgeneric delete-region (mark1 mark2)
  (:documentation "Delete the objects in the buffer that are
between mark1 and mark2.  An error is signaled if the two marks
are positioned in different buffers.  It is acceptable to pass an
offset in place of one of the marks."))

(defmethod delete-region ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
        (offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (delete-buffer-range (buffer mark1) offset1 (- offset2 offset1))))

(defmethod delete-region ((mark1 mark-mixin) offset2)
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (delete-buffer-range (buffer mark1) offset1 (- offset2 offset1))))

(defmethod delete-region (offset1 (mark2 mark-mixin))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (delete-buffer-range (buffer mark2) offset1 (- offset2 offset1))))

(defgeneric buffer-object (buffer offset)
  (:documentation "Return the object at the offset in the buffer.  The first object
has offset 0. If offset is less than zero or greater than or equal to
the size of the buffer, a no-such-offset condition is signaled."))

(defmethod buffer-object ((buffer standard-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (element* (slot-value buffer 'contents) offset))

(defgeneric (setf buffer-object) (object buffer offset)
  (:documentation "Set the object at the offset in the buffer. The first object
has offset 0. If offset is less than zero or greater than or equal to
the size of the buffer, a no-such-offset condition is signaled."))

(defmethod (setf buffer-object) (object (buffer standard-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
          (make-condition 'no-such-offset :offset offset))
  (setf (element* (slot-value buffer 'contents) offset) object))

(defgeneric buffer-sequence (buffer offset1 offset2)
  (:documentation "Return the contents of the buffer starting at offset1 and ending at
offset2-1 as a sequence.  If either of the offsets is less than zero
or greater than or equal to the size of the buffer, a no-such-offset
condition is signaled.  If offset2 is smaller than or equal to
offset1, an empty sequence will be returned."))

(defmethod buffer-sequence ((buffer standard-buffer) offset1 offset2)
  (assert (<= 0 offset1 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset1))
  (assert (<= 0 offset2 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset2))
  (if (< offset1 offset2)
      (loop with result = (make-array (- offset2 offset1))
	    for offset from offset1 below offset2
	    for i upfrom 0
	    do (setf (aref result i) (buffer-object buffer offset))
	    finally (return result))
      (make-array 0)))  

(defgeneric object-before (mark)
  (:documentation "Return the object that is immediately before the mark.  If mark is at
the beginning of the buffer, a no-such-offset condition is signaled.
If the mark is at the beginning of a line, but not at the beginning
of the buffer, a newline character is returned."))

(defmethod object-before ((mark mark-mixin))
  (buffer-object (buffer mark) (1- (offset mark))))

(defgeneric object-after (mark)
  (:documentation "Return the object that is immediately after the mark.  If mark is at
the end of the buffer, a no-such-offset condition is signaled.  If
the mark is at the end of a line, but not at the end of the buffer, a
newline character is returned."))

(defmethod object-after ((mark mark-mixin))
  (buffer-object (buffer mark) (offset mark)))

(defgeneric region-to-sequence (mark1 mark2)
  (:documentation "Return a freshly allocated sequence of the objects after mark1 and
before mark2.  An error is signaled if the two marks are positioned
in different buffers.  If mark1 is positioned at an offset equal to
or greater than that of mark2, an empty sequence is returned.  It is
acceptable to pass an offset in place of one of the marks."))

(defmethod region-to-sequence ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (buffer-sequence (buffer mark1) (offset mark1) (offset mark2)))

(defmethod region-to-sequence ((offset integer) (mark mark-mixin))
  (buffer-sequence (buffer mark) offset (offset mark)))

(defmethod region-to-sequence ((mark mark-mixin) (offset integer))
  (buffer-sequence (buffer mark) (offset mark) offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer modification protocol

(defmethod (setf buffer-object) :before (object (buffer standard-buffer) offset)
  (declare (ignore object))
  (setf (offset (low-mark buffer))
        (min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
        (max (offset (high-mark buffer)) offset))
  (setf (slot-value buffer 'modified) t))

(defmethod insert-buffer-object :before ((buffer standard-buffer) offset object)
  (declare (ignore object))
  (setf (offset (low-mark buffer))
	(min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
	(max (offset (high-mark buffer)) offset))
  (setf (slot-value buffer 'modified) t))

(defmethod insert-buffer-sequence :before ((buffer standard-buffer) offset sequence)
  (declare (ignore sequence))
  (setf (offset (low-mark buffer))
	(min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
	(max (offset (high-mark buffer)) offset))
  (setf (slot-value buffer 'modified) t))

(defmethod delete-buffer-range :before ((buffer standard-buffer) offset n)
  (setf (offset (low-mark buffer))
	(min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
	(max (offset (high-mark buffer)) (+ offset n)))
  (setf (slot-value buffer 'modified) t))

(defgeneric clear-modify (buffer))

(defmethod clear-modify ((buffer standard-buffer))
  (beginning-of-buffer (high-mark buffer))
  (end-of-buffer (low-mark buffer))
  (setf (slot-value buffer 'modified) nil))
