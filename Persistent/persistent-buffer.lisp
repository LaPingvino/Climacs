;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

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

;;; A persistent buffer uses a persistent data structure for its
;;; contents, provides cursors into contents, and contains cursors
;;; into the current contents.

(in-package :climacs-buffer)

;;; For now, pos contains just an integer, while it might contain a cons
;;; of two adjacent buffer elements for higher performance (with the help
;;; of buffer implementation, especially the rebalancing part).
(defclass persistent-cursor ()
  ((buffer :reader buffer :initarg :buffer) ; TODO: fix overlap with mark?
   (pos :accessor cursor-pos))
  (:documentation "The (non-persistent) cursor into PERSISTENT-BUFFER."))

(defclass left-sticky-persistent-cursor (persistent-cursor) ())

(defclass right-sticky-persistent-cursor (persistent-cursor) ())

(defmethod cursor-pos ((cursor left-sticky-persistent-cursor))
  (1+ (slot-value cursor 'pos)))

(defmethod (setf cursor-pos) (position (cursor left-sticky-persistent-cursor))
  (assert (<= 0 position (size (buffer cursor))) ()
	  "Cursor position out of bounds: ~S, ~S" cursor position)
  (setf (slot-value cursor 'pos) (1- position)))

(defmethod cursor-pos ((cursor right-sticky-persistent-cursor))
  (slot-value cursor 'pos))

(defmethod (setf cursor-pos) (position (cursor right-sticky-persistent-cursor))
  (assert (<= 0 position (size (buffer cursor))) ()
	  "Cursor position out of bounds: ~S, ~S" cursor position)
  (setf (slot-value cursor 'pos) position))

(defclass persistent-buffer (buffer)
  ((low-mark :reader low-mark)
   (high-mark :reader high-mark)
   (cursors :reader cursors :initform nil)
   (modified :initform nil :reader modified-p))
  (:documentation "The Climacs persistent buffer base class
\(non-instantiable)."))

(defmethod initialize-instance :after ((cursor left-sticky-persistent-cursor)
				       &rest initargs &key (position 0))
  (declare (ignorable initargs))
  (with-slots (buffer pos) cursor
    (setf pos (1- position))
    (with-slots (cursors) buffer
      (push (flexichain::make-weak-pointer cursor buffer) cursors))))

(defmethod initialize-instance :after ((cursor right-sticky-persistent-cursor)
				       &rest initargs &key (position 0))
  (declare (ignorable initargs))
  (with-slots (buffer pos) cursor
    (setf pos position)
    (with-slots (cursors) buffer
      (push (flexichain::make-weak-pointer cursor buffer) cursors))))

(defclass binseq-buffer (persistent-buffer)
  ((contents :initform (list-binseq nil)))
  (:documentation "An instantiable subclass of PERSISTENT-BUFFER that
uses a binary sequence for the CONTENTS."))

(defclass obinseq-buffer (persistent-buffer)
  ((contents :initform (list-obinseq nil)))
  (:documentation "An instantiable subclass of PERSISTENT-BUFFER that
uses an optimized binary sequence (only non-nil atoms are allowed as
elements) for the CONTENTS."))

(defclass persistent-left-sticky-mark (left-sticky-mark mark-mixin) ()
  (:documentation "A LEFT-STICKY-MARK subclass suitable for use in a
PERSISTENT-BUFFER."))

(defclass persistent-right-sticky-mark (right-sticky-mark mark-mixin) ()
  (:documentation "A RIGHT-STICKY-MARK subclass suitable for use in a
PERSISTENT-BUFFER."))

(defmethod initialize-instance :after ((mark persistent-left-sticky-mark)
				       &rest args &key (offset 0))
  "Associates a created mark with the buffer for which it was created."
  (declare (ignorable args))
  (assert (<= 0 offset (size (buffer mark))) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'left-sticky-persistent-cursor
		       :buffer (buffer mark)
		       :position offset)))

(defmethod initialize-instance :after ((mark persistent-right-sticky-mark)
				       &rest args &key (offset 0))
  "Associates a created mark with the buffer for which it was created."
  (declare (ignorable args))
  (assert (<= 0 offset (size (buffer mark))) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value mark 'cursor)
	(make-instance 'right-sticky-persistent-cursor
		       :buffer (buffer mark)
		       :position offset)))

(defmethod initialize-instance :after ((buffer persistent-buffer) &rest args)
  "Create the low-mark and high-mark."
  (declare (ignorable args))
  (with-slots (low-mark high-mark) buffer
    (setf low-mark (make-instance 'persistent-left-sticky-mark :buffer buffer))
    (setf high-mark (make-instance 'persistent-right-sticky-mark
				   :buffer buffer))))

(defmethod clone-mark ((mark persistent-left-sticky-mark) &optional type)
  (unless type
    (setf type 'persistent-left-sticky-mark))
  (make-instance type :buffer (buffer mark) :offset (offset mark)))

(defmethod clone-mark ((mark persistent-right-sticky-mark) &optional type)
  (unless type
    (setf type 'persistent-right-sticky-mark))
  (make-instance type :buffer (buffer mark) :offset (offset mark)))

(defmethod size ((buffer binseq-buffer))
  (binseq-length (slot-value buffer 'contents)))

(defmethod size ((buffer obinseq-buffer))
  (obinseq-length (slot-value buffer 'contents)))

(defmethod number-of-lines ((buffer persistent-buffer))
  (loop for offset from 0 below (size buffer)
     count (eql (buffer-object buffer offset) #\Newline)))

(defmethod buffer-line-number ((buffer persistent-buffer) (offset integer))
  (loop for i from 0 below offset
     count (eql (buffer-object buffer i) #\Newline)))

(defmethod buffer-column-number ((buffer persistent-buffer) (offset integer))
  (loop for i downfrom offset
     while (> i 0)
     until (eql (buffer-object buffer (1- i)) #\Newline)
     count t))

;;; the old value of the CONTENTS slot is dropped upon modification
;;; it can be saved for UNDO purposes in a history tree, by an UNDOABLE-BUFFER

(defmethod insert-buffer-object ((buffer binseq-buffer) offset object)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq-insert (slot-value buffer 'contents) offset object)))

(defmethod insert-buffer-object ((buffer obinseq-buffer) offset object)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value buffer 'contents)
	(obinseq-insert (slot-value buffer 'contents) offset object)))

(defmethod insert-buffer-sequence ((buffer binseq-buffer) offset sequence)
  (let ((binseq (list-binseq (loop for e across sequence collect e))))
    (setf (slot-value buffer 'contents)
	  (binseq-insert* (slot-value buffer 'contents) offset binseq))))

(defmethod insert-buffer-sequence ((buffer obinseq-buffer) offset sequence)
  (let ((obinseq (list-obinseq (loop for e across sequence collect e))))
    (setf (slot-value buffer 'contents)
	  (obinseq-insert* (slot-value buffer 'contents) offset obinseq))))

(defmethod delete-buffer-range ((buffer binseq-buffer) offset n)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq-remove* (slot-value buffer 'contents) offset n)))

(defmethod delete-buffer-range ((buffer obinseq-buffer) offset n)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value buffer 'contents)
	(obinseq-remove* (slot-value buffer 'contents) offset n)))

(defmethod buffer-object ((buffer binseq-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (binseq-get (slot-value buffer 'contents) offset))

(defmethod (setf buffer-object) (object (buffer binseq-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value buffer 'contents)
	(binseq-set (slot-value buffer 'contents) offset object)))

(defmethod buffer-object ((buffer obinseq-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (obinseq-get (slot-value buffer 'contents) offset))

(defmethod (setf buffer-object) (object (buffer obinseq-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (setf (slot-value buffer 'contents)
	(obinseq-set (slot-value buffer 'contents) offset object)))

(defmethod buffer-sequence ((buffer binseq-buffer) offset1 offset2)
  (assert (<= 0 offset1 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset1))
  (assert (<= 0 offset2 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset2))
  (coerce
   (let ((len (- offset2 offset1)))
     (if (> len 0)
	 (binseq-list
	  (binseq-sub (slot-value buffer 'contents) offset1 len))
	 nil))
   'vector))

(defmethod buffer-sequence ((buffer obinseq-buffer) offset1 offset2)
  (assert (<= 0 offset1 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset1))
  (assert (<= 0 offset2 (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset2))
  (coerce
   (let ((len (- offset2 offset1)))
     (if (> len 0)
	 (obinseq-list
	  (obinseq-sub (slot-value buffer 'contents) offset1 len))
	 nil))
   'vector))

(defmethod insert-buffer-object
    :before ((buffer persistent-buffer) offset object)
  (declare (ignore object))
  (setf (offset (low-mark buffer))
	(min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
	(max (offset (high-mark buffer)) offset))
  (setf (slot-value buffer 'modified) t))

(defmethod insert-buffer-sequence
    :before ((buffer persistent-buffer) offset sequence)
  (declare (ignore sequence))
  (setf (offset (low-mark buffer))
	(min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
	(max (offset (high-mark buffer)) offset))
  (setf (slot-value buffer 'modified) t))

(defmethod delete-buffer-range
    :before ((buffer persistent-buffer) offset n)
  (setf (offset (low-mark buffer))
	(min (offset (low-mark buffer)) offset))
  (setf (offset (high-mark buffer))
	(max (offset (high-mark buffer)) (+ offset n)))
  (setf (slot-value buffer 'modified) t))

(defmethod clear-modify ((buffer persistent-buffer))
  (beginning-of-buffer (high-mark buffer))
  (end-of-buffer (low-mark buffer))
  (setf (slot-value buffer 'modified) nil))

;;; I hope the code below is not wrong, although it is slow for now. It should
;;; look like flexichain::adjust-cursors, but I am planning to write that in
;;; a more compact form. The two functions below should not return anything.
(defun adjust-cursors-on-insert (buffer start &optional (increment 1))
  (loop for c in (cursors buffer); TODO: use side-effects to get rid of consing
     as wpc = (flexichain::weak-pointer-value c buffer)
     when wpc
     collect (progn
	       (when (<= start (slot-value wpc 'pos))
		 (incf (slot-value wpc 'pos) increment))
	       c)))

(defun adjust-cursors-on-delete (buffer start n)
   (loop with end = (+ start n) ; TODO: use side-effects to get rid of consing
      for c in (cursors buffer)
      as wpc = (flexichain::weak-pointer-value c buffer)
      when wpc
      collect (progn
 	       (cond
 		 ((<= (cursor-pos wpc) start))
 		 ((< start (cursor-pos wpc) end)
 		  (setf (cursor-pos wpc) start))
 		 (t (decf (cursor-pos wpc) n)))
 	       c)))

(defmethod insert-buffer-object
    :after ((buffer persistent-buffer) offset object)
  (with-slots (cursors) buffer
    (setf cursors (adjust-cursors-on-insert buffer offset))))

(defmethod insert-buffer-sequence
    :after ((buffer persistent-buffer) offset sequence)
  (with-slots (cursors) buffer
    (setf cursors (adjust-cursors-on-insert buffer offset (length sequence)))))

(defmethod delete-buffer-range
    :after ((buffer persistent-buffer) offset n)
  (with-slots (cursors) buffer
    (setf cursors (adjust-cursors-on-delete buffer offset n))))