;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :climacs-buffer)

(defclass delegating-buffer (buffer)
  ((implementation :initarg :implementation :reader implementation))
  (:documentation "Buffer class that delegates the buffer protocol
functionality to a buffer implementation object stored in the
IMPLEMENTATION slot. Do not bind it directly to a mark. Instead, bind
the buffer from the IMPLEMENTATION slot."))

(defmethod low-mark ((buffer delegating-buffer))
  (low-mark (implementation buffer)))

(defmethod high-mark ((buffer delegating-buffer))
  (high-mark (implementation buffer)))

(defmethod modified-p ((buffer delegating-buffer))
  (modified-p (implementation buffer)))

(defmethod clear-modify ((buffer delegating-buffer))
  (clear-modify (implementation buffer)))

(defmethod size ((buffer delegating-buffer))
  (size (implementation buffer)))

(defmethod number-of-lines ((buffer delegating-buffer))
  (number-of-lines (implementation buffer)))

(defmethod insert-buffer-object ((buffer delegating-buffer) offset object)
  (insert-buffer-object (implementation buffer) offset object))

(defmethod insert-buffer-sequence ((buffer delegating-buffer) offset sequence)
  (insert-buffer-sequence (implementation buffer) offset sequence))

(defmethod delete-buffer-range ((buffer delegating-buffer) offset n)
  (delete-buffer-range (implementation buffer) offset n))

(defmethod buffer-object ((buffer delegating-buffer) offset)
  (buffer-object (implementation buffer) offset))

(defmethod (setf buffer-object) (object (buffer delegating-buffer) offset)
  (setf (buffer-object (implementation buffer) offset) object))

(defmethod buffer-sequence ((buffer delegating-buffer) offset1 offset2)
  (buffer-sequence (implementation buffer) offset1 offset2))

(defmethod buffer-line-number ((buffer delegating-buffer) offset)
  (buffer-line-number (implementation buffer) offset))

(defmethod buffer-column-number ((buffer delegating-buffer) offset)
  (buffer-column-number (implementation buffer) offset))