;;; -*- Mode: Lisp; Package: CLIMACS-KILL-RING -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004 by
;;;           Elliott Johnson (ejohnson@fasl.info)

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

;;; kill ring system

(in-package :climacs-kill-ring)

(defclass kill-ring ()
  ((max-size :type (integer 5 *) ;5 element minimum from flexichain protocol 
	     :initarg :max-size)
   (cursorchain :type standard-cursorchain
		:accessor kill-ring-chain
		:initform (make-instance 'standard-cursorchain))
   (yankpoint   :type left-sticky-flexicursor
	        :accessor kill-ring-cursor))
  (:documentation "A class for all kill rings"))

(defmethod initialize-instance :after((kr kill-ring) &rest args)
  "Adds in the yankpoint"
  (declare (ignore args))
  (with-slots (cursorchain yankpoint) kr
     (setf yankpoint (make-instance 'left-sticky-flexicursor :chain cursorchain))))

(defgeneric kill-ring-length (kr)
  (:documentation "Returns the current length of the kill ring"))

(defgeneric kill-ring-max-size (kr)
  (:documentation "Returns the value of a kill ring's maximum size"))

(defgeneric (setf kill-ring-max-size) (kr size)
  (:documentation "Alters the maximum size of a kill ring, even 
if it means dropping elements to do so."))

(defgeneric reset-yank-position (kr)
  (:documentation "Moves the current yank point back to the start of 
                   of kill ring position"))

(defgeneric rotate-yank-position (kr &optional times)
  (:documentation "Moves the yank point associated with a kill-ring 
                   one or times many positions away from the start 
                   of ring position.  If times is greater than the 
                   current length then the cursor will wrap to the 
                   start of ring position and continue rotating."))

(defgeneric kill-ring-standard-push (kr vector)
  (:documentation "Pushes a vector of objects onto the kill ring creating a new
start of ring position.  This function is much like an every-
day lisp push with size considerations.  If the length of the
kill ring is greater than the maximum size, then \"older\"
elements will be removed from the ring until the maximum size
is reached."))

(defgeneric kill-ring-concatenating-push (kr vector)
  (:documentation "Concatenates the contents of vector onto the end
                   of the current contents of the top of the kill ring.
                   If the kill ring is empty the a new entry is pushed."))

(defgeneric kill-ring-reverse-concatenating-push (kr vector)
  (:documentation "Concatenates the contents of vector onto the front
of the current contents of the top of the kill ring. If the kill ring
is empty a new entry is pushed."))

(defgeneric kill-ring-yank (kr &optional reset)
  (:documentation "Returns the vector of objects currently pointed to
                   by the cursor.  If reset is T, a call to
                   reset-yank-position is called befor the object is 
                   yanked.  The default for reset is NIL"))

(defmethod kill-ring-length ((kr kill-ring))
  (nb-elements (kill-ring-chain kr)))

(defmethod kill-ring-max-size ((kr kill-ring))
  (with-slots (max-size) kr
     max-size))

(defmethod (setf kill-ring-max-size) (size (kr kill-ring))
  (unless (typep size 'integer)
    (error "Error, ~S, is not an integer value" size))
  (if (< size 5)
    (set (slot-value kr 'max-size) 5)
    (setf (slot-value kr 'max-size) size))
  (let ((len (kill-ring-length kr)))
    (if (> len size)
	(loop for n from 1 to (- len size)
	      do (pop-end (kill-ring-chain kr))))))

(defmethod reset-yank-position ((kr kill-ring))
  (setf (cursor-pos (kill-ring-cursor kr)) 0)
  t) 

(defmethod rotate-yank-position ((kr kill-ring) &optional (times 1))
    (if (> (kill-ring-length kr) 0)
	(let* ((curs (kill-ring-cursor kr))
	       (pos (mod (+ times (cursor-pos curs))
			 (kill-ring-length kr))))
	  (setf (cursor-pos curs) pos))))

(defmethod kill-ring-standard-push ((kr kill-ring) vector)
  (let ((chain (kill-ring-chain kr)))
    (if (>= (kill-ring-length kr)
	    (kill-ring-max-size kr))
	(progn
	  (pop-end chain)
	  (push-start chain vector))
        (push-start chain vector)))
  (reset-yank-position kr))

(defmethod kill-ring-concatenating-push ((kr kill-ring) vector)
  (let ((chain (kill-ring-chain kr)))
    (if (zerop (kill-ring-length kr))
	(push-start chain vector)
        (push-start chain 
		    (concatenate 'vector 
				 (pop-start chain) 
				 vector))))
  (reset-yank-position kr))

(defmethod kill-ring-reverse-concatenating-push ((kr kill-ring) vector)
  (let ((chain (kill-ring-chain kr)))
    (if (zerop (kill-ring-length kr))
	(push-start chain vector)
	(push-start chain
		    (concatenate 'vector
				 vector
				 (pop-start chain))))))

(defmethod kill-ring-yank ((kr kill-ring) &optional (reset NIL))
  (if reset (reset-yank-position kr))
  (element> (kill-ring-cursor kr)))