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
  ((max-size :type 'fixnum
	     :initarg :max-size
	     :accessor kr-max-size)
   (flexichain :type standard-flexichain
	       :initarg :flexichain
	       :accessor kr-flexi))
  (:documentation "Basic flexichain without resizing"))

(defun initialize-kill-ring (size)
  "Construct a kill ring of a given size"
  (make-instance 'kill-ring
		 :max-size size
		 :flexichain (make-instance 'standard-flexichain)))

;; Didn't see a real reason to make gf's for these.

(defun kr-length (kr)
  "Returns the length of a kill-rings flexichain"
  (nb-elements (kr-flexi kr)))

(defun kr-resize (kr size)
  "Resize a kill-ring to the value of size"
  (kr-p kr)
  (setf (slot-value kr 'max-size) size)
  (let ((len (kr-length kr)))
    (if (> len size)
	(loop for n from 1 to (- len size)
	      do (pop-end (kr-flexi kr))))))

(defun kr-push (kr object)
  "Push an object onto a kill-ring with size considerations"
  (let ((flexi (kr-flexi kr)))
    (if (>= (kr-length kr)(kr-max-size kr))
	((lambda (flex obj)
	   (pop-end flex)
	   (push-start flex obj))
	 flexi object)
        (push-start flexi object))))

(defun kr-pop (kr)
  "Pops an object off of a kill-ring"
  (if (> (nb-elements (kr-flexi kr)) 0)
      (pop-start (kr-flexi kr))
      nil))

(defun kr-rotate (kr &optional (n -1))
  "Rotates the kill-ring either once forward or an optional amount +/-"
  (assert (typep n 'fixnum)(n) "Can not rotate the kill ring ~S positions" n)
  (let ((flexi (kr-flexi kr)))
    (rotate flexi n)))

(defun kr-copy (kr)
  "Copies out a member of a kill-ring without deleting it"
  (let ((object (kr-pop kr)))
    (kr-push kr object)
    object))

(defun kr-copy-in (buffer kr offset1 offset2)
  "Non destructively copies in buffer region to the kill-ring"
  (kr-push kr (buffer-sequence buffer offset1 offset2)))

(defun kr-cut-in (buffer kr offset1 offset2)
  "Destructively cuts a given buffer region into the kill-ring"
  (kr-copy-in buffer kr offset1 offset2)
  (climacs-buffer::delete-buffer-range buffer offset1 (- offset2 offset1))) 
				       
(defun kr-copy-out (mark kr)
  "Copies an element from a kill-ring to a buffer at the given offset"
  (insert-sequence mark (kr-copy kr)))

(defun kr-cut-out (mark kr)
  "Cuts an element from a kill-ring out to a buffer at a given offset"
  (insert-sequence mark (kr-pop kr)))