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
  ((max-size :type unsigned-byte
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


(defgeneric kr-length (kr)
  (:documentation "Returns the length of a kill-ring's flexichain"))

(defmethod kr-length ((kr kill-ring))
  (nb-elements (kr-flexi kr)))

(defgeneric kr-resize (kr size)
  (:documentation "Resize a kill ring to the value of SIZE"))

(defmethod kr-resize ((kr kill-ring) size)
  (setf (slot-value kr 'max-size) size)
  (let ((len (kr-length kr)))
    (if (> len size)
	(loop for n from 1 to (- len size)
	      do (pop-end (kr-flexi kr))))))

(defgeneric kr-push (kr object)
  (:documentation "Push an object onto a kill ring with size considerations"))
  
(defmethod kr-push ((kr kill-ring) object)
  (let ((flexi (kr-flexi kr)))
    (if (>= (kr-length kr)(kr-max-size kr))
	((lambda (flex obj)
	   (pop-end flex)
	   (push-start flex obj))
	 flexi object)
        (push-start flexi object))))

(defgeneric kr-pop (kr)
  (:documentation "Pops an object off of a kill ring"))

(defmethod kr-pop ((kr kill-ring))
  (if (> (nb-elements (kr-flexi kr)) 0)
      (pop-start (kr-flexi kr))
      nil))

(defgeneric kr-rotate (kr &optional n)
  (:documentation "Rotates the kill ring either once forward or an optional amound +/-"))

(defmethod kr-rotate ((kr kill-ring) &optional (n -1))
  (assert (typep n 'fixnum)(n) "Can not rotate the kill ring ~S positions" n)
  (let ((flexi (kr-flexi kr)))
    (rotate flexi n)))

(defgeneric kr-copy (kr)
  (:documentation "Copies out a member of a kill ring without deleting it"))

(defmethod kr-copy ((kr kill-ring))
  (let ((object (kr-pop kr)))
    (kr-push kr object)
    object))

