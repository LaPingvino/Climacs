;;; -*- Mode: Lisp; Package: CLIMACS-BUFFER -*-

;;;  (c) copyright 2004 by
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

;;; A `syntax' is a CLOS object that determines how a buffer is to be
;;; rendered.  The `redisplay-with-syntax' functions are specialized
;;; on the syntax.

(in-package :climacs-syntax)

(defclass syntax () ())

(defgeneric redisplay-with-syntax (pane syntax))

(defgeneric full-redisplay (pane syntax))

(defclass basic-syntax (syntax)
  ((top :reader top)
   (bot :reader bot)
   (scan :reader scan)
   (saved-offset :initform nil :accessor saved-offset)
   (cursor-x :initform nil)
   (cursor-y :initform nil)
   (space-width :initform nil)
   (tab-width :initform nil)))

(defmethod initialize-instance :after ((syntax basic-syntax) &rest args &key buffer pane)
  (declare (ignore args))
  (with-slots (top bot scan space-width tab-width) syntax
     (setf top (make-instance 'standard-left-sticky-mark :buffer buffer)
	   bot (make-instance 'standard-right-sticky-mark :buffer buffer)
	   scan (make-instance 'standard-left-sticky-mark :buffer buffer))
     (let* ((medium (sheet-medium pane))
	    (style (medium-text-style medium)))
       (setf space-width (text-style-width style medium)
	     tab-width (* 8 space-width)))))

(defun present-contents (pane syntax)
  (with-slots (saved-offset scan) syntax
     (unless (null saved-offset)
       (present (coerce (region-to-sequence saved-offset scan) 'string)
		'string
		:stream pane)
       (setf saved-offset nil))))

(defun display-line (pane syntax)
  (with-slots (saved-offset bot scan cursor-x cursor-y space-width tab-width) syntax
     (loop when (mark= scan (point pane))
	     do (multiple-value-bind (x y) (stream-cursor-position pane)
		  (setf cursor-x (+ x (if (null saved-offset)
					  0
					  (* space-width (- (offset scan) saved-offset))))
			cursor-y y))
	   when (mark= scan bot)
	     do (present-contents pane syntax)
		(return)
	   until (eql (object-after scan) #\Newline)
	   do (let ((obj (object-after scan)))
		(cond ((eql obj #\Space)
		       (present-contents pane syntax)
		       (princ obj pane))
		      ((eql obj #\Tab)
		       (present-contents pane syntax)
		       (let ((x (stream-cursor-position pane)))
			 (stream-increment-cursor-position
			  pane (- tab-width (mod x tab-width)) 0)))
		      ((constituentp obj)
		       (when (null saved-offset)
			 (setf saved-offset (offset scan))))
		      (t
		       (present-contents pane syntax)
		       (princ obj pane))))
	      (incf (offset scan))
	   finally (present-contents pane syntax)
		   (incf (offset scan))
		   (terpri pane))))

(defmethod redisplay-with-syntax (pane (syntax basic-syntax))
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (with-slots (top bot scan cursor-x cursor-y) syntax
       (beginning-of-line top)
       (end-of-line bot)
       (multiple-value-bind (x y w h) (bounding-rectangle* pane)
	 (declare (ignore x y w))
	 (let ((nb-lines (max 1 (floor h (+ height (stream-vertical-spacing pane))))))
	   (loop while (> (1+ (- (line-number bot) (line-number top))) nb-lines)
		 do (beginning-of-line bot)
		    (decf (offset bot)))
	   (loop until (end-of-buffer-p bot)
		 while (< (1+ (- (line-number bot) (line-number top))) nb-lines)
		 do (incf (offset bot))
		    (end-of-line bot))
	   (loop while (mark< (point pane) top)
		 do (decf (offset top))
		    (beginning-of-line top)
		    (beginning-of-line bot)
		    (decf (offset bot)))
	   (loop while (mark> (point pane) bot)
		 do (end-of-line top)
		    (incf (offset top))
		    (incf (offset bot))
		    (end-of-line bot))
	   (setf (offset scan) (offset top))
	   (loop until (mark= scan bot)
		 do (display-line pane syntax))
	   (when (mark= scan (point pane))
	     (multiple-value-bind (x y) (stream-cursor-position pane)
	       (setf cursor-x x
		     cursor-y y)))
	   (draw-line* pane
		       cursor-x (- cursor-y (* 0.2 height))
		       cursor-x (+ cursor-y (* 0.8 height))
		       :ink +red+))))))
