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
  ())

(defmethod redisplay-with-syntax (pane (syntax basic-syntax))
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium))
	 (width (text-style-width style medium))
	 (tab-width (* 8 width))
	 (buffer (buffer pane))
	 (size (size (buffer pane)))
	 (offset 0)
	 (offset1 nil)
	 (cursor-x nil)
	 (cursor-y nil))
    (labels ((present-contents ()
	     (unless (null offset1)
	       (present (coerce (buffer-sequence buffer offset1 offset) 'string)
			'string
			:stream pane)
	       (setf offset1 nil)))
	   (display-line ()
	     (loop when (= offset (offset (point pane)))
		     do (multiple-value-bind (x y) (stream-cursor-position pane)
			  (setf cursor-x (+ x (if (null offset1)
						  0
						  (* width (- offset offset1))))
				cursor-y y))
		   when (= offset size)
		     do (present-contents)
			(return)
		   until (eql (buffer-object buffer offset) #\Newline)
		   do (let ((obj (buffer-object buffer offset)))
			(cond ((eql obj #\Space)
			       (present-contents)
			       (princ obj pane))
			      ((eql obj #\Tab)
			       (present-contents)
			       (let ((x (stream-cursor-position pane)))
				 (stream-increment-cursor-position
				  pane (- tab-width (mod x tab-width)) 0)))
			      ((constituentp obj)
			       (when (null offset1)
				 (setf offset1 offset)))
			      (t
			       (present-contents)
			       (princ obj pane))))
		      (incf offset)
		   finally (present-contents)
			   (incf offset)
			   (terpri pane))))
      (loop while (< offset size)
	    do (display-line))
      (when (= offset (offset (point pane)))
	(multiple-value-bind (x y) (stream-cursor-position pane)
	  (setf cursor-x x
		cursor-y y))))
    (draw-line* pane
		cursor-x (- cursor-y (* 0.2 height))
		cursor-x (+ cursor-y (* 0.8 height))
		:ink +red+)))
