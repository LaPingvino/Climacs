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

(defun redisplay-pane (pane)
  "redisplay the pane according to its syntax"
  (redisplay-with-syntax pane (syntax pane)))

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

(defmethod initialize-instance :after ((syntax basic-syntax) &rest args &key pane)
  (declare (ignore args))
  (let ((buffer (buffer pane)))
    (with-slots (top bot scan space-width tab-width) syntax
       (setf top (make-instance 'standard-left-sticky-mark :buffer buffer)
	     bot (make-instance 'standard-right-sticky-mark :buffer buffer))
       (let* ((medium (sheet-medium pane))
	      (style (medium-text-style medium)))
	 (setf space-width (text-style-width style medium)
	       tab-width (* 8 space-width))))))

(define-presentation-type url ()
  :inherit-from 'string)

(defmethod present-contents (contents pane (syntax basic-syntax))
  (unless (null contents)
    (present contents
	     (if (and (>= (length contents) 7) (string= (subseq contents 0 7) "http://"))
		 'url
		 'string)
	     :stream pane)))

;;(defmacro maybe-updating-output (stuff &body body)
;;  `(progn ,@body))

 (defmacro maybe-updating-output (stuff &body body)
   `(updating-output ,stuff ,@body))

(defmethod display-line (pane (syntax basic-syntax))
  (with-slots (saved-offset bot scan cursor-x cursor-y space-width tab-width) syntax
     (flet ((compute-contents ()
	      (unless (null saved-offset)
		(prog1 (coerce (buffer-sequence (buffer pane) saved-offset scan) 'string)
		       (setf saved-offset nil)))))
       (macrolet ((output-word (&body body)
		    `(let ((contents (compute-contents)))
		       (if (null contents)
			   ,(if body
				`(maybe-updating-output (pane :unique-id (incf id))
				  ,@body)
				`(progn))
			   (progn
			   (maybe-updating-output (pane :unique-id (incf id)
							:cache-value contents
							:cache-test #'string=)
						    (present-contents contents pane syntax))
			     ,(when body
				    `(maybe-updating-output (pane :unique-id (incf id))
				      ,@body)))))))
	 (loop with id = 0
	       when (mark= scan (point pane))
		 do (multiple-value-bind (x y) (stream-cursor-position pane)
		      (setf cursor-x (+ x (if (null saved-offset)
					      0
					      (* space-width (- scan saved-offset))))
			    cursor-y y))
	       when (mark= scan bot)
		 do (output-word)
		    (return)
	       until (eql (buffer-object (buffer pane) scan) #\Newline)
	       do (let ((obj (buffer-object (buffer pane) scan)))
		    (cond ((eql obj #\Space)
			   (output-word (princ obj pane)))
			  ((eql obj #\Tab)
			   (output-word)
			   (let ((x (stream-cursor-position pane)))
			     (stream-increment-cursor-position
			      pane (- tab-width (mod x tab-width)) 0)))
			  ((constituentp obj)
			   (when (null saved-offset)
			     (setf saved-offset scan)))
			  (t
			   (output-word (princ obj pane)))))
		  (incf scan)
	       finally (output-word (terpri pane))
		       (incf scan))))))

(defmethod redisplay-with-syntax (pane (syntax basic-syntax))
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (with-slots (top bot scan cursor-x cursor-y) syntax
       (beginning-of-line top)
       (end-of-line bot)
       (multiple-value-bind (x y w h) (bounding-rectangle* pane)
	 (declare (ignore x y w))
	 (let ((nb-lines-in-pane (max 1 (floor h (+ height (stream-vertical-spacing pane)))))
	       (nb-lines-on-display (1+ (number-of-lines-in-region top bot))))
	   ;; adjust the region on display to fit the pane
	   (loop repeat (- nb-lines-on-display nb-lines-in-pane)
		 do (beginning-of-line bot)
		    (decf (offset bot)))
	   (loop until (end-of-buffer-p bot)
		 repeat (- nb-lines-in-pane nb-lines-on-display)
		 do (incf (offset bot))
		    (end-of-line bot))
	   ;; move region on display if point is outside the current region
	   (when (or (mark< (point pane) top) (mark> (point pane) bot))
	     (setf (offset top) (offset (point pane)))
	     (loop do (beginning-of-line top)
		   repeat (floor nb-lines-in-pane 2)
		   until (beginning-of-buffer-p top)
		   do (decf (offset top))
		      (beginning-of-line top))
	     (setf (offset bot) (offset top))
	     (loop do (end-of-line bot)
		   repeat (1- nb-lines-in-pane)
		   until (end-of-buffer-p bot)
		   do (incf (offset bot))
		      (end-of-line bot)))
	   (setf scan (offset top))
	   (loop for id from 0
		 until (mark= scan bot)
		 do (maybe-updating-output (pane :unique-id id)
		      (display-line pane syntax)))
	   (when (mark= scan (point pane))
	     (multiple-value-bind (x y) (stream-cursor-position pane)
	       (setf cursor-x x
		     cursor-y y)))
	   (maybe-updating-output (pane :all-new t :fixed-position t)
	      (draw-line* pane
			  ;; cursors with odd x-positions were invisible
			  ;; so we strip off the low bit to make them even.
			  (logand -2 cursor-x) (- cursor-y (* 0.2 height))
			  (logand -2 cursor-x) (+ cursor-y (* 0.8 height))
			  :ink +red+)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Texinfo syntax

(defclass texinfo-syntax (basic-syntax) ())

(define-presentation-type texinfo-command ()
  :inherit-from 'string)

(defmethod present-contents (contents pane (syntax texinfo-syntax))
  (unless (null contents)
    (if (char= (aref contents 0) #\@)
	(with-drawing-options (pane :ink +red+)
	  (present contents 'texinfo-command :stream pane))
	(present contents 'string :stream pane))))

