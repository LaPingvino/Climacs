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

(defclass syntax (name-mixin) ())

(defgeneric redisplay-with-syntax (pane syntax))

(defun redisplay-pane (pane)
  "redisplay the pane according to its syntax"
  (redisplay-with-syntax pane (syntax pane)))

(defgeneric full-redisplay (pane syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax completion

(defparameter *syntaxes* '())

(defmacro define-syntax (class-name (name superclasses) &body body)
  `(progn (push '(,name . ,class-name) *syntaxes*)
	  (defclass ,class-name ,superclasses
	       ,@body
	    (:default-initargs :name ,name))))

(define-presentation-method accept
    ((type syntax) stream (view textual-view) &key)
  (multiple-value-bind (pathname success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far *syntaxes* '() :action action
			 :name-key #'car
			 :value-key #'cdr))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (declare (ignore success))
    (or pathname string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic syntax

(define-syntax basic-syntax ("Basic" (syntax))
  ((top :reader top)
   (bot :reader bot)
   (scan :reader scan)
   (cursor-x :initform 2)
   (cursor-y :initform 2)
   (space-width :initform nil)
   (tab-width :initform nil)
   (cache :initform nil)))

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

(defgeneric present-contents (contenst pane syntax))

(defmethod present-contents (contents pane (syntax basic-syntax))
  (unless (null contents)
    (present contents
	     (if (and (>= (length contents) 7) (string= (subseq contents 0 7) "http://"))
		 'url
		 'string)
	     :stream pane)))

(defgeneric display-line (pane syntax line))

(defmethod display-line (pane (syntax basic-syntax) line)
  (let ((saved-index nil)
	(id 0))
    (flet ((output-word (index)
	     (unless (null saved-index)
	       (let ((contents (coerce (subseq line saved-index index) 'string)))
		 (updating-output (pane :unique-id (incf id)
					:cache-value contents
					:cache-test #'string=)
		   (present-contents contents pane syntax)))
	       (setf saved-index nil))))
      (with-slots (bot scan cursor-x cursor-y space-width tab-width) syntax
	 (loop for index from 0
	       for obj across line
	       when (mark= scan (point pane))
		 do (multiple-value-bind (x y) (stream-cursor-position pane)
		      (setf cursor-x (+ x (if (null saved-index)
					      0
					      (* space-width (- index saved-index))))
			    cursor-y y))
	       do (cond ((eql obj #\Space)
			 (output-word index)
			 (stream-increment-cursor-position pane space-width 0))
			((eql obj #\Tab)
			 (output-word index)
			 (let ((x (stream-cursor-position pane)))
			   (stream-increment-cursor-position
			    pane (- tab-width (mod x tab-width)) 0)))
			((constituentp obj)
			 (when (null saved-index)
			   (setf saved-index index)))
			((characterp obj)
			 (output-word index)
			 (updating-output (pane :unique-id (incf id)
						:cache-value obj)
			   (present obj)))
			(t
			 (output-word index)
			 (updating-output (pane :unique-id (incf id)
						:cache-value obj
						:cache-test #'eq)
			   (present obj))))
		  (incf scan)
	       finally (output-word index)
		       (when (mark= scan (point pane))
			 (multiple-value-bind (x y) (stream-cursor-position pane)
			   (setf cursor-x x
				 cursor-y y)))
		       (terpri pane)
		       (incf scan))))))

(defgeneric compute-cache (pane syntax))

(defmethod compute-cache (pane (syntax basic-syntax))
  (with-slots (top bot cache) syntax
     (let* ((buffer (buffer pane))
	    (high-mark (high-mark buffer))
	    (low-mark (low-mark buffer)))
       (when (or (mark< low-mark top) (mark> high-mark bot))
	 (setf cache nil))
       (if (null cache)
	   (let ((nb-lines-on-display (1+ (number-of-lines-in-region top bot)))
		 (mark1 (clone-mark top))
		 (mark2 (clone-mark top)))
	     (setf cache (make-instance 'standard-flexichain))
	     (loop for line from 0 below nb-lines-on-display
		   do (beginning-of-line mark1)
		      (end-of-line mark2)
		      (insert* cache line (region-to-sequence mark1 mark2))
		   unless (end-of-buffer-p mark2)
		     do (setf (offset mark1) (1+ (offset mark2))
			      (offset mark2) (offset mark1))))
	   (let ((nb-lines-on-display (1+ (number-of-lines-in-region top bot)))
		 (mark1 (clone-mark low-mark))
		 (mark2 (clone-mark low-mark))
		 (size1 (number-of-lines-in-region top low-mark))
		 (size2 (number-of-lines-in-region high-mark bot)))
	     (loop repeat (- (nb-elements cache) size1 size2)
		   do (delete* cache size1))
	     (loop for line from size1
		   repeat (- nb-lines-on-display (nb-elements cache))
		   do (beginning-of-line mark1)
		      (end-of-line mark2)
		      (insert* cache line (region-to-sequence mark1 mark2))
		   unless (end-of-buffer-p mark2)
		     do (setf (offset mark1) (1+ (offset mark2))
			      (offset mark2) (offset mark1))))))))

(defun position-window (pane syntax)
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (with-slots (top bot cache) syntax
       (beginning-of-line top)
       (end-of-line bot)
       (multiple-value-bind (x y w h) (bounding-rectangle* pane)
	 (declare (ignore x y w))
	 (let ((nb-lines-in-pane (max 1 (floor h (+ height (stream-vertical-spacing pane)))))
	       (nb-lines-on-display (1+ (number-of-lines-in-region top bot))))
	   ;; adjust the region on display to fit the pane
	   (loop repeat (- nb-lines-on-display nb-lines-in-pane)
		 do (beginning-of-line bot)
		    (decf (offset bot))
		    (unless (null cache)
		      (pop-end cache)))
	   (loop until (end-of-buffer-p bot)
		 repeat (- nb-lines-in-pane nb-lines-on-display)
		 do (incf (offset bot))
		    (end-of-line bot)
		    (setf cache nil))
	   ;; move region on display if point is outside the current region
	   (when (or (mark< (point pane) top) (mark> (point pane) bot))
	     (setf cache nil)
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
		      (end-of-line bot))))))))


;;; this one should not be necessary. 
(defun round-up (x)
  (cond ((zerop x) 2)
	((evenp x) x)
	(t (1+ x))))

(defmethod redisplay-with-syntax (pane (syntax basic-syntax))
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (with-slots (top bot scan cache cursor-x cursor-y) syntax
       (position-window pane syntax)
       (compute-cache pane syntax)
       (setf scan (offset top))
       (loop for id from 0 below (nb-elements cache)
	     do (updating-output (pane :unique-id id)
		  (display-line pane syntax (element* cache id))))
       (when (mark= scan (point pane))
	 (multiple-value-bind (x y) (stream-cursor-position pane)
	   (setf cursor-x x
		 cursor-y y)))
       (updating-output (pane :unique-id -1)
	 (draw-line* pane
		     ;; cursors with odd or zero x-positions were invisible
		     ;; so we round them up to even. 
		     ;; We don't know why, though.
		     (round-up cursor-x) (- cursor-y (* 0.2 height))
		     (round-up cursor-x) (+ cursor-y (* 0.8 height))
		     :ink +red+)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Texinfo syntax

(define-syntax texinfo-syntax ("Texinfo" (basic-syntax)) ())

(define-presentation-type texinfo-command ()
  :inherit-from 'string)

(defmethod present-contents (contents pane (syntax texinfo-syntax))
  (unless (null contents)
    (if (char= (aref contents 0) #\@)
	(with-drawing-options (pane :ink +red+)
	  (present contents 'texinfo-command :stream pane))
	(present contents 'string :stream pane))))


