;;; -*- Mode: Lisp; Package: CLIMACS-BUFFER -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
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
;;; Tabify

(defclass tabify-mixin ()
  ((space-width :initarg nil :reader space-width)
   (tab-width :initarg nil :reader tab-width)))

(defgeneric tab-space-count (tabify))

(defmethod tab-space-count (tabify)
  1)

(defmethod tab-space-count ((tabify tabify-mixin))
  (round (tab-width tabify) (space-width tabify)))

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

(defun make-cache ()
  (let ((cache (make-instance 'standard-flexichain)))
    (insert* cache 0 nil)
    cache))

(define-syntax basic-syntax ("Basic" (syntax tabify-mixin))
  ((top :reader top)
   (bot :reader bot)
   (scan :reader scan)
   (cursor-x :initform 2)
   (cursor-y :initform 2)
   (cache :initform (make-cache))))

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

(defgeneric present-contents (contents pane syntax))

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

(defgeneric fill-cache (pane syntax)
  (:documentation "fill nil cache entries from the buffer"))

(defmethod fill-cache (pane (syntax basic-syntax))
  (with-slots (top bot cache) syntax
     (let ((mark1 (clone-mark top))
	   (mark2 (clone-mark top)))
       (loop for line from 0 below (nb-elements cache)
	     do (beginning-of-line mark1)
		(end-of-line mark2)
	     when (null (element* cache line))
	       do (setf (element* cache line) (region-to-sequence mark1 mark2))
	     unless (end-of-buffer-p mark2)
	       do (setf (offset mark1) (1+ (offset mark2))
			(offset mark2) (offset mark1))))))

(defun nb-lines-in-pane (pane)
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (multiple-value-bind (x y w h) (bounding-rectangle* pane)
      (declare (ignore x y w))
      (max 1 (floor h (+ height (stream-vertical-spacing pane)))))))

;;; make the region on display fit the size of the pane as closely as
;;; possible by adjusting bot leaving top intact.  Also make the cache
;;; size fit the size of the region on display.
(defun adjust-cache-size-and-bot (pane syntax)
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot cache) syntax
       (setf (offset bot) (offset top))
       (loop until (end-of-buffer-p bot)
	     repeat (1- nb-lines-in-pane)
	     do (forward-object bot)
		(end-of-line bot))
       (let ((nb-lines-on-display (1+ (number-of-lines-in-region top bot))))
	 (loop repeat (- (nb-elements cache) nb-lines-on-display)
	       do (pop-end cache))
	 (loop repeat (- nb-lines-on-display (nb-elements cache))
	       do (push-end cache nil))))))

;;; put all-nil entries in the cache
(defun empty-cache (cache)
  (loop for i from 0 below (nb-elements cache)
	do (setf (element* cache i) nil)))	     

;;; empty the cache and try to put point close to the middle
;;; of the pane by moving top half a pane-size up.
(defun reposition-window (pane syntax)
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot cache) syntax
       (empty-cache cache)
       (setf (offset top) (offset (point pane)))
       (loop do (beginning-of-line top)
	     repeat (floor nb-lines-in-pane 2)
	     until (beginning-of-buffer-p top)
	     do (decf (offset top))
		(beginning-of-line top)))))

;;; Make the cache reflect the contents of the buffer starting at top,
;;; trying to preserve contents as much as possible, and inserting a
;;; nil entry where buffer contents is unknonwn.  The size of the
;;; cache size at the end may be smaller than, equal to, or greater
;;; than the number of lines in the pane. 
(defun adjust-cache (pane syntax)
  (let* ((buffer (buffer pane))
	 (high-mark (high-mark buffer))
	 (low-mark (low-mark buffer))
	 (nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot cache) syntax
       (beginning-of-line top)
       (end-of-line bot)
       (if (or (mark< (point pane) top)
	       (>= (number-of-lines-in-region top (point pane)) nb-lines-in-pane)
	       (and (mark< low-mark top)
		    (>= (number-of-lines-in-region top high-mark) (nb-elements cache))))
	   (reposition-window pane syntax)
	   (when (mark>= high-mark low-mark)
	     (let* ((n1 (number-of-lines-in-region top low-mark))
		    (n2 (1+ (number-of-lines-in-region low-mark high-mark)))
		    (n3 (number-of-lines-in-region high-mark bot))
		    (diff (- (+ n1 n2 n3) (nb-elements cache))))
	       (cond ((>= (+ n1 n2 n3) (+ (nb-elements cache) 20))
		      (setf (offset bot) (offset top))
		      (end-of-line bot)
		      (loop for i from n1 below (nb-elements cache)
			    do (setf (element* cache i) nil)))
		     ((>= diff 0)
		      (loop repeat diff do (insert* cache n1 nil))
		      (loop for i from (+ n1 diff) below (+ n1 n2)
			    do (setf (element* cache i) nil)))
		     (t
		      (loop repeat (- diff) do (delete* cache n1))
		      (loop for i from n1 below (+ n1 n2)
			    do (setf (element* cache i) nil)))))))))
  (adjust-cache-size-and-bot pane syntax))

(defun page-down (pane syntax)
  (adjust-cache pane syntax)
  (with-slots (top bot cache) syntax
     (when (mark> (size (buffer bot)) bot)
       (empty-cache cache)
       (setf (offset top) (offset bot))
       (beginning-of-line top)
       (setf (offset (point pane)) (offset top)))))

(defun page-up (pane syntax)
  (adjust-cache pane syntax)
  (with-slots (top bot cache) syntax
     (when (> (offset top) 0)
       (let ((nb-lines-in-region (number-of-lines-in-region top bot)))
	 (setf (offset bot) (offset top))
	 (end-of-line bot)
	 (loop repeat  nb-lines-in-region
	       while (> (offset top) 0)
	       do (decf (offset top))
		  (beginning-of-line top))
	 (setf (offset (point pane)) (offset top))
	 (adjust-cache pane syntax)
	 (setf (offset (point pane)) (offset bot))
	 (beginning-of-line (point pane))
	 (empty-cache cache)))))

(defmethod redisplay-with-syntax (pane (syntax basic-syntax))
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (with-slots (top bot scan cache cursor-x cursor-y) syntax
       (adjust-cache pane syntax)
       (fill-cache pane syntax)
       (loop with start-offset = (offset top)
	     for id from 0 below (nb-elements cache)
	     do (setf scan start-offset)
		(updating-output
		    (pane :unique-id id
			  :cache-value (if (<= start-offset
					       (offset (point pane))
					       (+ start-offset (length (element* cache id))))
					   (cons nil nil)
					   (element* cache id))
			  :cache-test #'eq)
		  (display-line pane syntax (element* cache id)))
		(incf start-offset (1+ (length (element* cache id)))))
       (when (mark= scan (point pane))
	 (multiple-value-bind (x y) (stream-cursor-position pane)
	   (setf cursor-x x
		 cursor-y y)))
       (updating-output (pane :unique-id -1)
	 (draw-rectangle* pane
			  (1- cursor-x) (- cursor-y (* 0.2 height))
			  (+ cursor-x 2) (+ cursor-y (* 0.8 height))
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


