;;; -*- Mode: Lisp; Package: CLIMACS-FUNDAMENTAL-SYNTAX -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
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

;;; Syntax for unknown buffer contents.  Parse contents into lines.



(in-package :climacs-fundamental-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the syntax object

(define-syntax fundamental-syntax (basic-syntax)
  ((lines :initform (make-instance 'standard-flexichain))
   (scan))
  (:name "Fundamental"))

(defmethod initialize-instance :after ((syntax fundamental-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; update syntax

(defclass line-object ()
  ((start-mark :initarg :start-mark :reader start-mark)))

(defmethod update-syntax-for-display (buffer (syntax fundamental-syntax) top bot)
  nil)

(defmethod update-syntax (buffer (syntax fundamental-syntax))
  (let* ((low-mark (low-mark buffer))
	 (high-mark (high-mark buffer)))
    (when (mark<= low-mark high-mark)
      (beginning-of-line low-mark)
      (end-of-line high-mark)
      (with-slots (lines scan) syntax
	(let ((low-index 0)
	      (high-index (nb-elements lines)))
	  (loop while (< low-index high-index)
		do (let* ((middle (floor (+ low-index high-index) 2))
			  (line-start (start-mark (element* lines middle))))
		     (cond ((mark> low-mark line-start)
			    (setf low-index (1+ middle)))
			   (t
			    (setf high-index middle)))))
	  ;; discard lines that have to be re-analyzed
	  (loop while (and (< low-index (nb-elements lines))
			   (mark<= (start-mark (element* lines low-index))
				   high-mark))
		do (delete* lines low-index))
	  ;; analyze new lines
	  (setf (offset scan) (offset low-mark))
	  (loop while (and (mark<= scan high-mark)
			   (not (end-of-buffer-p scan)))
		for i from low-index
		do (progn (insert* lines i (make-instance
					    'line-object
					    :start-mark (clone-mark scan)))
			  (end-of-line scan)
			  (unless (end-of-buffer-p scan)
			    ;; skip newline
			    (forward-object scan)))))))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
	(tab-width (tab-width pane)))
    (loop while (< start end)
       do (ecase (buffer-object buffer start)
	    (#\Newline (terpri pane)
		       (setf (aref *cursor-positions* (incf *current-line*))
			     (multiple-value-bind (x y) (stream-cursor-position pane)
			       (declare (ignore x))
			       y)))
	    (#\Space (stream-increment-cursor-position
		      pane space-width 0))
	    (#\Tab (let ((x (stream-cursor-position pane)))
		     (stream-increment-cursor-position
		      pane (- tab-width (mod x tab-width)) 0))))
	 (incf start))))		    


(defun display-cursor (pane current-p)
  (with-slots (top) pane
    (let* ((cursor-line (number-of-lines-in-region top (point pane)))
	   (height (text-style-height (medium-text-style pane) pane))
	   (cursor-y (+ (* cursor-line (+ height (stream-vertical-spacing pane)))))
	   (cursor-column 
	    (buffer-display-column
	     (buffer (point pane)) (offset (point pane))
	     (round (tab-width pane) (space-width pane))))
	   (cursor-x (* cursor-column (text-style-width (medium-text-style pane) pane))))
      (updating-output (pane :unique-id -1)
	(draw-rectangle* pane
			 (1- cursor-x) (- cursor-y (* 0.2 height))
			 (+ cursor-x 2) (+ cursor-y (* 0.8 height))
			 :ink (if current-p +red+ +blue+))))))

(defmethod display-line (pane mark)
  (setf mark (clone-mark mark))
  (let ((saved-offset nil)
	(id 0))
    (flet ((output-word ()
	     (unless (null saved-offset)
	       (let ((contents (coerce (region-to-sequence
					saved-offset
					mark)
				       'string)))
		 (updating-output (pane :unique-id (incf id)
					:cache-value contents
					:cache-test #'string=)
		   (unless (null contents)
		     (present contents 'string :stream pane))))
	       (setf saved-offset nil))))
      (with-slots (bot scan cursor-x cursor-y) pane
	 (loop with space-width = (space-width pane)
	       with tab-width = (tab-width pane)
	       until (end-of-line-p mark)
	       do (let ((obj (object-after mark)))
		    (cond ((eql obj #\Space)
			 (output-word)
			 (stream-increment-cursor-position pane space-width 0))
			((eql obj #\Tab)
			 (output-word)
			 (let ((x (stream-cursor-position pane)))
			   (stream-increment-cursor-position
			    pane (- tab-width (mod x tab-width)) 0)))
			((constituentp obj)
			 (when (null saved-offset)
			   (setf saved-offset (offset mark))))
			((characterp obj)
			 (output-word)
			 (updating-output (pane :unique-id (incf id)
						:cache-value obj)
			   (present obj 'character :stream pane)))
			(t
			 (output-word)
			 (updating-output (pane :unique-id (incf id)
						:cache-value obj
						:cache-test #'eq)
			   (present obj 'character :stream pane)))))
	       do (forward-object mark)
	       finally (output-word)
		       (terpri pane))))))

(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax fundamental-syntax) current-p)
  (with-slots (top bot) pane
     (setf *cursor-positions* (make-array (1+ (number-of-lines-in-region top bot)))
	   *current-line* 0
	   (aref *cursor-positions* 0) (stream-cursor-position pane))
     (setf *white-space-start* (offset top))
     (with-slots (lines) syntax
       (with-slots (lines scan) syntax
	 (let ((low-index 0)
	       (high-index (nb-elements lines)))
	   (loop while (< low-index high-index)
		 do (let* ((middle (floor (+ low-index high-index) 2))
			   (line-start (start-mark (element* lines middle))))
		      (cond ((mark> top line-start)
			     (setf low-index (1+ middle)))
			    ((mark< top line-start)
			     (setf high-index middle))
			    (t
			     (setf low-index middle
				   high-index middle)))))
	   (loop for i from low-index
		 while (and (< i (nb-elements lines))
			    (mark< (start-mark (element* lines i))
				   bot))
		 do (let ((line (element* lines i)))
		      (updating-output (pane :unique-id line
					     :id-test #'eq
					     :cache-value line
					     :cache-test #'eq)
			(display-line pane (start-mark (element* lines i))))))))))
  (display-cursor pane current-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

(defmethod backward-expression (mark (syntax fundamental-syntax))
  nil)

(defmethod forward-expression (mark (syntax fundamental-syntax))
  nil)

;; do this better
(defmethod syntax-line-indentation (mark tab-width (syntax fundamental-syntax))
  0)
