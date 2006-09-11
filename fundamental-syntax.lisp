;; -*- Mode: Lisp; Package: CLIMACS-FUNDAMENTAL-SYNTAX -*-

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
;;; The syntax object and misc stuff.

(define-syntax fundamental-syntax (syntax)
  ((lines :initform (make-instance 'standard-flexichain))
   (scan))
  (:name "Fundamental"))

(defmethod initialize-instance :after ((syntax fundamental-syntax) &rest args)
  (declare (ignore args))
  (with-slots (buffer scan) syntax
     (setf scan (clone-mark (low-mark buffer) :left))))

(setf *default-syntax* 'fundamental-syntax)

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
					:cache-test #'eql)
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
           finally
             (output-word)
             (terpri))))))

(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax fundamental-syntax) current-p)
  (with-slots (top bot) pane
    (setf *cursor-positions* (make-array (1+ (number-of-lines-in-region top bot)))
          *current-line* 0
          (aref *cursor-positions* 0) (stream-cursor-position pane))
    (setf *white-space-start* (offset top))
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
                (updating-output (pane :unique-id i
                                       :id-test #'eql
                                       :cache-value line
                                       :cache-test #'equal)
                  (display-line pane (start-mark (element* lines i)))))))))
  (when (region-visible-p pane) (display-region pane syntax))
  (display-cursor pane syntax current-p))

(defmethod display-cursor ((pane climacs-pane) (syntax fundamental-syntax) current-p)
  (let ((point (point pane)))
    (multiple-value-bind (cursor-x cursor-y line-height)
	(offset-to-screen-position (offset point) pane)
      (updating-output (pane :unique-id -1 :cache-value (offset point))
	(draw-rectangle* pane
			 (1- cursor-x) cursor-y
			 (+ cursor-x 2) (+ cursor-y line-height)
			 :ink (if current-p +red+ +blue+))
        ;; Move the position of the viewport if point is outside the
        ;; visible area. The trick is that we do this inside the body
        ;; of `updating-output', so the view will only be re-focused
        ;; when point is actually moved.
        (let ((x-position (abs (transform-position (sheet-transformation pane) 0 0)))
              (viewport-width (bounding-rectangle-width (or (pane-viewport pane) pane))))
          #+nil(print (list cursor-x (+ x-position (bounding-rectangle-width (pane-viewport pane)))) *terminal-io*)
          (cond ((> cursor-x (+ x-position viewport-width))
                 (move-sheet pane (round (- (- cursor-x viewport-width))) 0))
                ((> x-position cursor-x)
                 (move-sheet pane (if (> viewport-width cursor-x)
                                      0
                                      (round (- cursor-x)))
                             0))))))))

(defmethod display-region ((pane climacs-pane) (syntax fundamental-syntax))
  (highlight-region pane (point pane) (mark pane)))

(defgeneric highlight-region (pane mark1 offset2 &optional ink))

(defmethod highlight-region ((pane climacs-pane) (offset1 integer) (offset2 integer)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  ;; FIXME stream-vertical-spacing between lines
  ;; FIXME note sure updating output is working properly...
  ;; we'll call offset1 CURSOR and offset2 MARK
  (multiple-value-bind (cursor-x cursor-y line-height)
      (offset-to-screen-position offset1 pane)
    (multiple-value-bind (mark-x mark-y)
	(offset-to-screen-position offset2 pane)
      (cond
	;; mark and point are above the screen
	((and (null cursor-y) (null mark-y)
	      (null cursor-x) (null mark-x))
	 nil)
	;; mark and point are below the screen
	((and (null cursor-y) (null mark-y)
	      cursor-x mark-x)
	 nil)
	;; mark or point is above the screen, and point or mark below it
	((and (null cursor-y) (null mark-y)
	      (or (and cursor-x (null mark-x))
		  (and (null cursor-x) mark-x)))
	 (let ((width (stream-text-margin pane))
	       (height (bounding-rectangle-height
			(window-viewport pane))))
	   (updating-output (pane :unique-id -3
				  :cache-value (list cursor-y mark-y cursor-x mark-x
						     height width ink))
	     (draw-rectangle* pane
			      0 0
			      width height
			      :ink ink))))
	;; mark is above the top of the screen
	((and (null mark-y) (null mark-x))
	 (let ((width (stream-text-margin pane)))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list mark-y mark-x cursor-y width))
	       (draw-rectangle* pane
				0 0
				width cursor-y
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-y cursor-x))
	       (draw-rectangle* pane
				0 cursor-y 
				cursor-x (+ cursor-y line-height)
				:ink ink)))))
	;; mark is below the bottom of the screen
	((and (null mark-y) mark-x)
	 (let ((width (stream-text-margin pane))
	       (height (bounding-rectangle-height
			(window-viewport pane))))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list cursor-y width height))
	       (draw-rectangle* pane
				0 (+ cursor-y line-height)
				width height
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-x cursor-y width))
	       (draw-rectangle* pane
				cursor-x cursor-y
				width (+ cursor-y line-height)
				:ink ink)))))
	;; mark is at point
	((and (= mark-x cursor-x) (= mark-y cursor-y))
	 nil)
	;; mark and point are on the same line
	((= mark-y cursor-y)
	 (updating-output (pane :unique-id -3
				:cache-value (list offset1 offset2 ink))
	   (draw-rectangle* pane
			    mark-x mark-y
			    cursor-x (+ cursor-y line-height)
			    :ink ink)))
	;; mark and point are both visible, mark above point
	((< mark-y cursor-y)
	 (let ((width (stream-text-margin pane)))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list mark-x mark-y width))
	       (draw-rectangle* pane
				mark-x mark-y
				width (+ mark-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-x cursor-y))
	       (draw-rectangle* pane
				0 cursor-y
				cursor-x (+ cursor-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list mark-y cursor-y width))
	       (draw-rectangle* pane
				0 (+ mark-y line-height)
				width cursor-y
				:ink ink)))))
	;; mark and point are both visible, point above mark
	(t
	 (let ((width (stream-text-margin pane)))
	   (updating-output (pane :unique-id -3
				  :cache-value ink)
	     (updating-output (pane :cache-value (list cursor-x cursor-y width))
	       (draw-rectangle* pane
				cursor-x cursor-y
				width (+ cursor-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list mark-x mark-y))
	       (draw-rectangle* pane
				0 mark-y
				mark-x (+ mark-y line-height)
				:ink ink))
	     (updating-output (pane :cache-value (list cursor-y mark-y width))
	       (draw-rectangle* pane
				0 (+ cursor-y line-height)
				width mark-y
				:ink ink)))))))))

(defmethod highlight-region ((pane climacs-pane) (mark1 mark) (mark2 mark)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  (highlight-region pane (offset mark1) (offset mark2) ink))

(defmethod highlight-region ((pane climacs-pane) (mark1 mark) (offset2 integer)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  (highlight-region pane (offset mark1) offset2 ink))

(defmethod highlight-region ((pane climacs-pane) (offset1 integer) (mark2 mark)
			     &optional (ink (compose-in +green+ (make-opacity .1))))
  (highlight-region pane offset1 (offset mark2) ink))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse 

;; do this better
(defmethod syntax-line-indentation (mark tab-width (syntax fundamental-syntax))
  0)
