;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2005 by
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

;;; The CLIM pane used for displaying Climacs objects

(in-package :climacs-pane)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tabify

(defgeneric space-width (tabify))
(defgeneric tab-width (tabify))
(defgeneric tab-space-count (tabify))

(defclass tabify-mixin ()
  ((space-width :initform nil :reader space-width)
   (tab-width :initform nil :reader tab-width)))

(defmethod tab-space-count ((tabify t))
  1)

(defmethod tab-space-count ((tabify tabify-mixin))
  (round (tab-width tabify) (space-width tabify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View

(defclass climacs-textual-view (textual-view tabify-mixin)
  ())

(defclass filename-mixin ()
  ((filename :initform nil :accessor filename)))

;(defgeneric indent-tabs-mode (climacs-buffer))

(defclass climacs-buffer (standard-buffer abbrev-mixin filename-mixin name-mixin)
  ((needs-saving :initform nil :accessor needs-saving)
   (syntax :initarg :syntax :initform (make-instance 'basic-syntax) :accessor syntax)
   (indent-tabs-mode :initarg indent-tabs-mode :initform t
                     :accessor indent-tabs-mode))
  (:default-initargs :name "*scratch*"))


(defclass climacs-pane (application-pane)
  ((buffer :initform (make-instance 'climacs-buffer) :accessor buffer)
   (point :initform nil :initarg :point :reader point)
   (mark :initform nil :initarg :mark :accessor mark)
   (top :reader top)
   (bot :reader bot)
   (scan :reader scan)
   (cursor-x :initform 2)
   (cursor-y :initform 2)
   (space-width :initform nil)
   (tab-width :initform nil)
   (full-redisplay-p :initform nil :accessor full-redisplay-p)
   (cache :initform (let ((cache (make-instance 'standard-flexichain)))
		      (insert* cache 0 nil)
		      cache))))

(defmethod tab-width ((pane climacs-pane))
  (tab-width (stream-default-view pane)))

(defmethod space-width ((pane climacs-pane))
  (space-width (stream-default-view pane)))

(defmethod initialize-instance :after ((pane climacs-pane) &rest args)
  (declare (ignore args))
  (with-slots (buffer point mark) pane
     (when (null point)
       (setf point (make-instance 'standard-right-sticky-mark
		      :buffer buffer)))
     (when (null mark)
       (setf mark (make-instance 'standard-right-sticky-mark
		      :buffer buffer))))
  (with-slots (buffer top bot scan) pane
     (setf top (make-instance 'standard-left-sticky-mark :buffer buffer)
	   bot (make-instance 'standard-right-sticky-mark :buffer buffer)))
  (setf (stream-default-view pane) (make-instance 'climacs-textual-view))
  (with-slots (space-width tab-width) (stream-default-view pane)
     (let* ((medium (sheet-medium pane))
	    (style (medium-text-style medium)))
       (setf space-width (text-style-width style medium)
	     tab-width (* 8 space-width)))))

(defmethod (setf buffer) :after (buffer (pane climacs-pane))
  (with-slots (point mark top bot) pane
       (setf point (make-instance 'standard-right-sticky-mark
		      :buffer buffer)
	     mark (make-instance 'standard-right-sticky-mark
		     :buffer buffer)
	     top (make-instance 'standard-left-sticky-mark :buffer buffer)
	     bot (make-instance 'standard-right-sticky-mark :buffer buffer))))

(define-presentation-type url ()
  :inherit-from 'string)

(defgeneric present-contents (contents pane))

(defmethod present-contents (contents pane)
  (unless (null contents)
    (present contents
	     (if (and (>= (length contents) 7) (string= (subseq contents 0 7) "http://"))
		 'url
		 'string)
	     :stream pane)))

(defgeneric display-line (pane line offset syntax view))

(defmethod display-line (pane line offset (syntax basic-syntax) (view textual-view))
  (declare (ignore offset))
  (let ((saved-index nil)
	(id 0))
    (flet ((output-word (index)
	     (unless (null saved-index)
	       (let ((contents (coerce (subseq line saved-index index) 'string)))
		 (updating-output (pane :unique-id (incf id)
					:cache-value contents
					:cache-test #'string=)
		   (present-contents contents pane)))
	       (setf saved-index nil))))
      (with-slots (bot scan cursor-x cursor-y) pane
	 (loop with space-width = (space-width pane)
	       with tab-width = (tab-width pane)
	       for index from 0
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
			   (present obj 'character :stream pane)))
			(t
			 (output-word index)
			 (updating-output (pane :unique-id (incf id)
						:cache-value obj
						:cache-test #'eq)
			   (present obj 'character :stream pane))))
		  (incf scan)
	       finally (output-word index)
		       (when (mark= scan (point pane))
			 (multiple-value-bind (x y) (stream-cursor-position pane)
			   (setf cursor-x x
				 cursor-y y)))
		       (terpri pane)
		       (incf scan))))))

(defgeneric fill-cache (pane)
  (:documentation "fill nil cache entries from the buffer"))

(defmethod fill-cache (pane)
  (with-slots (top bot cache) pane
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
(defun adjust-cache-size-and-bot (pane)
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot cache) pane
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
(defun reposition-window (pane)
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top cache) pane
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
(defun adjust-cache (pane)
  (let* ((buffer (buffer pane))
	 (high-mark (high-mark buffer))
	 (low-mark (low-mark buffer))
	 (nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot cache) pane
       (beginning-of-line top)
       (end-of-line bot)
       (if (or (mark< (point pane) top)
	       (>= (number-of-lines-in-region top (point pane)) nb-lines-in-pane)
	       (and (mark< low-mark top)
		    (>= (number-of-lines-in-region top high-mark) (nb-elements cache))))
	   (reposition-window pane)
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
  (adjust-cache-size-and-bot pane))

(defun page-down (pane)
  (adjust-cache pane)
  (with-slots (top bot cache) pane
     (when (mark> (size (buffer bot)) bot)
       (empty-cache cache)
       (setf (offset top) (offset bot))
       (beginning-of-line top)
       (setf (offset (point pane)) (offset top)))))

(defun page-up (pane)
  (adjust-cache pane)
  (with-slots (top bot cache) pane
     (when (> (offset top) 0)
       (let ((nb-lines-in-region (number-of-lines-in-region top bot)))
	 (setf (offset bot) (offset top))
	 (end-of-line bot)
	 (loop repeat  nb-lines-in-region
	       while (> (offset top) 0)
	       do (decf (offset top))
		  (beginning-of-line top))
	 (setf (offset (point pane)) (offset top))
	 (adjust-cache pane)
	 (setf (offset (point pane)) (offset bot))
	 (beginning-of-line (point pane))
	 (empty-cache cache)))))

(defun display-cache (pane cursor-ink)
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (with-slots (top bot scan cache cursor-x cursor-y) pane
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
		  (display-line pane (element* cache id) start-offset
				(syntax (buffer pane)) (stream-default-view pane)))
		(incf start-offset (1+ (length (element* cache id)))))
       (when (mark= scan (point pane))
	 (multiple-value-bind (x y) (stream-cursor-position pane)
	   (setf cursor-x x
		 cursor-y y)))
       (updating-output (pane :unique-id -1)
	 (draw-rectangle* pane
			  (1- cursor-x) (- cursor-y (* 0.2 height))
			  (+ cursor-x 2) (+ cursor-y (* 0.8 height))
			  :ink cursor-ink)))))  

(defgeneric redisplay-pane (pane current-p))

(defmethod redisplay-pane ((pane climacs-pane) current-p)
  (if (full-redisplay-p pane)
      (progn (reposition-window pane)
	     (adjust-cache-size-and-bot pane)
	     (setf (full-redisplay-p pane) nil))
      (adjust-cache pane))
  (fill-cache pane)
  (display-cache pane (if current-p +red+ +blue+)))

(defgeneric full-redisplay (pane))

(defmethod full-redisplay ((pane climacs-pane))
  (setf (full-redisplay-p pane) t))