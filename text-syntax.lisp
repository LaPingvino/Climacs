;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

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

;;; Syntax for analysing ordinary text.

;;; Invariants after a complete syntax analysis:
;;;
;;; There is exactly one left-sticky mark at every offset followed by
;;; character other than a newline and preceded either by nothing
;;; (beginning of the buffer), by a newline character at the beginning
;;; of the buffer, or by two newline characters.
;;; 
;;; There is exactly one right-sticky mark at every offset preceded by
;;; a character other than a newline and followed either by nothing
;;; (end of the buffer), by a newline character at the end of the
;;; buffer, or by two newline characters.
;;;
;;; It follows that:
;;;   * there can never be two marks in the same place,
;;;   * there are as many left-sticky marks as right-sticky marks, 
;;;   * left-sticky and right-sticky marks alternate, starting with a 
;;;     left-sticky mark
;;;
;;; N.B.: These invariants only hold AFTER a complete syntax analysis.
;;;       we do now know what might have happened during the editing
;;;       phase between to invocations of the analysis.

(in-package :climacs-syntax) ;;; Put this in a separate package once it works

(defun index-of-mark-after-offset (flexichain offset)
  "Searches for the mark after `offset' in the marks stored in `flexichain'."
  (loop with low-position = 0
     with high-position = (nb-elements flexichain)
     for middle-position = (floor (+ low-position high-position) 2)
     until (= low-position high-position)
     do (if (mark>= (element* flexichain middle-position) offset)
            (setf high-position middle-position)
            (setf low-position (floor (+ low-position 1 high-position) 2)))
     finally (return low-position)))

(define-syntax text-syntax ("Text" (basic-syntax))
  ((paragraphs :initform (make-instance 'standard-flexichain))))

(defmethod update-syntax (buffer (syntax text-syntax))
  (let* ((high-offset (min (+ (offset (high-mark buffer)) 3) (size buffer)))
	 (low-offset (max (- (offset (low-mark buffer)) 3) 0)))
    (with-slots (paragraphs) syntax
       (let ((pos1 (index-of-mark-after-offset paragraphs low-offset)))
	 ;; start by deleting all syntax marks that are between the low and
	 ;; the high marks
	 (loop repeat (- (nb-elements paragraphs) pos1)
	       while (mark<= (element* paragraphs pos1) high-offset)
	       do (delete* paragraphs pos1))
	 ;; check the zone between low-offset and high-offset for
	 ;; paragraph delimiters
	 (loop with buffer-size = (size buffer)
	       for offset from low-offset to high-offset
	       do (cond ((and (< offset buffer-size)
			      (not (eql (buffer-object buffer offset) #\Newline))
			      (or (zerop offset)
				  (and (eql (buffer-object buffer (1- offset)) #\Newline)
				       (or (= offset 1)
					   (eql (buffer-object buffer (- offset 2)) #\Newline)))))
			 (insert* paragraphs pos1
				  (make-instance 'standard-left-sticky-mark
				     :buffer buffer :offset offset))
			 (incf pos1))
			((and (plusp offset)
			      (not (eql (buffer-object buffer (1- offset)) #\Newline))
			      (or (= offset buffer-size)
				  (and (eql (buffer-object buffer offset) #\Newline)
				       (or (= offset (1- buffer-size))
					   (eql (buffer-object buffer (1+ offset)) #\Newline)))))
			 (insert* paragraphs pos1
				  (make-instance 'standard-right-sticky-mark
				     :buffer buffer :offset offset))
			 (incf pos1))
			(t nil)))))))

(defgeneric beginning-of-paragraph (mark text-syntax))

(defmethod beginning-of-paragraph (mark (syntax text-syntax))
  (with-slots (paragraphs) syntax
     (let ((pos1 (index-of-mark-after-offset paragraphs (offset mark))))
       (when (> pos1 0)
	 (setf (offset mark)
	       (if (typep (element* paragraphs (1- pos1)) 'right-sticky-mark)
		   (offset (element* paragraphs (- pos1 2)))
		   (offset (element* paragraphs (1- pos1)))))))))

(defgeneric end-of-paragraph (mark text-syntax))

(defmethod end-of-paragraph (mark (syntax text-syntax))
  (with-slots (paragraphs) syntax
    (let ((pos1 (index-of-mark-after-offset
                 paragraphs
                 ;; if mark is at paragraph-end, jump to end of next
                 ;; paragraph
                 (1+ (offset mark)))))
      (when (< pos1 (nb-elements paragraphs))
	 (setf (offset mark)
	       (if (typep (element* paragraphs pos1) 'left-sticky-mark)
		   (offset (element* paragraphs (1+ pos1)))
		   (offset (element* paragraphs pos1))))))))

(defmethod syntax-line-indentation (mark tab-width (syntax text-syntax))
  (loop with indentation = 0
        with mark2 = (clone-mark mark)
        until (beginning-of-buffer-p mark2)
        do (previous-line mark2)
           (setf indentation (line-indentation mark2 tab-width))
        while (empty-line-p mark2)
        finally (return indentation)))
