;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2005 by
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

(define-syntax text-syntax ("Text" (basic-syntax))
  ((paragraphs :initform (make-instance 'standard-flexichain))))

(defmethod redisplay-with-syntax :before (pane (syntax text-syntax))
  (let* ((buffer (buffer pane))
	 (high-offset (min (+ (offset (high-mark buffer)) 3) (size buffer)))
	 (low-offset (max (- (offset (low-mark buffer)) 3) 0)))
    (with-slots (paragraphs) syntax
       (let* ((nb-paragraphs (nb-elements paragraphs))
	      (pos2 nb-paragraphs)
	      (pos1 0))
	 ;; start by deleting all syntax marks that are between the low and
	 ;; the high marks
	 (loop until (= pos1 pos2)
	       do (cond ((mark< (element* paragraphs (floor (+ pos1 pos2) 2))
				low-offset)
			 (setf pos1 (floor (+ pos1 1 pos2) 2)))
			(t
			 (setf pos2 (floor (+ pos1 pos2) 2)))))
	 (loop repeat (- nb-paragraphs pos1)
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
