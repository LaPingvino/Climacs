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
;;;
;;; D.H.: Invariant text needs to change to reflect sentences.
;;;       Should there be paragraph invariants and sentence invariants?
;;;       Did I ducttape this in the wrong place?
;;;       Sentence invariants:  
;;;       Left stickies after . ? and !, at the end of the buffer
;;;       Right stickies at non whitespace characters preceeded by space and punctuation.
;;;       

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

(define-syntax text-syntax (basic-syntax)
  ((paragraphs :initform (make-instance 'standard-flexichain))
   (sentence-beginnings :initform (make-instance 'standard-flexichain))
   (sentence-endings :initform (make-instance 'standard-flexichain)))
  (:name "Text")
  (:pathname-types "text" "txt" "README"))

(defmethod update-syntax (buffer (syntax text-syntax))
  (let* ((high-offset (min (+ (offset (high-mark buffer)) 3) (size buffer)))
	 (low-offset (max (- (offset (low-mark buffer)) 3) 0)))
    (with-slots (paragraphs sentence-beginnings sentence-endings) syntax
      (let ((pos1 (index-of-mark-after-offset paragraphs low-offset))
	    (pos-sentence-beginnings (index-of-mark-after-offset sentence-beginnings low-offset))
	    (pos-sentence-endings (index-of-mark-after-offset sentence-endings low-offset)))
	 ;; start by deleting all syntax marks that are between the low and
	 ;; the high marks
	 (loop repeat (- (nb-elements paragraphs) pos1)
	       while (mark<= (element* paragraphs pos1) high-offset)
	       do (delete* paragraphs pos1))
	 (loop repeat (- (nb-elements sentence-beginnings) pos-sentence-beginnings)
	       while (mark<= (element* sentence-beginnings pos-sentence-beginnings) high-offset)
	       do (delete* sentence-beginnings pos-sentence-beginnings))
	 (loop repeat (- (nb-elements sentence-endings) pos-sentence-endings)
	       while (mark<= (element* sentence-endings pos-sentence-endings) high-offset)
	       do (delete* sentence-endings pos-sentence-endings))

	 ;; check the zone between low-offset and high-offset for
	 ;; paragraph delimiters and sentence delimiters
	 (loop with buffer-size = (size buffer)
	       for offset from low-offset to high-offset              ;; Could be rewritten with even fewer buffer-object calls,
	       for current-object = nil then (if (>= offset high-offset) nil (buffer-object buffer offset)) ;;  but it'd be premature optimization, and messy besides.  
	       for next-object =  nil then (if (>= offset (- high-offset 1)) nil (buffer-object buffer (1+ offset)))
	       for prev-object =  nil then (if (= offset low-offset) nil (buffer-object buffer (1- offset)))
	       for before-prev-object = nil then (if (<= offset (1+ low-offset)) nil (buffer-object buffer (- offset 2)))
	       do (progn 
 		    (cond ((and (< offset buffer-size)
				(member prev-object '(#\. #\? #\!))
 				(or (= offset (1- buffer-size))
 				    (and (member current-object '(#\Newline #\Space #\Tab))
 					 (or (= offset 1)
 					     (not (member before-prev-object '(#\Newline #\Space #\Tab)))))))
 			   (let ((m (clone-mark (low-mark buffer) :left)))
 			     (setf (offset m) offset)
 			     (insert* sentence-endings pos-sentence-endings m))
 			   (incf pos-sentence-endings))

 			((and (>= offset 0)
 			      (not (member current-object '(#\. #\? #\! #\Newline #\Space #\Tab)))
 			      (or (= offset 0)
 				  (member prev-object '(#\Newline #\Space #\Tab)))
 			      (or (<= offset 1)
 				  (member before-prev-object '(#\. #\? #\! #\Newline #\Space #\Tab))))
 			 (let ((m (clone-mark (low-mark buffer) :right)))
 			   (setf (offset m) offset)
 			   (insert* sentence-beginnings pos-sentence-beginnings m))
 			 (incf pos-sentence-beginnings))
 			(t nil))

		    ;; Paragraphs

		    (cond ((and (< offset buffer-size) ;; Ends
			      (not (eql current-object #\Newline))
			      (or (zerop offset)
				  (and (eql prev-object #\Newline)
				       (or (= offset 1)
					   (eql before-prev-object #\Newline)))))
			 (let ((m (clone-mark (low-mark buffer) :left)))
			   (setf (offset m) offset)
			   (insert* paragraphs pos1 m))
			 (incf pos1))

			((and (plusp offset) ;;Beginnings
			      (not (eql prev-object #\Newline))
			      (or (= offset buffer-size)
				  (and (eql current-object #\Newline)
				       (or (= offset (1- buffer-size))
					   (eql next-object #\Newline)))))
			 (let ((m (clone-mark (low-mark buffer) :right)))
			   (setf (offset m) offset)
			   (insert* paragraphs pos1 m))
			 (incf pos1))
			(t nil))))))))

(defmethod backward-one-paragraph (mark (syntax text-syntax))
  (with-slots (paragraphs) syntax
     (let ((pos1 (index-of-mark-after-offset paragraphs (offset mark))))
       (when (> pos1 0)
	 (setf (offset mark)
	       (if (typep (element* paragraphs (1- pos1)) 'right-sticky-mark)
		   (offset (element* paragraphs (- pos1 2)))
		   (offset (element* paragraphs (1- pos1)))))
         t))))

(defmethod forward-one-paragraph (mark (syntax text-syntax))
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
		   (offset (element* paragraphs pos1))))
         t))))

 (defmethod backward-one-sentence (mark (syntax text-syntax))
   (with-slots (sentence-beginnings) syntax
      (let ((pos1 (index-of-mark-after-offset sentence-beginnings (offset mark))))
        (when (> pos1 0)
          (setf (offset mark)
                (offset (element* sentence-beginnings (1- pos1))))
          t))))

 (defmethod forward-one-sentence (mark (syntax text-syntax))
   (with-slots (sentence-endings) syntax
     (let ((pos1 (index-of-mark-after-offset
                  sentence-endings
                  ;; if mark is at sentence-end, jump to end of next
                  ;; sentence
                  (1+ (offset mark)))))
       (when (< pos1 (nb-elements sentence-endings))
 	 (setf (offset mark)
               (offset (element* sentence-endings pos1)))
         t))))

(defmethod syntax-line-indentation (mark tab-width (syntax text-syntax))
  (loop with indentation = 0
        with mark2 = (clone-mark mark)
        until (beginning-of-buffer-p mark2)
        do (climacs-motion:backward-line mark2 syntax)
           (setf indentation (line-indentation mark2 tab-width))
        while (empty-line-p mark2)
        finally (return indentation)))
