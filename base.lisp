;;; -*- Mode: Lisp; Package: CLIMACS-BASE -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004 by
;;;           Elliott Johnson (ejohnson@fasl.info)

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

;;; Basic functionality built on top of the buffer protocol.  Here is
;;; where we define slightly higher level functions such as
;;; {previous,next}-line, {forward,backward}-word, etc. that can be
;;; directly implemented in terms of the buffer protocol, but that are
;;; not, strictly speaking, part of that protocol.

(in-package :climacs-base)

(defmacro do-buffer-region ((object offset buffer offset1 offset2)
                            &body body)
  "Iterate over the elements of the region delimited by offset1 and offset2.
The body is executed for each element, with object being the current object
(setf-able), and offset being its offset."
  `(symbol-macrolet ((,object (buffer-object ,buffer ,offset)))
     (loop for ,offset from ,offset1 to ,offset2
           do ,@body)))

(defgeneric backward-object (mark &optional count))

(defmethod backward-object ((mark climacs-buffer::mark-mixin)
                            &optional (count 1))
  (decf (offset mark) count))

(defgeneric forward-object (mark &optional count))

(defmethod forward-object ((mark climacs-buffer::mark-mixin)
                           &optional (count 1))
  (incf (offset mark) count))

(defun previous-line (mark &optional column)
  "Move a mark up one line conserving horizontal position."
  (unless column
    (setf column (column-number mark)))
  (beginning-of-line mark)
  (if (beginning-of-buffer-p mark)
      (incf (offset mark) column)
      (progn (decf (offset mark))
	     (when (> (column-number mark) column)
	       (beginning-of-line mark)
	       (incf (offset mark) column)))))

(defun next-line (mark &optional column)
  "Move a mark down one line conserving horizontal position."
  (unless column
    (setf column (column-number mark)))
  (end-of-line mark)
  (if (end-of-buffer-p mark)
      (progn (beginning-of-line mark)
	     (incf (offset mark) column))
      (progn (incf (offset mark))
	     (end-of-line mark)
	     (when (> (column-number mark) column)
	       (beginning-of-line mark)
	       (incf (offset mark) column)))))

(defun open-line (mark)
  "Create a new line in a buffer."
  (insert-object mark #\Newline)
  (decf (offset mark)))

(defun kill-line (mark)
  "Remove a line from a buffer."
  (if (end-of-line-p mark)
      (unless (end-of-buffer-p mark)
	(delete-range mark))
      (let ((offset (offset mark)))
	(end-of-line mark)
	(delete-region offset mark))))

(defun buffer-number-of-lines-in-region (buffer offset1 offset2)
  "Helper function for number-of-lines-in-region.  Count newline
characters in the region between offset1 and offset2"
  (loop while (< offset1 offset2)
	count (eql (buffer-object buffer offset1) #\Newline)
	do (incf offset1)))

(defgeneric number-of-lines-in-region (mark1 mark2)
  (:documentation "Return the number of lines (or rather the number of
Newline characters) in the region between MARK and MARK2.  It is
acceptable to pass an offset in place of one of the marks"))

(defmethod number-of-lines-in-region ((mark1 mark) (mark2 mark))
  (buffer-number-of-lines-in-region (buffer mark1) (offset mark1) (offset mark2)))

(defmethod number-of-lines-in-region ((offset integer) (mark mark))
  (buffer-number-of-lines-in-region (buffer mark) offset (offset mark)))

(defmethod number-of-lines-in-region ((mark mark) (offset integer))
  (buffer-number-of-lines-in-region (buffer mark) (offset mark) offset))

(defun constituentp (obj)
  "A predicate to ensure that an object is a constituent character."
  (and (characterp obj)
       #+sbcl (sb-impl::constituentp obj)
       #-sbcl (alphanumericp obj)))

(defun whitespacep (obj)
  "A predicate to ensure that an object is a whitespace character."
  (and (characterp obj)
       #+sbcl (sb-impl::whitespacep obj)
       #-sbcl (member obj '(#\Space #\Tab))))

(defun forward-to-word-boundary (mark)
  "Forward the mark forward to the beginning of the next word."
  (loop until (end-of-buffer-p mark)
	until (constituentp (object-after mark))
	do (incf (offset mark))))

(defun backward-to-word-boundary (mark)
  "Move the mark backward to the end of the previous word."
  (loop until (beginning-of-buffer-p mark)
	until (constituentp (object-before mark))
	do (decf (offset mark))))

(defun forward-word (mark)
  "Forward the mark to the next word."
  (forward-to-word-boundary mark)
  (loop until (end-of-buffer-p mark)
	while (constituentp (object-after mark))
	do (incf (offset mark))))

(defun backward-word (mark)
  "Shuttle the mark to the start of the previous word."
  (backward-to-word-boundary mark)
  (loop until (beginning-of-buffer-p mark)
	while (constituentp (object-before mark))
	do (decf (offset mark))))

(defun delete-word (mark)
  "Delete until the end of the word"
  (loop until (end-of-buffer-p mark)
	until (constituentp (object-after mark))
	do (delete-range mark))
  (loop until (end-of-buffer-p mark)
	while (constituentp (object-after mark))
	do (delete-range mark)))

(defun backward-delete-word (mark)
  "Delete until the beginning of the word"
  (loop until (beginning-of-buffer-p mark)
	until (constituentp (object-before mark))
	do (delete-range mark -1))
  (loop until (beginning-of-buffer-p mark)
	while (constituentp (object-before mark))
	do (delete-range mark -1)))

(defun previous-word (mark)
  "Return a freshly allocated sequence, that is word before the mark"
  (region-to-sequence
   (loop for i downfrom (offset mark)
	 while (and (plusp i)
		    (constituentp (buffer-object (buffer mark) (1- i))))
	 finally (return i))
   mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

(defun downcase-buffer-region (buffer offset1 offset2)
  (do-buffer-region (object offset buffer offset1 offset2)
    (when (and (constituentp object) (upper-case-p object))
      (setf object (char-downcase object)))))

(defgeneric downcase-region (mark1 mark2)
  (:documentation "Convert all characters after mark1 and before mark2 to
lowercase. An error is signaled if the two marks are positioned in different
buffers. It is acceptable to pass an offset in place of one of the marks."))

(defmethod downcase-region ((mark1 climacs-buffer::mark-mixin)
                            (mark2 climacs-buffer::mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (downcase-buffer-region (buffer mark1) (offset mark1) (offset mark2)))

(defmethod downcase-region ((offset integer) (mark climacs-buffer::mark-mixin))
  (downcase-buffer-region (buffer mark) offset (offset mark)))

(defmethod downcase-region ((mark climacs-buffer::mark-mixin) (offset integer))
  (downcase-buffer-region (buffer mark) (offset mark) offset))

(defun downcase-word (mark &optional (n 1))
  "Convert the next N words to lowercase, leaving mark after the last word."
  (loop repeat n
        do (forward-to-word-boundary mark)
           (let ((offset (offset mark)))
             (forward-word mark)
             (downcase-region offset mark))))

(defun upcase-buffer-region (buffer offset1 offset2)
  (do-buffer-region (object offset buffer offset1 offset2)
    (when (and (constituentp object) (lower-case-p object))
      (setf object (char-upcase object)))))

(defgeneric upcase-region (mark1 mark2)
  (:documentation "Convert all characters after mark1 and before mark2 to
uppercase. An error is signaled if the two marks are positioned in different
buffers. It is acceptable to pass an offset in place of one of the marks."))

(defmethod upcase-region ((mark1 climacs-buffer::mark-mixin)
                          (mark2 climacs-buffer::mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (upcase-buffer-region (buffer mark1) (offset mark1) (offset mark2)))

(defmethod upcase-region ((offset integer) (mark climacs-buffer::mark-mixin))
  (upcase-buffer-region (buffer mark) offset (offset mark)))

(defmethod upcase-region ((mark climacs-buffer::mark-mixin) (offset integer))
  (upcase-buffer-region (buffer mark) (offset mark) offset))

(defun upcase-word (mark &optional (n 1))
  "Convert the next N words to uppercase, leaving mark after the last word."
  (loop repeat n
        do (forward-to-word-boundary mark)
           (let ((offset (offset mark)))
             (forward-word mark)
             (upcase-region offset mark))))

(defun capitalize-buffer-region (buffer offset1 offset2)
  (let ((previous-char-constituent-p
         (and (plusp offset1)
              (constituentp (buffer-object buffer (1- offset1))))))
    (do-buffer-region (object offset buffer offset1 offset2)
      (when (constituentp object)
        (if previous-char-constituent-p
            (when (upper-case-p object)
              (setf object (char-downcase object)))
            (when (lower-case-p object)
              (setf object (char-upcase object)))))
      (setf previous-char-constituent-p (constituentp object)))))

(defgeneric capitalize-region (mark1 mark2)
  (:documentation "Capitalize all words after mark1 and before mark2.
An error is signaled if the two marks are positioned in different buffers.
It is acceptable to pass an offset in place of one of the marks."))

(defmethod capitalize-region ((mark1 climacs-buffer::mark-mixin)
                              (mark2 climacs-buffer::mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (capitalize-buffer-region (buffer mark1) (offset mark1) (offset mark2)))

(defmethod capitalize-region ((offset integer)
                              (mark climacs-buffer::mark-mixin))
  (capitalize-buffer-region (buffer mark) offset (offset mark)))

(defmethod capitalize-region ((mark climacs-buffer::mark-mixin)
                              (offset integer))
  (capitalize-buffer-region (buffer mark) (offset mark) offset))

(defun capitalize-word (mark &optional (n 1))
  "Capitalize the next N words, leaving mark after the last word."
  (loop repeat n
        do (forward-to-word-boundary mark)
           (let ((offset (offset mark)))
             (forward-word mark)
             (capitalize-region offset mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Named objects

(defgeneric name (obj))

(defclass name-mixin ()
  ((name :initarg :name :accessor name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Search

(defun buffer-looking-at (buffer offset vector &key (test #'eql))
  "return true if and only if BUFFER contains VECTOR at OFFSET"
  (and (<= (+ offset (length vector)) (size buffer))
       (loop for i from offset
	     for obj across vector
	     unless (funcall test (buffer-object buffer i) obj)
	       return nil
	     finally (return t))))

(defun looking-at (mark vector &key (test #'eql))
  "return true if and only if BUFFER contains VECTOR after MARK"
  (buffer-looking-at (buffer mark) (offset mark) vector :test test))


(defun buffer-search-forward (buffer offset vector &key (test #'eql))
  "return the smallest offset of BUFFER >= OFFSET containing VECTOR
or NIL if no such offset exists"
  (loop for i from offset to (size buffer)
	when (buffer-looking-at buffer i vector :test test)
	  return i
	finally (return nil)))
			      

(defun buffer-search-backward (buffer offset vector &key (test #'eql))
  "return the largest offset of BUFFER <= (- OFFSET (length VECTOR))
containing VECTOR or NIL if no such offset exists"
  (loop for i downfrom (- offset (length vector)) to 0
	when (buffer-looking-at buffer i vector :test test)
	  return i
	finally (return nil)))			       

(defun search-forward (mark vector &key (test #'eql))
  "move MARK forward after the first occurence of VECTOR after MARK"
  (let ((offset (buffer-search-forward
		 (buffer mark) (offset mark) vector :test test)))
    (when offset
      (setf (offset mark) (+ offset (length vector))))))

(defun search-backward (mark vector &key (test #'eql))
  "move MARK backward before the first occurence of VECTOR before MARK"
  (let ((offset (buffer-search-backward
		 (buffer mark) (offset mark) vector :test test)))
    (when offset
      (setf (offset mark) offset))))

(defun buffer-search-word-backward (buffer offset word &key (test #'eql))
  "return the largest offset of BUFFER <= (- OFFSET (length WORD))
containing WORD as a word or NIL if no such offset exists"
  (loop for i downfrom (- offset (length word)) to 0
	when (and (or (zerop i) (whitespacep (buffer-object buffer (1- i))))
	      (buffer-looking-at buffer i word :test test))
	  return i
	finally (return nil)))

(defun buffer-search-word-forward (buffer offset word &key (test #'eql))
  "Return the smallest offset of BUFFER >= (+ OFFSET (length WORD))
containing WORD as a word or NIL if no such offset exists"
  (loop for i upfrom (+ offset (length word)) to (- (size buffer) (max (length word) 1))
	when (and (whitespacep (buffer-object buffer (1- i)))
		  (buffer-looking-at buffer i word :test test))
	  return i
	finally (return nil)))
