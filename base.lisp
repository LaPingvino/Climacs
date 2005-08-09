;;; -*- Mode: Lisp; Package: CLIMACS-BASE -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

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
\(setf-able), and offset being its offset."
  `(symbol-macrolet ((,object (buffer-object ,buffer ,offset)))
     (loop for ,offset from ,offset1 below ,offset2
           do ,@body)))

(defmethod previous-line (mark &optional column (count 1))
  "Move a mark up COUNT lines conserving horizontal position."
  (unless column
    (setf column (column-number mark)))
  (loop repeat count
	do (beginning-of-line mark)
	until (beginning-of-buffer-p mark)
	do (backward-object mark))
  (end-of-line mark)
  (when (> (column-number mark) column)
    (beginning-of-line mark)
    (incf (offset mark) column)))

(defmethod previous-line ((mark p-line-mark-mixin) &optional column (count 1))
  "Move a mark up COUNT lines conserving horizontal position."
  (unless column
    (setf column (column-number mark)))
  (let* ((line (line-number mark))
	 (goto-line (max 0 (- line count))))
    (setf (offset mark)
	  (+ column (buffer-line-offset (buffer mark) goto-line)))))

(defmethod next-line (mark &optional column (count 1))
  "Move a mark down COUNT lines conserving horizontal position."
  (unless column
    (setf column (column-number mark)))
  (loop repeat count
	do (end-of-line mark)
	until (end-of-buffer-p mark)
	do (forward-object mark))
  (end-of-line mark)
  (when (> (column-number mark) column)
    (beginning-of-line mark)
    (incf (offset mark) column)))

(defmethod next-line ((mark p-line-mark-mixin) &optional column (count 1))
  "Move a mark down COUNT lines conserving horizontal position."
  (unless column
    (setf column (column-number mark)))
  (let* ((line (line-number mark))
	 (goto-line (min (number-of-lines (buffer mark))
			 (+ line count))))
    (setf (offset mark)
	  (+ column (buffer-line-offset (buffer mark) goto-line)))))

(defmethod open-line ((mark left-sticky-mark) &optional (count 1))
  "Create a new line in a buffer after the mark."
  (loop repeat count
     do (insert-object mark #\Newline)))

(defmethod open-line ((mark right-sticky-mark) &optional (count 1))
  "Create a new line in a buffer after the mark."
  (loop repeat count
     do (insert-object mark #\Newline)
        (decf (offset mark))))

(defun kill-line (mark)
  "Remove a line from a buffer."
  (if (end-of-line-p mark)
      (unless (end-of-buffer-p mark)
	(delete-range mark))
      (let ((offset (offset mark)))
	(end-of-line mark)
	(delete-region offset mark))))

(defun empty-line-p (mark)
  "Check whether the mark is in an empty line."
  (and (beginning-of-line-p mark) (end-of-line-p mark)))

(defun line-indentation (mark tab-width)
  "Return the distance from the beginning of the line and the first
constituent character of the line."
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop with indentation = 0
          until (end-of-buffer-p mark2)
          as object = (object-after mark2)
          while (or (eql object #\Space) (eql object #\Tab))
          do (incf indentation
                   (if (eql (object-after mark2) #\Tab) tab-width 1))
             (incf (offset mark2))
          finally (return indentation))))

(defmethod buffer-number-of-lines-in-region (buffer offset1 offset2)
  "Helper method for number-of-lines-in-region.  Count newline
characters in the region between offset1 and offset2."
  (loop while (< offset1 offset2)
	count (eql (buffer-object buffer offset1) #\Newline)
	do (incf offset1)))

(defmethod buffer-number-of-lines-in-region
    ((buffer binseq2-buffer) offset1 offset2)
  "Helper method for NUMBER-OF-LINES-IN-REGION."
  (- (buffer-line-number buffer offset2)
     (buffer-line-number buffer offset1)))

(defun buffer-display-column (buffer offset tab-width)
  (let ((line-start-offset (- offset (buffer-column-number buffer offset))))
    (loop with column = 0
          for i from line-start-offset below offset
          do (incf column (if (eql (buffer-object buffer i) #\Tab)
                              (- tab-width (mod column tab-width))
                              1))
          finally (return column))))

(defgeneric number-of-lines-in-region (mark1 mark2)
  (:documentation "Return the number of lines (or rather the
number of Newline characters) in the region between MARK and
MARK2.  An error is signaled if the two marks are positioned in
different buffers.  It is acceptable to pass an offset in place of
one of the marks"))

(defmethod number-of-lines-in-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (buffer-number-of-lines-in-region (buffer mark1) offset1 offset2)))

(defmethod number-of-lines-in-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (buffer-number-of-lines-in-region (buffer mark2) offset1 offset2)))

(defmethod number-of-lines-in-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (buffer-number-of-lines-in-region (buffer mark1) offset1 offset2)))

(defun constituentp (obj)
  "A predicate to ensure that an object is a constituent character."
  (and (characterp obj)
       #+sbcl (sb-impl::constituentp obj)
       #-sbcl (or (alphanumericp obj)
                  (member obj '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
                                #\: #\< #\= #\> #\? #\@ #\^ #\~ #\_
                                #\{ #\} #\[ #\] )))))

(defun whitespacep (obj)
  "A predicate to ensure that an object is a whitespace character."
  (and (characterp obj)
       (member obj '(#\Space #\Tab #\Newline #\Page #\Return))))

(defun forward-to-word-boundary (mark)
  "Move the mark forward to the beginning of the next word."
  (loop until (end-of-buffer-p mark)
	until (constituentp (object-after mark))
	do (incf (offset mark))))

(defun backward-to-word-boundary (mark)
  "Move the mark backward to the end of the previous word."
  (loop until (beginning-of-buffer-p mark)
	until (constituentp (object-before mark))
	do (decf (offset mark))))

(defun forward-word (mark &optional (count 1))
  "Forward the mark to the next word."
  (loop repeat count
	do (forward-to-word-boundary mark)
	   (loop until (end-of-buffer-p mark)
		 while (constituentp (object-after mark))
		 do (incf (offset mark)))))

(defun backward-word (mark &optional (count 1))
  "Shuttle the mark to the start of the previous word."
  (loop repeat count
	do (backward-to-word-boundary mark)
	   (loop until (beginning-of-buffer-p mark)
		 while (constituentp (object-before mark))
		 do (decf (offset mark)))))

(defun delete-word (mark &optional (count 1))
  "Delete until the end of the word"
  (let ((mark2 (clone-mark mark)))
    (forward-word mark2 count)
    (delete-region mark mark2)))

(defun backward-delete-word (mark &optional (count 1))
  "Delete until the beginning of the word"
  (let ((mark2 (clone-mark mark)))
    (backward-word mark2 count)
    (delete-region mark mark2)))

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

(defun buffer-region-case (buffer offset1 offset2)
  (let ((possibly-uppercase t)
        (possibly-lowercase t)
        (possibly-capitalized t))
    (do-buffer-region (object offset buffer offset1 offset2)
      (unless (characterp object)
        (return-from buffer-region-case nil))
      (when (lower-case-p object)
        (setf possibly-uppercase nil))
      (when (upper-case-p object)
        (setf possibly-lowercase nil))
      (when (plusp offset)
        (let ((previous-object (buffer-object buffer (1- offset))))
          (when (and (characterp previous-object)
                     (if (constituentp previous-object)
                         (upper-case-p object)
                         (lower-case-p object)))
            (setf possibly-capitalized nil)))))
    (cond (possibly-uppercase :upper-case)
          (possibly-lowercase :lower-case)
          (possibly-capitalized :capitalized)
          (t nil))))

;;; I'd rather have update-buffer-range methods spec. on buffer for this,
;;; for performance and history-size reasons --amb
(defun downcase-buffer-region (buffer offset1 offset2)
  (do-buffer-region (object offset buffer offset1 offset2)
    (when (and (constituentp object) (upper-case-p object))
      (setf object (char-downcase object)))))

(defgeneric downcase-region (mark1 mark2)
  (:documentation "Convert all characters after mark1 and before mark2 to
lowercase. An error is signaled if the two marks are positioned in different
buffers. It is acceptable to pass an offset in place of one of the marks."))

(defmethod downcase-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (downcase-buffer-region (buffer mark1) offset1 offset2)))

(defmethod downcase-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (downcase-buffer-region (buffer mark2) offset1 offset2)))

(defmethod downcase-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (downcase-buffer-region (buffer mark1) offset1 offset2)))

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

(defmethod upcase-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (upcase-buffer-region (buffer mark1) offset1 offset2)))

(defmethod upcase-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (upcase-buffer-region (buffer mark2) offset1 offset2)))

(defmethod upcase-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (upcase-buffer-region (buffer mark1) offset1 offset2)))

(defun upcase-word (mark &optional (n 1))
  "Convert the next N words to uppercase, leaving mark after the last word."
  (loop repeat n
        do (forward-to-word-boundary mark)
           (let ((offset (offset mark)))
             (forward-word mark)
             (upcase-region offset mark))))

(defun capitalize-buffer-region (buffer offset1 offset2)
  (let ((previous-char-constituent-p nil))
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

(defmethod capitalize-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (capitalize-buffer-region (buffer mark1) offset1 offset2)))

(defmethod capitalize-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (capitalize-buffer-region (buffer mark2) offset1 offset2)))

(defmethod capitalize-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (capitalize-buffer-region (buffer mark1) offset1 offset2)))

(defun capitalize-word (mark &optional (n 1))
  "Capitalize the next N words, leaving mark after the last word."
  (loop repeat n
        do (forward-to-word-boundary mark)
           (let ((offset (offset mark)))
             (forward-word mark)
             (capitalize-region offset mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Tabify

(defun tabify-buffer-region (buffer offset1 offset2 tab-width)
  (flet ((looking-at-spaces (buffer offset count)
           (loop for i from offset
                 repeat count
                 unless (char= (buffer-object buffer i) #\Space)
                 return nil
                 finally (return t))))
    (loop for offset = offset1 then (1+ offset)
          until (>= offset offset2)
          do (let* ((column (buffer-display-column
                             buffer offset tab-width))
                    (count (- tab-width (mod column tab-width))))
               (when (looking-at-spaces buffer offset count)
                 (finish-output)
                 (delete-buffer-range buffer offset count)
                 (insert-buffer-object buffer offset #\Tab)
                 (decf offset2 (1- count)))))))

(defgeneric tabify-region (mark1 mark2 tab-width)
  (:documentation "Replace sequences of tab-width spaces with tabs
in the region delimited by mark1 and mark2."))

(defmethod tabify-region ((mark1 mark) (mark2 mark) tab-width)
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (tabify-buffer-region (buffer mark1) offset1 offset2 tab-width)))

(defmethod tabify-region ((offset1 integer) (mark2 mark) tab-width)
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (tabify-buffer-region (buffer mark2) offset1 offset2 tab-width)))

(defmethod tabify-region ((mark1 mark) (offset2 integer) tab-width)
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (tabify-buffer-region (buffer mark1) offset1 offset2 tab-width)))

(defun untabify-buffer-region (buffer offset1 offset2 tab-width)
  (loop for offset = offset1 then (1+ offset)
        until (>= offset offset2)
        when (char= (buffer-object buffer offset) #\Tab)
        do (let* ((column (buffer-display-column buffer
                                                 offset
                                                 tab-width))
                  (count (- tab-width (mod column tab-width))))
             (delete-buffer-range buffer offset 1)
             (loop repeat count
                   do (insert-buffer-object buffer offset #\Space))
             (incf offset (1- count))
             (incf offset2 (1- count)))))

(defgeneric untabify-region (mark1 mark2 tab-width)
  (:documentation "Replace tabs with tab-width spaces in the region
delimited by mark1 and mark2."))

(defmethod untabify-region ((mark1 mark) (mark2 mark) tab-width)
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (untabify-buffer-region (buffer mark1) offset1 offset2 tab-width)))

(defmethod untabify-region ((offset1 integer) (mark2 mark) tab-width)
  (let ((offset2 (offset mark2)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (untabify-buffer-region (buffer mark2) offset1 offset2 tab-width)))

(defmethod untabify-region ((mark1 mark) (offset2 integer) tab-width)
  (let ((offset1 (offset mark1)))
    (when (> offset1 offset2)
      (rotatef offset1 offset2))
    (untabify-buffer-region (buffer mark1) offset1 offset2 tab-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Indentation

(defgeneric indent-line (mark indentation tab-width)
  (:documentation "Indent the line containing mark with indentation
spaces. Use tabs and spaces if tab-width is not nil, otherwise use
spaces only."))

(defun indent-line* (mark indentation tab-width left)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop until (end-of-buffer-p mark2)
       as object = (object-after mark2)
       while (or (eql object #\Space) (eql object #\Tab))
       do (delete-range mark2 1))
    (loop until (zerop indentation)
       do (cond ((and tab-width (>= indentation tab-width))
		 (insert-object mark2 #\Tab)
		 (when left ; spaces must follow tabs
		   (forward-object mark2))
		 (decf indentation tab-width))
		(t
		 (insert-object mark2 #\Space)
		 (decf indentation))))))

(defmethod indent-line ((mark left-sticky-mark) indentation tab-width)
  (indent-line* mark indentation tab-width t))

(defmethod indent-line ((mark right-sticky-mark) indentation tab-width)
  (indent-line* mark indentation tab-width nil))

(defun delete-indentation (mark)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (delete-range mark -1)
    (loop until (end-of-buffer-p mark)
          while (whitespacep (object-after mark))
          do (delete-range mark 1))
    (loop until (beginning-of-buffer-p mark)
          while (whitespacep (object-before mark))
          do (delete-range mark -1))
    (when (and (not (beginning-of-buffer-p mark))
	       (constituentp (object-before mark)))
      (insert-object mark #\Space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Auto fill

(defun fill-line (mark syntax-line-indentation-function fill-column tab-width
		  &optional (compress-whitespaces t))
  "Breaks the contents of line pointed to by MARK up to MARK into
multiple lines such that none of them is longer than FILL-COLUMN. If
COMPRESS-WHITESPACES is non-nil, whitespaces are compressed after the
decision is made to break the line at a point. For now, the
compression means just the deletion of trailing whitespaces."
  (let ((begin-mark (clone-mark mark)))
    (beginning-of-line begin-mark)
    (loop with column = 0
          with line-beginning-offset = (offset begin-mark)
          with walking-mark = (clone-mark begin-mark)
          while (mark< walking-mark mark)
          as object = (object-after walking-mark)
          do (case object
               (#\Space
                (setf (offset begin-mark) (offset walking-mark))
                (incf column))
               (#\Tab
                (setf (offset begin-mark) (offset walking-mark))
                (incf column (- tab-width (mod column tab-width))))
               (t
                (incf column)))
             (when (and (>= column fill-column)
			(/= (offset begin-mark) line-beginning-offset))
	       (when compress-whitespaces
		 (let ((offset (buffer-search-backward
				(buffer begin-mark)
				(offset begin-mark)
				#(nil)
				:test #'(lambda (o1 o2)
					  (declare (ignore o2))
					  (not (whitespacep o1))))))
		   (when offset
		     (delete-region begin-mark (1+ offset)))))
               (insert-object begin-mark #\Newline)
               (incf (offset begin-mark))
               (let ((indentation
                      (funcall syntax-line-indentation-function begin-mark)))
                 (indent-line begin-mark indentation tab-width))
               (beginning-of-line begin-mark)
               (setf line-beginning-offset (offset begin-mark))
               (setf (offset walking-mark) (offset begin-mark))
               (setf column 0))
             (incf (offset walking-mark)))))

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

(defun non-greedy-match-forward (a buffer i)
  (let ((p (automaton::initial a)))
    (loop for j from i below (size buffer)
       for q = (automaton::sstep p (buffer-object buffer j)) do
	 (unless q
	   (return nil))
	 (if (automaton::accept q)
	     (return (1+ j))
	     (setq p q))
       finally (return nil))))

(defun buffer-re-search-forward (a buffer offset)
  "Returns as the first value the smallest offset of BUFFER >= OFFSET
with contents accepted by deterministic automaton A; otherwise,
returns nil. If the first value is non-nil, the second value is the
offset after the matched contents."
  (if (automaton::singleton a)
      (buffer-search-forward buffer offset (automaton::singleton a))
      (loop for i from offset below (size buffer) do
	 (let ((j (non-greedy-match-forward a buffer i)))
	   (when j (return (values i j))))
	 finally (return nil))))

(defun reversed-deterministic-automaton (a)
  "Reverses and determinizes A, then returns it."
  (if (automaton::singleton a)
      (progn
	(setf (automaton::singleton a) (reverse (automaton::singleton a)))
	a)
      (automaton::determinize2
       a
       (make-instance 'automaton::state-set :ht (automaton::areverse a)))))

(defun non-greedy-match-backward (a buffer i)
  (let ((p (automaton::initial a)))
    (loop for j downfrom i to 0
       for q = (automaton::sstep p (buffer-object buffer j)) do
	 (unless q
	   (return nil))
	 (if (automaton::accept q)
	     (return j)
	     (setq p q))
       finally (return nil))))

(defun buffer-re-search-backward (a buffer offset)
  "Returns as the first value the largest offset of BUFFER <= OFFSET
with contents accepted by (reversed) deterministic automaton A;
otherwise, returns nil. If the first value is non-nil, the second
value is the offset after the matched contents."
  (if (automaton::singleton a)
      (buffer-search-backward buffer offset (automaton::singleton a))
      (loop for i downfrom (min offset (1- (size buffer))) to 0 do
	 (let ((j (non-greedy-match-backward a buffer i)))
	   (when j (return (values j i))))
	 finally (return nil))))

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

(defun re-search-forward (mark re)
  "move MARK forward after the first occurence of string matching RE
after MARK"
  (let ((a (automaton::determinize
	     (automaton::regexp-automaton
	      (automaton::string-regexp re)))))
    (multiple-value-bind (i j)
	(buffer-re-search-forward a (buffer mark) (offset mark))
      (when i
	(setf (offset mark) j)))))

(defun re-search-backward (mark re)
  "move MARK backward before the first occurence of string matching RE
before MARK"
  (let ((a (reversed-deterministic-automaton
	    (automaton::regexp-automaton
	     (automaton::string-regexp re)))))
    (multiple-value-bind (i j)
	(buffer-re-search-backward a (buffer mark) (offset mark))
      (declare (ignorable j))
    (when i
      (setf (offset mark) i)))))

(defun buffer-search-word-backward (buffer offset word &key (test #'eql))
  "return the largest offset of BUFFER <= (- OFFSET (length WORD))
containing WORD as a word or NIL if no such offset exists"
  (let ((wlen (length word))
	(blen (size buffer)))
    (loop
       for i downfrom (- offset wlen) to 0
       for j = (+ i wlen)
       when (and (or (zerop i) (whitespacep (buffer-object buffer (1- i))))
		 (buffer-looking-at buffer i word :test test)
		 (not (and (< (+ i wlen) blen)
			   (constituentp (buffer-object buffer (+ i wlen))))))
         return i
       finally (return nil))))

(defun buffer-search-word-forward (buffer offset word &key (test #'eql))
  "Return the smallest offset of BUFFER >= OFFSET containing WORD as a
word or NIL if no such offset exists"
  (let ((wlen (length word))
	(blen (size buffer)))
    (loop
       for i upfrom offset to (- blen (max wlen 1))
       for j = (+ i wlen)
       when (and (or (zerop i) (whitespacep (buffer-object buffer (1- i))))
		 (buffer-looking-at buffer i word :test test)
		 (not (and (< j blen)
			   (constituentp (buffer-object buffer j)))))
         return i
       finally (return nil))))
