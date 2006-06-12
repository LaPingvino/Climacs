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

(defmacro do-buffer-region-lines ((line-var mark1 mark2) &body body)
  "Iterate over the lines in the region delimited by `mark1' and `mark2'.
   For each line, `line-var' will be bound to a mark positioned
   at the beginning of the line and `body' will be executed. Note
   that the iteration will always start from the mark specifying
   the earliest position in the buffer."
  (let ((mark-sym (gensym))
        (mark2-sym (gensym)))
    `(progn
       (let* ((,mark-sym (clone-mark ,mark1))
              (,mark2-sym (clone-mark ,mark2)))
         (when (mark< ,mark2-sym ,mark-sym)
           (rotatef ,mark-sym ,mark2-sym))
         (loop while (and (mark<= ,mark-sym ,mark2-sym)
                          (not (end-of-buffer-p ,mark-sym)))
            do              
            (let ((,line-var (clone-mark ,mark-sym)))
              ,@body)
            (end-of-line ,mark-sym)
            (unless (end-of-buffer-p ,mark-sym)
              (forward-object ,mark-sym)))))))

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
      (let ((result (buffer-search-forward
		     buffer offset (automaton::singleton a))))
	(when result
	  (values result (+ result (length (automaton::singleton a))))))
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
      (let ((result (buffer-search-backward
		     buffer offset (nreverse (automaton::singleton a)))))
	(when result
	  (values result (+ result (length (automaton::singleton a))))))
      (loop for i downfrom (min offset (1- (size buffer))) to 0 do
	(let ((j (non-greedy-match-backward a buffer i)))
	  (when j (return (values j (1+ i)))))
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
	(setf (offset mark) j)
	(values mark i)))))

(defun re-search-backward (mark re)
  "move MARK backward before the first occurence of string matching RE
before MARK"
  (let ((a (reversed-deterministic-automaton
	    (automaton::regexp-automaton
	     (automaton::string-regexp re)))))
    (multiple-value-bind (i j)
	(buffer-re-search-backward a (buffer mark) (1- (offset mark)))
      (declare (ignorable j))
    (when i
      (setf (offset mark) i)
      (values mark j)))))

(defun buffer-search-word-backward (buffer offset word &key (test #'eql))
  "return the largest offset of BUFFER <= (- OFFSET (length WORD))
containing WORD as a word or NIL if no such offset exists"
  (let ((wlen (length word))
	(blen (size buffer)))
    (loop
       for i downfrom (- offset wlen) to 0
       for j = (+ i wlen)
       when (and (or (zerop i) (whitespacep t (buffer-object buffer (1- i))))
		 (buffer-looking-at buffer i word :test test)
		 (not (and (< (+ i wlen) blen)
			   (constituentp (buffer-object buffer (+ i wlen))))))
         return i
       finally (return nil))))

(defun search-word-backward (mark word)
  (let ((offset (buffer-search-word-backward (buffer mark) (offset mark) word)))
    (when offset
      (setf (offset mark) offset))))

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
	 ;; should this be (+ i wlen)? jqs 2006-05-14
         return i
       finally (return nil))))

(defun search-word-forward (mark word)
  (let ((wlen (length word))
	(offset (buffer-search-word-forward (buffer mark) (offset mark) word)))
    (when offset
      (setf (offset mark) (+ offset wlen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Kill ring

(defvar *kill-ring* (make-instance 'kill-ring :max-size 7))