(defpackage :climacs-buffer
  (:use :clim-lisp)
  (:export #:buffer #:standard-buffer
	   #:mark #:left-sticky-mark #:right-sticky-mark
	   #:standard-left-sticky-mark #:standard-right-sticky-mark
	   #:clone-mark #:no-such-offset #:size #:number-of-lines
	   #:offset #:mark< #:mark<= #:mark= #:mark> #:mark>=
	   #:beginning-of-buffer #:end-of-buffer
	   #:beginning-of-buffer-p #:end-of-buffer-p
	   #:beginning-of-line #:end-of-line
	   #:beginning-of-line-p #:end-of-line-p
	   #:line-number #:column-number
	   #:insert-buffer-text #:insert-text
	   #:delete-buffer-text #:delete-text
	   #:delete-region
	   #:buffer-char
	   #:char-before #:char-after #:region-to-string
	   #:find-text-after #:find-text-before))

(in-package :climacs-buffer)

(defclass buffer () ())

(defclass standard-buffer (buffer)
  ((text :initform (list nil))
   (marks :initform '())))

(defgeneric buffer (mark))  

(defclass mark () ())

(defclass left-sticky-mark (mark) ())

(defclass right-sticky-mark (mark) ())

(defclass mark-mixin ()
  ((buffer :initarg :buffer :reader buffer)
   (offset :initarg :offset :initform 0 :accessor offset)))

(defmethod initialize-instance :after ((mark mark-mixin) &rest args)
  (declare (ignore args))
  (push mark (slot-value (buffer mark) 'marks)))

(defclass standard-left-sticky-mark (left-sticky-mark mark-mixin) ())

(defclass standard-right-sticky-mark (right-sticky-mark mark-mixin) ())

(defgeneric clone-mark (mark &optional type))

(defmethod clone-mark ((mark standard-left-sticky-mark) &optional type)
  (unless type
    (setf type 'standard-left-sticky-mark))
  (make-instance 'type :buffer (buffer mark) :offset (offset mark)))

(defmethod clone-mark ((mark standard-right-sticky-mark) &optional type)
  (unless type
    (setf type 'standard-right-sticky-mark))
  (make-instance 'type :buffer (buffer mark) :offset (offset mark)))

(define-condition no-such-offset (simple-error)
  ((offset :reader offset :initarg :offset))
  (:report (lambda (condition stream)
	     (format stream "No such offset: ~a" (offset condition)))))

(defgeneric size (buffer))

(defmethod size ((buffer standard-buffer))
  (1- (length (slot-value buffer 'text))))


(defgeneric number-of-lines (buffer))

(defmethod number-of-lines ((buffer standard-buffer))
  (count #\Newline (cdr (slot-value buffer 'text))))

(defgeneric mark< (mark1 mark2))

(defmethod mark< ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (< (offset mark1) (offset mark2)))

(defmethod mark< ((mark1 mark-mixin) (mark2 integer))
  (< (offset mark1) mark2))

(defmethod mark< ((mark1 integer) (mark2 mark-mixin))
  (< mark1 (offset mark2)))

(defgeneric mark<= (mark1 mark2))

(defmethod mark<= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (<= (offset mark1) (offset mark2)))

(defmethod mark<= ((mark1 mark-mixin) (mark2 integer))
  (<= (offset mark1) mark2))

(defmethod mark<= ((mark1 integer) (mark2 mark-mixin))
  (<= mark1 (offset mark2)))

(defgeneric mark= (mark1 mark2))

(defmethod mark= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (= (offset mark1) (offset mark2)))

(defmethod mark= ((mark1 mark-mixin) (mark2 integer))
  (= (offset mark1) mark2))

(defmethod mark= ((mark1 integer) (mark2 mark-mixin))
  (= mark1 (offset mark2)))

(defgeneric mark> (mark1 mark2))

(defmethod mark> ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (> (offset mark1) (offset mark2)))

(defmethod mark> ((mark1 mark-mixin) (mark2 integer))
  (> (offset mark1) mark2))

(defmethod mark> ((mark1 integer) (mark2 mark-mixin))
  (> mark1 (offset mark2)))

(defgeneric mark>= (mark1 mark2))

(defmethod mark>= ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (>= (offset mark1) (offset mark2)))

(defmethod mark>= ((mark1 mark-mixin) (mark2 integer))
  (>= (offset mark1) mark2))

(defmethod mark>= ((mark1 integer) (mark2 mark-mixin))
  (>= mark1 (offset mark2)))


(defgeneric beginning-of-buffer (mark))

(defmethod beginning-of-buffer ((mark mark-mixin))
  (setf (offset mark) 0))

(defgeneric end-of-buffer (mark))

(defmethod end-of-buffer ((mark mark-mixin))
  (setf (offset mark) (size (buffer mark))))

(defgeneric beginning-of-buffer-p (mark))

(defmethod beginning-of-buffer-p ((mark mark-mixin))
  (zerop (offset mark)))

(defgeneric end-of-buffer-p (mark))

(defmethod end-of-buffer-p ((mark mark-mixin))
  (= (offset mark) (size (buffer mark))))

(defgeneric beginning-of-line (mark))

(defmethod beginning-of-line ((mark mark-mixin))
  (loop until (or (beginning-of-buffer-p mark)
		  (char= (char-before mark) #\Newline))
	do (decf (offset mark))))

(defgeneric end-of-line (mark))

(defmethod end-of-line ((mark mark-mixin))
  (loop until (or (end-of-buffer-p mark)
		  (char= (char-after mark) #\Newline))
	do (incf (offset mark))))

(defgeneric beginning-of-line-p (mark))

(defmethod beginning-of-line-p ((mark mark-mixin))
  (or (beginning-of-buffer-p mark)
      (char= (char-before mark) #\Newline)))

(defgeneric end-of-line-p (mark))

(defmethod end-of-line-p ((mark mark-mixin))
  (or (end-of-buffer-p mark)
      (char= (char-after mark) #\Newline)))

(defgeneric line-number (mark))

(defmethod line-number ((mark mark-mixin))
  (count #\Newline (cdr (slot-value (buffer mark) 'text))
	 :end (offset mark)))

(defgeneric column-number (mark))

(defmethod column-number ((mark mark-mixin))
  (loop for offset downfrom (offset mark)
	while (> offset 0)
	until (char= (buffer-char (buffer mark) (1- offset)) #\Newline)
	count t))

(defgeneric insert-buffer-text (buffer offset string))

(defmethod insert-buffer-text ((buffer standard-buffer) offset (char character))
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (push char (cdr (nthcdr offset (slot-value buffer 'text))))
  (loop for mark in (slot-value buffer 'marks)
	when (or (> (offset mark) offset)
		 (and (= (offset mark) offset)
		      (typep mark 'right-sticky-mark)))
	  do (incf (offset mark))))
      
(defmethod insert-buffer-text ((buffer standard-buffer) offset (string string))
  (loop for elem across string
	do (insert-buffer-text buffer offset elem)))


(defgeneric insert-text (mark string))

(defmethod insert-text ((mark mark-mixin) string)
  (insert-buffer-text (buffer mark) (offset mark) string))

(defgeneric delete-buffer-text (buffer offset n))

(defmethod delete-buffer-text ((buffer standard-buffer) offset n)
  (assert (<= 0 offset (size buffer)) ()
	  (make-condition 'no-such-offset :offset offset))
  (with-slots (text marks) buffer
     (setf (cdr (nthcdr offset text)) (nthcdr (+ offset n 1) text))
     (loop for mark in marks
	   when (> (offset mark) offset)
	     do (setf (offset mark) (max offset (- (offset mark) n))))))

(defgeneric delete-text (mark &optional n))

(defmethod delete-text ((mark mark-mixin) &optional (n 1))
  (cond ((plusp n) (delete-buffer-text (buffer mark) (offset mark) n))
	((minusp n) (delete-buffer-text (buffer mark) (+ (offset mark) n) (- n)))
	(t nil)))

(defgeneric delete-region (mark1 mark2))

(defmethod delete-region ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (when (> (offset mark1) (offset mark2))
    (delete-buffer-text (buffer mark1)
			(offset mark1)
			(- (offset mark2) (offset mark1)))))

(defmethod delete-region ((mark1 mark-mixin) offset2)
  (when (> offset2 (offset mark1))
    (delete-buffer-text (buffer mark1)
			(offset mark1)
			(- offset2 (offset mark1)))))

(defmethod delete-region (offset1 (mark2 mark-mixin))
  (when (> (offset mark2) offset1)
    (delete-buffer-text (buffer mark2)
			offset1
			(- (offset mark2) offset1))))

(defgeneric buffer-char (buffer offset))

(defmethod buffer-char ((buffer standard-buffer) offset)
  (assert (<= 0 offset (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset))
  (nth (1+ offset) (slot-value buffer 'text)))

(defgeneric buffer-string (buffer offset1 offset2))

(defmethod buffer-string ((buffer standard-buffer) offset1 offset2)
  (assert (<= 0 offset1 (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset1))
  (assert (<= 0 offset2 (1- (size buffer))) ()
	  (make-condition 'no-such-offset :offset offset2))
  (coerce (subseq (slot-value buffer 'text) (1+ offset1) (1+ offset2)) 'string))

(defgeneric char-before (mark))

(defmethod char-before ((mark mark-mixin))
  (buffer-char (buffer mark) (1- (offset mark))))

(defgeneric char-after (mark))

(defmethod char-after ((mark mark-mixin))
  (buffer-char (buffer mark) (offset mark)))

(defgeneric region-to-string (mark1 mark2))

(defmethod region-to-string ((mark1 mark-mixin) (mark2 mark-mixin))
  (assert (eq (buffer mark1) (buffer mark2)))
  (buffer-string (buffer mark1) (offset mark1) (offset mark2)))


