;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(cl:defpackage :climacs-tests
  (:use :cl :rtest :climacs-buffer :climacs-base))

(cl:in-package :climacs-tests)

(deftest standard-buffer-make-instance.test-1
  (let* ((buffer (make-instance 'standard-buffer))
	 (low (slot-value buffer 'low-mark))
	 (high (slot-value buffer 'high-mark)))
    (and (= (offset low) 0)
	 (= (offset high) 0)
	 (null (modified-p buffer))
	 (eq (buffer low) buffer)
	 (eq (buffer high) buffer)))
  t)

(deftest standard-buffer-mark-make-instance.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(make-instance 'standard-left-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest standard-buffer-mark-make-instance.test-2
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(make-instance 'standard-right-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest standard-buffer-clone-mark.test-1
  (flet ((%all-eq (&optional x y)
	   (cond
	     ((null x) nil)
	     (t (when (eq x y) y)))))
    (let* ((buffer (make-instance 'standard-buffer))
	   (low (slot-value buffer 'low-mark))
	   (high (slot-value buffer 'high-mark))
	   (low2 (clone-mark low))
	   (high2 (clone-mark high))
	   (low3 (clone-mark high 'standard-left-sticky-mark))
	   (high3 (clone-mark low 'standard-right-sticky-mark)))
      (and (reduce #'%all-eq
		  (list (class-of low) (class-of low2) (class-of low3)))
	   (reduce #'%all-eq
		  (list (class-of high) (class-of high2) (class-of high3)))
	   (= (offset low) (offset low2) (offset low3)
	      (offset high) (offset high2) (offset high3) 0))))
  t)

;;; NOTE: the current implementation uses vectors wherever sequences are
;;; expected (and strings are vectors of characters)

(deftest standard-buffer-insert-buffer-object.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-object buffer 0 #\a)
    (and (= (size buffer) 1) (buffer-sequence buffer 0 1)))
  "a")

(deftest standard-buffer-insert-buffer-object.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 0 #\a)
    (and (= (size buffer) 2) (buffer-sequence buffer 0 2)))
  "ab")

(deftest standard-buffer-insert-buffer-object.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 1 #\a)
    (and (= (size buffer) 2) (buffer-sequence buffer 0 2)))
  "ba")

(deftest standard-buffer-insert-buffer-object.test-4
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-object buffer 1 #\a))
    (error (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest standard-buffer-insert-buffer-object.test-5
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-object buffer -1 #\a))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-insert-buffer-sequence.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(deftest standard-buffer-insert-buffer-sequence.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 3 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "cliClimacSmacs")

(deftest standard-buffer-insert-buffer-sequence.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 0 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "ClimacSclimacs")

(deftest standard-buffer-insert-buffer-sequence.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "")
    (and (= (size buffer) 0) (buffer-sequence buffer 0 0)))
  "")

(deftest standard-buffer-insert-buffer-sequence.test-5
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest standard-buffer-insert-buffer-sequence.test-6
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer -1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-delete-buffer-range.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 7)
    (size buffer))
  0)

(deftest standard-buffer-delete-buffer-range.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 3)
    (and (= (size buffer) 4) (buffer-sequence buffer 0 4)))
  "macs")

(deftest standard-buffer-delete-buffer-range.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 4)
    (and (= (size buffer) 3) (buffer-sequence buffer 0 3)))
  "cli")

(deftest standard-buffer-delete-buffer-range.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 0)
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(deftest standard-buffer-delete-buffer-range.test-5
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-delete-buffer-range.test-6
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer 6 2))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(deftest standard-buffer-insert-object.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(deftest standard-buffer-insert-object.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 4)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(deftest standard-buffer-insert-sequence.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(deftest standard-buffer-insert-sequence.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 10)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(deftest standard-buffer-delete-range.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m 2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest standard-buffer-delete-range.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m -2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 1)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "cmacs")

(deftest standard-buffer-delete-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest standard-buffer-delete-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest standard-buffer-delete-region.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest standard-buffer-delete-region.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest standard-buffer-delete-region.test-5
  (handler-case
      (let ((buffer (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m (make-instance 'standard-right-sticky-mark
				:buffer buffer :offset 3))
	      (m2 (make-instance 'standard-right-sticky-mark
				 :buffer buffer2 :offset 5)))
	  (delete-region m2 m)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest standard-buffer-delete-region.test-6
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m 5)
      (delete-region 1 m2)
      (and (= (size buffer) 3)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 1)
	   (= (offset m2) 1)
	   (buffer-sequence buffer 0 3))))
  "ccs")

(deftest standard-buffer-number-of-lines.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (number-of-lines buffer))
  0)

(deftest standard-buffer-number-of-lines.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
")
    (number-of-lines buffer))
  2)

(deftest standard-buffer-mark-relations.test-1
  (let ((buffer (make-instance 'standard-buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (let ((m0 (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 0))
	    (m1 (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 3))
	    (m1a (make-instance 'standard-right-sticky-mark
				:buffer buffer :offset 3))
	    (m2 (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 4))
	    (m2a (make-instance 'standard-left-sticky-mark
				:buffer buffer :offset 5))
	    (m3 (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 7)))
	(setf (offset m2) 5)
	(and (mark< m0 m1) (not (mark> m0 m1)) (not (mark>= m0 m1))
	     (mark< m0 m2) (not (mark> m0 m2)) (not (mark>= m0 m2))
	     (mark< m0 m3) (not (mark> m0 m3)) (not (mark>= m0 m3))
	     (mark< m1 m2) (not (mark> m1 m2)) (not (mark>= m1 m2))
	     (mark< m1 m3) (not (mark> m1 m3)) (not (mark>= m1 m3))
	     (mark< m2 m3) (not (mark> m2 m3)) (not (mark>= m2 m3))
	     (mark<= m1 m1a) (not (mark> m1 m1a))
	     (mark>= m1 m1a) (not (mark< m1 m1a))
	     (mark> m3 m2) (not (mark< m3 m2)) (not (mark<= m3 m2))
	     (mark> m3 m1) (not (mark< m3 m1)) (not (mark<= m3 m1))
	     (mark> m3 m0) (not (mark< m3 m0)) (not (mark<= m3 m0))
	     (mark> m2 m1) (not (mark< m2 m1)) (not (mark<= m2 m1))
	     (mark> m2 m0) (not (mark< m2 m0)) (not (mark<= m2 m0))
	     (mark>= m2 m2a) (not (mark> m2 m2a))
	     (mark>= m2 m2a) (not (mark< m2 m2a))
	     (mark= m1 m1a)
	     (mark= m2 m2a)
	     (beginning-of-buffer-p m0) (not (beginning-of-buffer-p m3))
	     (end-of-buffer-p m3) (not (end-of-buffer-p m0))
	     (beginning-of-line-p m0) (not (beginning-of-line-p m3))
	     (end-of-line-p m3) (not (end-of-line-p m0))
	     (every #'(lambda (m) (zerop (line-number m)))
		    (list m0 m1 m1a m2 m2a m3)))))
  t)

(deftest standard-buffer-setf-offset.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance 'standard-left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) -1)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-setf-offset.test-2
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance 'standard-left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) 8)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(deftest standard-buffer-mark<.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'standard-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'standard-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark< m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest standard-buffer-mark>.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'standard-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'standard-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark> m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest standard-buffer-mark<=.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'standard-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'standard-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark<= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest standard-buffer-mark>=.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'standard-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'standard-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark>= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest standard-buffer-mark=.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'standard-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'standard-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest standard-buffer-line-number.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 0 (line-number m1) (1- (line-number m2)))))
  t)

(deftest standard-buffer-column-number.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 3 (column-number m1) (column-number m2))))
  t)

(deftest standard-buffer-beginning-of-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-line-p m))
	   (progn (beginning-of-line m) (beginning-of-line-p m)))))
  t)

(deftest standard-buffer-end-of-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-line-p m))
	   (progn (end-of-line m) (end-of-line-p m)))))
  t)

(deftest standard-buffer-beginning-of-buffer.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-buffer-p m))
	   (progn (beginning-of-buffer m) (beginning-of-buffer-p m)))))
  t)

(deftest standard-buffer-end-of-buffer.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-buffer-p m))
	   (progn (end-of-buffer m) (end-of-buffer-p m)))))
  t)

(deftest standard-buffer-buffer-object.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-object buffer 3))
  #\m)

(deftest standard-buffer-buffer-object.test-2
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer -1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-buffer-object.test-3
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer 7))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(deftest standard-buffer-buffer-sequence.test-1
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(buffer-sequence buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-buffer-sequence.test-2
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(buffer-sequence buffer 0 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest standard-buffer-buffer-sequence.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-sequence buffer 5 3))
  #())

(deftest standard-buffer-object-before.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-before (high-mark buffer)))
  #\s)

(deftest standard-buffer-object-before.test-2
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-before (low-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest standard-buffer-object-after.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-after (low-mark buffer)))
  #\c)

(deftest standard-buffer-object-after.test-2
  (handler-case
      (let ((buffer (make-instance 'standard-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-after (high-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(deftest standard-buffer-region-to-sequence.test-1
  (let ((seq "climacs")
	(buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest standard-buffer-region-to-sequence.test-1a
  (let ((seq "climacs")
	(buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence 0 (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest standard-buffer-region-to-sequence.test-1b
  (let ((seq "climacs")
	(buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) 7)))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest standard-buffer-region-to-sequence.test-2
  (let ((seq "climacs")
	(buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (region-to-sequence (high-mark buffer) (low-mark buffer)))
  #())

(deftest standard-buffer-region-to-sequence.test-3
  (handler-case
      (let ((buffer1 (make-instance 'standard-buffer))
	    (buffer2 (make-instance 'standard-buffer)))
	(region-to-sequence (low-mark buffer1) (high-mark buffer2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)


;;;; performance tests

(defmacro deftimetest (name form &rest results)
  `(deftest ,name
     (time
      (progn
	(format t "~&; Performance test ~a" ',name)
	,form))
     ,@results))

(deftimetest standard-buffer-performance.test-1
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b 0 #\a)
     finally (return (size b)))
  100000)

(deftimetest standard-buffer-performance.test-1a
  (let ((b (loop with b = (make-instance 'standard-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b 0 #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest standard-buffer-performance.test-1b
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b (size b) #\a)
     finally (return (size b)))
  100000)

(deftimetest standard-buffer-performance.test-1ba
  (let ((b (loop with b = (make-instance 'standard-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (size b) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest standard-buffer-performance.test-1c
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b (floor (size b) 2) #\a)
     finally (return (size b)))
  100000)

(deftimetest standard-buffer-performance.test-1ca
  (let ((b (loop with b = (make-instance 'standard-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (floor (size b) 2) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest standard-buffer-performance.test-1cb
  (let ((b (loop with b = (make-instance 'standard-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (floor (size b) 2) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b (floor (size b) 2) 1)
       finally (return (size b))))
  0)

(deftimetest standard-buffer-performance.test-2
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b 0 "a")
     finally (return (size b)))
  100000)

(deftimetest standard-buffer-performance.test-2b
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (size b) "a")
     finally (return (size b)))
  100000)

(deftimetest standard-buffer-performance.test-2c
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (floor (size b) 2) "a")
     finally (return (size b)))
  100000)

(deftimetest standard-buffer-performance.test-3
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b 0 "abcdefghij")
     finally (return (size b)))
  1000000)

(deftimetest standard-buffer-performance.test-3b
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (size b) "abcdefghij")
     finally (return (size b)))
  1000000)

(deftimetest standard-buffer-performance.test-3c
  (loop with b = (make-instance 'standard-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (floor (size b) 2) "abcdefghij")
     finally (return (size b)))
  1000000)