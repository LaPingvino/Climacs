;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(cl:defpackage :climacs-tests
  (:use :cl :rtest :climacs-buffer :climacs-base))

(cl:in-package :climacs-tests)

(defmacro defmultitest (name form &rest results)
  (let ((name-string (symbol-name name)))
    (flet ((%deftest-wrapper (bc lsm rsm tn f rs)
	     (let ((alist (list (cons '%%buffer bc)
				(cons '%%left-sticky-mark lsm)
				(cons '%%right-sticky-mark rsm))))
	       `(deftest ,tn
		  ,(sublis alist f)
		  ,@(mapcar (lambda (r) (sublis alist r)) rs)))))
      `(progn
	 ,(%deftest-wrapper
	   ''standard-buffer
	   ''standard-left-sticky-mark
	   ''standard-right-sticky-mark
	   (intern (concatenate 'string "STANDARD-BUFFER-" name-string))
	   form
	   results)
	 ,(%deftest-wrapper
	   ''binseq-buffer
	   ''persistent-left-sticky-mark
	   ''persistent-right-sticky-mark
	   (intern (concatenate 'string "BINSEQ-BUFFER-" name-string))
	   form
	   results)
	 ,(%deftest-wrapper
	   ''obinseq-buffer
	   ''persistent-left-sticky-mark
	   ''persistent-right-sticky-mark
	   (intern (concatenate 'string "OBINSEQ-BUFFER-" name-string))
	   form
	   results)))))

(defmultitest buffer-make-instance.test-1
  (let* ((buffer (make-instance %%buffer))
	 (low (slot-value buffer 'low-mark))
	 (high (slot-value buffer 'high-mark)))
    (and (= (offset low) 0)
	 (= (offset high) 0)
	 (null (modified-p buffer))
	 (eq (buffer low) buffer)
	 (eq (buffer high) buffer)))
  t)

(defmultitest mark-make-instance.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(make-instance %%left-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(defmultitest mark-make-instance.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(make-instance %%right-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(defmultitest clone-mark.test-1
  (flet ((%all-eq (&optional x y)
	   (cond
	     ((null x) nil)
	     (t (when (eq x y) y)))))
    (let* ((buffer (make-instance %%buffer))
	   (low (slot-value buffer 'low-mark))
	   (high (slot-value buffer 'high-mark))
	   (low2 (clone-mark low))
	   (high2 (clone-mark high))
	   (low3 (clone-mark high :left))
	   (high3 (clone-mark low :right)))
      (and (reduce #'%all-eq
		  (list (class-of low) (class-of low2) (class-of low3)))
	   (reduce #'%all-eq
		  (list (class-of high) (class-of high2) (class-of high3)))
	   (= (offset low) (offset low2) (offset low3)
	      (offset high) (offset high2) (offset high3) 0))))
  t)

;;; NOTE: the current implementation uses vectors wherever sequences are
;;; expected (and strings are vectors of characters)

(defmultitest insert-buffer-object.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\a)
    (values
     (offset (low-mark buffer))
     (offset (high-mark buffer))
     (modified-p buffer)
     (size buffer)
     (buffer-sequence buffer 0 1)))
  0 1 t 1 "a")

(defmultitest insert-buffer-object.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 0 #\a)
    (values
     (offset (low-mark buffer))
     (offset (high-mark buffer))
     (modified-p buffer)
     (size buffer)
     (buffer-sequence buffer 0 2)))
  0 2 t 2 "ab")

(defmultitest insert-buffer-object.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 1 #\a)
    (values
     (offset (low-mark buffer))
     (offset (high-mark buffer))
     (modified-p buffer)
     (size buffer)
     (buffer-sequence buffer 0 2)))
  0 2 t 2 "ba")

(defmultitest insert-buffer-object.test-4
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-object buffer 1 #\a))
    (error (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(defmultitest insert-buffer-object.test-5
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-object buffer -1 #\a))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest insert-buffer-sequence.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(defmultitest insert-buffer-sequence.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 3 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "cliClimacSmacs")

(defmultitest insert-buffer-sequence.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 0 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "ClimacSclimacs")

(defmultitest insert-buffer-sequence.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "")
    (and (= (size buffer) 0) (buffer-sequence buffer 0 0)))
  "")

(defmultitest insert-buffer-sequence.test-5
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(defmultitest insert-buffer-sequence.test-6
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer -1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest delete-buffer-range.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 7)
    (values
     (offset (low-mark buffer))
     (offset (high-mark buffer))
     (modified-p buffer)
     (size buffer)))
  0 0 t 0)

(defmultitest delete-buffer-range.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 3)
    (values
     (offset (low-mark buffer))
     (offset (high-mark buffer))
     (modified-p buffer)
     (size buffer)
     (buffer-sequence buffer 0 4)))
  0 4 t 4 "macs")

(defmultitest delete-buffer-range.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 4)
    (and (= (size buffer) 3) (buffer-sequence buffer 0 3)))
  "cli")

(defmultitest delete-buffer-range.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 0)
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(defmultitest delete-buffer-range.test-5
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest delete-buffer-range.test-6
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer 6 2))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(defmultitest insert-object.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(defmultitest insert-object.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 4)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(defmultitest insert-sequence.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%left-sticky-mark
			     :buffer buffer :offset 5)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 12)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(defmultitest insert-sequence.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%right-sticky-mark
			     :buffer buffer :offset 5)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 10)
	   (= (offset m2) 12)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(defmultitest delete-range.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m 2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(defmultitest delete-range.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m -2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 1)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "cmacs")

(defmultitest delete-region.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(defmultitest delete-region.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(defmultitest delete-region.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(defmultitest delete-region.test-4
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(defmultitest delete-region.test-5
  (handler-case
      (let ((buffer (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m (make-instance %%right-sticky-mark
				:buffer buffer :offset 3))
	      (m2 (make-instance %%right-sticky-mark
				 :buffer buffer2 :offset 5)))
	  (delete-region m2 m)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(defmultitest delete-region.test-6
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance %%left-sticky-mark
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

(defmultitest number-of-lines.test-1
  (let ((buffer (make-instance %%buffer)))
    (number-of-lines buffer))
  0)

(defmultitest number-of-lines.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
")
    (number-of-lines buffer))
  2)

(defmultitest mark-relations.test-1
  (let ((buffer (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (let ((m0 (make-instance %%right-sticky-mark
			       :buffer buffer :offset 0))
	    (m1 (make-instance %%left-sticky-mark
			       :buffer buffer :offset 3))
	    (m1a (make-instance %%right-sticky-mark
				:buffer buffer :offset 3))
	    (m2 (make-instance %%right-sticky-mark
			       :buffer buffer :offset 4))
	    (m2a (make-instance %%left-sticky-mark
				:buffer buffer :offset 5))
	    (m3 (make-instance %%left-sticky-mark
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

(defmultitest setf-offset.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance %%left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) -1)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest setf-offset.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance %%left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) 8)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(defmultitest backward-object.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let* ((m1 (make-instance %%left-sticky-mark
			      :buffer buffer :offset 4))
	   (m2 (clone-mark m1)))
      (backward-object m1 2)
      (region-to-sequence m1 m2)))
  "im")

(defmultitest backward-object.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let* ((m1 (make-instance %%right-sticky-mark
				  :buffer buffer :offset 2))
	       (m2 (clone-mark m1)))
	  (backward-object m1 3)
	  (region-to-sequence m1 m2)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest forward-object.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let* ((m1 (make-instance %%left-sticky-mark
			      :buffer buffer :offset 4))
	   (m2 (clone-mark m1)))
      (forward-object m1 2)
      (region-to-sequence m1 m2)))
  "ac")

(defmultitest forward-object.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let* ((m1 (make-instance %%right-sticky-mark
				  :buffer buffer :offset 6))
	       (m2 (clone-mark m1)))
	  (forward-object m1 3)
	  (region-to-sequence m1 m2)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 9)))
  t)

(defmultitest setf-buffer-object.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (setf (buffer-object buffer 0) #\C)
    (buffer-sequence buffer 0 (size buffer)))
  "Climacs")

(defmultitest setf-buffer-object.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(setf (buffer-object buffer 0) #\a))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 0)))
  t)

(defmultitest setf-buffer-object.test-3
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(setf (buffer-object buffer -1) #\a))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest mark<.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance %%left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance %%left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark< m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(defmultitest mark>.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance %%left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance %%left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark> m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(defmultitest mark<=.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance %%left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance %%left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark<= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(defmultitest mark>=.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance %%left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance %%left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark>= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(defmultitest mark=.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance %%left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance %%left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(defmultitest line-number.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance %%left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance %%right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 0 (line-number m1) (1- (line-number m2)))))
  t)

(defmultitest buffer-column-number.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "		climacs")
    (values
     (buffer-object buffer 2)
     (buffer-column-number buffer 2)))
  #\c 2)

(defmultitest buffer-column-number.test-2
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "
		climacs")
    (values
     (buffer-object buffer 3)
     (buffer-column-number buffer 3)))
  #\c 2)

(defmultitest column-number.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance %%left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance %%right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 3 (column-number m1) (column-number m2))))
  t)

(defmultitest beginning-of-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-line-p m))
	   (progn (beginning-of-line m) (beginning-of-line-p m)))))
  t)

(defmultitest end-of-line.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-line-p m))
	   (progn (end-of-line m) (end-of-line-p m)))))
  t)

(defmultitest beginning-of-buffer.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-buffer-p m))
	   (progn (beginning-of-buffer m) (beginning-of-buffer-p m)))))
  t)

(defmultitest end-of-buffer.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance %%left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-buffer-p m))
	   (progn (end-of-buffer m) (end-of-buffer-p m)))))
  t)

(defmultitest buffer-object.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-object buffer 3))
  #\m)

(defmultitest buffer-object.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer -1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest buffer-object.test-3
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer 7))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(defmultitest buffer-sequence.test-1
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(buffer-sequence buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest buffer-sequence.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(buffer-sequence buffer 0 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(defmultitest buffer-sequence.test-3
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-sequence buffer 5 3))
  #())

(defmultitest object-before.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-before (high-mark buffer)))
  #\s)

(defmultitest object-before.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-before (low-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(defmultitest object-after.test-1
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-after (low-mark buffer)))
  #\c)

(defmultitest object-after.test-2
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-after (high-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(defmultitest region-to-sequence.test-1
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(defmultitest region-to-sequence.test-1a
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence 0 (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(defmultitest region-to-sequence.test-1aa
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (high-mark buffer) 0)))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(defmultitest region-to-sequence.test-1b
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) 7)))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(defmultitest region-to-sequence.test-1ba
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence 7 (low-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(defmultitest region-to-sequence.test-2
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (region-to-sequence (high-mark buffer) (low-mark buffer)))
  "climacs")

(defmultitest region-to-sequence.test-3
  (handler-case
      (let ((buffer1 (make-instance %%buffer))
	    (buffer2 (make-instance %%buffer)))
	(region-to-sequence (low-mark buffer1) (high-mark buffer2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

;;;; performance tests

(defmultitest performance.test-1
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-object b 0 #\a)
      finally (return (size b))))
  100000)

(defmultitest performance.test-1a
  (time
   (let ((b (loop with b = (make-instance %%buffer)
	       for i from 0 below 100000
	       do (insert-buffer-object b 0 #\a)
	       finally (return b))))
     (loop for i from 0 below 100000
	do (delete-buffer-range b 0 1)
	finally (return (size b)))))
  0)

(defmultitest performance.test-1b
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-object b (size b) #\a)
      finally (return (size b))))
  100000)

(defmultitest performance.test-1ba
  (time
   (let ((b (loop with b = (make-instance %%buffer)
	       for i from 0 below 100000
	       do (insert-buffer-object b (size b) #\a)
	       finally (return b))))
     (loop for i from 0 below 100000
	do (delete-buffer-range b 0 1)
	finally (return (size b)))))
  0)

(defmultitest performance.test-1c
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-object b (floor (size b) 2) #\a)
      finally (return (size b))))
  100000)

(defmultitest performance.test-1ca
  (time
   (let ((b (loop with b = (make-instance %%buffer)
	       for i from 0 below 100000
	       do (insert-buffer-object b (floor (size b) 2) #\a)
	       finally (return b))))
     (loop for i from 0 below 100000
	do (delete-buffer-range b 0 1)
	finally (return (size b)))))
  0)

(defmultitest performance.test-1cb
  (time
   (let ((b (loop with b = (make-instance %%buffer)
	       for i from 0 below 100000
	       do (insert-buffer-object b (floor (size b) 2) #\a)
	       finally (return b))))
     (loop for i from 0 below 100000
	do (delete-buffer-range b (floor (size b) 2) 1)
	finally (return (size b)))))
  0)

(defmultitest performance.test-2
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-sequence b 0 "a")
      finally (return (size b))))
  100000)

(defmultitest performance.test-2b
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-sequence b (size b) "a")
      finally (return (size b))))
  100000)

(defmultitest performance.test-2c
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-sequence b (floor (size b) 2) "a")
      finally (return (size b))))
  100000)

(defmultitest performance.test-3
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-sequence b 0 "abcdefghij")
      finally (return (size b))))
  1000000)

(defmultitest performance.test-3b
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-sequence b (size b) "abcdefghij")
      finally (return (size b))))
  1000000)

(defmultitest performance.test-3c
  (time
   (loop with b = (make-instance %%buffer)
      for i from 0 below 100000
      do (insert-buffer-sequence b (floor (size b) 2) "abcdefghij")
      finally (return (size b))))
  1000000)
