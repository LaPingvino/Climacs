;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

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

(in-package :climacs-tests)

;;;; binseq tests

(deftest binseq-buffer-make-instance.test-1
  (let* ((buffer (make-instance 'binseq-buffer))
	 (low (slot-value buffer 'low-mark))
	 (high (slot-value buffer 'high-mark)))
    (and (= (offset low) 0)
	 (= (offset high) 0)
	 (null (modified-p buffer))
	 (eq (buffer low) buffer)
	 (eq (buffer high) buffer)))
  t)

(deftest binseq-buffer-mark-make-instance.test-2
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(make-instance 'persistent-left-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest binseq-buffer-mark-make-instance.test-3
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(make-instance 'persistent-right-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest binseq-buffer-clone-mark.test-1
  (flet ((%all-eq (&optional x y)
	   (cond
	     ((null x) nil)
	     (t (when (eq x y) y)))))
    (let* ((buffer (make-instance 'binseq-buffer))
	   (low (slot-value buffer 'low-mark))
	   (high (slot-value buffer 'high-mark))
	   (low2 (clone-mark low))
	   (high2 (clone-mark high))
	   (low3 (clone-mark high 'persistent-left-sticky-mark))
	   (high3 (clone-mark low 'persistent-right-sticky-mark)))
      (and (reduce #'%all-eq
		  (list (class-of low) (class-of low2) (class-of low3)))
	   (reduce #'%all-eq
		  (list (class-of high) (class-of high2) (class-of high3)))
	   (= (offset low) (offset low2) (offset low3)
	      (offset high) (offset high2) (offset high3) 0))))
  t)

;;; NOTE: the current implementation uses vectors wherever sequences are
;;; expected (and strings are vectors of characters)

(deftest binseq-buffer-insert-buffer-object.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-object buffer 0 #\a)
    (and (= (size buffer) 1) (buffer-sequence buffer 0 1)))
  "a")

(deftest binseq-buffer-insert-buffer-object.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 0 #\a)
    (and (= (size buffer) 2) (buffer-sequence buffer 0 2)))
  "ab")

(deftest binseq-buffer-insert-buffer-object.test-3
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 1 #\a)
    (and (= (size buffer) 2) (buffer-sequence buffer 0 2)))
  "ba")

(deftest binseq-buffer-insert-buffer-object.test-4
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-object buffer 1 #\a))
    (error (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest binseq-buffer-insert-buffer-object.test-5
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-object buffer -1 #\a))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-insert-buffer-sequence.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(deftest binseq-buffer-insert-buffer-sequence.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 3 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "cliClimacSmacs")

(deftest binseq-buffer-insert-buffer-sequence.test-3
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 0 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "ClimacSclimacs")

(deftest binseq-buffer-insert-buffer-sequence.test-4
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "")
    (and (= (size buffer) 0) (buffer-sequence buffer 0 0)))
  "")

(deftest binseq-buffer-insert-buffer-sequence.test-5
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest binseq-buffer-insert-buffer-sequence.test-6
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer -1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-delete-buffer-range.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 7)
    (size buffer))
  0)

(deftest binseq-buffer-delete-buffer-range.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 3)
    (and (= (size buffer) 4) (buffer-sequence buffer 0 4)))
  "macs")

(deftest binseq-buffer-delete-buffer-range.test-3
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 4)
    (and (= (size buffer) 3) (buffer-sequence buffer 0 3)))
  "cli")

(deftest binseq-buffer-delete-buffer-range.test-4
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 0)
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(deftest binseq-buffer-delete-buffer-range.test-5
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-delete-buffer-range.test-6
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer 6 2))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(deftest binseq-buffer-insert-object.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(deftest binseq-buffer-insert-object.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 4)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(deftest binseq-buffer-insert-sequence.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(deftest binseq-buffer-insert-sequence.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 10)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(deftest binseq-buffer-delete-range.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m 2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest binseq-buffer-delete-range.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m -2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 1)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "cmacs")

(deftest binseq-buffer-delete-region.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest binseq-buffer-delete-region.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest binseq-buffer-delete-region.test-3
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest binseq-buffer-delete-region.test-4
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest binseq-buffer-delete-region.test-5
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m (make-instance 'persistent-right-sticky-mark
				:buffer buffer :offset 3))
	      (m2 (make-instance 'persistent-right-sticky-mark
				 :buffer buffer2 :offset 5)))
	  (delete-region m2 m)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest binseq-buffer-delete-region.test-6
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
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

(deftest binseq-buffer-number-of-lines.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (number-of-lines buffer))
  0)

(deftest binseq-buffer-number-of-lines.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
")
    (number-of-lines buffer))
  2)

(deftest binseq-buffer-mark-relations.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (let ((m0 (make-instance 'persistent-right-sticky-mark
			       :buffer buffer :offset 0))
	    (m1 (make-instance 'persistent-left-sticky-mark
			       :buffer buffer :offset 3))
	    (m1a (make-instance 'persistent-right-sticky-mark
				:buffer buffer :offset 3))
	    (m2 (make-instance 'persistent-right-sticky-mark
			       :buffer buffer :offset 4))
	    (m2a (make-instance 'persistent-left-sticky-mark
				:buffer buffer :offset 5))
	    (m3 (make-instance 'persistent-left-sticky-mark
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

(deftest binseq-buffer-setf-offset.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance 'persistent-left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) -1)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-setf-offset.test-2
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance 'persistent-left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) 8)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(deftest binseq-buffer-mark<.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark< m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest binseq-buffer-mark>.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark> m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest binseq-buffer-mark<=.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark<= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest binseq-buffer-mark>=.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark>= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest binseq-buffer-mark=.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest binseq-buffer-line-number.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 0 (line-number m1) (1- (line-number m2)))))
  t)

(deftest binseq-buffer-buffer-column-number.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "		climacs")
    (values
     (buffer-object buffer 2)
     (buffer-column-number buffer 2)))
  #\c 2)

(deftest binseq-buffer-buffer-column-number.test-2
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "
		climacs")
    (values
     (buffer-object buffer 3)
     (buffer-column-number buffer 3)))
  #\c 2)

(deftest binseq-buffer-column-number.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 3 (column-number m1) (column-number m2))))
  t)

(deftest binseq-buffer-beginning-of-line.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-line-p m))
	   (progn (beginning-of-line m) (beginning-of-line-p m)))))
  t)

(deftest binseq-buffer-end-of-line.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-line-p m))
	   (progn (end-of-line m) (end-of-line-p m)))))
  t)

(deftest binseq-buffer-beginning-of-buffer.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-buffer-p m))
	   (progn (beginning-of-buffer m) (beginning-of-buffer-p m)))))
  t)

(deftest binseq-buffer-end-of-buffer.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-buffer-p m))
	   (progn (end-of-buffer m) (end-of-buffer-p m)))))
  t)

(deftest binseq-buffer-buffer-object.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-object buffer 3))
  #\m)

(deftest binseq-buffer-buffer-object.test-2
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer -1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-buffer-object.test-3
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer 7))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(deftest binseq-buffer-buffer-sequence.test-1
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(buffer-sequence buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-buffer-sequence.test-2
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(buffer-sequence buffer 0 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest binseq-buffer-buffer-sequence.test-3
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-sequence buffer 5 3))
  #())

(deftest binseq-buffer-object-before.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-before (high-mark buffer)))
  #\s)

(deftest binseq-buffer-object-before.test-2
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-before (low-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest binseq-buffer-object-after.test-1
  (let ((buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-after (low-mark buffer)))
  #\c)

(deftest binseq-buffer-object-after.test-2
  (handler-case
      (let ((buffer (make-instance 'binseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-after (high-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(deftest binseq-buffer-region-to-sequence.test-1
  (let ((seq "climacs")
	(buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest binseq-buffer-region-to-sequence.test-1a
  (let ((seq "climacs")
	(buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence 0 (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest binseq-buffer-region-to-sequence.test-1b
  (let ((seq "climacs")
	(buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) 7)))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest binseq-buffer-region-to-sequence.test-2
  (let ((seq "climacs")
	(buffer (make-instance 'binseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (region-to-sequence (high-mark buffer) (low-mark buffer)))
  #())

(deftest binseq-buffer-region-to-sequence.test-3
  (handler-case
      (let ((buffer1 (make-instance 'binseq-buffer))
	    (buffer2 (make-instance 'binseq-buffer)))
	(region-to-sequence (low-mark buffer1) (high-mark buffer2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

;;;; obinseq tests

(deftest obinseq-buffer-make-instance.test-1
  (let* ((buffer (make-instance 'obinseq-buffer))
	 (low (slot-value buffer 'low-mark))
	 (high (slot-value buffer 'high-mark)))
    (and (= (offset low) 0)
	 (= (offset high) 0)
	 (null (modified-p buffer))
	 (eq (buffer low) buffer)
	 (eq (buffer high) buffer)))
  t)

(deftest obinseq-buffer-mark-make-instance.test-2
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(make-instance 'persistent-left-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest obinseq-buffer-mark-make-instance.test-3
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(make-instance 'persistent-right-sticky-mark :buffer buffer :offset 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest obinseq-buffer-clone-mark.test-1
  (flet ((%all-eq (&optional x y)
	   (cond
	     ((null x) nil)
	     (t (when (eq x y) y)))))
    (let* ((buffer (make-instance 'obinseq-buffer))
	   (low (slot-value buffer 'low-mark))
	   (high (slot-value buffer 'high-mark))
	   (low2 (clone-mark low))
	   (high2 (clone-mark high))
	   (low3 (clone-mark high 'persistent-left-sticky-mark))
	   (high3 (clone-mark low 'persistent-right-sticky-mark)))
      (and (reduce #'%all-eq
		  (list (class-of low) (class-of low2) (class-of low3)))
	   (reduce #'%all-eq
		  (list (class-of high) (class-of high2) (class-of high3)))
	   (= (offset low) (offset low2) (offset low3)
	      (offset high) (offset high2) (offset high3) 0))))
  t)

;;; NOTE: the current implementation uses vectors wherever sequences are
;;; expected (and strings are vectors of characters)

(deftest obinseq-buffer-insert-buffer-object.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-object buffer 0 #\a)
    (and (= (size buffer) 1) (buffer-sequence buffer 0 1)))
  "a")

(deftest obinseq-buffer-insert-buffer-object.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 0 #\a)
    (and (= (size buffer) 2) (buffer-sequence buffer 0 2)))
  "ab")

(deftest obinseq-buffer-insert-buffer-object.test-3
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 1 #\a)
    (and (= (size buffer) 2) (buffer-sequence buffer 0 2)))
  "ba")

(deftest obinseq-buffer-insert-buffer-object.test-4
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-object buffer 1 #\a))
    (error (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest obinseq-buffer-insert-buffer-object.test-5
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-object buffer -1 #\a))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-insert-buffer-sequence.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(deftest obinseq-buffer-insert-buffer-sequence.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 3 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "cliClimacSmacs")

(deftest obinseq-buffer-insert-buffer-sequence.test-3
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 0 "ClimacS")
    (and (= (size buffer) 14) (buffer-sequence buffer 0 14)))
  "ClimacSclimacs")

(deftest obinseq-buffer-insert-buffer-sequence.test-4
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "")
    (and (= (size buffer) 0) (buffer-sequence buffer 0 0)))
  "")

(deftest obinseq-buffer-insert-buffer-sequence.test-5
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest obinseq-buffer-insert-buffer-sequence.test-6
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer -1 "climacs"))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-delete-buffer-range.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 7)
    (size buffer))
  0)

(deftest obinseq-buffer-delete-buffer-range.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 3)
    (and (= (size buffer) 4) (buffer-sequence buffer 0 4)))
  "macs")

(deftest obinseq-buffer-delete-buffer-range.test-3
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 4)
    (and (= (size buffer) 3) (buffer-sequence buffer 0 3)))
  "cli")

(deftest obinseq-buffer-delete-buffer-range.test-4
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 0)
    (and (= (size buffer) 7) (buffer-sequence buffer 0 7)))
  "climacs")

(deftest obinseq-buffer-delete-buffer-range.test-5
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-delete-buffer-range.test-6
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(delete-buffer-range buffer 6 2))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(deftest obinseq-buffer-insert-object.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(deftest obinseq-buffer-insert-object.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-object m #\X)
      (and (= (size buffer) 8)
	   (eq (buffer m) buffer)
	   (= (offset m) 4)
	   (buffer-sequence buffer 0 8))))
  "cliXmacs")

(deftest obinseq-buffer-insert-sequence.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 3)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(deftest obinseq-buffer-insert-sequence.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3)))
      (insert-sequence m "ClimacS")
      (and (= (size buffer) 14)
	   (eq (buffer m) buffer)
	   (= (offset m) 10)
	   (buffer-sequence buffer 0 14))))
  "cliClimacSmacs")

(deftest obinseq-buffer-delete-range.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m 2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest obinseq-buffer-delete-range.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-range m -2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 1)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "cmacs")

(deftest obinseq-buffer-delete-region.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest obinseq-buffer-delete-region.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m m2)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest obinseq-buffer-delete-region.test-3
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest obinseq-buffer-delete-region.test-4
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-right-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 5)))
      (delete-region m2 m)
      (and (= (size buffer) 5)
	   (eq (buffer m) buffer)
	   (eq (buffer m2) buffer)
	   (= (offset m) 3)
	   (= (offset m2) 3)
	   (buffer-sequence buffer 0 5))))
  "clics")

(deftest obinseq-buffer-delete-region.test-5
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m (make-instance 'persistent-right-sticky-mark
				:buffer buffer :offset 3))
	      (m2 (make-instance 'persistent-right-sticky-mark
				 :buffer buffer2 :offset 5)))
	  (delete-region m2 m)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest obinseq-buffer-delete-region.test-6
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-left-sticky-mark
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

(deftest obinseq-buffer-number-of-lines.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (number-of-lines buffer))
  0)

(deftest obinseq-buffer-number-of-lines.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs
")
    (number-of-lines buffer))
  2)

(deftest obinseq-buffer-mark-relations.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (let ((m0 (make-instance 'persistent-right-sticky-mark
			       :buffer buffer :offset 0))
	    (m1 (make-instance 'persistent-left-sticky-mark
			       :buffer buffer :offset 3))
	    (m1a (make-instance 'persistent-right-sticky-mark
				:buffer buffer :offset 3))
	    (m2 (make-instance 'persistent-right-sticky-mark
			       :buffer buffer :offset 4))
	    (m2a (make-instance 'persistent-left-sticky-mark
				:buffer buffer :offset 5))
	    (m3 (make-instance 'persistent-left-sticky-mark
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

(deftest obinseq-buffer-setf-offset.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance 'persistent-left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) -1)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-setf-offset.test-2
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let ((m (make-instance 'persistent-left-sticky-mark
				:buffer buffer :offset 4)))
	  (setf (offset m) 8)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 8)))
  t)

(deftest obinseq-buffer-mark<.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark< m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest obinseq-buffer-mark>.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark> m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest obinseq-buffer-mark<=.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark<= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest obinseq-buffer-mark>=.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark>= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest obinseq-buffer-mark=.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(insert-buffer-sequence buffer2 0 "climacs")
	(let ((m1 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer :offset 4))
	      (m2 (make-instance 'persistent-left-sticky-mark
				 :buffer buffer2 :offset 4)))
	  (mark= m1 m2)))
    (error (c)
      (declare (ignore c))
      'caught))
  caught)

(deftest obinseq-buffer-line-number.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 0 (line-number m1) (1- (line-number m2)))))
  t)

(deftest obinseq-buffer-buffer-column-number.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "		climacs")
    (values
     (buffer-object buffer 2)
     (buffer-column-number buffer 2)))
  #\c 2)

(deftest obinseq-buffer-buffer-column-number.test-2
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "
		climacs")
    (values
     (buffer-object buffer 3)
     (buffer-column-number buffer 3)))
  #\c 2)

(deftest obinseq-buffer-column-number.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-instance 'persistent-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'persistent-right-sticky-mark
			     :buffer buffer :offset 11)))
      (= 3 (column-number m1) (column-number m2))))
  t)

(deftest obinseq-buffer-beginning-of-line.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-line-p m))
	   (progn (beginning-of-line m) (beginning-of-line-p m)))))
  t)

(deftest obinseq-buffer-end-of-line.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-line-p m))
	   (progn (end-of-line m) (end-of-line-p m)))))
  t)

(deftest obinseq-buffer-beginning-of-buffer.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (beginning-of-buffer-p m))
	   (progn (beginning-of-buffer m) (beginning-of-buffer-p m)))))
  t)

(deftest obinseq-buffer-end-of-buffer.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-instance 'persistent-left-sticky-mark
			    :buffer buffer :offset 11)))
      (and (not (end-of-buffer-p m))
	   (progn (end-of-buffer m) (end-of-buffer-p m)))))
  t)

(deftest obinseq-buffer-buffer-object.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-object buffer 3))
  #\m)

(deftest obinseq-buffer-buffer-object.test-2
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer -1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-buffer-object.test-3
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer 7))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(deftest obinseq-buffer-buffer-sequence.test-1
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(buffer-sequence buffer -1 0))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-buffer-sequence.test-2
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(buffer-sequence buffer 0 1))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 1)))
  t)

(deftest obinseq-buffer-buffer-sequence.test-3
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (buffer-sequence buffer 5 3))
  #())

(deftest obinseq-buffer-object-before.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-before (high-mark buffer)))
  #\s)

(deftest obinseq-buffer-object-before.test-2
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-before (low-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) -1)))
  t)

(deftest obinseq-buffer-object-after.test-1
  (let ((buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (object-after (low-mark buffer)))
  #\c)

(deftest obinseq-buffer-object-after.test-2
  (handler-case
      (let ((buffer (make-instance 'obinseq-buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-after (high-mark buffer)))
    (climacs-buffer::no-such-offset (c)
      (= (climacs-buffer::condition-offset c) 7)))
  t)

(deftest obinseq-buffer-region-to-sequence.test-1
  (let ((seq "climacs")
	(buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest obinseq-buffer-region-to-sequence.test-1a
  (let ((seq "climacs")
	(buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence 0 (high-mark buffer))))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest obinseq-buffer-region-to-sequence.test-1b
  (let ((seq "climacs")
	(buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-sequence (low-mark buffer) 7)))
      (and (not (eq seq seq2)) seq2)))
  "climacs")

(deftest obinseq-buffer-region-to-sequence.test-2
  (let ((seq "climacs")
	(buffer (make-instance 'obinseq-buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (region-to-sequence (high-mark buffer) (low-mark buffer)))
  #())

(deftest obinseq-buffer-region-to-sequence.test-3
  (handler-case
      (let ((buffer1 (make-instance 'obinseq-buffer))
	    (buffer2 (make-instance 'obinseq-buffer)))
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

;;;; binseq performance tests

(deftimetest binseq-buffer-performance.test-1
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b 0 #\a)
     finally (return (size b)))
  100000)

(deftimetest binseq-buffer-performance.test-1a
  (let ((b (loop with b = (make-instance 'binseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b 0 #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest binseq-buffer-performance.test-1b
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b (size b) #\a)
     finally (return (size b)))
  100000)

(deftimetest binseq-buffer-performance.test-1ba
  (let ((b (loop with b = (make-instance 'binseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (size b) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest binseq-buffer-performance.test-1c
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b (floor (size b) 2) #\a)
     finally (return (size b)))
  100000)

(deftimetest binseq-buffer-performance.test-1ca
  (let ((b (loop with b = (make-instance 'binseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (floor (size b) 2) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest binseq-buffer-performance.test-1cb
  (let ((b (loop with b = (make-instance 'binseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (floor (size b) 2) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b (floor (size b) 2) 1)
       finally (return (size b))))
  0)

(deftimetest binseq-buffer-performance.test-2
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b 0 "a")
     finally (return (size b)))
  100000)

(deftimetest binseq-buffer-performance.test-2b
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (size b) "a")
     finally (return (size b)))
  100000)

(deftimetest binseq-buffer-performance.test-2c
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (floor (size b) 2) "a")
     finally (return (size b)))
  100000)

(deftimetest binseq-buffer-performance.test-3
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b 0 "abcdefghij")
     finally (return (size b)))
  1000000)

(deftimetest binseq-buffer-performance.test-3b
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (size b) "abcdefghij")
     finally (return (size b)))
  1000000)

(deftimetest binseq-buffer-performance.test-3c
  (loop with b = (make-instance 'binseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (floor (size b) 2) "abcdefghij")
     finally (return (size b)))
  1000000)

;;;; obinseq performance tests

(deftimetest obinseq-buffer-performance.test-1
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b 0 #\a)
     finally (return (size b)))
  100000)

(deftimetest obinseq-buffer-performance.test-1a
  (let ((b (loop with b = (make-instance 'obinseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b 0 #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest obinseq-buffer-performance.test-1b
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b (size b) #\a)
     finally (return (size b)))
  100000)

(deftimetest obinseq-buffer-performance.test-1ba
  (let ((b (loop with b = (make-instance 'obinseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (size b) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest obinseq-buffer-performance.test-1c
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-object b (floor (size b) 2) #\a)
     finally (return (size b)))
  100000)

(deftimetest obinseq-buffer-performance.test-1ca
  (let ((b (loop with b = (make-instance 'obinseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (floor (size b) 2) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b 0 1)
       finally (return (size b))))
  0)

(deftimetest obinseq-buffer-performance.test-1cb
  (let ((b (loop with b = (make-instance 'obinseq-buffer)
	      for i from 0 below 100000
	      do (insert-buffer-object b (floor (size b) 2) #\a)
	      finally (return b))))
    (loop for i from 0 below 100000
       do (delete-buffer-range b (floor (size b) 2) 1)
       finally (return (size b))))
  0)

(deftimetest obinseq-buffer-performance.test-2
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b 0 "a")
     finally (return (size b)))
  100000)

(deftimetest obinseq-buffer-performance.test-2b
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (size b) "a")
     finally (return (size b)))
  100000)

(deftimetest obinseq-buffer-performance.test-2c
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (floor (size b) 2) "a")
     finally (return (size b)))
  100000)

(deftimetest obinseq-buffer-performance.test-3
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b 0 "abcdefghij")
     finally (return (size b)))
  1000000)

(deftimetest obinseq-buffer-performance.test-3b
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (size b) "abcdefghij")
     finally (return (size b)))
  1000000)

(deftimetest obinseq-buffer-performance.test-3c
  (loop with b = (make-instance 'obinseq-buffer)
     for i from 0 below 100000
     do (insert-buffer-sequence b (floor (size b) 2) "abcdefghij")
     finally (return (size b)))
  1000000)