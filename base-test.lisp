;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(in-package :climacs-tests)

(deftest standard-buffer-previous-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 8)))
      (previous-line mark)
      (offset mark)))
  0)

(deftest standard-buffer-previous-line.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 11)))
      (previous-line mark 2)
      (offset mark)))
  2)

(deftest standard-buffer-previous-line.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 7)))
      (previous-line mark)
      (offset mark)))
  7)

(deftest standard-buffer-previous-line.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 7)))
      (previous-line mark 2)
      (offset mark)))
  2)

(deftest standard-buffer-previous-line.test-5
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 0)))
      (previous-line mark)
      (offset mark)))
  0)

(deftest standard-buffer-previous-line.test-6
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 0)))
      (previous-line mark 2)
      (offset mark)))
  2)

(deftest standard-buffer-previous-line.test-7
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs2")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 15)))
      (previous-line mark)
      (offset mark)))
  7)

(deftest standard-buffer-next-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 6)))
      (next-line mark)
      (offset mark)))
  14)

(deftest standard-buffer-next-line.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 3)))
      (next-line mark 2)
      (offset mark)))
  10)

(deftest standard-buffer-next-line.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 8)))
      (next-line mark)
      (offset mark)))
  8)

(deftest standard-buffer-next-line.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 8)))
      (next-line mark 2)
      (offset mark)))
  10)

(deftest standard-buffer-next-line.test-5
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 15)))
      (next-line mark)
      (offset mark)))
  15)

(deftest standard-buffer-next-line.test-6
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 15)))
      (next-line mark 2)
      (offset mark)))
  10)

(deftest standard-buffer-next-line.test-7
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 0)))
      (next-line mark)
      (offset mark)))
  8)