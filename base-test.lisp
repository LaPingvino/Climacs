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

(deftest standard-buffer-open-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 0)))
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "
climacs" 0)

(deftest standard-buffer-open-line.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 0)))
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "
climacs" 0)

(deftest standard-buffer-open-line.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 7)))
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs
" 7)

(deftest standard-buffer-open-line.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 7)))
      (open-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs
" 7)

(deftest standard-buffer-kill-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 0)))
      (kill-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  #() 0)

(deftest standard-buffer-kill-line.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 0)))
      (kill-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  #() 0)

(deftest standard-buffer-kill-line.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 7)))
      (kill-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs" 7)

(deftest standard-buffer-kill-line.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 7)))
      (kill-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacs" 7)

(deftest standard-buffer-kill-line.test-5
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-left-sticky-mark
			       :buffer buffer :offset 7)))
      (kill-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacsclimacs" 7)

(deftest standard-buffer-kill-line.test-6
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((mark (make-instance 'standard-right-sticky-mark
			       :buffer buffer :offset 7)))
      (kill-line mark)
      (values (buffer-sequence buffer 0 (size buffer)) (offset mark))))
  "climacsclimacs" 7)

(deftest standard-buffer-empty-line-p.test-1
  (let* ((buffer (make-instance 'standard-buffer))
	 (m1 (make-instance 'standard-left-sticky-mark :buffer buffer))
	 (m2 (make-instance 'standard-right-sticky-mark :buffer buffer)))
    (values (empty-line-p m1) (empty-line-p m2)))
  t t)

(deftest standard-buffer-empty-line-p.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-object buffer 0 #\a)
    (let ((m1 (make-instance 'standard-left-sticky-mark :buffer buffer))
	  (m2 (make-instance 'standard-right-sticky-mark :buffer buffer)))
      (values (empty-line-p m1) (empty-line-p m2))))
  nil nil)

(deftest standard-buffer-empty-line-p.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-object buffer 0 #\a)
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 1))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 1)))
      (values (empty-line-p m1) (empty-line-p m2))))
  nil nil)

(deftest standard-buffer-empty-line-p.test-4
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "a
b")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 1))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 1)))
      (values (empty-line-p m1) (empty-line-p m2))))
  nil nil)

(deftest standard-buffer-line-indentation.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 0))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 0))
	  (m3 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 10))
	  (m4 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 10)))
      (values
       (line-indentation m1 8)
       (line-indentation m2 8)
       (line-indentation m3 8)
       (line-indentation m4 8)
       (offset m1)
       (offset m2)
       (offset m3)
       (offset m4))))
  10 10 10 10 0 0 10 10)

(deftest standard-buffer-line-indentation.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  		climacs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 0))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 0))
	  (m3 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 11))
	  (m4 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 11)))
      (values
       (line-indentation m1 8)
       (line-indentation m2 8)
       (line-indentation m3 8)
       (line-indentation m4 8)
       (offset m1)
       (offset m2)
       (offset m3)
       (offset m4))))
  18 18 18 18 0 0 11 11)

(deftest standard-buffer-line-indentation.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs	")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 0))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 0))
	  (m3 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 11))
	  (m4 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 11)))
      (values
       (line-indentation m1 8)
       (line-indentation m2 8)
       (line-indentation m3 8)
       (line-indentation m4 8)
       (offset m1)
       (offset m2)
       (offset m3)
       (offset m4))))
  10 10 10 10 0 0 11 11)

(deftest standard-buffer-buffer-number-of-lines-in-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (values
     (climacs-base::buffer-number-of-lines-in-region buffer 0 6)
     (climacs-base::buffer-number-of-lines-in-region buffer 0 7)
     (climacs-base::buffer-number-of-lines-in-region buffer 0 10)
     (climacs-base::buffer-number-of-lines-in-region buffer 0 13)
     (climacs-base::buffer-number-of-lines-in-region buffer 0 14)
     (climacs-base::buffer-number-of-lines-in-region buffer 7 10)
     (climacs-base::buffer-number-of-lines-in-region buffer 8 13)
     (climacs-base::buffer-number-of-lines-in-region buffer 8 14)))
    0 0 1 1 1 1 0 0)

(deftest standard-buffer-buffer-display-column.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "		cli	macs")
    (values
     (buffer-display-column buffer 0 8)
     (buffer-display-column buffer 1 8)
     (buffer-display-column buffer 2 8)
     (buffer-display-column buffer 5 8)
     (buffer-display-column buffer 6 8)))
  0 8 16 19 24)

(deftest standard-buffer-number-of-lines-in-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "
climacs
climacs
")
    (let ((m1l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 0))
	  (m1r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 0))
	  (m2l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 1))
	  (m2r (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 1))
	  (m3l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 3))
	  (m3r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 3))
	  (m4l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 8))
	  (m4r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 8))
	  (m5l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 15))
	  (m5r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 15))
	  (m6l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 16))
	  (m6r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 16)))
      (values
       (number-of-lines-in-region m1l m1r)
       (number-of-lines-in-region m1r m1l)
       (number-of-lines-in-region m1l m2l)
       (number-of-lines-in-region m2r m1r)
       (number-of-lines-in-region m1l m2r)
       (number-of-lines-in-region m2r m1l)
       (number-of-lines-in-region m1r m2l)
       (number-of-lines-in-region m1l m3l)
       (number-of-lines-in-region m1r m3r)
       (number-of-lines-in-region m4r m1l)
       (number-of-lines-in-region m4l m1r)
       (number-of-lines-in-region m3l m5l)
       (number-of-lines-in-region m5r m4r)
       (number-of-lines-in-region m5l m6l)
       (number-of-lines-in-region m6r m5r)
       (number-of-lines-in-region m6l m6r)
       (number-of-lines-in-region m1l m6r)
       (number-of-lines-in-region m3r m6l))))
  0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 2 1)

(deftest standard-buffer-number-of-lines-in-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 6))
	  (m1r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 6))
	  (m2l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 7))
	  (m2r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 7)))
      (values
       (number-of-lines-in-region m1l 10)
       (number-of-lines-in-region 10 m1l)
       (number-of-lines-in-region m1r 10)
       (number-of-lines-in-region 10 m1r)
       (number-of-lines-in-region m1l 3)
       (number-of-lines-in-region 3 m2l)
       (number-of-lines-in-region 3 m2r)
       (number-of-lines-in-region m2l 10)
       (number-of-lines-in-region 10 m2r))))
  1 1 1 1 0 0 0 1 1)

(deftest constituentp.test-1 ; NOTE: more tests may be needed for sbcl
  (values
   (constituentp #\a)
   (constituentp #\Newline)
   (constituentp #\Space)
   (constituentp #\Tab)
   (constituentp "a")
   (constituentp #\Null))
  t nil nil nil nil nil)

(deftest whitespacep.test-1
  (values
   (not (null (whitespacep #\a)))
   (not (null (whitespacep #\Newline)))
   (not (null (whitespacep #\Space)))
   (not (null (whitespacep #\Tab)))
   (not (null (whitespacep " ")))
   (not (null (whitespacep #\Null))))
  nil nil t t nil nil)

(deftest standard-buffer-forward-to-word-boundary.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  climacs
climacs")
    (let ((m0l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 0))
	  (m0r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 0))
	  (m1l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 5))
	  (m1r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 5))
	  (m2l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 17))
	  (m2r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 17)))
      (values
       (progn (climacs-base::forward-to-word-boundary m0l) (offset m0l))
       (progn (climacs-base::forward-to-word-boundary m0r) (offset m0r))
       (progn (climacs-base::forward-to-word-boundary m1l) (offset m1l))
       (progn (climacs-base::forward-to-word-boundary m1r) (offset m1r))
       (progn (climacs-base::forward-to-word-boundary m2l) (offset m2l))
       (progn (climacs-base::forward-to-word-boundary m2r) (offset m2r)))))
  2 2 5 5 17 17)

(deftest standard-buffer-backward-to-word-boundary.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs  ")
    (let ((m0l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 17))
	  (m0r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 17))
	  (m1l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 10))
	  (m1r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 10))
	  (m2l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 0))
	  (m2r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 0)))
      (values
       (progn (climacs-base::backward-to-word-boundary m0l) (offset m0l))
       (progn (climacs-base::backward-to-word-boundary m0r) (offset m0r))
       (progn (climacs-base::backward-to-word-boundary m1l) (offset m1l))
       (progn (climacs-base::backward-to-word-boundary m1r) (offset m1r))
       (progn (climacs-base::backward-to-word-boundary m2l) (offset m2l))
       (progn (climacs-base::backward-to-word-boundary m2r) (offset m2r)))))
  15 15 10 10 0 0)

(deftest standard-buffer-forward-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  climacs
climacs")
    (let ((m0l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 0))
	  (m0r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 0))
	  (m1l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 5))
	  (m1r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 15))
	  (m2l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 17))
	  (m2r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 17)))
      (values
       (progn (forward-word m0l) (offset m0l))
       (progn (forward-word m0r) (offset m0r))
       (progn (forward-word m1l) (offset m1l))
       (progn (forward-word m1r) (offset m1r))
       (progn (forward-word m2l) (offset m2l))
       (progn (forward-word m2r) (offset m2r)))))
  9 9 9 17 17 17)

(deftest standard-buffer-backward-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs  ")
    (let ((m0l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 17))
	  (m0r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 17))
	  (m1l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 10))
	  (m1r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 5))
	  (m2l (make-instance 'standard-left-sticky-mark
			      :buffer buffer :offset 0))
	  (m2r (make-instance 'standard-right-sticky-mark
			      :buffer buffer :offset 0)))
      (values
       (progn (backward-word m0l) (offset m0l))
       (progn (backward-word m0r) (offset m0r))
       (progn (backward-word m1l) (offset m1l))
       (progn (backward-word m1r) (offset m1r))
       (progn (backward-word m2l) (offset m2l))
       (progn (backward-word m2r) (offset m2r)))))
  8 8 8 0 0 0)

(deftest standard-buffer-delete-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3)))
      (delete-word m)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "cli" 3)

(deftest standard-buffer-delete-word.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  climacs climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 0)))
      (delete-word m 2)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  #() 0)

(deftest standard-buffer-backward-delete-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 3)))
      (backward-delete-word m)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "macs" 0)

(deftest standard-buffer-backward-delete-word.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs climacs  ")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 17)))
      (backward-delete-word m 2)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  #() 0)

(deftest standard-buffer-previous-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "climacs climacs")
    (let ((m0 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 7))
	  (m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 8))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 10)))
      (values
       (climacs-base::previous-word m0)
       (climacs-base::previous-word m1)
       (climacs-base::previous-word m2))))
  "climacs" #() "cl")

(deftest standard-buffer-downcase-buffer-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "CLi	mac5")
    (climacs-base::downcase-buffer-region buffer 1 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "Cli	mac5")

(deftest standard-buffer-downcase-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 1))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 8)))
      (downcase-region m2 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_cli	mac5_")

(deftest standard-buffer-downcase-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 1)))
      (downcase-region 8 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_cli	mac5_")

(deftest standard-buffer-downcase-region.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 8)))
      (downcase-region 1 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_cli	mac5_")

(deftest standard-buffer-downcase-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "CLI MA CS CLIMACS")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 0)))
      (downcase-word m 3)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "cli ma cs CLIMACS" 9)

(deftest standard-buffer-upcase-buffer-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "cli	mac5")
    (climacs-base::upcase-buffer-region buffer 1 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "cLI	MAC5")

(deftest standard-buffer-upcase-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 1))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 8)))
      (upcase-region m2 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_CLI	MAC5_")

(deftest standard-buffer-upcase-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 1)))
      (upcase-region 8 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_CLI	MAC5_")

(deftest standard-buffer-upcase-region.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 8)))
      (upcase-region 1 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_CLI	MAC5_")

(deftest standard-buffer-upcase-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "cli ma cs climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 0)))
      (upcase-word m 3)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "CLI MA CS climacs" 9)

(deftest standard-buffer-capitalize-buffer-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "cli ma cs")
    (climacs-base::capitalize-buffer-region buffer 1 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "cLi Ma Cs")

(deftest standard-buffer-capitalize-buffer-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "CLI mA Cs")
    (climacs-base::capitalize-buffer-region buffer 0 (size buffer))
    (buffer-sequence buffer 0 (size buffer)))
  "Cli Ma Cs")

(deftest standard-buffer-capitalize-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 1))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 8)))
      (capitalize-region m2 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_Cli	Mac5_")

(deftest standard-buffer-capitalize-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 1)))
      (capitalize-region 8 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_Cli	Mac5_")

(deftest standard-buffer-capitalize-region.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "_Cli	mac5_")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 8)))
      (capitalize-region 1 m1)
      (buffer-sequence buffer 0 (size buffer))))
  "_Cli	Mac5_")

(deftest standard-buffer-capitalize-word.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "cli ma cs climacs")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 0)))
      (capitalize-word m 3)
      (values
       (buffer-sequence buffer 0 (size buffer))
       (offset m))))
  "Cli Ma Cs climacs" 9)

(deftest standard-buffer-tabify-buffer-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "c       l       im              acs")
    (climacs-base::tabify-buffer-region buffer 0 (size buffer) 8)    
    (buffer-sequence buffer 0 (size buffer)))
  "c	l	im		acs")

(deftest standard-buffer-tabify-buffer-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "c      l       im              acs")
    (climacs-base::tabify-buffer-region buffer 0 (size buffer) 8)    
    (buffer-sequence buffer 0 (size buffer)))
  "c      l       im	       acs")

(deftest standard-buffer-tabify-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 7)))
      (tabify-region m2 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim	acs")

(deftest standard-buffer-tabify-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 3)))
      (tabify-region 7 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim	acs")

(deftest standard-buffer-tabify-region.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "clim    acs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 7)))
      (tabify-region 3 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim	acs")

(deftest standard-buffer-untabify-buffer-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "c	l	im		acs")
    (climacs-base::untabify-buffer-region buffer 0 (size buffer) 8)
    (buffer-sequence buffer 0 (size buffer)))
  "c       l       im              acs")

(deftest standard-buffer-untabify-buffer-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "c      l       im	       acs")
    (climacs-base::untabify-buffer-region buffer 0 (size buffer) 8)    
    (buffer-sequence buffer 0 (size buffer)))
  "c      l       im              acs")

(deftest standard-buffer-untabify-region.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "clim	acs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 3))
	  (m2 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 5)))
      (untabify-region m2 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim    acs")

(deftest standard-buffer-untabify-region.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "clim	acs")
    (let ((m1 (make-instance 'standard-right-sticky-mark
			     :buffer buffer :offset 3)))
      (untabify-region 5 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim    acs")

(deftest standard-buffer-untabify-region.test-3
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "clim	acs")
    (let ((m1 (make-instance 'standard-left-sticky-mark
			     :buffer buffer :offset 5)))
      (untabify-region 3 m1 4)
      (buffer-sequence buffer 0 (size buffer))))
  "clim    acs")

(deftest standard-buffer-indent-line.test-1
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (make-instance 'standard-left-sticky-mark
			    :buffer buffer :offset 0)))
      (indent-line m 4 nil)
      (buffer-sequence buffer 0 (size buffer))))
  "    climacs   ")

(deftest standard-buffer-indent-line.test-2
  (let ((buffer (make-instance 'standard-buffer)))
    (insert-buffer-sequence buffer 0 "  	climacs   ")
    (let ((m (make-instance 'standard-right-sticky-mark
			    :buffer buffer :offset 0)))
      (indent-line m 5 4)
      (buffer-sequence buffer 0 (size buffer))))
  "	 climacs   ")
