;;; -*- Mode: Lisp; Package: CLIMACS-CORE -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

(in-package :climacs-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Misc stuff

(defun possibly-fill-line ()
  (let* ((pane (current-window))
         (buffer (buffer pane)))
    (when (auto-fill-mode pane)
      (let* ((fill-column (auto-fill-column pane))
             (point (point pane))
             (offset (offset point))
             (tab-width (tab-space-count (stream-default-view pane)))
             (syntax (syntax buffer)))
        (when (>= (buffer-display-column buffer offset tab-width)
                  (1- fill-column))
          (fill-line point
                     (lambda (mark)
                       (syntax-line-indentation mark tab-width syntax))
                     fill-column
                     tab-width
                     (syntax buffer)))))))

(defun insert-character (char)
  (let* ((window (current-window))
	 (point (point window)))
    (unless (constituentp char)
      (possibly-expand-abbrev point))
    (when (whitespacep (syntax (buffer window)) char)
      (possibly-fill-line))
    (if (and (slot-value window 'overwrite-mode) (not (end-of-line-p point)))
	(progn
	  (delete-range point)
	  (insert-object point char))
	(insert-object point char))))

(defun back-to-indentation (mark syntax)
  (beginning-of-line mark)
  (loop until (end-of-line-p mark)
     while (whitespacep syntax (object-after mark))
     do (forward-object mark)))

(defun delete-horizontal-space (mark syntax &optional (backward-only-p nil))
  (let ((mark2 (clone-mark mark)))
    (loop until (beginning-of-line-p mark)
	  while (whitespacep syntax (object-before mark))
	  do (backward-object mark))
    (unless backward-only-p
      (loop until (end-of-line-p mark2)
	    while (whitespacep syntax (object-after mark2))
	    do (forward-object mark2)))
    (delete-region mark mark2)))

(defun goto-position (mark pos)
  (setf (offset mark) pos))

(defun goto-line (mark line-number)
  (loop with m = (clone-mark (low-mark (buffer mark))
		       :right)
	initially (beginning-of-buffer m)
	do (end-of-line m)
	until (end-of-buffer-p m)
	repeat (1- line-number)
	do (incf (offset m))
	   (end-of-line m)
	finally (beginning-of-line m)
		(setf (offset mark) (offset m))))

(defun indent-current-line (pane point)
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (indentation (syntax-line-indentation point
                                               tab-space-count
                                               (syntax buffer))))
    (indent-line point indentation (and (indent-tabs-mode buffer)
                                        tab-space-count))))

(defun insert-pair (mark syntax &optional (count 0) (open #\() (close #\)))
  (cond ((> count 0)
	 (loop while (and (not (end-of-buffer-p mark))
			  (whitespacep syntax (object-after mark)))
	       do (forward-object mark)))
	((< count 0)
	 (setf count (- count))
	 (loop repeat count do (backward-expression mark syntax))))
  (unless (or (beginning-of-buffer-p mark)
	      (whitespacep syntax (object-before mark)))
    (insert-object mark #\Space))
  (insert-object mark open)
  (let ((here (clone-mark mark)))
    (loop repeat count
	  do (forward-expression here syntax))
    (insert-object here close)
    (unless (or (end-of-buffer-p here)
		(whitespacep syntax (object-after here)))
      (insert-object here #\Space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

(defun downcase-word (mark &optional (n 1))
  "Convert the next N words to lowercase, leaving mark after the last word."
  (let ((syntax (syntax (buffer mark))))
    (loop repeat n
       do (forward-to-word-boundary mark syntax)
       (let ((offset (offset mark)))
         (forward-word mark syntax 1 nil)
         (downcase-region offset mark)))))

(defun upcase-word (mark syntax &optional (n 1))
  "Convert the next N words to uppercase, leaving mark after the last word."
  (loop repeat n
     do (forward-to-word-boundary mark syntax)
     (let ((offset (offset mark)))
       (forward-word mark syntax 1 nil)
       (upcase-region offset mark))))

(defun capitalize-word (mark &optional (n 1))
  "Capitalize the next N words, leaving mark after the last word."
  (let ((syntax (syntax (buffer mark))))
    (loop repeat n
       do (forward-to-word-boundary mark syntax)
       (let ((offset (offset mark)))
         (forward-word mark syntax 1 nil)
         (capitalize-region offset mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Indentation

(defun indent-region (pane mark1 mark2)
  "Indent all lines in the region delimited by `mark1' and `mark2'
   according to the rules of the active syntax in `pane'."
  (let* ((buffer (buffer pane))
         (view (clim:stream-default-view pane))
         (tab-space-count (tab-space-count view))
         (tab-width (and (indent-tabs-mode buffer)
                         tab-space-count))
         (syntax (syntax buffer)))
    (do-buffer-region-lines (line mark1 mark2)
      (let ((indentation (syntax-line-indentation
                          line
                          tab-space-count
                          syntax)))
        (indent-line line indentation tab-width))
      ;; We need to update the syntax every time we perform an
      ;; indentation, so that subsequent indentations will be
      ;; correctly indented (this matters in list forms). FIXME: This
      ;; should probably happen automatically.
      (update-syntax buffer syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Auto fill

(defun fill-line (mark syntax-line-indentation-function fill-column tab-width syntax
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
					  (not (whitespacep syntax o1))))))
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

(defun fill-region (mark1 mark2 syntax-line-indentation-function fill-column tab-width syntax
                    &optional (compress-whitespaces t))
  "Fill the region delimited by `mark1' and `mark2'. `Mark1' must be
mark<= `mark2.'"
  (let* ((buffer (buffer mark1)))
    (do-buffer-region (object offset buffer
                              (offset mark1) (offset mark2))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (when (>= (buffer-display-column buffer (offset mark2) tab-width)
              (1- fill-column))
      (fill-line mark2
                 syntax-line-indentation-function
                 fill-column
                 tab-width
                 compress-whitespaces
                 syntax))))

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
		 (when left             ; spaces must follow tabs
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
          while (buffer-whitespacep (object-after mark))
          do (delete-range mark 1))
    (loop until (beginning-of-buffer-p mark)
          while (buffer-whitespacep (object-before mark))
          do (delete-range mark -1))
    (when (and (not (beginning-of-buffer-p mark))
	       (constituentp (object-before mark)))
      (insert-object mark #\Space))))
