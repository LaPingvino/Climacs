;;; -*- Mode: Lisp; Package: CLIMACS-EDITING; -*-

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

;;; Climacs editing

;;; See information in motion.lisp
;;;
;;; An editing function is a function named FORWARD-<frob>-<unit> or
;;; BACKWARD-<frob>-<unit>, or just <frob>-<unit> in the case where
;;; discering between forward and backward commands does not make
;;; sense (an example is TRANSPOSE-<unit>).
;;;
;;; A proper unit is a unit for which all the functions required by
;;; the motion protocol has been implemented, this can be trivially
;;; done by using the macro CLIMACS-MOTION:DEFINE-MOTION-COMMANDS.
;;;
;;; Given a proper unit,
;;;
;;;   (DEFINE-EDIT-FNS <unit>)
;;;
;;; defines the editing functions FORWARD-DELETE-<unit>,
;;; BACKWARD-DELETE-<unit>, FORWARD-KILL-<unit>, BACKWARD-KILL-<unit>
;;; and TRANSPOSE-<unit>.
;;;
;;; This file also holds definitions for other miscellaneus
;;; editing-related functions. The definitions in this file should
;;; have to do with immediate editing, understood as insertion,
;;; deletion or movement of buffer contents. Transformation of buffer
;;; contents (such as converting the case of a region) should not be
;;; here (FIXME: It actually is. Moving it to misc-commands.lisp is
;;; tempting, but I don't like putting too much non-command-related
;;; into the command files Perhaps the solution is a
;;; transformation.lisp?).

(in-package :climacs-editing)

(defmacro define-edit-fns (unit &key plural)
  (labels ((symbol (&rest strings)
             (intern (apply #'concat strings)))
           (concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings))))
    (let* ((unit-name (string-downcase unit))
           (plural (or plural (concat unit-name "s")))
           (upper-plural (string-upcase plural))
           (forward-delete (symbol "FORWARD-DELETE-" unit))
           (backward-delete (symbol "BACKWARD-DELETE-" unit))
           (forward-kill (symbol "FORWARD-KILL-" unit))
           (backward-kill (symbol "BACKWARD-KILL-" unit))
           (transpose (symbol "TRANSPOSE-" upper-plural))
           (forward (find-symbol (concat "FORWARD-" (string-upcase unit))))
           (backward (find-symbol (concat "BACKWARD-" (string-upcase unit)))))
      (unless (and forward backward)
        (error "The unit ~A is not known." unit))
      `(progn
         (defgeneric ,forward-delete
             (mark syntax &optional count limit-action)
           (:documentation
            ,(concat "Delete COUNT " plural " beginning from MARK.")))
         (defmethod ,forward-delete
             (mark syntax &optional (count 1) limit-action)
           (let ((mark2 (clone-mark mark)))
             (,forward mark2 syntax count limit-action)
             (delete-region mark mark2)))
         (defmethod ,forward-delete :around
             (mark syntax &optional (count 1) limit-action)
           (cond ((minusp count)
                  (,backward-delete mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,backward-delete
             (mark syntax &optional count limit-action)
           (:documentation
            ,(concat "Delete COUNT " plural " backwards beginning from MARK.")))
         (defmethod ,backward-delete
             (mark syntax &optional (count 1) limit-action)
           (let ((mark2 (clone-mark mark)))
             (,backward mark2 syntax count limit-action)
             (delete-region mark mark2)))
         (defmethod ,backward-delete :around
             (mark syntax &optional (count 1) limit-action)
           (cond ((minusp count)
                  (,forward-delete mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,forward-kill
             (mark syntax &optional count concatenate-p limit-action)
           (:documentation
            ,(concat "Kill COUNT " plural " beginning from MARK.")))
         (defmethod ,forward-kill
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (let ((start (offset mark)))
             (,forward mark syntax count limit-action)
             (unless (mark= mark start)
               (if concatenate-p
                   (if (plusp count)
                       (kill-ring-concatenating-push *kill-ring*
                                                     (region-to-sequence start mark))
                       (kill-ring-reverse-concatenating-push *kill-ring*
                                                             (region-to-sequence start mark)))
                   (kill-ring-standard-push *kill-ring*
                                            (region-to-sequence start mark)))
               (delete-region start mark))))
         (defmethod ,forward-kill :around
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (declare (ignore concatenate-p))
           (cond ((minusp count)
                  (,backward-kill mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,backward-kill
             (mark syntax &optional count concatenate-p limit-action)
           (:documentation
            ,(concat "Kill COUNT " plural " backwards beginning from MARK.")))
         (defmethod ,backward-kill
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (let ((start (offset mark)))
             (,backward mark syntax count limit-action)
             (unless (mark= mark start)
               (if concatenate-p
                   (if (plusp count)
                       (kill-ring-concatenating-push *kill-ring*
                                                     (region-to-sequence start mark))
                       (kill-ring-reverse-concatenating-push *kill-ring*
                                                             (region-to-sequence start mark)))
                   (kill-ring-standard-push *kill-ring*
                                            (region-to-sequence start mark)))
               (delete-region start mark))))
         (defmethod ,backward-kill :around
             (mark syntax &optional (count 1) concatenate-p limit-action)
           (declare (ignore concatenate-p))
           (cond ((minusp count)
                  (,forward-kill mark syntax (- count) limit-action))
                 ((plusp count)
                  (call-next-method))
                 (t t)))
         (defgeneric ,transpose
             (mark syntax)
           (:documentation
            ,(concat "Transpose two " plural " at MARK.")))
         (defmethod ,transpose
             (mark syntax)
           (let (start1 end1 start2 end2)
             (,backward mark syntax 1 nil)
             (setf start1 (clone-mark mark))
             (,forward mark syntax 1 #'error-limit-action)
             (setf end1 (clone-mark mark))
             (,forward mark syntax 1 #'error-limit-action)
             (setf end2 (clone-mark mark))
             (,backward mark syntax 1 nil)
             (setf start2 (clone-mark mark))
             (let ((obj1 (buffer-sequence (buffer mark) (offset start1) (offset end1)))
                   (obj2 (buffer-sequence (buffer mark) (offset start2) (offset end2))))
               (,forward-delete mark syntax 1 nil)
               (insert-sequence mark obj1)
               ;; KLUDGE: Having to do this manually is ugly, but it
               ;; is necessary if the motion functions uses syntax
               ;; information.
               (update-syntax (buffer syntax)
                              syntax)
               (,backward mark syntax 2 nil)
               (,forward-delete mark syntax 1 nil)
               (insert-sequence mark obj2)
               (update-syntax (buffer syntax)
                              syntax)
               (,forward mark syntax 1 nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Object editing

(defun transpose-objects (mark)
  (unless (beginning-of-buffer-p mark)
    (when (end-of-line-p mark)
      (backward-object mark))
    (unless (beginning-of-buffer-p mark)
      (let ((object (object-after mark)))
        (delete-range mark)
        (backward-object mark)
        (insert-object mark object)
        (forward-object mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Line editing

(defmethod open-line ((mark left-sticky-mark) &optional (count 1))
  "Create a new line in a buffer after the mark."
  (loop repeat count
     do (insert-object mark #\Newline)))

(defmethod open-line ((mark right-sticky-mark) &optional (count 1))
  "Create a new line in a buffer after the mark."
  (loop repeat count
     do (insert-object mark #\Newline)
        (decf (offset mark))))

(define-edit-fns line)
(define-edit-fns line-start)

;; Autogenerated TRANSPOSE-LINES is not good enough.
(defmethod transpose-lines
    (mark syntax)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (backward-line mark syntax))
  (let* ((bol (offset mark))
	 (eol (progn (end-of-line mark)
		     (offset mark)))
	 (line (buffer-sequence (buffer mark) bol eol)))
    (delete-region bol mark)
    ;; Remove newline at end of line as well.
    (unless (end-of-buffer-p mark)
      (delete-range mark))
    (end-of-line mark)
    (insert-object mark #\Newline)
    (forward-line mark syntax 0)
    (insert-sequence mark line)
    (insert-object mark #\Newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Word editing

(define-edit-fns word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Page editing

(define-edit-fns page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Paragraph editing

(define-edit-fns paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Sentence editing

(define-edit-fns sentence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Expression editing

(define-edit-fns expression)
(define-edit-fns definition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

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
  (let ((syntax (syntax (buffer mark))))
    (loop repeat n
       do (forward-to-word-boundary mark syntax)
       (let ((offset (offset mark)))
         (forward-word mark syntax 1 nil)
         (downcase-region offset mark)))))

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

(defun upcase-word (mark syntax &optional (n 1))
  "Convert the next N words to uppercase, leaving mark after the last word."
  (loop repeat n
     do (forward-to-word-boundary mark syntax)
     (let ((offset (offset mark)))
       (forward-word mark syntax 1 nil)
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
  (let ((syntax (syntax (buffer mark))))
    (loop repeat n
       do (forward-to-word-boundary mark syntax)
       (let ((offset (offset mark)))
         (forward-word mark syntax 1 nil)
         (capitalize-region offset mark)))))

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

(defun delete-indentation (mark syntax)
  (beginning-of-line mark)
  (unless (beginning-of-buffer-p mark)
    (delete-range mark -1)
    (loop until (end-of-buffer-p mark)
          while (whitespacep syntax (object-after mark))
          do (delete-range mark 1))
    (loop until (beginning-of-buffer-p mark)
          while (whitespacep syntax (object-before mark))
          do (delete-range mark -1))
    (when (and (not (beginning-of-buffer-p mark))
	       (constituentp (object-before mark)))
      (insert-object mark #\Space))))

(defun indent-region (pane mark1 mark2)
  "Indent all lines in the region delimited by `mark1' and `mark2'
   according to the rules of the active syntax in `pane'."
  (let* ((buffer (buffer pane))
         (view (stream-default-view pane))
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