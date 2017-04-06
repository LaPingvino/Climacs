;;; -*- Mode: Lisp; Package: CLIMACS-CORE -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
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

;;; Input/Output of buffers to and from streams.

(in-package :climacs-core)

(define-condition buffer-contains-noncharacter (buffer-writing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Buffer ~A contains non-character object"
                     (name (buffer condition)))))
  (:documentation "This error is signalled whenever an attempt is
made to save a buffer that contains a non-character object."))

(defun buffer-contains-noncharacter (buffer filepath)
  "Signal an error of type `buffer-contains-noncharacter' with
the buffer `buffer' and the filepath `filepath'."
  (error 'buffer-contains-noncharacter :buffer buffer :filepath filepath))

(defmethod check-buffer-writability ((application-frame climacs) (filepath pathname)
                                     (buffer drei-buffer))
  (do-buffer-region (object offset buffer 0 (size buffer))
    (unless (characterp object)
      (buffer-contains-noncharacter buffer filepath)))
  (call-next-method))

(defmethod frame-save-buffer-to-stream ((application-frame climacs) (buffer climacs-buffer) stream)
  (let ((seq (buffer-sequence buffer 0 (size buffer))))
    (if (every #'characterp seq)
        (write-sequence seq stream)
        (display-message "Cannot save to file, buffer contains non-character object"))))

(defun input-from-stream (stream buffer offset)
  (let* ((seq (make-string 10)))
    (loop for count = (#+mcclim read-sequence #-mcclim cl:read-sequence
                                seq stream)
       while (> count 0)
       do
         (progn
           (insert-buffer-sequence buffer offset
                                   (if (= count (length seq))
                                       seq
                                       (subseq seq 0 count)))
           (incf offset count)))))

(defmethod frame-make-buffer-from-stream ((application-frame climacs) stream)
  (let* ((buffer (make-new-buffer)))
    (input-from-stream stream buffer 0)
    (clear-undo-history buffer)
    buffer))
