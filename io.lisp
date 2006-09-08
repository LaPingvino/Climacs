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

(defmethod frame-save-buffer-to-stream ((application-frame climacs) (buffer climacs-buffer) stream)
  (let ((seq (buffer-sequence buffer 0 (size buffer))))
    (write-sequence seq stream)))

(defun input-from-stream (stream buffer offset)
  (let* ((seq (make-string (file-length stream)))
         (count (#+mcclim read-sequence #-mcclim cl:read-sequence
                          seq stream)))
    (insert-buffer-sequence buffer offset
                            (if (= count (length seq))
                                seq
                                (subseq seq 0 count)))))

(defmethod frame-make-buffer-from-stream ((application-frame climacs) stream)
  (let* ((buffer (make-new-buffer application-frame)))
    (input-from-stream stream buffer 0)
    buffer))
