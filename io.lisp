;;; -*- Mode: Lisp; Package: CLIMACS-BUFFER -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :climacs-base)

(defun input-from-stream (stream buffer offset)
  (let ((eof-object (cons nil nil)))
    (loop for obj = (read-char stream nil eof-object)
	  until (eq obj eof-object)
	  do (insert-buffer-object buffer offset obj)
	     (incf offset))))

(defun output-to-stream (stream buffer offset1 offset2)
  (loop for offset from offset1 below offset2
	when (characterp (buffer-object buffer offset))
	  do (write-char (buffer-object buffer offset) stream)))
