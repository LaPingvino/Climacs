;;; -*- Mode: Lisp; Package: CLIMACS-BASE -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004 by
;;;           Elliott Johnson (ejohnson@fasl.info)

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

;;; Basic functionality built on top of the buffer protocol.  Here is
;;; where we define slightly higher level functions such as
;;; {previous,next}-line, {forward,backward}-word, etc. that can be
;;; directly implemented in terms of the buffer protocol, but that are
;;; not, strictly speaking, part of that protocol.

(in-package :climacs-base)

(defun previous-line (mark)
  "Move a mark up one line conserving horizontal position."
  (let ((column (column-number mark)))
    (beginning-of-line mark)
    (if (beginning-of-buffer-p mark)
	(incf (offset mark) column)
	(progn (decf (offset mark))
	       (when (> (column-number mark) column)
		 (beginning-of-line mark)
		 (incf (offset mark) column))))))

(defun next-line (mark)
  "Move a mark down one line conserving horizontal position."
  (let ((column (column-number mark)))
    (end-of-line mark)
    (if (end-of-buffer-p mark)
	(progn (beginning-of-line mark)
	       (incf (offset mark) column))
	(progn (incf (offset mark))
	       (end-of-line mark)
	       (when (> (column-number mark) column)
		 (beginning-of-line mark)
		 (incf (offset mark) column))))))

(defun open-line (mark)
  "Create a new line in a buffer."
  (insert-object mark #\Newline)
  (decf (offset mark)))

(defun kill-line (mark)
  "Remove a line from a buffer."
  (if (end-of-line-p mark)
      (unless (end-of-buffer-p mark)
	(delete-range mark))
      (let ((offset (offset mark)))
	(end-of-line mark)
	(delete-region offset mark))))

(defun constituentp (obj)
  "A predicate to ensure that an object is a constituent character."
  (and (characterp obj)
       #+sbcl (sb-impl::constituentp obj)
       #-sbcl (alphanumericp obj)))

(defun forward-word (mark)
  "Forward the mark to the next word."
  (loop until (end-of-buffer-p mark)
	until (constituentp (object-after mark))
	do (incf (offset mark)))
  (loop until (end-of-buffer-p mark)
	while (constituentp (object-after mark))
	do (incf (offset mark))))

(defun backward-word (mark)
  "Shuttle the mark to the start of the previous word."
  (loop until (beginning-of-buffer-p mark)
	until (constituentp (object-before mark))
	do (decf (offset mark)))
  (loop until (beginning-of-buffer-p mark)
	while (constituentp (object-before mark))
	do (decf (offset mark))))

