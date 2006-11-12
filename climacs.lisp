;;; -*- Mode: Lisp; Package: CLIMACS -*-

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

;;; Entry points for the Climacs editor.

(in-package :climacs)

(defun climacs (&key new-process (process-name "Climacs")
                (width 900) (height 400))
  "Starts up a climacs session"
  (let ((frame (make-application-frame 'climacs :width width :height height)))
    (flet ((run ()
	     (run-frame-top-level frame)))
      (if new-process
	  (clim-sys:make-process #'run :name process-name)
	  (run)))))

(defun climacs-rv (&key new-process (process-name "Climacs")
                (width 900) (height 400))
  "Starts up a climacs session with alternative colors."
  ;; SBCL doesn't inherit dynamic bindings when starting new
  ;; processes, so start a new processes and THEN setup the colors.
  (flet ((run ()
           (let ((*background-color* +black+)
                 (*foreground-color* +gray+)
                 (*info-bg-color* +darkslategray+)
                 (*info-fg-color* +gray+)
                 (*mini-bg-color* +black+)
                 (*mini-fg-color* +white+))
             (climacs :new-process nil :width width :height height))))
    (if new-process
      (clim-sys:make-process #'run :name process-name)
      (run))))
