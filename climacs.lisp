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

(defun find-climacs-frame ()
  (let ((frame-manager (find-frame-manager)))
    (when frame-manager
      (find-if (lambda (x) (and (typep x 'climacs) 
                                (eq (clim:frame-state x) :enabled)))
               (frame-manager-frames frame-manager)))))

(defun climacs (&rest args &key new-process (process-name "Climacs")
                (width 900) (height 400))
  "Starts up a climacs session"
  (declare (ignore new-process process-name width height))
  (apply #'climacs-common nil args))

(defun climacs-rv (&rest args &key new-process (process-name "Climacs")
                   (width 900) (height 400))
  "Starts up a climacs session with alternative colors."
  ;; SBCL doesn't inherit dynamic bindings when starting new
  ;; processes, so start a new processes and THEN setup the colors.
  (declare (ignore width height))
  (flet ((run ()
           (let ((*background-color* +black+)
                 (*foreground-color* +gray+)
                 (*info-bg-color* +darkslategray+)
                 (*info-fg-color* +gray+)
                 (*mini-bg-color* +black+)
                 (*mini-fg-color* +white+))
             (apply #'climacs-common nil :new-process nil args))))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run))))

(defun edit-file (thing &rest args
                  &key (process-name "Climacs") (width 900) (height 400))
  "Edit THING in an existing climacs process or start a new one. THING
can be a filename (edit the file) or symbol (edit its function definition)."
  (declare (ignore process-name width height))
  (let ((climacs-frame (find-climacs-frame))
        (command 
         (typecase thing
           (null nil)
           (symbol (list 'drei-lisp-syntax::com-edit-definition thing))
           ((or string pathname)
            (truename thing)  ; raise file-error if file doesn't exist
            (list 'esa-io::com-find-file thing))
           (t (error 'type-error :datum thing
                     :expected-type '(or null string pathname symbol))))))
    (if climacs-frame
        (when command
          (execute-frame-command climacs-frame command))
        (apply #'climacs-common command :new-process t args)))
  t)

(defun climacs-common (command &key new-process (process-name "Climacs")
                       (width 900) (height 400))
  (let* ((frame (make-application-frame 'climacs :width width :height height))
         (*application-frame* frame)
         (esa:*esa-instance* frame))
    (adopt-frame (find-frame-manager) *application-frame*)
    (when command (execute-frame-command *application-frame* command))
    (flet ((run () (run-frame-top-level frame)))
      (if new-process
          (clim-sys:make-process #'run :name process-name)
          (run)))))

;;; Append to end of *ed-functions* so we don't overwrite the user's
;;; preferred editor
#+sbcl
(unless (member 'edit-file sb-ext:*ed-functions*)
  (setf sb-ext:*ed-functions* (append sb-ext:*ed-functions* (list 'edit-file))))
