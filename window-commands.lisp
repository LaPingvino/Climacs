;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

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

;;; Windows commands for the Climacs editor. 

(in-package :climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands for splitting windows

(define-command (com-split-window-vertically :name t :command-table window-table) ()
  (split-window t))

(set-key 'com-split-window-vertically
	 'window-table
	 '((#\x :control) (#\2)))

(define-command (com-split-window-horizontally :name t :command-table window-table) ()
  (split-window))

(set-key 'com-split-window-horizontally
	 'window-table
	 '((#\x :control) (#\3)))

(define-command (com-other-window :name t :command-table window-table) ()
  (other-window))

(set-key 'com-other-window
	 'window-table
	 '((#\x :control) (#\o)))

(defun click-to-offset (window x y)
  (with-slots (top bot) window
       (let ((new-x (floor x (stream-character-width window #\m)))
	     (new-y (floor y (stream-line-height window)))
	     (buffer (buffer window)))
	 (loop for scan from (offset top)
	       with lines = 0
	       until (= scan (offset bot))
	       until (= lines new-y)
	       when (eql (buffer-object buffer scan) #\Newline)
		 do (incf lines)
	       finally (loop for columns from 0
			     until (= scan (offset bot))
			     until (eql (buffer-object buffer scan) #\Newline)
			     until (= columns new-x)
			     do (incf scan))
		       (return scan)))))

(define-command (com-switch-to-this-window :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (other-window window)
  (when (buffer-pane-p window)
    (setf (offset (point window))
	  (click-to-offset window x y))))

(define-presentation-to-command-translator blank-area-to-switch-to-this-window
    (blank-area com-switch-to-this-window window-table
                :echo nil
                ;; Putting the point in typeout-panes can cause errors.
                :tester ((object presentation)
                         (declare (ignore presentation))
                         (not (typep object 'typeout-pane))))
    (window x y)
  (list window x y))

(define-gesture-name :select-other :pointer-button (:right) :unique nil)

(define-command (com-mouse-save :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (and (buffer-pane-p window)
	     (eq window (current-window)))
    (setf (offset (mark window))
	  (click-to-offset window x y))
    (drei-commands::com-exchange-point-and-mark)
    (drei-commands::com-copy-region)))

(define-presentation-to-command-translator blank-area-to-mouse-save
    (blank-area com-mouse-save window-table :echo nil :gesture :select-other)
    (window x y)
  (list window x y))

(define-gesture-name :middle-button :pointer-button (:middle) :unique nil)

(define-command (com-yank-here :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (buffer-pane-p window)
    (other-window window)
    (setf (offset (point window))
	  (click-to-offset window x y))
    (drei-commands::com-yank)))

(define-presentation-to-command-translator blank-area-to-yank-here
    (blank-area com-yank-here window-table :echo nil :gesture :middle-button)
    (window x y)
  (list window x y))

(defun single-window ()
  (loop until (null (cdr (windows *application-frame*)))
	do (rotatef (car (windows *application-frame*))
		    (cadr (windows *application-frame*)))
	   (com-delete-window))
  (setf *standard-output* (car (windows *application-frame*))))

(define-command (com-single-window :name t :command-table window-table) ()
  (single-window))

(set-key 'com-single-window
	 'window-table
	 '((#\x :control) (#\1)))

(define-command (com-scroll-other-window :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-down other-window))))

(set-key 'com-scroll-other-window
	 'window-table
	 '((#\v :control :meta)))

(define-command (com-scroll-other-window-up :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-up other-window))))

(set-key 'com-scroll-other-window-up
	 'window-table
	 '((#\V :control :meta :shift)))

(define-command (com-delete-window :name t :command-table window-table) ()
  (delete-window))

(set-key 'com-delete-window
	 'window-table
	 '((#\x :control) (#\0)))

