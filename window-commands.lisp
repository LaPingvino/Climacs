;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2007 by
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

;;; Windows commands for the Climacs editor. 

(in-package :climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands for splitting windows

(defun split-window-maybe-cloning (vertically-p clone-current-view-p)
  "Split `(current-window)', vertically if `vertically-p' is true,
horizontally otherwise. If `clone-current-view-p' is true, use a
clone of `(current-view)' for the new window."
  (handler-bind ((view-already-displayed
                  #'(lambda (condition)
                      (declare (ignore condition))
                      ;; If this happens, `clone-current-view-p' is false.
                      (display-message "Can't split: no view available for new window")
                      (return-from split-window-maybe-cloning nil))))
    (split-window vertically-p clone-current-view-p)))

(define-command (com-split-window-vertically :name t
                                             :command-table window-table)
    ((clone-current-view 'boolean :default nil))
  (split-window-maybe-cloning t clone-current-view))

(set-key `(com-split-window-vertically ,*numeric-argument-marker*)
	 'window-table
	 '((#\x :control) (#\2)))

(define-command (com-split-window-horizontally :name t
                                               :command-table window-table)
    ((clone-current-view 'boolean :default nil))
  (split-window-maybe-cloning nil clone-current-view))

(set-key `(com-split-window-horizontally ,*numeric-argument-marker*)
	 'window-table
	 '((#\x :control) (#\3)))

(define-command (com-other-window :name t :command-table window-table) ()
  (other-window))

(set-key 'com-other-window
	 'window-table
	 '((#\x :control) (#\o)))

(defun click-to-offset (window x y)
  (with-accessors ((top top) (bot bot)) (view window)
    (let ((new-x (floor x (stream-character-width window #\m)))
          (new-y (floor y (stream-line-height window)))
          (buffer (buffer (view window))))
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
  (when (and (buffer-pane-p window)
             (typep (view window) 'point-mark-view))
    (setf (offset (point (view window)))
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
    (setf (offset (mark (view window)))
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
    (setf (offset (point (view window)))
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

(defun scroll-typeout-window (window y)
  "Scroll `window' down by `y' device units, but taking care not
to scroll past the size of `window'. If `window' does not have a
viewport, do nothing."
  (let ((viewport (pane-viewport window)))
    (unless (null viewport)            ; Can't scroll without viewport
      (multiple-value-bind (x-displacement y-displacement)
          (transform-position (sheet-transformation window) 0 0)
        (scroll-extent window
                       (- x-displacement)
                       (max 0 (min (+ (- y-displacement) y)
                                   (- (bounding-rectangle-height window)
                                      (bounding-rectangle-height viewport)))))))))

(define-command (com-scroll-other-window :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (if (typeout-pane-p other-window)
          (scroll-typeout-window other-window (bounding-rectangle-height (pane-viewport other-window)))
          (page-down (view other-window))))))

(set-key 'com-scroll-other-window
	 'window-table
	 '((#\v :control :meta)))

(define-command (com-scroll-other-window-up :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (if (typeout-pane-p other-window)
          (scroll-typeout-window other-window (- (bounding-rectangle-height (pane-viewport other-window))))
          (page-up (view other-window))))))

(set-key 'com-scroll-other-window-up
	 'window-table
	 '((#\V :control :meta :shift)))

(define-command (com-delete-window :name t :command-table window-table) ()
  (delete-window))

(set-key 'com-delete-window
	 'window-table
	 '((#\x :control) (#\0)))

