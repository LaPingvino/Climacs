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

(in-package :climacs-gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands for splitting windows

(defun replace-constellation (constellation additional-constellation vertical-p)
  (let* ((parent (sheet-parent constellation))
	 (children (sheet-children parent))
	 (first (first children))
	 (second (second children))
	 (third (third children))
	 (first-split-p (= (length (sheet-children parent)) 2))
	 (parent-region (sheet-region parent))
	 (parent-height (rectangle-height parent-region))
	 (parent-width (rectangle-width parent-region))
	 (filler (when first-split-p (make-pane 'basic-pane))) ;Prevents resizing.
         (adjust #+mcclim (make-pane 'clim-extensions:box-adjuster-gadget)))
    (assert (member constellation children))
    
    (when first-split-p (setf (sheet-region filler) (sheet-region parent)) 
      (sheet-adopt-child parent filler))

    (sheet-disown-child parent constellation)

    (if vertical-p
	(resize-sheet constellation parent-width (/ parent-height 2))
	(resize-sheet constellation  (/ parent-width 2) parent-height))
    
    (let ((new (if vertical-p
		   (vertically ()
		     constellation adjust additional-constellation)
		   (horizontally ()
		     constellation adjust additional-constellation))))
      (sheet-adopt-child parent new)

      (when first-split-p (sheet-disown-child parent filler))
      (reorder-sheets parent 
		      (if (eq constellation first)
			  (if third
			      (list new second third)
			      (list new second))
			  (if third
			      (list first second new)
			      (list first new)))))))

(defun find-parent (sheet)
  (loop for parent = (sheet-parent sheet)
	  then (sheet-parent parent)
	until (typep parent 'vrack-pane)
	finally (return parent)))

(defclass typeout-pane (application-pane esa-pane-mixin) ())

(defun make-typeout-constellation (&optional label)
  (let* ((typeout-pane
	  (make-pane 'typeout-pane :foreground *fg-color* :background *bg-color*
		     :width 900 :height 400 :display-time nil))
	 (label
	  (make-pane 'label-pane :label label))
	 (vbox
	  (vertically ()
	    (scrolling (:scroll-bar :vertical) typeout-pane) label)))
    (values vbox typeout-pane)))

(defun typeout-window (&optional (label "Typeout") (pane (current-window)))
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-typeout-constellation label)
      (let* ((current-window pane)
	     (constellation-root (find-parent current-window)))
	(push new-pane (windows *application-frame*))
	(other-window)
	(replace-constellation constellation-root vbox t)
	(full-redisplay current-window)
	new-pane))))

(define-command (com-describe-bindings :name t :command-table help-table)
    ((sort-by-keystrokes 'boolean :prompt "Sort by keystrokes?"))
  (let* ((window (current-window))
	 (buffer (buffer (current-window)))
	 (stream (typeout-window
		   (format nil "~10THelp: Describe Bindings for ~A" (name buffer))))
	 (command-table (command-table window)))
    (esa::describe-bindings stream command-table
		       (if sort-by-keystrokes
			   #'esa::sort-by-keystrokes
			   #'esa::sort-by-name))))

(set-key `(com-describe-bindings ,*numeric-argument-p*) 'help-table '((#\h :control) (#\b)))

(defun make-pane-constellation (&optional (with-scrollbars *with-scrollbars*))
  "make a vbox containing a scroller pane as its first child and an
info pane as its second child.  The scroller pane contains a viewport
which contains an extended pane.  Return the vbox and the extended pane
as two values.
If with-scrollbars nil, omit the scroller."
  (let* ((extended-pane
	  (make-pane 'extended-pane
		     :width 900 :height 400
		     :name 'window
		     :end-of-line-action :scroll
		     :incremental-redisplay t
		     :background *bg-color*
		     :foreground *fg-color*
		     :display-function 'display-window
		     :command-table 'global-climacs-table))
	 (vbox
	  (vertically ()
	    (if with-scrollbars
		(scrolling ()
		  extended-pane)
		extended-pane)
	    (make-pane 'climacs-info-pane
		       :background *info-bg-color*
		       :foreground *info-fg-color*
		       :master-pane extended-pane
		       :width 900))))
    (values vbox extended-pane)))

(defun split-window (&optional (vertically-p nil) (pane (current-window)))
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window pane)
	     (constellation-root (find-parent current-window)))
        (setf (offset (point (buffer current-window))) (offset (point current-window))
	      (buffer new-pane) (buffer current-window)
              (auto-fill-mode new-pane) (auto-fill-mode current-window)
              (auto-fill-column new-pane) (auto-fill-column current-window))
	(push new-pane (windows *application-frame*))
	(setf *standard-output* new-pane)
	(replace-constellation constellation-root vbox vertically-p)
	(full-redisplay current-window)
	(full-redisplay new-pane)
	new-pane))))

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

(defun other-window (&optional pane)
  (if (and pane (find pane (windows *application-frame*)))
      (setf (windows *application-frame*)
	    (append (list pane)
		    (remove pane (windows *application-frame*))))
      (setf (windows *application-frame*)
	    (append (cdr (windows *application-frame*))
		    (list (car (windows *application-frame*))))))
  (setf *standard-output* (car (windows *application-frame*))))
  
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
    (blank-area com-switch-to-this-window window-table :echo nil)
    (window x y)
  (list window x y))

(define-gesture-name :select-other :pointer-button (:right) :unique nil)

(define-command (com-mouse-save :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (and (buffer-pane-p window)
	     (eq window (current-window)))
    (setf (offset (mark window))
	  (click-to-offset window x y))
    (com-exchange-point-and-mark)
    (com-copy-region)))

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
    (com-yank)))

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

(defun delete-window (&optional (window (current-window)))
  (unless (null (cdr (windows *application-frame*)))
    (let* ((constellation (find-parent window))
	   (box (sheet-parent constellation))
	   (box-children (sheet-children box))
	   (other (if (eq constellation (first box-children))
		      (third box-children)
		      (first box-children)))
	   (parent (sheet-parent box))
	   (children (sheet-children parent))
	   (first (first children))
	   (second (second children))
	   (third (third children)))
      (setf (windows *application-frame*)
	    (remove window (windows *application-frame*)))
      (setf *standard-output* (car (windows *application-frame*)))
      (sheet-disown-child box other)
      (sheet-adopt-child parent other)
      (sheet-disown-child parent box)
      (reorder-sheets parent (if (eq box first)
				 (if third
				     (list other second third)
				     (list other second))
				 (if third
				     (list first second other)
				     (list first other)))))))

(define-command (com-delete-window :name t :command-table window-table) ()
  (delete-window))

(set-key 'com-delete-window
	 'window-table
	 '((#\x :control) (#\0)))

