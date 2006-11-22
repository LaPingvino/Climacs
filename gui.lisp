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

;;; GUI for the Climacs editor. 

(in-package :climacs-gui)

(defvar *default-external-format* :utf-8
  "The encoding to use by default when reading and saving
files.")

(defvar *with-scrollbars* t
  "If T, classic look and feel. If NIL, stripped-down look (:")

(defvar *show-info-pane-mark-position* nil
  "If T, show the line number and column number in the info pane
  of all panes. If NIL, don't. This is off by default, as finding
  the line and column numbers is potentially expensive.")

(defclass climacs-buffer (drei-buffer)
  ((%external-format :initform *default-external-format*
                     :accessor external-format
                     :documentation "The external format that was
used when reading the source destination of the buffer
contents.")))

(defclass climacs-pane (drei-pane esa-pane-mixin)
  ()
  (:default-initargs
   :buffer (make-instance 'climacs-buffer)
    :command-table 'global-climacs-table
    :width 900 :height 400))

;; Ensure that only one pane can be active.
(defmethod (setf active) :after ((new-val (eql t)) (climacs-pane climacs-pane))
  (mapcar #'(lambda (pane)
              (unless (eq climacs-pane pane)
                (setf (active pane) nil)))
          (windows (pane-frame climacs-pane))))

(defmethod command-table ((drei climacs-pane))
  (command-table (pane-frame drei)))

(defclass typeout-pane (application-pane esa-pane-mixin)
  ((%active :accessor active
            :initform nil
            :initarg :active)))

(defmethod buffer ((pane typeout-pane)))

(defmethod point ((pane typeout-pane)))

(defmethod mark ((pane typeout-pane)))

(defmethod full-redisplay ((pane typeout-pane)))

(defgeneric buffer-pane-p (pane)
  (:documentation "Returns T when a pane contains a buffer."))

(defmethod buffer-pane-p (pane)
  (declare (ignore pane))
  nil)

(defmethod buffer-pane-p ((pane climacs-pane))
  t)

(defmethod in-focus-p ((pane climacs-pane))
  (eq pane (first (windows *application-frame*))))

(defvar *info-bg-color* +gray85+)
(defvar *info-fg-color* +black+)
(defvar *mini-bg-color* +white+)
(defvar *mini-fg-color* +black+)

(defclass climacs-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t
      :background *info-bg-color*
      :foreground *info-fg-color*
      :width 900))

(defclass climacs-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :default-view +drei-textual-view+
   :background *mini-bg-color*
   :foreground *mini-fg-color*
   :width 900))

;;; Basic command tables follow. The global command table,
;;; `global-climacs-table', inherits from these, so they should not
;;; contain any overly syntax-specific commands. The idea is that it
;;; should be safe for any syntax to inherit its command-table from
;;; `global-climacs-table' (so the usual movement, search and
;;; navigation-commands are available), without risking adding alien
;;; commands that require the buffer to be in a specific syntax.

;;; Basic functionality
(make-command-table 'base-table :errorp nil)
;;; Buffers
(make-command-table 'buffer-table :errorp nil)
;;; Commands used for climacs development
(make-command-table 'development-table :errorp nil)
;;; Panes
(make-command-table 'pane-table :errorp nil)
;;; Windows
(make-command-table 'window-table :errorp nil)

;;; customization of help.  FIXME: this might be better done by having
;;; the functions that the ESA commands call be customizeable generic
;;; functions; however, while they're not, scribbling over the ESA
;;; command tables is a bad thing.
(make-command-table 'climacs-help-table :inherit-from '(help-table)
                    :errorp nil)

;;; We have a special command table for typeout panes because we want
;;; to keep being able to do window, buffer, etc, management, but we do
;;; not want any actual editing commands.
(make-command-table 'typeout-pane-table
                    :errorp nil
                    :inherit-from '(global-esa-table
                                    base-table
                                    pane-table
                                    window-table
                                    development-table
                                    climacs-help-table))

(defclass climacs-command-table (standard-command-table)
  ())

(defmethod command-table-inherit-from ((table climacs-command-table))
  (append (when *current-syntax* (list (command-table *current-syntax*)))
          '(global-climacs-table)
          (call-next-method)))

(define-application-frame climacs (esa-frame-mixin
				   standard-application-frame)
  ((%buffers :initform '() :accessor buffers)
   (%groups :initform (make-hash-table :test #'equal) :accessor groups)
   (%active-group :initform nil :accessor active-group)
   (%kill-ring :initform (make-instance 'kill-ring :max-size 7) :accessor kill-ring)
   (%command-table :initform (make-instance 'climacs-command-table
                                            :name 'climacs-dispatching-table)
                   :accessor find-applicable-command-table))
  (:command-table (global-climacs-table
                   :inherit-from (esa-io-table
                                  keyboard-macro-table
                                  climacs-help-table
                                  base-table
                                  buffer-table
                                  case-table
                                  development-table
                                  info-table
                                  pane-table
                                  window-table
                                  editor-table
                                  global-esa-table)))
  (:menu-bar nil)
  (:panes
   (climacs-window
    (let* ((climacs-pane (make-pane 'climacs-pane
                                    :active t))
	   (info-pane (make-pane 'climacs-info-pane
                                 :master-pane climacs-pane)))
      (setf (windows *application-frame*) (list climacs-pane)
	    (buffers *application-frame*) (list (buffer climacs-pane)))
      (vertically ()
	(if *with-scrollbars*
	    (scrolling ()
	      climacs-pane)
	    climacs-pane)
	info-pane)))
   (minibuffer (make-pane 'climacs-minibuffer-pane)))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 climacs-window
	 minibuffer)))
  (:top-level ((lambda (frame)
                 (let ((*kill-ring* (kill-ring frame)))
                   (esa-top-level frame :prompt "M-x "))))))

(define-esa-top-level ((frame climacs) command-parser
                       command-unparser
                       partial-command-parser
                       prompt)
    :bindings ((*current-point* (current-point))
               (*current-mark* (current-mark))
               (*previous-command* (previous-command *current-window*))
               (*current-syntax* (and *current-buffer*
                                      (syntax *current-buffer*)))))

(defmethod frame-standard-input ((frame climacs))
  (get-frame-pane frame 'minibuffer))

(defmethod frame-current-buffer ((application-frame climacs))
  "Return the current buffer."
  (buffer (frame-current-window application-frame)))

(defun any-buffer ()
  "Return some buffer, any buffer, as long as it is a buffer!"
  (first (buffers *application-frame*)))

(define-presentation-type read-only ())
(define-presentation-method highlight-presentation 
    ((type read-only) record stream state)
  nil)
(define-presentation-type modified ())
(define-presentation-method highlight-presentation 
    ((type modified) record stream state)
  nil)

(defun display-info (frame pane)
  (let* ((master-pane (master-pane pane))
	 (buffer (buffer master-pane))
	 (size (size buffer))
	 (top (top master-pane))
	 (bot (bot master-pane))
         (point (point master-pane)))
    (princ "   " pane)
    (with-output-as-presentation (pane buffer 'read-only)
      (princ (cond
               ((read-only-p buffer) "%")
               ((needs-saving buffer) "*")
               (t "-"))
             pane))
    (with-output-as-presentation (pane buffer 'modified)
      (princ (cond
               ((needs-saving buffer) "*")
               ((read-only-p buffer) "%")
               (t "-"))
             pane))
    (princ "  " pane)
    (with-text-face (pane :bold)
      (with-output-as-presentation (pane buffer 'buffer)
        (format pane "~A" (name buffer)))
      ;; FIXME: bare 25.
      (format pane "~V@T" (max (- 25 (length (name buffer))) 1)))
    (format pane "  ~A  "
	    (cond ((and (mark= size bot)
			(mark= 0 top))
		   "")
		  ((mark= size bot)
		   "Bot")
		  ((mark= 0 top)
		   "Top")
		  (t (format nil "~a%"
			     (round (* 100 (/ (offset top)
					      size)))))))
    (when *show-info-pane-mark-position*
     (format pane "(~A,~A)     "
             (1+ (line-number point))
             (column-number point)))
    (with-text-family (pane :sans-serif)
      (princ #\( pane)
      (display-syntax-name (syntax buffer) pane :pane (master-pane pane))
      (format pane "~{~:[~*~; ~A~]~}" (list
				       (slot-value master-pane 'overwrite-mode)
				       "Ovwrt"
				       (auto-fill-mode master-pane)
				       "Fill"
				       (isearch-mode master-pane)
				       "Isearch"))
      (princ #\) pane))
    (with-text-family (pane :sans-serif)
      (princ (if (recordingp frame)
		 "Def"
		 "")
	     pane))))

(defmethod execute-frame-command :around ((frame climacs) command)
  (handling-drei-conditions
    (with-undo ((buffers frame))
      (call-next-method))
    (loop for buffer in (buffers frame)
       do (when (modified-p buffer)
            (clear-modify buffer)))))

(defmethod execute-frame-command :after ((frame climacs) command)
  (when (eq frame *application-frame*)
    (loop for buffer in (buffers frame)
       do (when (syntax buffer)
            (update-syntax buffer (syntax buffer)))
       do (when (modified-p buffer)
            (setf (needs-saving buffer) t)))))

(define-command (com-full-redisplay :name t :command-table base-table) ()
  "Redisplay the contents of the current window.
FIXME: does this really have that effect?"
  (full-redisplay (current-window)))

(set-key 'com-full-redisplay
	 'base-table
	 '((#\l :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pane functions

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

(defun make-pane-constellation (&optional (with-scrollbars *with-scrollbars*))
  "make a vbox containing a scroller pane as its first child and an
info pane as its second child.  The scroller pane contains a viewport
which contains an extended pane.  Return the vbox and the extended pane
as two values.
If with-scrollbars nil, omit the scroller."
  (let* ((climacs-pane
	  (make-pane 'climacs-pane :name 'window))
	 (vbox
	  (vertically ()
	    (if with-scrollbars
		(scrolling ()
		  climacs-pane)
		climacs-pane)
	    (make-pane 'climacs-info-pane
                       :master-pane climacs-pane))))
    (values vbox climacs-pane)))

(defgeneric setup-split-pane (orig-pane new-pane)
  (:documentation "Perform split-setup operations `new-pane',
which is supposed to be a pane that has been freshly split from
`orig-pane'."))

(defmethod setup-split-pane ((orig-pane climacs-pane) (new-pane climacs-pane))
  (setf (offset (point (buffer orig-pane))) (offset (point orig-pane))
        (buffer new-pane) (buffer orig-pane)
        (auto-fill-mode new-pane) (auto-fill-mode orig-pane)
        (auto-fill-column new-pane) (auto-fill-column orig-pane)))

(defmethod setup-split-pane ((orig-pane typeout-pane) (new-pane climacs-pane))
  (setf (buffer new-pane) (any-buffer)))

(defun split-window (&optional (vertically-p nil) (pane (current-window)))
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window pane)
	     (constellation-root (find-parent current-window)))
        (setup-split-pane current-window new-pane)
	(push new-pane (windows *application-frame*))
        (setf (active new-pane) t)
	(setf *standard-output* new-pane)
	(replace-constellation constellation-root vbox vertically-p)
	(full-redisplay current-window)
	(full-redisplay new-pane)
	new-pane))))

(defun make-typeout-constellation (&optional label)
  (let* ((typeout-pane
	  (make-pane 'typeout-pane :foreground *foreground-color* :background *background-color*
                     :width 900 :height 400 :display-time nil :name label))
	 (label
	  (make-pane 'label-pane :label label))
	 (vbox
	  (vertically ()
	    (scrolling (:scroll-bar :vertical) typeout-pane) label)))
    (values vbox typeout-pane)))

(defun typeout-window (&optional (label "Typeout") (pane (current-window)))
  "Get a typeout pane labelled `label'. If a pane with this label
already exists, it will be returned. Otherwise, a new pane will
be created."
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (or (find label (windows *application-frame*) :key #'pane-name)
        (multiple-value-bind (vbox new-pane) (make-typeout-constellation label)
          (let* ((current-window pane)
                 (constellation-root (find-parent current-window)))
            (push new-pane (windows *application-frame*))
            (other-window)
            (replace-constellation constellation-root vbox t)
            (full-redisplay current-window)
            new-pane)))))

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

(defun other-window (&optional pane)
  (if (and pane (find pane (windows *application-frame*)))
      (setf (windows *application-frame*)
            (append (list pane)
                    (remove pane (windows *application-frame*))))
      (setf (windows *application-frame*)
            (append (rest (windows *application-frame*))
                    (list (first (windows *application-frame*))))))
  (setf (active (first (windows *application-frame*))) t)
  (setf *standard-output* (first (windows *application-frame*))))

;;; For the ESA help functions.

(defmethod help-stream ((frame climacs) title)
  (typeout-window (format nil "~10T~A" title)))
