;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
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

;;; The CLIM pane used for displaying Climacs objects

(in-package :climacs-pane)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tabify

(defvar *use-tabs-for-indentation* nil
  "If non-NIL, use tabs when indenting lines. Otherwise, use spaces.")

(defgeneric space-width (tabify))
(defgeneric tab-width (tabify))
(defgeneric tab-space-count (tabify))

(defclass tabify-mixin ()
  ((space-width :initform nil :reader space-width)
   (tab-width :initform nil :reader tab-width)))

(defmethod tab-space-count ((tabify t))
  1)

(defmethod tab-space-count ((tabify tabify-mixin))
  (round (tab-width tabify) (space-width tabify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo

(defclass undo-mixin ()
  ((tree :initform (make-instance 'standard-undo-tree) :reader undo-tree)
   (undo-accumulate :initform '() :accessor undo-accumulate)
   (performing-undo :initform nil :accessor performing-undo)))

(defclass climacs-undo-record (standard-undo-record)
  ((buffer :initarg :buffer)))

(defclass simple-undo-record (climacs-undo-record)
  ((offset :initarg :offset :reader undo-offset)))

(defclass insert-record (simple-undo-record)
  ((objects :initarg :objects)))

(defclass delete-record (simple-undo-record)
  ((length :initarg :length)))

(defclass compound-record (climacs-undo-record)
  ((records :initform '() :initarg :records)))

(defmethod print-object  ((object delete-record) stream)
  (with-slots (offset length) object
     (format stream "[offset: ~a length: ~a]" offset length)))

(defmethod print-object  ((object insert-record) stream)
  (with-slots (offset objects) object
     (format stream "[offset: ~a objects: ~a]" offset objects)))

(defmethod print-object  ((object compound-record) stream)
  (with-slots (records) object
     (format stream "[records: ~a]" records)))

(defmethod insert-buffer-object :before ((buffer undo-mixin) offset object)
  (declare (ignore object))
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
	     :buffer buffer :offset offset :length 1)
	  (undo-accumulate buffer))))

(defmethod insert-buffer-sequence :before ((buffer undo-mixin) offset sequence)
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
	     :buffer buffer :offset offset :length (length sequence))
	  (undo-accumulate buffer))))

(defmethod delete-buffer-range :before ((buffer undo-mixin) offset n)
  (unless (performing-undo buffer)
    (push (make-instance 'insert-record
	     :buffer buffer :offset offset
	     :objects (buffer-sequence buffer offset (+ offset n)))
	  (undo-accumulate buffer))))

(defmacro with-undo ((get-buffers-exp) &body body)
  "Evaluate `body', registering any changes to buffer contents in
the undo memory for the respective buffer, permitting individual
undo for each buffer. `get-buffers-exp' should be a form, that
will be evaluated whenever a complete list of buffers is
needed (to set up all buffers to prepare for undo, and to check
them all for changes after `body' has run)."
  (let ((buffer-sym (gensym)))
   `(progn
      (dolist (,buffer-sym ,get-buffers-exp)
        (setf (undo-accumulate ,buffer-sym) '()))
      (unwind-protect (progn ,@body)
        (dolist (,buffer-sym ,get-buffers-exp)
          (cond ((null (undo-accumulate ,buffer-sym)) nil)
                ((null (cdr (undo-accumulate ,buffer-sym)))
                 (add-undo (car (undo-accumulate ,buffer-sym))
                           (undo-tree ,buffer-sym)))
                (t
                 (add-undo (make-instance 'compound-record
                                          :buffer ,buffer-sym
                                          :records (undo-accumulate ,buffer-sym))
                           (undo-tree ,buffer-sym)))))))))

(defmethod flip-undo-record :around ((record climacs-undo-record))
  (with-slots (buffer) record
     (let ((performing-undo (performing-undo buffer)))
       (setf (performing-undo buffer) t)
       (unwind-protect (call-next-method)
	 (setf (performing-undo buffer) performing-undo)))))

(defmethod flip-undo-record ((record insert-record))
  (with-slots (buffer offset objects) record
     (change-class record 'delete-record
		   :length (length objects))
     (insert-buffer-sequence buffer offset objects)))

(defmethod flip-undo-record ((record delete-record))
  (with-slots (buffer offset length) record
     (change-class record 'insert-record
		   :objects (buffer-sequence buffer offset (+ offset length)))
     (delete-buffer-range buffer offset length)))

(defmethod flip-undo-record ((record compound-record))
  (with-slots (records) record
     (mapc #'flip-undo-record records)
     (setf records (nreverse records))))

;;; undo-mixin delegation (here because of the package)

(defmethod undo-tree ((buffer delegating-buffer))
  (undo-tree (implementation buffer)))

(defmethod undo-accumulate ((buffer delegating-buffer))
  (undo-accumulate (implementation buffer)))

(defmethod (setf undo-accumulate) (object (buffer delegating-buffer))
  (setf (undo-accumulate (implementation buffer)) object))

(defmethod performing-undo ((buffer delegating-buffer))
  (performing-undo (implementation buffer)))

(defmethod (setf performing-undo) (object (buffer delegating-buffer))
  (setf (performing-undo (implementation buffer)) object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Isearch

(defclass isearch-state ()
  ((search-string :initarg :search-string :accessor search-string)
   (search-mark :initarg :search-mark :accessor search-mark)
   (search-forward-p :initarg :search-forward-p :accessor search-forward-p)
   (search-success-p :initarg :search-success-p :accessor search-success-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Query replace

(defclass query-replace-state ()
  ((string1 :initarg :string1 :accessor string1)
   (string2 :initarg :string2 :accessor string2)
   (buffers :initarg :buffers :accessor buffers)
   (mark :initarg :mark :accessor mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readonly

(defclass read-only-mixin ()
     ((read-only-p :initform nil :accessor read-only-p)))

(define-condition buffer-read-only (simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report (lambda (condition stream)
	     (format stream "Attempt to change read only buffer: ~a"
		     (condition-buffer condition))))
  (:documentation "This condition is signalled whenever an attempt
is made to alter a buffer which has been set read only."))

(defmethod insert-buffer-object ((buffer read-only-mixin) offset object)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod insert-buffer-sequence ((buffer read-only-mixin) offset sequence)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod delete-buffer-range ((buffer read-only-mixin) offset n)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod (setf buffer-object) (object (buffer read-only-mixin) offset)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod read-only-p ((buffer delegating-buffer))
  (read-only-p (implementation buffer)))

(defmethod (setf read-only-p) (flag (buffer delegating-buffer))
  (setf (read-only-p (implementation buffer)) flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View

(defclass climacs-textual-view (textual-view tabify-mixin)
  ())

(defparameter +climacs-textual-view+ (make-instance 'climacs-textual-view))

;(defgeneric indent-tabs-mode (climacs-buffer))

(defclass extended-standard-buffer (read-only-mixin standard-buffer undo-mixin abbrev-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass extended-binseq2-buffer (read-only-mixin binseq2-buffer p-undo-mixin abbrev-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass climacs-buffer (delegating-buffer esa-buffer-mixin)
  ((needs-saving :initform nil :accessor needs-saving)
   (syntax :accessor syntax)
   (point :initform nil :initarg :point :accessor point)
   (indent-tabs-mode :initarg indent-tabs-mode
                     :initform *use-tabs-for-indentation*
                     :accessor indent-tabs-mode))
  (:default-initargs
   :name "*scratch*"
   :implementation (make-instance 'extended-standard-buffer)))

(defmethod initialize-instance :after ((buffer climacs-buffer) &rest args)
  (declare (ignore args))
  (with-slots (syntax point) buffer
     (setf syntax (make-instance
		   *default-syntax* :buffer (implementation buffer))
	   point (clone-mark (low-mark buffer) :right))))

(defmethod (setf syntax) :after (syntax (buffer climacs-buffer))
  (setf (offset (low-mark buffer)) 0
        (offset (high-mark buffer)) (size buffer)))

(defclass climacs-pane (application-pane)
  ((buffer :initform (make-instance 'climacs-buffer) :accessor buffer)
   (point :initform nil :initarg :point :accessor point)
   (mark :initform nil :initarg :mark :accessor mark)
   (top :reader top)
   (bot :reader bot)
   (scan :reader scan)
   (cursor-x :initform 2)
   (cursor-y :initform 2)
   (space-width :initform nil)
   (tab-width :initform nil)
   (auto-fill-mode :initform nil :accessor auto-fill-mode)
   (auto-fill-column :initform 70 :accessor auto-fill-column)
   (isearch-mode :initform nil :accessor isearch-mode)
   (isearch-states :initform '() :accessor isearch-states)
   (isearch-previous-string :initform nil :accessor isearch-previous-string)
   (query-replace-mode :initform nil :accessor query-replace-mode)
   (query-replace-state :initform nil :accessor query-replace-state)
   (region-visible-p :initform nil :accessor region-visible-p)
   (full-redisplay-p :initform nil :accessor full-redisplay-p))
  (:default-initargs
   :default-view +climacs-textual-view+))

(defmethod tab-width ((pane climacs-pane))
  (tab-width (stream-default-view pane)))

(defmethod space-width ((pane climacs-pane))
  (space-width (stream-default-view pane)))

(defmethod initialize-instance :after ((pane climacs-pane) &rest args)
  (declare (ignore args))
  (with-slots (buffer point mark) pane
     (setf point (clone-mark (point buffer)))
     (when (null point)
       (setf point (clone-mark (low-mark buffer) :right)))
     (when (null mark)
       (setf mark (clone-mark (low-mark buffer) :right))))
  (with-slots (buffer top bot scan) pane
     (setf top (clone-mark (low-mark buffer) :left)
	   bot (clone-mark (high-mark buffer) :right)))
  #-(and)
  (with-slots (space-width tab-width) (stream-default-view pane)
     (let* ((medium (sheet-medium pane))
	    (style (medium-text-style medium)))
       (setf space-width (text-style-width style medium)
	     tab-width (* 8 space-width)))))

(defmethod note-sheet-grafted :around ((pane climacs-pane))
  (call-next-method)
  (with-slots (space-width tab-width) (stream-default-view pane)
     (let ((medium (sheet-medium pane)))
       (setf (medium-text-style medium) (medium-default-text-style medium))
       (let ((style (medium-text-style medium)))
	 (setf space-width (text-style-width style medium)
	       tab-width (* 8 space-width))))))


(defmethod (setf buffer) :after (buffer (pane climacs-pane))
  (with-slots (point mark top bot) pane
       (setf point (clone-mark (point buffer))
	     mark (clone-mark (low-mark buffer) :right)
	     top (clone-mark (low-mark buffer) :left)
	     bot (clone-mark (high-mark buffer) :right))))

;; FIXME: Move this somewhere else.
(define-presentation-type url ()
  :inherit-from 'string)

(defun nb-lines-in-pane (pane)
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (multiple-value-bind (x y w h) (bounding-rectangle* pane)
      (declare (ignore x y w))
      (max 1 (floor h (+ height (stream-vertical-spacing pane)))))))

;;; make the region on display fit the size of the pane as closely as
;;; possible by adjusting bot leaving top intact.
(defun adjust-pane-bot (pane)
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot) pane
       (setf (offset bot) (offset top))
       (end-of-line bot)
       (loop until (end-of-buffer-p bot)
	     repeat (1- nb-lines-in-pane)
	     do (forward-object bot)
		(end-of-line bot)))))

;;; Try to put point close to the middle of the pane by moving top
;;; half a pane-size up.
(defun reposition-pane (pane)
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top) pane
      (setf (offset top) (offset (point pane)))
      (loop do (beginning-of-line top)
         repeat (floor nb-lines-in-pane 2)
         until (beginning-of-buffer-p top)
         do (decf (offset top))
         (beginning-of-line top)))))

;; Adjust the bottom and top marks of the pane to be correct, and
;; reposition the pane if point is outside the visible area.
(defun adjust-pane (pane)
  (let* ((buffer (buffer pane))
	 (low-mark (low-mark buffer))
	 (nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot) pane
      (beginning-of-line top)
      (end-of-line bot)
      (when (or (mark< (point pane) top)
                (>= (number-of-lines-in-region top (point pane)) nb-lines-in-pane)
                (mark< low-mark top))
        (reposition-pane pane))))
  (adjust-pane-bot pane))

(defun page-down (pane)
  (with-slots (top bot) pane
     (when (mark> (size (buffer bot)) bot)
       (setf (offset top) (offset bot))
       (beginning-of-line top)
       (setf (offset (point pane)) (offset top)))))

(defun page-up (pane)
  (with-slots (top bot) pane
     (when (> (offset top) 0)
       (let ((nb-lines-in-region (number-of-lines-in-region top bot)))
	 (setf (offset bot) (offset top))
	 (end-of-line bot)
	 (loop repeat  nb-lines-in-region
	       while (> (offset top) 0)
	       do (decf (offset top))
		  (beginning-of-line top))
	 (setf (offset (point pane)) (offset top))
	 (setf (offset (point pane)) (offset bot))
	 (beginning-of-line (point pane))))))

(defgeneric fix-pane-viewport (pane))

(defmethod fix-pane-viewport ((pane climacs-pane))
  (change-space-requirements
   pane
   :min-width (bounding-rectangle-width (stream-current-output-record pane))
   :max-height (bounding-rectangle-width (or (pane-viewport pane) pane))))

(defgeneric redisplay-pane (pane current-p))

(defmethod redisplay-pane ((pane climacs-pane) current-p)
  (if (full-redisplay-p pane)
      (progn (reposition-pane pane)
	     (adjust-pane-bot pane)
	     (setf (full-redisplay-p pane) nil))
      (adjust-pane pane))
  (update-syntax-for-display (buffer pane) (syntax (buffer pane)) (top pane) (bot pane))
  (redisplay-pane-with-syntax pane (syntax (buffer pane)) current-p)
  (fix-pane-viewport pane))

(defgeneric full-redisplay (pane))

(defmethod full-redisplay ((pane climacs-pane))
  (setf (full-redisplay-p pane) t))

(defgeneric display-cursor (pane syntax current-p))

(defgeneric display-region (pane syntax))

(defun offset-to-screen-position (offset pane)
  "Returns the position of offset as a screen position.
Returns X Y LINE-HEIGHT CHAR-WIDTH as values if offset is on the screen,
NIL if offset is before the beginning of the screen,
and T if offset is after the end of the screen."
  (with-slots (top bot) pane
     (cond
       ((< offset (offset top)) nil)
       ((< (offset bot) offset) t)
       (t
	(let* ((line (number-of-lines-in-region top offset))
	       (style (medium-text-style pane))
	       (style-width (text-style-width style pane))
	       (ascent (text-style-ascent style pane))
	       (descent (text-style-descent style pane))
	       (height (+ ascent descent))
	       (y (+ (* line (+ height (stream-vertical-spacing pane)))))
	       (column 
		(buffer-display-column
		 (buffer pane) offset
		 (round (tab-width pane) (space-width pane))))
	       (x (* column style-width)))
	  (values x y height style-width))))))