;;; -*- Mode: Lisp; Package: CLIMACS-CORE -*-

;;;  (c) copyright 2008 by
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

;;; Typeout pane support.

(in-package :climacs-gui)

(defclass typeout-view (drei-view textual-view)
  ((%output-history :accessor output-history
                    :initform (make-instance 'standard-tree-output-record)
                    :initarg :output-history
                    :documentation "The output record history
that will be replayed whenever the views contents are shown.")
   (%dirty :accessor dirty
           :initform t
           :documentation "This value indicates whether the
output has changed since it was last replayed.")
   (%cursor-position :accessor last-cursor-position
                     :initform nil
                     :documentation "A list (X Y) specifying
where drawing ended the last time, and where it should start the
next time. If NIL, no previous position has been recorded."))
  (:metaclass modual-class)
  (:documentation "A noneditable Drei view displaying an output
record history."))

(defun typeout-view-p (view)
  "Return true if `view' is a typeout view, false otherwise."
  (typep view 'typeout-view))

(defmethod handle-redisplay ((pane drei-pane) (view typeout-view) (region region))
  (if (and (not (dirty view))
           (find (output-history view)
                 (output-record-children (stream-output-history pane))))
      (replay (stream-output-history pane) pane region)
      (call-next-method)))

(defmethod display-drei-view-contents ((pane pane) (view typeout-view))
  (when (or (dirty view)
            (not (eq (output-record-parent (output-history view))
                     (stream-output-history pane))))
    (with-output-recording-options (pane :record nil :draw t)
      (with-bounding-rectangle* (x1 y1 x2 y2) (or (pane-viewport-region pane)
                                                  (sheet-region pane))
        (draw-rectangle* pane x1 y1 x2 y2 :ink +background-ink+))
      (replay-output-record (output-history view) pane
                            (or (pane-viewport-region pane)
                                (sheet-region pane))))
    (unless (eq (output-record-parent (output-history view))
                (stream-output-history pane))
      (setf (output-record-parent (output-history view)) nil)
      (add-output-record (output-history view) (stream-output-history pane))))
  (setf (dirty view) nil))

(defmethod bounding-rectangle* ((view typeout-view))
  (if (output-history view)
      (bounding-rectangle* (output-history view))
      (values 0 0 0 0)))

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

(defmethod page-down ((pane sheet) (view typeout-view))
  (scroll-typeout-window pane (bounding-rectangle-height (pane-viewport pane))))

(defmethod page-up ((pane sheet) (view typeout-view))
  (scroll-typeout-window
   pane (- (bounding-rectangle-height (pane-viewport pane)))))

(defun ensure-typeout-view (climacs label)
  "Ensure that `climacs' has a typeout view with the name
`label', and return that view."
  (check-type label string)
  (or (find-if #'(lambda (view)
                   (and (typeout-view-p view)
                        (string= (name view) label)))
               (views climacs))
      (make-new-view-for-climacs climacs 'typeout-view
       :name label)))

;; Because specialising on the type of `climacs' is so useful...
(defun invoke-with-typeout (climacs label continuation)
  "Call `continuation' with a single argument, a
stream meant for typeout. `Climacs' is the Climacs instance in
which the typeout pane should be shown, and `label' is the name
of the created typeout view. Returns NIL."
  (let* ((typeout-view (ensure-typeout-view climacs label))
         (pane-with-typeout (or (find typeout-view (windows climacs)
                                 :key #'view)
                                (let ((pane (split-window t)))
                                  (setf (view pane) typeout-view)
                                  pane))))
    (let ((new-record (with-output-to-output-record (pane-with-typeout)
                        (with-output-recording-options (pane-with-typeout :record t :draw t)
                          (when (last-cursor-position typeout-view)
                            (setf (stream-cursor-position pane-with-typeout)
                                  (values-list (last-cursor-position typeout-view))))
                          (funcall continuation pane-with-typeout)
                          (setf (last-cursor-position typeout-view)
                                (multiple-value-list (stream-cursor-position pane-with-typeout)))))))
      (add-output-record new-record (output-history typeout-view))
      (setf (dirty typeout-view) t)
      nil)))

(defmacro with-typeout ((stream &optional (label "Typeout")) &body body)
  "Evaluate `body' with `stream' bound to a stream that can be
used for typeout. `Label' is the name of the created typeout
view."
  `(invoke-with-typeout *esa-instance* ,label
                        #'(lambda (,stream)
                            ,@body)))

;;; An implementation of the Gray streams protocol that uses a Climacs
;;; typeout view to draw the output.

(defclass typeout-stream (fundamental-character-output-stream)
  ((%climacs :reader climacs-instance
             :initform (error "Must provide a Climacs instance for typeout streams")
             :initarg :climacs)
   (%label :reader label
           :initform (error "A typeout stream must have a label")
           :initarg :label))
  (:documentation "An output stream that performs output on
a (single) Climacs typeout pane. If the typeout pane is deleted
manually by the user, the stream will recreate it the next time
output is performed."))

(defmethod stream-write-char ((stream typeout-stream) char)
  (with-typeout (typeout (label stream))
    (stream-write-char typeout char)))

(defmethod stream-line-column ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-line-column typeout)))

(defmethod stream-start-line-p ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-start-line-p typeout)))

(defmethod stream-write-string ((stream typeout-stream) string &optional (start 0) end)
  (with-typeout (typeout (label stream))
    (stream-write-string typeout string start end)))

(defmethod stream-terpri ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-terpri typeout)))

(defmethod stream-fresh-line ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-fresh-line typeout)))

(defmethod stream-finish-output ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-finish-output typeout)))

(defmethod stream-force-output ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-force-output typeout)))

(defmethod stream-clear-output ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (stream-clear-output typeout)))

(defmethod stream-advance-to-column ((stream typeout-stream) (column integer))
  (with-typeout (typeout (label stream))
    (stream-advance-to-column typeout column)))

(defmethod interactive-stream-p ((stream typeout-stream))
  (with-typeout (typeout (label stream))
    (interactive-stream-p typeout)))

(defun make-typeout-stream (climacs label)
  (make-instance 'typeout-stream :climacs climacs :label label))
