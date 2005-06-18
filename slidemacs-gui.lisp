;;; -*- Mode: Lisp -*-

;;;  (c) copyright 2005 by
;;;           Brian Mastenbrook (brian@mastenbrook.net)
;;;           Christophe Rhodes (c.rhodes@gold.ac.uk)
;;;           Robert Strandh (strandh@labri.fr)

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

(in-package :climacs-slidemacs-editor)

;;; Share the same package to make it easy to reuse the parser

(define-syntax slidemacs-gui-syntax (slidemacs-editor-syntax)
  ((lexer :reader lexer)
   (valid-parse :initform 1) (parser))
  (:name "Slidemacs-GUI")
  (:pathname-types))

(defvar *slidemacs-display* nil)

(defvar *current-slideset*)
(defvar *did-display-a-slide*)

(defun slidemacs-entity-string (entity)
  (coerce (buffer-sequence (buffer entity)
                           (1+ (start-offset entity))
                           (1- (end-offset entity)))
          'string))

(defparameter *no-check-point* nil)

(defmethod display-parse-tree ((parse-tree slidemacs-slideset) (syntax slidemacs-gui-syntax) pane)
  (with-slots (slideset-info nonempty-list-of-slides slidemacs-slideset-name) parse-tree
    (let ((*current-slideset* (slidemacs-entity-string slidemacs-slideset-name))
          (*did-display-a-slide* nil))
      (display-parse-tree nonempty-list-of-slides syntax pane)
      (unless *did-display-a-slide*
        (display-parse-tree slideset-info syntax pane)))))

(defun traverse-list-entry (list-entry unit-type function)
  (when (and
         (slot-exists-p list-entry 'items)
         (slot-exists-p list-entry 'item)
         (typep (slot-value list-entry 'item) unit-type))
    (funcall function (slot-value list-entry 'item))
    (traverse-list-entry (slot-value list-entry 'items) unit-type function)))

(defmethod display-parse-tree-for-postscript ((parse-tree slidemacs-slideset) (syntax slidemacs-gui-syntax) stream)
  (with-slots (slideset-info nonempty-list-of-slides slidemacs-slideset-name) parse-tree
    (let ((*current-slideset* (slidemacs-entity-string slidemacs-slideset-name))
          (*did-display-a-slide* nil)
          (*no-check-point* t))
      (display-parse-tree slideset-info syntax stream)
      (new-page stream)
      (traverse-list-entry nonempty-list-of-slides
                           'slidemacs-slide
                           (lambda (slide)
                             (display-parse-tree slide syntax stream)
                             (new-page stream))))))

(defmethod display-parse-tree ((parse-tree slidemacs-slideset-keyword) (syntax slidemacs-gui-syntax) pane)
  (format *debug-io* "Oops!~%")
  (call-next-method))

(defmethod display-parse-tree :around ((entity slidemacs-entry) (syntax slidemacs-gui-syntax) pane)
  (let ((*handle-whitespace* nil))
    (call-next-method)))

(defun display-text-with-wrap-for-pane (text pane)
  (let* ((text (substitute #\space #\newline text))
         (split (remove
                 ""
                 (loop with start = 0
                       with length = (length text)
                       for cur from 0 upto length
                       for is-space =
                       (or (eql cur length)
                           (eql (elt text cur) #\space))
                       when is-space
                       collect
                       (prog1
                           (subseq text start cur)
                         (setf start (1+ cur))))
                 :test #'equal)))
    (write-string (pop split) pane)
    (loop
     with margin = (stream-text-margin pane)
     for word in split
     do (if (> (+ (stream-cursor-position pane)
                  (stream-string-width pane word))
               margin)
            (progn
              (terpri pane)
              (write-string word pane))
            (progn
              (write-string " " pane)
              (write-string word pane))))
    (terpri pane)))

(defparameter *slidemacs-sizes*
  '(:title 48
    :bullet 32
    :graph-node 14
    :slideset-title 48
    :slideset-info 32))

(defmethod display-parse-tree ((parse-tree slideset-info) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:sans-serif :bold ,(getf *slidemacs-sizes* :slideset-title)))
    (display-text-with-wrap-for-pane
     *current-slideset* pane)
    (terpri pane))
  (with-slots (author-institution-pairs opt-slide-venue opt-slide-date)
      parse-tree
    (display-parse-tree author-institution-pairs syntax pane)
    (display-parse-tree opt-slide-venue syntax pane)
    (display-parse-tree opt-slide-date syntax pane)))

(defmethod display-parse-tree ((entity slide-author) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:sans-serif :roman ,(getf *slidemacs-sizes* :slideset-info)))
    (with-slots (author) entity
      (display-text-with-wrap-for-pane
       (slidemacs-entity-string author) pane))))

(defmethod display-parse-tree ((entity slide-institution) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:sans-serif :roman ,(getf *slidemacs-sizes* :slideset-info)))
    (with-slots (institution) entity
      (display-text-with-wrap-for-pane
       (slidemacs-entity-string institution) pane))))

(defmethod display-parse-tree ((entity author-institution-pair) (syntax slidemacs-gui-syntax) pane)
  (call-next-method)
  (terpri pane))

(defmethod display-parse-tree ((entity slide-venue) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:sans-serif :roman ,(getf *slidemacs-sizes* :slideset-info)))
    (with-slots (venue) entity
      (display-text-with-wrap-for-pane
       (slidemacs-entity-string venue) pane))))

(defun today-string ()
  (multiple-value-bind (second minute hour date month year day)
      (get-decoded-time)
    (declare (ignore second minute hour day))
    (format nil "~A ~A ~A"
            date
            (elt
             '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
             (1- month))
            year)))

(defmethod display-parse-tree ((entity slide-date) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:sans-serif :roman ,(getf *slidemacs-sizes* :slideset-info)))
    (with-slots (opt-date-string) entity
      (if (typep (slot-value opt-date-string 'item)
                 'empty-slidemacs-terminals)
          (display-text-with-wrap-for-pane (today-string) pane)
          (display-text-with-wrap-for-pane
           (slidemacs-entity-string opt-date-string) pane)))))

(defmethod display-parse-tree ((parse-tree slidemacs-slide) (syntax slidemacs-gui-syntax) pane)
  (when (or *no-check-point*
            (with-slots (point) pane
              (and (mark>= point (start-offset parse-tree))
                   (mark<= point (end-offset parse-tree)))))
    (when (boundp '*did-display-a-slide*)
      (setf *did-display-a-slide* t))
    (with-slots (slidemacs-slide-name nonempty-list-of-bullets)
        parse-tree
      (display-parse-tree slidemacs-slide-name syntax pane)
      (display-parse-tree nonempty-list-of-bullets syntax pane))))

(defmethod display-parse-tree ((parse-tree slidemacs-graph-slide) (syntax slidemacs-gui-syntax) pane)
  (when (or *no-check-point*
            (with-slots (point) pane
              (when (and (mark>= point (start-offset parse-tree))
                         (mark<= point (end-offset parse-tree))))))
    (when (boundp '*did-display-a-slide*)
      (setf *did-display-a-slide* t))
    (with-slots (slidemacs-slide-name orientation list-of-roots list-of-edges)
        parse-tree
      (display-parse-tree slidemacs-slide-name syntax pane)
      (let (roots edges italic (orientation-val :horizontal))
        (when (typep (slot-value orientation 'item) 'vertical-keyword)
          (setf orientation-val :vertical))
        (traverse-list-entry
         list-of-roots 'graph-root
         (lambda (entry)
           (with-slots (vertex-name) entry
             (with-slots (slidemacs-string) vertex-name
               (with-slots (item) slidemacs-string
                 (when (typep item 'slidemacs-italic-string)
                   (pushnew (slidemacs-entity-string vertex-name) italic :test #'equal))))
             (pushnew (slidemacs-entity-string vertex-name) roots
                      :test #'equal))))
        (traverse-list-entry
         list-of-edges 'graph-edge
         (flet ((push-if-italic (thing)
                  (with-slots (vertex-name) thing
                    (with-slots (slidemacs-string) vertex-name
                      (with-slots (item) slidemacs-string
                        (when (typep item 'slidemacs-italic-string)
                          (pushnew (slidemacs-entity-string vertex-name) italic :test #'equal)))))))
           (lambda (entry)
             (with-slots (from-vertex to-vertex) entry
               (let ((from (slidemacs-entity-string from-vertex))
                     (to (slidemacs-entity-string to-vertex)))
                 (push-if-italic from-vertex)
                 (push-if-italic to-vertex)
                 (pushnew (cons from to)
                          edges :test #'equal))))))
        (let (record)
          (with-new-output-record (pane 'standard-sequence-output-record rec)
            (format-graph-from-roots
             roots
             (lambda (node stream)
               (with-text-style (pane `(:sans-serif
                                        ,(if (find node italic :test #'equal)
                                             :italic :roman)
                                        ,(getf *slidemacs-sizes* :graph-node)))
                 (surrounding-output-with-border (pane :shape :drop-shadow)
                   (present node 'string :stream stream))))
             (lambda (node)
               (loop for edge in edges
                  if (equal (car edge) node)
                  collect (cdr edge)))
             :orientation orientation-val
             ;;:generation-separation "xxxxxx"
             :arc-drawer
             (lambda (stream obj1 obj2 x1 y1 x2 y2)
               (declare (ignore obj1 obj2)) 
               (draw-arrow* stream x1 y1 x2 y2 :line-thickness 1 :head-length 8 :head-width 4))
             :merge-duplicates t
             :duplicate-test #'equal
             :graph-type :tree
             )
            (setf record rec))
          ;; Isn't this a hack?
          (with-bounding-rectangle*
              (x1 y1 x2 y2) record
            (draw-rectangle* pane x1 y1 x2 y2 :ink +flipping-ink+)
            (draw-rectangle* pane x1 y1 x2 y2 :ink +flipping-ink+)))))))

(defmethod display-parse-tree ((entity slidemacs-slide-name) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:sans-serif :bold ,(getf *slidemacs-sizes* :title)))
    (display-text-with-wrap-for-pane
     (slidemacs-entity-string entity) pane)
    (terpri pane)))

(defmethod display-parse-tree ((entity slidemacs-bullet) (syntax slidemacs-gui-syntax) pane)
  (stream-increment-cursor-position pane (space-width pane) 0)
  (with-text-style (pane `(:sans-serif :roman ,(getf *slidemacs-sizes* :bullet)))
    (if (and (not *no-check-point*)
             (with-slots (point) pane
               (and (mark>= point (start-offset entity))
                    (mark<= point (end-offset entity)))))
        (with-text-face (pane :bold)
          (call-next-method))
        (call-next-method))))

(defmethod display-parse-tree ((entity bullet) (syntax slidemacs-gui-syntax) pane)
  (stream-increment-cursor-position pane (space-width pane) 0)
  (present (lexeme-string entity) 'string :stream pane)
  (stream-increment-cursor-position pane (space-width pane) 0))

(defmethod display-parse-tree ((entity talking-point) (syntax slidemacs-gui-syntax) pane)
  (with-slots (slidemacs-string) entity
    (let ((is-italic (typep (slot-value slidemacs-string 'item)
                            'slidemacs-italic-string))
          (bullet-text (slidemacs-entity-string entity)))
      (if is-italic
          (with-text-face (pane :italic)
            (display-text-with-wrap-for-pane bullet-text pane))
          (display-text-with-wrap-for-pane bullet-text pane))
      (terpri pane))))

(defun draw-picture (stream pattern)
  (multiple-value-bind (x y)
      (stream-cursor-position stream)
    #+nil
    (draw-pattern* stream pattern x y)
    (let ((width  (pattern-width pattern))
          (height (pattern-height pattern)))
    (draw-rectangle* stream x y (+ x width) (+ y height)
                     :filled t
                     :ink (transform-region
                           (make-translation-transformation x y)
                           pattern)))))

(defparameter *picture-cache*
  (make-hash-table :test #'equal))

(defun load-and-cache-xpm (pathname)
  (let ((hash-key (cons pathname (file-write-date pathname))))
    (let ((pattern (gethash hash-key *picture-cache*)))
      (if pattern pattern
          (setf (gethash hash-key *picture-cache*)
                (climi::xpm-parse-file pathname))))))

(defmethod display-parse-tree ((entity picture-node) (syntax slidemacs-gui-syntax) pane)
  (with-slots (picture-pathname) entity
    (let ((real-pathname (slidemacs-entity-string picture-pathname)))
      (if (probe-file real-pathname)
          (let ((pattern (load-and-cache-xpm real-pathname)))
            (format *debug-io* "Loaded ~S!~%" real-pathname)
            (with-output-recording-options (pane nil t)
              (draw-picture pane pattern)))
          (with-text-style (pane `(:sans-serif :roman ,(getf *slidemacs-sizes* :bullet)))
            (display-text-with-wrap-for-pane (format nil "Missing picture ~S" real-pathname) pane))))))

(defmethod display-parse-tree ((entity slidemacs-entry) (syntax slidemacs-gui-syntax) pane)
  (with-slots (ink face) entity
    (setf ink (medium-ink (sheet-medium pane))
          face (text-style-face (medium-text-style (sheet-medium pane))))
    (present (coerce (buffer-sequence (buffer syntax)
                                      (start-offset entity)
                                      (end-offset entity))
                     'string)
             'string
             :stream pane)))

(defparameter *slidemacs-gui-ink* +black+)

(defmethod redisplay-pane-with-syntax ((pane climacs-pane) (syntax slidemacs-gui-syntax) current-p)
  (with-drawing-options (pane :ink *slidemacs-gui-ink*)
    (with-slots (top bot point) pane
      (with-slots (lexer) syntax
        ;; display the parse tree if any
        (let ((token (1- (nb-lexemes lexer))))
          (loop while (and (>= token 0)
                           (parse-state-empty-p (slot-value (lexeme lexer token) 'state)))
             do (decf token))
          (if (not (parse-state-empty-p (slot-value (lexeme lexer token) 'state)))
               (display-parse-state
               (slot-value (lexeme lexer token) 'state) syntax pane)
              (format *debug-io* "Empty parse state.~%")))
        ;; DON'T display the lexemes
        )
;;; It's not necessary to draw the cursor, and in fact quite confusing
      )))

(defun postscript-print-pane (pane)
  (with-open-file (file-stream "slides.ps" :direction :output
                               :if-exists :supersede)
    (with-output-to-postscript-stream
        (stream file-stream)
  (with-drawing-options (stream :ink *slidemacs-gui-ink*)
    (with-slots (top bot point) pane
      (let ((syntax (syntax (buffer pane))))
        (with-slots (lexer) syntax
          ;; display the parse tree if any
          (let ((token (1- (nb-lexemes lexer))))
            (loop while (and (>= token 0)
                             (parse-state-empty-p (slot-value (lexeme lexer token) 'state)))
               do (decf token))
            (if (not (parse-state-empty-p (slot-value (lexeme lexer token) 'state)))
                (display-parse-tree-for-postscript (slot-value (slot-value (target-parse-tree (slot-value (lexeme lexer token) 'state)) 'item) 'item) syntax stream)              
                (format *debug-io* "Empty parse state.~%")))
          ;; DON'T display the lexemes
          ))
;;; It's not necessary to draw the cursor, and in fact quite confusing
      )))))

(defun talking-point-stop-p (lexeme)
  (or (typep lexeme 'bullet)
      (and (typep lexeme 'slidemacs-keyword)
           (or (word-is lexeme "info")
               (word-is lexeme "graph")))))

(climacs-gui::define-named-command com-next-talking-point ()
  (let* ((pane (climacs-gui::current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer)))
    (with-slots (point) pane
      (with-slots (lexer) syntax
        (let ((point-pos (offset point)))
          (loop for token from 0 below (nb-lexemes lexer)
               for lexeme = (lexeme lexer token)
             do
             (when (and (talking-point-stop-p lexeme)
                        (> (start-offset lexeme) point-pos))
               (return (setf (offset point) (start-offset lexeme)))))
          (full-redisplay pane))))))

(climacs-gui::define-named-command com-previous-talking-point ()
  (let* ((pane (climacs-gui::current-window))
         (buffer (buffer pane))
         (syntax (syntax buffer)))
    (with-slots (point) pane
      (with-slots (lexer) syntax
        (let ((point-pos (offset point)))
          (loop for token from (1- (nb-lexemes lexer)) downto 0
             for lexeme = (lexeme lexer token)
             do
             (when (and (talking-point-stop-p lexeme)
                        (< (start-offset lexeme) point-pos))
               (return (setf (offset point) (start-offset lexeme)))))
          (full-redisplay pane))))))

(defun adjust-font-sizes (decrease-p)
  (setf *slidemacs-sizes*
        (loop for thing in *slidemacs-sizes*
              if (or (not (numberp thing))
                     (and decrease-p (< thing 16)))
              collect thing
              else collect (if decrease-p (- thing 8) (+ thing 8)))))

(climacs-gui::define-named-command com-decrease-presentation-font-sizes ()
  (adjust-font-sizes t)
  (full-redisplay (climacs-gui::current-window)))

(climacs-gui::define-named-command com-increase-presentation-font-sizes ()
  (adjust-font-sizes nil)
  (full-redisplay (climacs-gui::current-window)))

(climacs-gui::define-named-command com-first-talking-point ()
  (climacs-gui::com-beginning-of-buffer)
  (com-next-talking-point))

(climacs-gui::define-named-command com-last-talking-point ()
  (climacs-gui::com-end-of-buffer)
  (com-previous-talking-point))

(climacs-gui::define-named-command com-flip-slidemacs-syntax ()
  (let* ((buffer (buffer (climacs-gui::current-window)))
         (syntax (syntax buffer)))
    (typecase syntax
      (slidemacs-gui-syntax
       (climacs-gui::set-syntax (make-instance 'slidemacs-editor-syntax
                                               :buffer buffer)))
      (slidemacs-editor-syntax
       (climacs-gui::set-syntax (make-instance 'slidemacs-gui-syntax
                                               :buffer buffer))))))

(climacs-gui::global-set-key '(#\= :control) 'com-next-talking-point)
(climacs-gui::global-set-key '(#\- :control) 'com-previous-talking-point)
(climacs-gui::global-set-key '(#\= :meta) 'com-increase-presentation-font-sizes)
(climacs-gui::global-set-key '(#\- :meta) 'com-decrease-presentation-font-sizes)
(climacs-gui::global-set-key '(#\= :control :meta) 'com-last-talking-point)
(climacs-gui::global-set-key '(#\- :control :meta) 'com-first-talking-point)
(climacs-gui::global-set-key '(#\s :control :meta) 'com-flip-slidemacs-syntax)

(defun next-text-size (size)
  (if (symbolp size) 16 ;obviously
      (+ size 4)))

(defun prev-text-size (size)
  (if (symbolp size) 12 ;obviously
      (if (> size 4)
          (- size 4)
          size)))

(climacs-gui::define-named-command com-increase-text-size ()
  (symbol-macrolet ((style (medium-text-style (sheet-medium (climacs-gui::current-window)))))
    (format *debug-io* "Size is ~S~%" (text-style-size style))
    (setf style (make-text-style (text-style-family style)
                                 (text-style-face style)
                                 (next-text-size (text-style-size style))))
    (format *debug-io* "Size is now ~S~%" (text-style-size style)))
  (full-redisplay (climacs-gui::current-window)))

(climacs-gui::define-named-command com-decrease-text-size ()
  (symbol-macrolet ((style (medium-text-style (sheet-medium (climacs-gui::current-window)))))
    (setf style (make-text-style (text-style-family style)
                                 (text-style-face style)
                                 (prev-text-size (text-style-size style)))))
  (full-redisplay (climacs-gui::current-window)))