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

(defmethod display-parse-tree ((parse-tree slidemacs-slideset) (syntax slidemacs-gui-syntax) pane)
  (with-slots (slideset-info nonempty-list-of-slides slidemacs-slideset-name) parse-tree
    (let ((*current-slideset* (lexeme-string slidemacs-slideset-name)))
      (display-parse-tree slideset-info syntax pane)
      (display-parse-tree nonempty-list-of-slides syntax pane))))

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
    (present (pop split) 'string :stream pane)
    (loop
     with margin = (stream-text-margin pane)
     for word in split
     do (if (> (+ (stream-cursor-position pane)
                  (stream-string-width pane word))
               margin)
            (progn
              (terpri pane)
              (present word 'string :stream pane))
            (progn
              (present " " 'string :stream pane)
              (present word 'string :stream pane))))
    (loop repeat 2 do (terpri pane))))

(defmethod display-parse-tree ((parse-tree slideset-info) (syntax slidemacs-gui-syntax) pane)
  ;; do nothing yet
  #+nil
  (with-slots (point) pane
    (when (and (mark>= point (start-offset parse-tree))
               (mark<= point (end-offset parse-tree)))
      (with-slots (opt-slide-author opt-slide-institution opt-slide-venue opt-slide-date)
          parse-tree
        (display-parse-tree slidemacs-slide-name syntax pane)
        (display-parse-tree nonempty-list-of-bullets syntax pane)))))

(defmethod display-parse-tree ((parse-tree slidemacs-slide) (syntax slidemacs-gui-syntax) pane)
  (with-slots (point) pane
              (when (and (mark>= point (start-offset parse-tree))
                         (mark<= point (end-offset parse-tree)))
                (with-slots (slidemacs-slide-name nonempty-list-of-bullets)
                    parse-tree
                  (display-parse-tree slidemacs-slide-name syntax pane)
                  (display-parse-tree nonempty-list-of-bullets syntax pane)))))

(defparameter *slidemacs-sizes*
  '(:title 64
    :bullet 32)) ;; must all be powers of 2

(defmethod display-parse-tree ((entity slidemacs-slide-name) (syntax slidemacs-gui-syntax) pane)
  (with-text-style (pane `(:serif :bold ,(getf *slidemacs-sizes* :title)))
    (display-text-with-wrap-for-pane
     (coerce (buffer-sequence (buffer syntax)
                              (1+ (start-offset entity))
                              (1- (end-offset entity)))
             'string) pane)))

(defmethod display-parse-tree ((entity slidemacs-bullet) (syntax slidemacs-gui-syntax) pane)
  (stream-increment-cursor-position pane (space-width pane) 0)
  (with-text-style (pane `(:serif :roman ,(getf *slidemacs-sizes* :bullet)))
    (with-slots (point) pane
      (if (and (mark>= point (start-offset entity))
               (mark<= point (end-offset entity)))
          (with-text-face (pane :bold)
            (call-next-method))
          (call-next-method)))))

(defmethod display-parse-tree ((entity bullet) (syntax slidemacs-gui-syntax) pane)
  (stream-increment-cursor-position pane (space-width pane) 0)
  (present (lexeme-string entity) 'string :stream pane)
  (stream-increment-cursor-position pane (space-width pane) 0))

(defmethod display-parse-tree ((entity talking-point) (syntax slidemacs-gui-syntax) pane)
  (let* ((bullet-text (coerce (buffer-sequence (buffer syntax)
                                               (1+ (start-offset entity))
                                               (1- (end-offset entity)))
                              'string)))
    (display-text-with-wrap-for-pane bullet-text pane)))

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

(defun set-pane-colors (pane c1 c2)
  (setf (medium-background (sheet-medium pane)) c1
        (medium-ink (sheet-medium pane)) c2
        *slidemacs-gui-ink* c2)
  (window-refresh pane))

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
             (when (and (typep lexeme 'bullet)
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
             (when (and (typep lexeme 'bullet)
                        (< (start-offset lexeme) point-pos))
               (return (setf (offset point) (start-offset lexeme)))))
          (full-redisplay pane))))))

(defun adjust-font-sizes (decrease-p)
  (setf *slidemacs-sizes*
        (loop for thing in *slidemacs-sizes*
              if (or (not (numberp thing))
                     (< thing 16))
              collect thing
              else collect (if decrease-p (- thing 8) (+ thing 8)))))

(climacs-gui::define-named-command com-set-colors-for-presentation ()
  (set-pane-colors (climacs-gui::current-window) +blue+ +white+))

(climacs-gui::define-named-command com-set-colors-for-editing ()
  (set-pane-colors (climacs-gui::current-window) +white+ +black+))

(climacs-gui::define-named-command com-decrease-presentation-font-sizes ()
  (adjust-font-sizes t)
  (full-redisplay (climacs-gui::current-window)))

(climacs-gui::define-named-command com-increase-presentation-font-sizes ()
  (adjust-font-sizes nil)
  (full-redisplay (climacs-gui::current-window)))

(climacs-gui::global-set-key '(#\= :control) 'com-next-talking-point)
(climacs-gui::global-set-key '(#\- :control) 'com-previous-talking-point)
(climacs-gui::global-set-key '(#\= :meta) 'com-increase-presentation-font-sizes)
(climacs-gui::global-set-key '(#\- :meta) 'com-decrease-presentation-font-sizes)