;;; -*- Mode: Lisp; Package: CLIMACS-MOTION; -*-

;;;  (c) copyright 2006 by
;;;           Taylor R. Campbell (campbell@mumble.net)
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

;;; Commands for moving the mark around

;;; See information in motion.lisp
;;;
;;; Given the general motion functions FORWARD-<unit> and
;;; BACKWARD-<unit>,
;;;
;;;   (DEFINE-MOTION-COMMANDS <unit> <command-table>)
;;;
;;; defines the motion commands Forward <unit> and Backward <unit> in
;;; <command-table>.  The following keyword parameters are recognized:
;;;
;;;   :NOUN
;;;     Noun to use in the docstring:  `Move point forward by one
;;;     <noun>.'  Default is the unit name, downcased.
;;;
;;;   :PLURAL
;;;     Plural form for the prompt, `Number of <plural>', and the rest
;;;     of the docstring; e.g.:  `With a numeric argument N, move point
;;;     forward by N <plural>.'
;;;

(in-package :climacs-commands)

(defmacro define-motion-commands (unit command-table &key
                                  noun
                                  plural)
  (labels ((symbol (&rest strings)
             (intern (apply #'concat strings)))
           (concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings))))
    (let ((forward (symbol "FORWARD-" unit))
          (backward (symbol "BACKWARD-" unit))
          (com-forward (symbol "COM-FORWARD-" unit))
          (com-backward (symbol "COM-BACKWARD-" unit))
          (noun (or noun (string-downcase unit)))
          (plural (or plural (concat (string-downcase unit) "s"))))
      `(PROGN
         (DEFINE-COMMAND (,com-forward :NAME T
                                       :COMMAND-TABLE ,command-table)
             ((COUNT 'INTEGER :PROMPT ,(concat "Number of " plural)))
           ,(concat "Move point forward by one " noun ".
With a numeric argument N, move point forward by N " plural ".
With a negative argument -N, move point backward by N " plural ".")
           (handler-case (,forward (POINT (CURRENT-WINDOW))
                                   (SYNTAX (BUFFER (CURRENT-WINDOW)))
                                   COUNT)
             (motion-limit-error ()
               (esa:display-message ,(concat "No more " plural)))))
         (DEFINE-COMMAND (,com-backward
                          :NAME T
                          :COMMAND-TABLE ,command-table)
             ((COUNT 'INTEGER :PROMPT ,(concat "Number of " plural)))
           ,(concat "Move point backward by one " noun ".
With a numeric argument N, move point backward by N " plural ".
With a negative argument -N, move point forward by N " plural ".")
           (handler-case (,backward (POINT (CURRENT-WINDOW))
                                    (SYNTAX (BUFFER (CURRENT-WINDOW)))
                                    COUNT)
             (motion-limit-error ()
               (esa:display-message ,(concat "No more " plural)))))))))

;;; Manually define some commands

(define-command (com-beginning-of-line :name t :command-table movement-table) ()
  "Move point to the beginning of the current line."
  (beginning-of-line (point (current-window))))

(define-command (com-end-of-line :name t :command-table movement-table) ()
  "Move point to the end of the current line."
  (end-of-line (point (current-window))))

;; Object movement comands - defined specially because FORWARD-OBJECT
;; and BACKWARD-OBJECT is part of the buffer protocol, not the
;; high-level motion abstraction.
(define-command (com-forward-object :name t :command-table movement-table)
    ((count 'integer :prompt "Number of objects"))
  "Move point forward by one object.
With a numeric argument N, move point forward by N objects.
With a negative argument -N, move point backward by M objects."
  (handler-case
      (forward-object (point (current-window))
                      count)
    (motion-limit-error nil
      (display-message "No more objects"))))

(define-command (com-backward-object :name t :command-table movement-table)
    ((count 'integer :prompt "number of objects"))
  "Move point backward by one object.
With a numeric argument N, move point backward by N objects.
With a negative argument -N, move point forward by N objects."
  (handler-case
      (backward-object (point (current-window))
                       count)
    (motion-limit-error nil
      (display-message "No more objects"))))

;;; Autogenerate commands
(define-motion-commands word movement-table)
(define-motion-commands line movement-table)
(define-motion-commands page movement-table)
(define-motion-commands paragraph movement-table)
(define-motion-commands sentence movement-table)

;;; Bind gestures to commands
(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :control)))

(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :right #-mcclim :right-arrow)))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :control)))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :left #-mcclim :left-arrow)))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :meta)))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :right #-mcclim :right-arrow :control)))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :meta)))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :left #-mcclim :left-arrow :control)))

(set-key `(com-forward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n :control)))

(set-key `(com-forward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :down #-mcclim :down-arrow)))

(set-key `(com-backward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\p :control)))

(set-key `(com-backward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :up #-mcclim :up-arrow)))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((:home)))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((#\a :control)))

(set-key 'com-end-of-line
	 'movement-table
	 '((#\e :control)))

(set-key 'com-end-of-line
	 'movement-table
	 '((:end)))

(set-key `(com-forward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\])))

(set-key `(com-backward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\[)))

(set-key `(com-backward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\{ :shift :meta)))

(set-key `(com-backward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :up #-mcclim :up-arrow :control)))

(set-key `(com-forward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\} :shift :meta)))

(set-key `(com-forward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#+mcclim :down #-mcclim :down-arrow :control)))