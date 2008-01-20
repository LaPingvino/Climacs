;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
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

;;; ASDF system definition for Climacs.

(defpackage :climacs.system
  (:use :cl :asdf))

(in-package :climacs.system)

(defsystem :climacs
  :depends-on (:mcclim :flexichain)
  :components
  ((:file "packages")
   (:file "text-syntax" :depends-on ("packages"))
;;    (:file "cl-syntax" :depends-on ("packages"))
;;    (:file "html-syntax" :depends-on ("packages"))
   (:file "prolog-syntax" :depends-on ("packages"))
   (:file "prolog2paiprolog" :depends-on ("prolog-syntax"))
;;    (:file "ttcn3-syntax" :depends-on ("packages"))
   (:file "climacs-lisp-syntax" :depends-on ("core" "groups"))
   (:file "climacs-lisp-syntax-commands" :depends-on ("climacs-lisp-syntax" "misc-commands"))
   (:file "structured-editing" :depends-on ("climacs-lisp-syntax-commands"))
   (:file "c-syntax" :depends-on ("core"))
   (:file "c-syntax-commands" :depends-on ("c-syntax" "misc-commands"))
   (:file "java-syntax" :depends-on ("core"))
   (:file "java-syntax-commands" :depends-on ("java-syntax" "misc-commands"))
   (:file "typeout" :depends-on ("packages"))
   (:file "gui" :depends-on ("packages" "typeout"))
   (:file "core" :depends-on ("gui"))
   (:file "io" :depends-on ("packages" "gui"))
   (:file "groups" :depends-on ("core"))
   (:file "climacs" :depends-on ("gui" "core"))
   (:file "developer-commands" :depends-on ("core"))
  
   (:file "file-commands" :depends-on ("gui" "core" "io"))
   (:file "misc-commands" :depends-on ("gui" "core" "groups"))
   (:file "search-commands" :depends-on ("gui" "core" "groups"))
   (:file "window-commands" :depends-on ("gui" "core"))
   ;; (:file "slidemacs" :depends-on ("packages" ))
;;    (:file "slidemacs-gui" :depends-on ("packages" "gui" "slidemacs"))
   ))
