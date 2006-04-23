;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(defparameter *climacs-directory* (directory-namestring *load-truename*))

(defsystem :climacs
  :depends-on (:mcclim :flexichain :esa :split-sequence)
  :components
  ((:module "cl-automaton"
	    :components ((:file "automaton-package")
			 (:file "eqv-hash" :depends-on ("automaton-package"))
			 (:file "state-and-transition" :depends-on ("eqv-hash"))
			 (:file "automaton" :depends-on ("state-and-transition" "eqv-hash"))
			 (:file "regexp" :depends-on ("automaton"))))
   (:module "Persistent"
            :components ((:file "binseq-package")
                         (:file "binseq" :depends-on ("binseq-package"))
                         (:file "obinseq" :depends-on ("binseq-package" "binseq"))
                         (:file "binseq2" :depends-on ("binseq-package" "obinseq" "binseq"))))

   (:file "packages" :depends-on ("cl-automaton" "Persistent"))
   (:file "buffer" :depends-on ("packages"))
   (:file "persistent-buffer"
          :pathname #p"Persistent/persistent-buffer.lisp"
          :depends-on ("packages" "buffer" "Persistent"))

   (:file "base" :depends-on ("packages" "buffer" "persistent-buffer"))
   (:file "io" :depends-on ("packages" "buffer"))
   (:file "abbrev" :depends-on ("packages" "buffer" "base"))
   (:file "syntax" :depends-on ("packages" "buffer" "base"))
   (:file "text-syntax" :depends-on ("packages" "base" "buffer" "syntax"))
   (:file "delegating-buffer" :depends-on ("packages" "buffer"))
   (:file "kill-ring" :depends-on ("packages"))
   (:file "undo" :depends-on ("packages"))
   (:file "persistent-undo"
          :pathname #p"Persistent/persistent-undo.lisp"
          :depends-on ("packages" "buffer" "persistent-buffer" "undo"))
   (:file "pane" :depends-on ("packages" "syntax" "buffer" "base"
                                         "persistent-undo" "persistent-buffer" "abbrev"
                                         "delegating-buffer" "undo"))
   (:file "fundamental-syntax" :depends-on ("packages" "syntax" "buffer" "pane"
                                                       "base"))
   (:file "cl-syntax" :depends-on ("packages" "buffer" "syntax" "base" "pane"))
   (:file "html-syntax" :depends-on ("packages" "buffer" "syntax" "base" "pane"))
   (:file "prolog-syntax" :depends-on ("packages" "base" "syntax" "pane" "buffer"))
   (:file "prolog2paiprolog" :depends-on ("prolog-syntax"))
   (:file "ttcn3-syntax" :depends-on ("packages" "buffer" "syntax" "base"
						 "pane"))
   (:file "lisp-syntax" :depends-on ("packages" "syntax" "buffer" "base" "pane"
						"gui"))
   (:file "lisp-syntax-commands" :depends-on ("lisp-syntax"))
   (:file "gui" :depends-on ("packages" "syntax" "base" "buffer" "undo" "pane"
                                        "kill-ring" "io" "text-syntax"
					"abbrev" ))
;;    (:file "buffer-commands" :depends-on ("gui"))
   (:file "developer-commands" :depends-on ("gui" "lisp-syntax"))
   (:file "file-commands" :depends-on ("gui"))
   (:file "misc-commands" :depends-on ("gui"))
   (:file "search-commands" :depends-on ("gui"))
   (:file "window-commands" :depends-on ("gui"))
   (:file "unicode-commands" :depends-on ("gui"))
   (:file "slidemacs" :depends-on ("packages" "buffer" "syntax" "base" "pane" ))
   (:file "slidemacs-gui" :depends-on ("packages" "slidemacs" "pane" "buffer" "syntax" "gui"))))

(defsystem :climacs.tests
  :depends-on (:climacs)
  :components
  ((:file "rt" :pathname #p"testing/rt.lisp")
   (:file "buffer-test" :depends-on ("rt"))
   (:file "base-test" :depends-on ("rt"))
   (:module
    "cl-automaton"
    :depends-on ("rt")
    :components
    ((:file "automaton-test-package")
     (:file "eqv-hash-test" :depends-on ("automaton-test-package"))
     (:file "state-and-transition-test" :depends-on ("automaton-test-package"))
     (:file "automaton-test" :depends-on ("automaton-test-package"))
     (:file "regexp-test" :depends-on ("automaton-test-package"))))))

#+asdf
(defmethod asdf:perform :around ((o asdf:compile-op)
                                 (c (eql (asdf:find-component (asdf:find-system :climacs) "skiplist-package"))))
  (cond
    ((null (probe-file (first (asdf::input-files o c))))
     (cerror "Retry loading climacs."
             "~@<You need to download & install Flexichain ~
               separately! See the file INSTALL in the Climacs distribution ~
               for instructions.~@:>" nil)
     (asdf:perform o c))
    (t (call-next-method o c))))
