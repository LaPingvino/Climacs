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

(in-package :common-lisp-user)

(defparameter *climacs-directory* (directory-namestring *load-truename*))

(defmacro climacs-defsystem ((module &key depends-on) &rest components)
  `(progn
    #+mk-defsystem
    (mk:defsystem ,module
	:source-pathname *climacs-directory*
	,@(and depends-on `(:depends-on ,depends-on))
	:components (:serial ,@components))
    #+asdf
    (asdf:defsystem ,module
	,@(and depends-on `(:depends-on ,depends-on))
	:serial t
	:components (,@(loop for c in components
			     for p = (merge-pathnames
				      (parse-namestring c)
				      (make-pathname :type "lisp"
						     :defaults *climacs-directory*))
			     collect `(:file ,(pathname-name p) :pathname ,p))))))

(climacs-defsystem (:climacs :depends-on (:clim-clx))
   "Flexichain/skiplist-package"
   "Flexichain/skiplist"
   "Flexichain/flexichain-package"
   "Flexichain/utilities"
   "Flexichain/flexichain"
   "Flexichain/flexicursor"
   "Persistent/binseq-package"
   "Persistent/binseq"
   "Persistent/obinseq"
   "translate"
   "packages"
   "buffer"
   "Persistent/persistent-buffer"
   "base"
   "io"
   "abbrev"
   "syntax"
   "text-syntax"
   "html-syntax"
   "cl-syntax"
   "kill-ring"
   "undo"
   "pane"
   "gui"
   ;;---- optional ----
   "testing/rt"
   "buffer-test"
   "base-test"
   "Persistent/persistent-buffer-test"
   "Persistent/persistent-base-test")

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
