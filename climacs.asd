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

(climacs-defsystem (:climacs)
   "packages"
   "buffer"
   "base"
   "abbrev"
   "gui")
