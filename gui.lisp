; SLIME 2004-12-13
CL-USER> (load "cvs-dir/mcclim/system")
; in: LAMBDA NIL
;     (OR "Lisp-Dep/mp-nil")
; ==>
;   "Lisp-Dep/mp-nil"
; 
; note: deleting unreachable code
; compilation unit finished
;   printed 1 note
T
CL-USER> (require 'clim-clx-user)
; loading system definition from #P"/usr/local/lib/sbcl/site-systems/clx.asd"
; into #<PACKAGE "ASDF4627">
; registering #<SYSTEM CLX {9811989}> as CLX
; in: LAMBDA (#:G5429 #:G5430 #:G5431 #:G5432 #:G5437 #:G5438)
;     (EQL SB-PCL::.CASE-ARG. #:G5429)
; 
; note: unable to
;   optimize
; due to type uncertainty:
;   The first argument is a T, not a SINGLE-FLOAT.
;   The second argument is a T, not a SINGLE-FLOAT.
; 
; note: unable to
;   optimize
; due to type uncertainty:
;   The first argument is a T, not a DOUBLE-FLOAT.
;   The second argument is a T, not a DOUBLE-FLOAT.
; 
; note: forced to do GENERIC-EQL (cost 10)
;       unable to do inline fixnum comparison (cost 4) because:
;       The first argument is a T, not a FIXNUM.
;       The second argument is a T, not a FIXNUM.
; in:
;      LAMBDA (#:G5443 #:G5444
;                  #:G5445
;                  #:G5446
;                  #:G5447
;                  #:G5448
;                  #:G5449
;                  #:G5454
;                  #:G5455)
;     (EQL SB-PCL::.CASE-ARG. #:G5443)
; 
; note: unable to
;   optimize
; due to type uncertainty:
;   The first argument is a T, not a SINGLE-FLOAT.
;   The second argument is a T, not a SINGLE-FLOAT.
; 
; note: unable to
;   optimize
; due to type uncertainty:
;   The first argument is a T, not a DOUBLE-FLOAT.
;   The second argument is a T, not a DOUBLE-FLOAT.

;     (EQL SB-PCL::.CASE-ARG. #:G5446)
; 
; note: unable to
;   optimize
; due to type uncertainty:
;   The first argument is a T, not a SINGLE-FLOAT.
;   The second argument is a T, not a SINGLE-FLOAT.
; 
; note: unable to
;   optimize
; due to type uncertainty:
;   The first argument is a T, not a DOUBLE-FLOAT.
;   The second argument is a T, not a DOUBLE-FLOAT.

;     (EQL SB-PCL::.CASE-ARG. #:G5443)
; 
; note: forced to do GENERIC-EQL (cost 10)
;       unable to do inline fixnum comparison (cost 4) because:
;       The first argument is a T, not a FIXNUM.
;       The second argument is a T, not a FIXNUM.

;     (EQL SB-PCL::.CASE-ARG. #:G5446)
; 
; note: forced to do GENERIC-EQL (cost 10)
;       unable to do inline fixnum comparison (cost 4) because:
;       The first argument is a T, not a FIXNUM.
;       The second argument is a T, not a FIXNUM.
; compilation unit finished
;   printed 9 notes
NIL
CL-USER> (load "cvs-dir/climacs/climacs.asd")
T
CL-USER> (require 'climacs)
; compiling file "/home/ejohnson/cvs-dir/climacs/Flexichain/skiplist-package.lisp" (written 16 AUG 2004 01:12:45 AM):
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/Flexichain/skiplist-package.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/Flexichain/skiplist.lisp" (written 16 AUG 2004 01:12:45 AM):
; compiling top level form: 
; compiling DEFCLASS SKIPLIST: 
; compiling DEFCLASS SKIPLIST: 
; compiling DEFCLASS SKIPLIST: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (SKIPLIST): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD PRINT-OBJECT (SKIPLIST T): 
; compiling top level form: 
; recognizing DEFUN ENTRY-OBJ
; compiling DEFUN ENTRY-OBJ: 
; compiling top level form: 
; recognizing DEFUN (SETF ENTRY-OBJ)
; compiling DEFUN (SETF ENTRY-OBJ): 
; compiling top level form: 
; recognizing DEFUN ENTRY-KEY
; compiling DEFUN ENTRY-KEY: 
; compiling top level form: 
; recognizing DEFUN (SETF ENTRY-KEY)
; compiling DEFUN (SETF ENTRY-KEY): 
; compiling top level form: 
; recognizing DEFUN ENTRY-NEXT
; compiling DEFUN ENTRY-NEXT: 
; compiling top level form: 
; recognizing DEFUN (SETF ENTRY-NEXT)
; compiling DEFUN (SETF ENTRY-NEXT): 
; compiling top level form: 
; recognizing DEFUN KEY-<
; compiling DEFUN KEY-<: 
; compiling top level form: 
; recognizing DEFUN KEY-<=
; compiling DEFUN KEY-<=: 
; compiling top level form: 
; recognizing DEFUN KEY-=
; compiling DEFUN KEY-=: 
; compiling top level form: 
; recognizing DEFUN KEY->
; compiling DEFUN KEY->: 
; compiling top level form: 
; recognizing DEFUN KEY->=
; compiling DEFUN KEY->=: 
; compiling top level form: 
; recognizing DEFUN LAST-ENTRY-P
; compiling DEFUN LAST-ENTRY-P: 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-EMPTY-P
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-EMPTY-P: 
; compiling top level form: 
; recognizing DEFUN FIND-ENTRY-LEVEL
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN FIND-ENTRY-LEVEL: 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-FIND
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-FIND: 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-FIND-FIRST
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-FIND-FIRST: 
; compiling top level form: 
; recognizing DEFUN PICK-A-LEVEL
; compiling DEFUN PICK-A-LEVEL: 
; compiling top level form: 
; recognizing DEFUN MAKE-ENTRY
; compiling DEFUN MAKE-ENTRY: 
; compiling top level form: 
; recognizing DEFUN (SETF SKIPLIST-FIND)
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN (SETF SKIPLIST-FIND): 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-DELETE
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-DELETE: 
; compiling top level form: 
; recognizing DEFUN UPDATE-INTERVAL
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN UPDATE-INTERVAL: 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-SLIDE-KEYS
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-SLIDE-KEYS: 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-ROTATE-PREFIX
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-ROTATE-PREFIX: 
; compiling top level form: 
; recognizing DEFUN UPDATE-INTERVAL-TO-END
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN UPDATE-INTERVAL-TO-END: 
; compiling top level form: 
; recognizing DEFUN SKIPLIST-ROTATE-SUFFIX
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN SKIPLIST-ROTATE-SUFFIX: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/Flexichain/skiplist.fasl written
; compilation finished in 0:00:02
; compiling file "/home/ejohnson/cvs-dir/climacs/Flexichain/flexichain-package.lisp" (written 27 DEC 2004 10:57:00 PM):
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/Flexichain/flexichain-package.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/Flexichain/utilities.lisp" (written 01 AUG 2004 08:27:19 AM):
; compiling top level form: 
; recognizing DEFUN SQUARE
; compiling DEFUN SQUARE: 
; compiling top level form: 
; recognizing DEFUN FIND-IF-2
; compiling DEFUN FIND-IF-2: 
; compiling top level form: 
; recognizing DEFUN MAKE-WEAK-POINTER
; compiling DEFUN MAKE-WEAK-POINTER: 
; compiling top level form: 
; recognizing DEFUN WEAK-POINTER-VALUE
; compiling DEFUN WEAK-POINTER-VALUE: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/Flexichain/utilities.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/Flexichain/flexichain.lisp" (written 27 DEC 2004 10:57:00 PM):
; compiling top level form: 
; compiling DEFCLASS FLEXICHAIN: 
; compiling DEFCLASS FLEXICHAIN: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (FLEXICHAIN): 
; compiling top level form: 
; compiling top level form: 
; compiling DEFINE-CONDITION FLEXI-INITIALIZATION-ERROR: 
; compiling top level form: 
; compiling DEFINE-CONDITION FLEXI-POSITION-ERROR: 
; compiling top level form: 
; compiling DEFINE-CONDITION FLEXI-INCOMPATIBLE-TYPE-ERROR: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN REQUIRED-SPACE
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN REQUIRED-SPACE: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (STANDARD-FLEXICHAIN): 
; compiling top level form: 
; compiling DEFMACRO WITH-VIRTUAL-GAP: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD NB-ELEMENTS (STANDARD-FLEXICHAIN): 
; compiling top level form: 
; compiling DEFMETHOD FLEXI-EMPTY-P (STANDARD-FLEXICHAIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN POSITION-INDEX
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN POSITION-INDEX: 
; compiling top level form: 
; recognizing DEFUN INDEX-POSITION
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN INDEX-POSITION: 
; compiling top level form: 
; recognizing DEFUN ENSURE-GAP-POSITION
; compiling DEFUN ENSURE-GAP-POSITION: 
; compiling top level form: 
; recognizing DEFUN ENSURE-ROOM
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN ENSURE-ROOM: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INSERT* (STANDARD-FLEXICHAIN T T): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INSERT-VECTOR* (STANDARD-FLEXICHAIN T T): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD DELETE* (STANDARD-FLEXICHAIN T): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD ELEMENT* (STANDARD-FLEXICHAIN T): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD (SETF ELEMENT*) (T STANDARD-FLEXICHAIN T): 
; compiling top level form: 
; compiling DEFMETHOD PUSH-START (STANDARD-FLEXICHAIN T): 
; compiling top level form: 
; compiling DEFMETHOD PUSH-END (STANDARD-FLEXICHAIN T): 
; compiling top level form: 
; compiling DEFMETHOD POP-START (STANDARD-FLEXICHAIN): 
; compiling top level form: 
; compiling DEFMETHOD POP-END (STANDARD-FLEXICHAIN): 
; compiling top level form: 
; compiling DEFMETHOD ROTATE (STANDARD-FLEXICHAIN): 
; compiling top level form: 
; recognizing DEFUN MOVE-GAP
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN MOVE-GAP: 
; compiling top level form: 
; recognizing DEFUN MOVE-EMPTY-GAP
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN MOVE-EMPTY-GAP: 
; compiling top level form: 
; recognizing DEFUN MOVE-LEFT-GAP
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN MOVE-LEFT-GAP: 
; compiling top level form: 
; recognizing DEFUN MOVE-RIGHT-GAP
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN MOVE-RIGHT-GAP: 
; compiling top level form: 
; recognizing DEFUN MOVE-MIDDLE-GAP
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN MOVE-MIDDLE-GAP: 
; compiling top level form: 
; recognizing DEFUN MOVE-NON-CONTIGUOUS-GAP
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN MOVE-NON-CONTIGUOUS-GAP: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD MOVE-ELEMENTS (STANDARD-FLEXICHAIN T T T T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD FILL-GAP (STANDARD-FLEXICHAIN T T): 
; compiling top level form: 
; recognizing DEFUN PUSH-ELEMENTS-LEFT
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN PUSH-ELEMENTS-LEFT: 
; compiling top level form: 
; recognizing DEFUN PUSH-ELEMENTS-RIGHT
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN PUSH-ELEMENTS-RIGHT: 
; compiling top level form: 
; recognizing DEFUN HOP-ELEMENTS-LEFT
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN HOP-ELEMENTS-LEFT: 
; compiling top level form: 
; recognizing DEFUN HOP-ELEMENTS-RIGHT
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN HOP-ELEMENTS-RIGHT: 
; compiling top level form: 
; recognizing DEFUN INCREASE-BUFFER-SIZE
; compiling DEFUN INCREASE-BUFFER-SIZE: 
; compiling top level form: 
; recognizing DEFUN DECREASE-BUFFER-SIZE
; compiling DEFUN DECREASE-BUFFER-SIZE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD RESIZE-BUFFER (STANDARD-FLEXICHAIN T): 
; compiling top level form: 
; recognizing DEFUN NORMALIZE-INDICES
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN NORMALIZE-INDICES: 
; compiling top level form: 
; recognizing DEFUN GAP-LOCATION
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN GAP-LOCATION: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/Flexichain/flexichain.fasl written
; compilation finished in 0:00:04
; compiling file "/home/ejohnson/cvs-dir/climacs/Flexichain/flexicursor.lisp" (written 06 SEP 2004 04:25:52 AM):
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFINE-CONDITION AT-BEGINNING-ERROR: 
; compiling top level form: 
; compiling DEFINE-CONDITION AT-END-ERROR: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFCLASS STANDARD-CURSORCHAIN: 
; compiling top level form: 
; recognizing DEFUN MAKE-WP
; compiling DEFUN MAKE-WP: 
; compiling top level form: 
; recognizing DEFUN WP-VALUE
; compiling DEFUN WP-VALUE: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD MOVE-ELEMENTS :AFTER (STANDARD-CURSORCHAIN T T T T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (LEFT-STICKY-FLEXICURSOR): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (RIGHT-STICKY-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD CLONE-CURSOR (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD CURSOR-POS (LEFT-STICKY-FLEXICURSOR): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD (SETF CURSOR-POS) (T LEFT-STICKY-FLEXICURSOR): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD CURSOR-POS (RIGHT-STICKY-FLEXICURSOR): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD (SETF CURSOR-POS) (T RIGHT-STICKY-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD AT-BEGINNING-P (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD AT-END-P (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD INSERT (STANDARD-FLEXICURSOR T): 
; compiling top level form: 
; compiling DEFMETHOD INSERT-SEQUENCE (STANDARD-FLEXICURSOR T): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD DELETE* :BEFORE (STANDARD-CURSORCHAIN T): 
; compiling top level form: 
; compiling DEFMETHOD DELETE> (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD DELETE< (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD ELEMENT> (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD (SETF ELEMENT>) (T STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD ELEMENT< (STANDARD-FLEXICURSOR): 
; compiling top level form: 
; compiling DEFMETHOD (SETF ELEMENT<) (T STANDARD-FLEXICURSOR): 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/Flexichain/flexicursor.fasl written
; compilation finished in 0:00:02
; compiling file "/home/ejohnson/cvs-dir/climacs/packages.lisp" (written 28 DEC 2004 09:38:37 PM):
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/packages.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/buffer.lisp" (written 27 DEC 2004 10:58:36 PM):
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFCLASS STANDARD-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD OFFSET (T): 
; compiling top level form: 
; compiling DEFMETHOD (SETF OFFSET) (T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (LEFT-STICKY-MARK): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (RIGHT-STICKY-MARK): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (STANDARD-BUFFER): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD CLONE-MARK (STANDARD-LEFT-STICKY-MARK): 
; compiling top level form: 
; compiling DEFMETHOD CLONE-MARK (STANDARD-RIGHT-STICKY-MARK): 
; compiling top level form: 
; compiling DEFINE-CONDITION NO-SUCH-OFFSET: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD SIZE (STANDARD-BUFFER): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD NUMBER-OF-LINES (STANDARD-BUFFER): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD MARK< (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD MARK< (MARK-MIXIN INTEGER): 
; compiling top level form: 
; compiling DEFMETHOD MARK< (INTEGER MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD MARK<= (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD MARK<= (MARK-MIXIN INTEGER): 
; compiling top level form: 
; compiling DEFMETHOD MARK<= (INTEGER MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD MARK= (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD MARK= (MARK-MIXIN INTEGER): 
; compiling top level form: 
; compiling DEFMETHOD MARK= (INTEGER MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD MARK> (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD MARK> (MARK-MIXIN INTEGER): 
; compiling top level form: 
; compiling DEFMETHOD MARK> (INTEGER MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD MARK>= (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD MARK>= (MARK-MIXIN INTEGER): 
; compiling top level form: 
; compiling DEFMETHOD MARK>= (INTEGER MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD BEGINNING-OF-BUFFER (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD END-OF-BUFFER (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD BEGINNING-OF-BUFFER-P (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD END-OF-BUFFER-P (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD BEGINNING-OF-LINE-P (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD END-OF-LINE-P (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD BEGINNING-OF-LINE (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD END-OF-LINE (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD LINE-NUMBER (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD COLUMN-NUMBER (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INSERT-BUFFER-OBJECT (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INSERT-BUFFER-SEQUENCE (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD INSERT-OBJECT (MARK-MIXIN T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD INSERT-SEQUENCE (MARK-MIXIN T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD DELETE-BUFFER-RANGE (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD DELETE-RANGE (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD DELETE-REGION (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD DELETE-REGION (MARK-MIXIN T): 
; compiling top level form: 
; compiling DEFMETHOD DELETE-REGION (T MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD BUFFER-OBJECT (STANDARD-BUFFER T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD BUFFER-SEQUENCE (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD OBJECT-BEFORE (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD OBJECT-AFTER (MARK-MIXIN): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD REGION-TO-SEQUENCE (MARK-MIXIN MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD REGION-TO-SEQUENCE (INTEGER MARK-MIXIN): 
; compiling top level form: 
; compiling DEFMETHOD REGION-TO-SEQUENCE (MARK-MIXIN INTEGER): 
; compiling top level form: 
; compiling DEFMETHOD INSERT-BUFFER-OBJECT :BEFORE (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling DEFMETHOD INSERT-BUFFER-SEQUENCE :BEFORE (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling DEFMETHOD DELETE-BUFFER-RANGE :BEFORE (STANDARD-BUFFER T T): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD RESET-LOW-HIGH-MARKS (STANDARD-BUFFER): 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/buffer.fasl written
; compilation finished in 0:00:04
; compiling file "/home/ejohnson/cvs-dir/climacs/base.lisp" (written 27 DEC 2004 03:32:46 AM):
; compiling top level form: 
; recognizing DEFUN PREVIOUS-LINE
; compiling DEFUN PREVIOUS-LINE: 
; compiling top level form: 
; recognizing DEFUN NEXT-LINE
; compiling DEFUN NEXT-LINE: 
; compiling top level form: 
; recognizing DEFUN OPEN-LINE
; compiling DEFUN OPEN-LINE: 
; compiling top level form: 
; recognizing DEFUN KILL-LINE
; compiling DEFUN KILL-LINE: 
; compiling top level form: 
; recognizing DEFUN BUFFER-NUMBER-OF-LINES-IN-REGION
; compiling DEFUN BUFFER-NUMBER-OF-LINES-IN-REGION: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling DEFMETHOD NUMBER-OF-LINES-IN-REGION (MARK MARK): 
; compiling top level form (SB-KERNEL:FIND-CLASSOID-CELL (QUOTE CLIMACS-BUFFER:MARK)): 
; compiling top level form: 
; compiling DEFMETHOD NUMBER-OF-LINES-IN-REGION (INTEGER MARK): 
; compiling top level form: 
; compiling DEFMETHOD NUMBER-OF-LINES-IN-REGION (MARK INTEGER): 
; compiling top level form: 
; recognizing DEFUN CONSTITUENTP
; compiling DEFUN CONSTITUENTP: 
; compiling top level form: 
; recognizing DEFUN FORWARD-WORD
; compiling DEFUN FORWARD-WORD: 
; compiling top level form: 
; recognizing DEFUN BACKWARD-WORD
; compiling DEFUN BACKWARD-WORD: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/base.fasl written
; compilation finished in 0:00:01
; compiling file "/home/ejohnson/cvs-dir/climacs/io.lisp" (written 27 DEC 2004 10:58:36 PM):
; compiling top level form: 
; recognizing DEFUN INPUT-FROM-STREAM
; compiling DEFUN INPUT-FROM-STREAM: 
; compiling top level form: 
; recognizing DEFUN OUTPUT-TO-STREAM
; compiling DEFUN OUTPUT-TO-STREAM: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/io.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/abbrev.lisp" (written 23 DEC 2004 12:00:33 AM):
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN STRING-UPPER-CASE-P
; compiling DEFUN STRING-UPPER-CASE-P: 
; compiling top level form: 
; compiling DEFMETHOD EXPAND-ABBREV (T DICTIONARY-ABBREV-EXPANDER): 
; compiling top level form: 
; recognizing DEFUN POSSIBLY-EXPAND-ABBREV
; compiling DEFUN POSSIBLY-EXPAND-ABBREV: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFCLASS ABBREV-MIXIN: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/abbrev.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/syntax.lisp" (written 28 DEC 2004 02:41:14 PM):
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN REDISPLAY-PANE
; compiling DEFUN REDISPLAY-PANE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (BASIC-SYNTAX): 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-TYPE URL: 
; compiling DEFINE-PRESENTATION-TYPE URL: 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-TYPE URL: 
; compiling top level form: 
; compiling DEFMETHOD PRESENT-CONTENTS (T T BASIC-SYNTAX): 
; compiling top level form: 
; compiling DEFMACRO MAYBE-UPDATING-OUTPUT: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD DISPLAY-LINE (T BASIC-SYNTAX): 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFMETHOD REDISPLAY-WITH-SYNTAX (T BASIC-SYNTAX): 
; compiling top level form: 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-TYPE TEXINFO-COMMAND: 
; compiling DEFINE-PRESENTATION-TYPE TEXINFO-COMMAND: 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-TYPE TEXINFO-COMMAND: 
; compiling top level form: 
; compiling DEFMETHOD PRESENT-CONTENTS (T T TEXINFO-SYNTAX): 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/syntax.fasl written
; compilation finished in 0:00:02
; compiling file "/home/ejohnson/cvs-dir/climacs/kill-ring.lisp" (written 28 DEC 2004 09:28:25 PM):
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN INITIALIZE-KILL-RING
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFUN INITIALIZE-KILL-RING: 
; compiling top level form: 
; recognizing DEFUN KR-LENGTH
; compiling DEFUN KR-LENGTH: 
; compiling top level form: 
; recognizing DEFUN KR-RESIZE
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN KR-RESIZE: 
; compiling top level form: 
; recognizing DEFUN KR-PUSH
; compiling DEFUN KR-PUSH: 
; compiling top level form: 
; recognizing DEFUN KR-POP
; compiling DEFUN KR-POP: 
; compiling top level form: 
; recognizing DEFUN KR-ROTATE
; compiling DEFUN KR-ROTATE: 
; compiling top level form: 
; recognizing DEFUN KR-COPY
; compiling DEFUN KR-COPY: 
; compiling top level form: 
; recognizing DEFUN KR-COPY-IN
; compiling DEFUN KR-COPY-IN: 
; compiling top level form: 
; recognizing DEFUN KR-CUT-IN
; compiling DEFUN KR-CUT-IN: 
; compiling top level form: 
; recognizing DEFUN KR-COPY-OUT
; compiling DEFUN KR-COPY-OUT: 
; compiling top level form: 
; recognizing DEFUN KR-CUT-OUT
; compiling DEFUN KR-CUT-OUT: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/kill-ring.fasl written
; compilation finished in 0:00:00
; compiling file "/home/ejohnson/cvs-dir/climacs/gui.lisp" (written 28 DEC 2004 09:26:34 PM):
; compiling top level form: 
; compiling top level form: 
; compiling DEFCLASS CLIMACS-BUFFER: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFCLASS CLIMACS-PANE: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFMETHOD INITIALIZE-INSTANCE :AFTER (CLIMACS-PANE): 
; compiling top level form: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling top level form (SB-KERNEL:FIND-CLASSOID-CELL (QUOTE CLIM:FRAME-MANAGER)): 
; compiling top level form: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form: 
; compiling DEFINE-APPLICATION-FRAME CLIMACS: 
; compiling top level form: 
; recognizing DEFUN CLIMACS
; compiling DEFUN CLIMACS: 
; compiling top level form: 
; recognizing DEFUN DISPLAY-INFO
; compiling DEFUN DISPLAY-INFO: 
; compiling top level form: 
; recognizing DEFUN DISPLAY-WIN
; compiling DEFUN DISPLAY-WIN: 
; compiling top level form: 
; recognizing DEFUN FIND-GESTURES
; compiling DEFUN FIND-GESTURES: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN CLIMACS-TOP-LEVEL
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFUN CLIMACS-TOP-LEVEL: 
; compiling top level form (SB-KERNEL:FIND-CLASSOID-CELL (QUOTE CLIM:KEYBOARD-EVENT)): 
; compiling top level form: 
; recognizing DEFUN COM-QUIT
; compiling DEFINE-COMMAND (COM-QUIT :NAME "Quit" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-QUIT%ACCEPTOR%1
; compiling DEFINE-COMMAND (COM-QUIT :NAME "Quit" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; recognizing DEFUN COM-QUIT%PARTIAL%2
; compiling DEFINE-COMMAND (COM-QUIT :NAME "Quit" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; recognizing DEFUN |COM-QUIT%unparser%3|
; compiling DEFINE-COMMAND (COM-QUIT :NAME "Quit" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-SELF-INSERT
; compiling DEFINE-COMMAND COM-SELF-INSERT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-SELF-INSERT%ACCEPTOR%4
; compiling DEFINE-COMMAND COM-SELF-INSERT: 
; compiling top level form: 
; recognizing DEFUN COM-SELF-INSERT%PARTIAL%5
; compiling DEFINE-COMMAND COM-SELF-INSERT: 
; compiling top level form: 
; recognizing DEFUN |COM-SELF-INSERT%unparser%6|
; compiling DEFINE-COMMAND COM-SELF-INSERT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-OBJECT
; compiling DEFINE-COMMAND COM-BACKWARD-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-OBJECT%ACCEPTOR%7
; compiling DEFINE-COMMAND COM-BACKWARD-OBJECT: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-OBJECT%PARTIAL%8
; compiling DEFINE-COMMAND COM-BACKWARD-OBJECT: 
; compiling top level form: 
; recognizing DEFUN |COM-BACKWARD-OBJECT%unparser%9|
; compiling DEFINE-COMMAND COM-BACKWARD-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-FORWARD-OBJECT
; compiling DEFINE-COMMAND COM-FORWARD-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-FORWARD-OBJECT%ACCEPTOR%10
; compiling DEFINE-COMMAND COM-FORWARD-OBJECT: 
; compiling top level form: 
; recognizing DEFUN COM-FORWARD-OBJECT%PARTIAL%11
; compiling DEFINE-COMMAND COM-FORWARD-OBJECT: 
; compiling top level form: 
; recognizing DEFUN |COM-FORWARD-OBJECT%unparser%12|
; compiling DEFINE-COMMAND COM-FORWARD-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BEGINNING-OF-LINE
; compiling DEFINE-COMMAND COM-BEGINNING-OF-LINE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BEGINNING-OF-LINE%ACCEPTOR%13
; compiling DEFINE-COMMAND COM-BEGINNING-OF-LINE: 
; compiling top level form: 
; recognizing DEFUN COM-BEGINNING-OF-LINE%PARTIAL%14
; compiling DEFINE-COMMAND COM-BEGINNING-OF-LINE: 
; compiling top level form: 
; recognizing DEFUN |COM-BEGINNING-OF-LINE%unparser%15|
; compiling DEFINE-COMMAND COM-BEGINNING-OF-LINE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-END-OF-LINE
; compiling DEFINE-COMMAND COM-END-OF-LINE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-END-OF-LINE%ACCEPTOR%16
; compiling DEFINE-COMMAND COM-END-OF-LINE: 
; compiling top level form: 
; recognizing DEFUN COM-END-OF-LINE%PARTIAL%17
; compiling DEFINE-COMMAND COM-END-OF-LINE: 
; compiling top level form: 
; recognizing DEFUN |COM-END-OF-LINE%unparser%18|
; compiling DEFINE-COMMAND COM-END-OF-LINE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-DELETE-OBJECT
; compiling DEFINE-COMMAND COM-DELETE-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-DELETE-OBJECT%ACCEPTOR%19
; compiling DEFINE-COMMAND COM-DELETE-OBJECT: 
; compiling top level form: 
; recognizing DEFUN COM-DELETE-OBJECT%PARTIAL%20
; compiling DEFINE-COMMAND COM-DELETE-OBJECT: 
; compiling top level form: 
; recognizing DEFUN |COM-DELETE-OBJECT%unparser%21|
; compiling DEFINE-COMMAND COM-DELETE-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-DELETE-OBJECT
; compiling DEFINE-COMMAND COM-BACKWARD-DELETE-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-DELETE-OBJECT%ACCEPTOR%22
; compiling DEFINE-COMMAND COM-BACKWARD-DELETE-OBJECT: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-DELETE-OBJECT%PARTIAL%23
; compiling DEFINE-COMMAND COM-BACKWARD-DELETE-OBJECT: 
; compiling top level form: 
; recognizing DEFUN |COM-BACKWARD-DELETE-OBJECT%unparser%24|
; compiling DEFINE-COMMAND COM-BACKWARD-DELETE-OBJECT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-PREVIOUS-LINE
; compiling DEFINE-COMMAND COM-PREVIOUS-LINE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-PREVIOUS-LINE%ACCEPTOR%25
; compiling DEFINE-COMMAND COM-PREVIOUS-LINE: 
; compiling top level form: 
; recognizing DEFUN COM-PREVIOUS-LINE%PARTIAL%26
; compiling DEFINE-COMMAND COM-PREVIOUS-LINE: 
; compiling top level form: 
; recognizing DEFUN |COM-PREVIOUS-LINE%unparser%27|
; compiling DEFINE-COMMAND COM-PREVIOUS-LINE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-NEXT-LINE
; compiling DEFINE-COMMAND COM-NEXT-LINE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-NEXT-LINE%ACCEPTOR%28
; compiling DEFINE-COMMAND COM-NEXT-LINE: 
; compiling top level form: 
; recognizing DEFUN COM-NEXT-LINE%PARTIAL%29
; compiling DEFINE-COMMAND COM-NEXT-LINE: 
; compiling top level form: 
; recognizing DEFUN |COM-NEXT-LINE%unparser%30|
; compiling DEFINE-COMMAND COM-NEXT-LINE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-OPEN-LINE
; compiling DEFINE-COMMAND COM-OPEN-LINE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-OPEN-LINE%ACCEPTOR%31
; compiling DEFINE-COMMAND COM-OPEN-LINE: 
; compiling top level form: 
; recognizing DEFUN COM-OPEN-LINE%PARTIAL%32
; compiling DEFINE-COMMAND COM-OPEN-LINE: 
; compiling top level form: 
; recognizing DEFUN |COM-OPEN-LINE%unparser%33|
; compiling DEFINE-COMMAND COM-OPEN-LINE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-KILL-LINE
; compiling DEFINE-COMMAND COM-KILL-LINE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-KILL-LINE%ACCEPTOR%34
; compiling DEFINE-COMMAND COM-KILL-LINE: 
; compiling top level form: 
; recognizing DEFUN COM-KILL-LINE%PARTIAL%35
; compiling DEFINE-COMMAND COM-KILL-LINE: 
; compiling top level form: 
; recognizing DEFUN |COM-KILL-LINE%unparser%36|
; compiling DEFINE-COMMAND COM-KILL-LINE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-FORWARD-WORD
; compiling DEFINE-COMMAND COM-FORWARD-WORD: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-FORWARD-WORD%ACCEPTOR%37
; compiling DEFINE-COMMAND COM-FORWARD-WORD: 
; compiling top level form: 
; recognizing DEFUN COM-FORWARD-WORD%PARTIAL%38
; compiling DEFINE-COMMAND COM-FORWARD-WORD: 
; compiling top level form: 
; recognizing DEFUN |COM-FORWARD-WORD%unparser%39|
; compiling DEFINE-COMMAND COM-FORWARD-WORD: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-WORD
; compiling DEFINE-COMMAND COM-BACKWARD-WORD: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-WORD%ACCEPTOR%40
; compiling DEFINE-COMMAND COM-BACKWARD-WORD: 
; compiling top level form: 
; recognizing DEFUN COM-BACKWARD-WORD%PARTIAL%41
; compiling DEFINE-COMMAND COM-BACKWARD-WORD: 
; compiling top level form: 
; recognizing DEFUN |COM-BACKWARD-WORD%unparser%42|
; compiling DEFINE-COMMAND COM-BACKWARD-WORD: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-TOGGLE-LAYOUT
; compiling DEFINE-COMMAND COM-TOGGLE-LAYOUT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-TOGGLE-LAYOUT%ACCEPTOR%43
; compiling DEFINE-COMMAND COM-TOGGLE-LAYOUT: 
; compiling top level form: 
; recognizing DEFUN COM-TOGGLE-LAYOUT%PARTIAL%44
; compiling DEFINE-COMMAND COM-TOGGLE-LAYOUT: 
; compiling top level form: 
; recognizing DEFUN |COM-TOGGLE-LAYOUT%unparser%45|
; compiling DEFINE-COMMAND COM-TOGGLE-LAYOUT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-EXTENDED-COMMAND
; compiling DEFINE-COMMAND COM-EXTENDED-COMMAND: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-EXTENDED-COMMAND%ACCEPTOR%46
; compiling DEFINE-COMMAND COM-EXTENDED-COMMAND: 
; compiling top level form: 
; recognizing DEFUN COM-EXTENDED-COMMAND%PARTIAL%47
; compiling DEFINE-COMMAND COM-EXTENDED-COMMAND: 
; compiling top level form: 
; recognizing DEFUN |COM-EXTENDED-COMMAND%unparser%48|
; compiling DEFINE-COMMAND COM-EXTENDED-COMMAND: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-INSERT-WEIRD-STUFF
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling DEFINE-COMMAND COM-INSERT-WEIRD-STUFF: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-INSERT-WEIRD-STUFF%ACCEPTOR%49
; compiling DEFINE-COMMAND COM-INSERT-WEIRD-STUFF: 
; compiling top level form: 
; recognizing DEFUN COM-INSERT-WEIRD-STUFF%PARTIAL%50
; compiling DEFINE-COMMAND COM-INSERT-WEIRD-STUFF: 
; compiling top level form: 
; recognizing DEFUN |COM-INSERT-WEIRD-STUFF%unparser%51|
; compiling DEFINE-COMMAND COM-INSERT-WEIRD-STUFF: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-INSERT-REVERSED-STRING
; compiling DEFINE-COMMAND COM-INSERT-REVERSED-STRING: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-INSERT-REVERSED-STRING%ACCEPTOR%52
; compiling DEFINE-COMMAND COM-INSERT-REVERSED-STRING: 
; compiling top level form: 
; recognizing DEFUN COM-INSERT-REVERSED-STRING%PARTIAL%53
; compiling DEFINE-COMMAND COM-INSERT-REVERSED-STRING: 
; compiling top level form: 
; recognizing DEFUN |COM-INSERT-REVERSED-STRING%unparser%54|
; compiling DEFINE-COMMAND COM-INSERT-REVERSED-STRING: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-TYPE COMPLETABLE-PATHNAME: 
; compiling DEFINE-PRESENTATION-TYPE COMPLETABLE-PATHNAME: 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-TYPE COMPLETABLE-PATHNAME: 
; compiling top level form: 
; recognizing DEFUN FILENAME-COMPLETER
; compiling DEFUN FILENAME-COMPLETER: 
; compiling top level form: 
; compiling DEFINE-PRESENTATION-METHOD ACCEPT: 
; compiling top level form (SB-KERNEL:FIND-CLASSOID-CELL (QUOTE CLIM:TEXTUAL-VIEW)): 
; compiling top level form: 
; recognizing DEFUN PATHNAME-FILENAME
; compiling DEFUN PATHNAME-FILENAME: 
; compiling top level form: 
; recognizing DEFUN COM-FIND-FILE
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFINE-COMMAND (COM-FIND-FILE :NAME "Find File" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-FIND-FILE%ACCEPTOR%55
; compiling DEFINE-COMMAND (COM-FIND-FILE :NAME "Find File" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; recognizing DEFUN COM-FIND-FILE%PARTIAL%56
; compiling DEFINE-COMMAND (COM-FIND-FILE :NAME "Find File" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; recognizing DEFUN |COM-FIND-FILE%unparser%57|
; compiling DEFINE-COMMAND (COM-FIND-FILE :NAME "Find File" :COMMAND-TABLE CLIMACS): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-SAVE-BUFFER
; compiling DEFINE-COMMAND COM-SAVE-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-SAVE-BUFFER%ACCEPTOR%58
; compiling DEFINE-COMMAND COM-SAVE-BUFFER: 
; compiling top level form: 
; recognizing DEFUN COM-SAVE-BUFFER%PARTIAL%59
; compiling DEFINE-COMMAND COM-SAVE-BUFFER: 
; compiling top level form: 
; recognizing DEFUN |COM-SAVE-BUFFER%unparser%60|
; compiling DEFINE-COMMAND COM-SAVE-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-WRITE-BUFFER
; compiling DEFINE-COMMAND COM-WRITE-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-WRITE-BUFFER%ACCEPTOR%61
; compiling DEFINE-COMMAND COM-WRITE-BUFFER: 
; compiling top level form: 
; recognizing DEFUN COM-WRITE-BUFFER%PARTIAL%62
; compiling DEFINE-COMMAND COM-WRITE-BUFFER: 
; compiling top level form: 
; recognizing DEFUN |COM-WRITE-BUFFER%unparser%63|
; compiling DEFINE-COMMAND COM-WRITE-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BEGINNING-OF-BUFFER
; compiling DEFINE-COMMAND COM-BEGINNING-OF-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BEGINNING-OF-BUFFER%ACCEPTOR%64
; compiling DEFINE-COMMAND COM-BEGINNING-OF-BUFFER: 
; compiling top level form: 
; recognizing DEFUN COM-BEGINNING-OF-BUFFER%PARTIAL%65
; compiling DEFINE-COMMAND COM-BEGINNING-OF-BUFFER: 
; compiling top level form: 
; recognizing DEFUN |COM-BEGINNING-OF-BUFFER%unparser%66|
; compiling DEFINE-COMMAND COM-BEGINNING-OF-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-END-OF-BUFFER
; compiling DEFINE-COMMAND COM-END-OF-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-END-OF-BUFFER%ACCEPTOR%67
; compiling DEFINE-COMMAND COM-END-OF-BUFFER: 
; compiling top level form: 
; recognizing DEFUN COM-END-OF-BUFFER%PARTIAL%68
; compiling DEFINE-COMMAND COM-END-OF-BUFFER: 
; compiling top level form: 
; recognizing DEFUN |COM-END-OF-BUFFER%unparser%69|
; compiling DEFINE-COMMAND COM-END-OF-BUFFER: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BROWSE-URL
; compiling DEFINE-COMMAND COM-BROWSE-URL: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-BROWSE-URL%ACCEPTOR%70
; compiling DEFINE-COMMAND COM-BROWSE-URL: 
; compiling top level form: 
; recognizing DEFUN COM-BROWSE-URL%PARTIAL%71
; compiling DEFINE-COMMAND COM-BROWSE-URL: 
; compiling top level form: 
; recognizing DEFUN |COM-BROWSE-URL%unparser%72|
; compiling DEFINE-COMMAND COM-BROWSE-URL: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-SET-MARK
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFINE-COMMAND COM-SET-MARK: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-SET-MARK%ACCEPTOR%73
; compiling DEFINE-COMMAND COM-SET-MARK: 
; compiling top level form: 
; recognizing DEFUN COM-SET-MARK%PARTIAL%74
; compiling DEFINE-COMMAND COM-SET-MARK: 
; compiling top level form: 
; recognizing DEFUN |COM-SET-MARK%unparser%75|
; compiling DEFINE-COMMAND COM-SET-MARK: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-COPY-IN
; compiling DEFINE-COMMAND COM-COPY-IN: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-COPY-IN%ACCEPTOR%76
; compiling DEFINE-COMMAND COM-COPY-IN: 
; compiling top level form: 
; recognizing DEFUN COM-COPY-IN%PARTIAL%77
; compiling DEFINE-COMMAND COM-COPY-IN: 
; compiling top level form: 
; recognizing DEFUN |COM-COPY-IN%unparser%78|
; compiling DEFINE-COMMAND COM-COPY-IN: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-CUT-IN
; compiling DEFINE-COMMAND COM-CUT-IN: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-CUT-IN%ACCEPTOR%79
; compiling DEFINE-COMMAND COM-CUT-IN: 
; compiling top level form: 
; recognizing DEFUN COM-CUT-IN%PARTIAL%80
; compiling DEFINE-COMMAND COM-CUT-IN: 
; compiling top level form: 
; recognizing DEFUN |COM-CUT-IN%unparser%81|
; compiling DEFINE-COMMAND COM-CUT-IN: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-CUT-OUT
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFINE-COMMAND COM-CUT-OUT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-CUT-OUT%ACCEPTOR%82
; compiling DEFINE-COMMAND COM-CUT-OUT: 
; compiling top level form: 
; recognizing DEFUN COM-CUT-OUT%PARTIAL%83
; compiling DEFINE-COMMAND COM-CUT-OUT: 
; compiling top level form: 
; recognizing DEFUN |COM-CUT-OUT%unparser%84|
; compiling DEFINE-COMMAND COM-CUT-OUT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-COPY-OUT
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-ACCESSOR # # ...)): 
; compiling DEFINE-COMMAND COM-COPY-OUT: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-COPY-OUT%ACCEPTOR%85
; compiling DEFINE-COMMAND COM-COPY-OUT: 
; compiling top level form: 
; recognizing DEFUN COM-COPY-OUT%PARTIAL%86
; compiling DEFINE-COMMAND COM-COPY-OUT: 
; compiling top level form: 
; recognizing DEFUN |COM-COPY-OUT%unparser%87|
; compiling DEFINE-COMMAND COM-COPY-OUT: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-KR-ROTATE
; compiling DEFINE-COMMAND COM-KR-ROTATE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-KR-ROTATE%ACCEPTOR%88
; compiling DEFINE-COMMAND COM-KR-ROTATE: 
; compiling top level form: 
; recognizing DEFUN COM-KR-ROTATE%PARTIAL%89
; compiling DEFINE-COMMAND COM-KR-ROTATE: 
; compiling top level form: 
; recognizing DEFUN |COM-KR-ROTATE%unparser%90|
; compiling DEFINE-COMMAND COM-KR-ROTATE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-KR-RESIZE
; compiling DEFINE-COMMAND COM-KR-RESIZE: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN COM-KR-RESIZE%ACCEPTOR%91
; compiling DEFINE-COMMAND COM-KR-RESIZE: 
; compiling top level form: 
; recognizing DEFUN COM-KR-RESIZE%PARTIAL%92
; compiling DEFINE-COMMAND COM-KR-RESIZE: 
; compiling top level form: 
; recognizing DEFUN |COM-KR-RESIZE%unparser%93|
; compiling DEFINE-COMMAND COM-KR-RESIZE: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form (SB-KERNEL:MAKE-VALUE-CELL (SB-PCL::ENSURE-CTOR # # ...)): 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN GLOBAL-SET-KEY
; compiling DEFUN GLOBAL-SET-KEY: 
; compiling top level form: 
; compiling LOOP FOR: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; recognizing DEFUN C-X-SET-KEY
; compiling DEFUN C-X-SET-KEY: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 
; compiling top level form: 

; /home/ejohnson/cvs-dir/climacs/gui.fasl written
; compilation finished in 0:00:05
NIL
CL-USER> (climacs-gui::climacs)
No such offset: 11
No such offset: 11
NIL
CL-USER> (climacs-gui::climacs)
NIL
CL-USER> (climacs-gui::climacs)
NIL
CL-USER> 