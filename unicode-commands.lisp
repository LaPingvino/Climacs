;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
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

;;; Unicode handling for the Climacs editor. 

(in-package :climacs-gui)

(define-command (com-insert-charcode :name t :command-table self-insert-table)
    ((code 'integer :prompt "Code point"))
  (insert-object (point (current-window)) (code-char code)))

(set-key '(com-insert-charcode 193) 'self-insert-table '((:dead--acute)(#\A)))
(set-key '(com-insert-charcode 201) 'self-insert-table '((:dead--acute)(#\E)))
(set-key '(com-insert-charcode 205) 'self-insert-table '((:dead--acute)(#\I)))
(set-key '(com-insert-charcode 211) 'self-insert-table '((:dead--acute)(#\O)))
(set-key '(com-insert-charcode 218) 'self-insert-table '((:dead--acute)(#\U)))
(set-key '(com-insert-charcode 221) 'self-insert-table '((:dead--acute)(#\Y)))
(set-key '(com-insert-charcode 225) 'self-insert-table '((:dead--acute)(#\a)))
(set-key '(com-insert-charcode 233) 'self-insert-table '((:dead--acute)(#\e)))
(set-key '(com-insert-charcode 237) 'self-insert-table '((:dead--acute)(#\i)))
(set-key '(com-insert-charcode 243) 'self-insert-table '((:dead--acute)(#\o)))
(set-key '(com-insert-charcode 250) 'self-insert-table '((:dead--acute)(#\u)))
(set-key '(com-insert-charcode 253) 'self-insert-table '((:dead--acute)(#\y)))
(set-key '(com-insert-charcode 199) 'self-insert-table '((:dead--acute)(#\C)))
(set-key '(com-insert-charcode 231) 'self-insert-table '((:dead--acute)(#\c)))
(set-key '(com-insert-charcode 215) 'self-insert-table '((:dead--acute)(#\x)))
(set-key '(com-insert-charcode 247) 'self-insert-table '((:dead--acute)(#\-)))
(set-key '(com-insert-charcode 222) 'self-insert-table '((:dead--acute)(#\T)))
(set-key '(com-insert-charcode 254) 'self-insert-table '((:dead--acute)(#\t)))
(set-key '(com-insert-charcode 223) 'self-insert-table '((:dead--acute)(#\s)))
(set-key '(com-insert-charcode 39) 'self-insert-table '((:dead--acute)(#\Space)))

(set-key '(com-insert-charcode 197) 'self-insert-table '((:dead--acute)(:dead--acute)(#\A)))
(set-key '(com-insert-charcode 229) 'self-insert-table '((:dead--acute)(:dead--acute)(#\a)))

(set-key '(com-insert-charcode 192) 'self-insert-table '((:dead--grave)(#\A)))
(set-key '(com-insert-charcode 200) 'self-insert-table '((:dead--grave)(#\E)))
(set-key '(com-insert-charcode 204) 'self-insert-table '((:dead--grave)(#\I)))
(set-key '(com-insert-charcode 210) 'self-insert-table '((:dead--grave)(#\O)))
(set-key '(com-insert-charcode 217) 'self-insert-table '((:dead--grave)(#\U)))
(set-key '(com-insert-charcode 224) 'self-insert-table '((:dead--grave)(#\a)))
(set-key '(com-insert-charcode 232) 'self-insert-table '((:dead--grave)(#\e)))
(set-key '(com-insert-charcode 236) 'self-insert-table '((:dead--grave)(#\i)))
(set-key '(com-insert-charcode 242) 'self-insert-table '((:dead--grave)(#\o)))
(set-key '(com-insert-charcode 249) 'self-insert-table '((:dead--grave)(#\u)))
(set-key '(com-insert-charcode 96) 'self-insert-table '((:dead--grave)(#\Space)))

(set-key '(com-insert-charcode 196) 'self-insert-table '((:dead--diaeresis :shift)(#\A)))
(set-key '(com-insert-charcode 203) 'self-insert-table '((:dead--diaeresis :shift)(#\E)))
(set-key '(com-insert-charcode 207) 'self-insert-table '((:dead--diaeresis :shift)(#\I)))
(set-key '(com-insert-charcode 214) 'self-insert-table '((:dead--diaeresis :shift)(#\O)))
(set-key '(com-insert-charcode 220) 'self-insert-table '((:dead--diaeresis :shift)(#\U)))
(set-key '(com-insert-charcode 228) 'self-insert-table '((:dead--diaeresis :shift)(#\a)))
(set-key '(com-insert-charcode 235) 'self-insert-table '((:dead--diaeresis :shift)(#\e)))
(set-key '(com-insert-charcode 239) 'self-insert-table '((:dead--diaeresis :shift)(#\i)))
(set-key '(com-insert-charcode 246) 'self-insert-table '((:dead--diaeresis :shift)(#\o)))
(set-key '(com-insert-charcode 252) 'self-insert-table '((:dead--diaeresis :shift)(#\u)))
(set-key '(com-insert-charcode 255) 'self-insert-table '((:dead--diaeresis :shift)(#\y)))
(set-key '(com-insert-charcode 34) 'self-insert-table '((:dead--diaeresis :shift)(#\Space)))

(set-key '(com-insert-charcode 195) 'self-insert-table '((:dead--tilde :shift)(#\A)))
(set-key '(com-insert-charcode 209) 'self-insert-table '((:dead--tilde :shift)(#\N)))
(set-key '(com-insert-charcode 227) 'self-insert-table '((:dead--tilde :shift)(#\a)))
(set-key '(com-insert-charcode 241) 'self-insert-table '((:dead--tilde :shift)(#\n)))
(set-key '(com-insert-charcode 198) 'self-insert-table '((:dead--tilde :shift)(#\E)))
(set-key '(com-insert-charcode 230) 'self-insert-table '((:dead--tilde :shift)(#\e)))
(set-key '(com-insert-charcode 208) 'self-insert-table '((:dead--tilde :shift)(#\D)))
(set-key '(com-insert-charcode 240) 'self-insert-table '((:dead--tilde :shift)(#\d)))
(set-key '(com-insert-charcode 216) 'self-insert-table '((:dead--tilde :shift)(#\O)))
(set-key '(com-insert-charcode 248) 'self-insert-table '((:dead--tilde :shift)(#\o)))
(set-key '(com-insert-charcode 126) 'self-insert-table '((:dead--tilde :shift)(#\Space)))

(set-key '(com-insert-charcode 194) 'self-insert-table '((:dead--circumflex :shift)(#\A)))
(set-key '(com-insert-charcode 202) 'self-insert-table '((:dead--circumflex :shift)(#\E)))
(set-key '(com-insert-charcode 206) 'self-insert-table '((:dead--circumflex :shift)(#\I)))
(set-key '(com-insert-charcode 212) 'self-insert-table '((:dead--circumflex :shift)(#\O)))
(set-key '(com-insert-charcode 219) 'self-insert-table '((:dead--circumflex :shift)(#\U)))
(set-key '(com-insert-charcode 226) 'self-insert-table '((:dead--circumflex :shift)(#\a)))
(set-key '(com-insert-charcode 234) 'self-insert-table '((:dead--circumflex :shift)(#\e)))
(set-key '(com-insert-charcode 238) 'self-insert-table '((:dead--circumflex :shift)(#\i)))
(set-key '(com-insert-charcode 244) 'self-insert-table '((:dead--circumflex :shift)(#\o)))
(set-key '(com-insert-charcode 251) 'self-insert-table '((:dead--circumflex :shift)(#\u)))
(set-key '(com-insert-charcode 94) 'self-insert-table '((:dead--circumflex :shift)(#\Space)))
