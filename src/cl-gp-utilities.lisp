;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-gp-utilities)

(deftype array-index ()
  "Array index type."
  '(and fixnum (integer 0 *)))


;;; Core macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; eval-when to ensure with-gensyms is available at compile-time,
  ;; for use in other macros i.e. when we expect to use it!
  (defmacro with-gensyms ((&rest names) &body body)
    `(let ,(loop for n in names
              collect `(,n (gensym)))
       ,@body)))


;;; Array copying macro
(defmacro copy-array (source source-start source-end
                      dest dest-start &optional key)
  "Copies elements SOURCE indices SOURCE-START and SOURCE-END to DEST,
inserting them into DEST at DEST-START onwards. If the function KEY is
supplied, it is applied to each element of SOURCE prior to its
insertion into DEST."
  `(loop
      for si of-type array-index from ,source-start to ,source-end
      for di of-type array-index = ,dest-start
      then (the array-index (1+ di))
      do (setf (aref ,dest di) ,(if key
                                    `(funcall ,key (aref ,source si))
                                  `(aref ,source si)))))

;;; Generator utility functions
(defun current (gen)
  (funcall gen :current))

(defun next (gen)
  (funcall gen :next))

(defun has-more-p (gen)
  (funcall gen :more))

;;; Consumer utility functions
(defun consume (con &rest args)
  (apply con args))
