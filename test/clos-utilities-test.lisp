;;;
;;; Copyright (c) 2008-2012 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-utilities.
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

(in-package :uk.co.deoxybyte-utilities-test)

(addtest (deoxybyte-utilities-tests) has-superclass-p/1
  (ensure (has-superclass-p (find-class 'x2) (find-class 'x1)))
  (ensure (has-superclass-p (find-class 'x3) (find-class 'x1)))
  (ensure (not (has-superclass-p (find-class 'y1) (find-class 'x1)))))

(addtest (deoxybyte-utilities-tests) all-superclasses/1
  (ensure (equal (list (find-class 'x1))
                 (all-superclasses (find-class 'x1))))
  (ensure (equal (mapcar #'find-class '(x1 x2))
                 (all-superclasses (find-class 'x2))))
  (ensure (equal (mapcar #'find-class '(y1 x1 x2 x3))
                 (all-superclasses (find-class 'x3)))))

(addtest (deoxybyte-utilities-tests) all-specialized-methods/1
  (ensure (subsetp (mapcar #'find-slot-reader-method
                                  '(x1) (list #'slotx1-of))
                   (all-specialized-methods (find-class 'x1))))
  (ensure (subsetp (mapcar #'find-slot-reader-method
                           '(x1 x2) (list #'slotx1-of #'slotx2-of))
                   (all-specialized-methods (find-class 'x2))))
  (ensure (subsetp (mapcar #'find-slot-reader-method
                           '(x1 x2 x3 y1) (list #'slotx1-of #'slotx2-of
                                                #'slotx3-of #'sloty1-of))
                   (all-specialized-methods (find-class 'x3)))))

(addtest (deoxybyte-utilities-tests) all-specialized-generic-functions/1
  (let ((mgf #+:sbcl #'sb-mop:method-generic-function
             #+:ccl #'ccl:method-generic-function))
    (ensure (subsetp (mapcar mgf (all-specialized-methods (find-class 'x1)))
                     (all-specialized-generic-functions (find-class 'x1))))
    (ensure (subsetp (mapcar mgf (all-specialized-methods (find-class 'x2)))
                     (all-specialized-generic-functions (find-class 'x2))))
    (ensure (subsetp (mapcar mgf (all-specialized-methods (find-class 'x3)))
                     (all-specialized-generic-functions (find-class 'x3))))))

(addtest (deoxybyte-utilities-tests) all-external-generic-functions/1
  (let ((external ()))
    (do-external-symbols (sym (find-package :common-lisp) external)
      (when (fboundp sym)
        (push (symbol-function sym) external)))
    (ensure (loop
               for gf in (all-external-generic-functions
                          (find-package :common-lisp))
               always (eql 'standard-generic-function (type-of gf))))
    (ensure (subsetp (all-external-generic-functions
                      (find-package :common-lisp)) external))))
