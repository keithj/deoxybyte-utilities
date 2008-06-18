;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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

(in-package :cl-gp-utilities-test)

(in-suite cl-gp-utilities-system:testsuite)

(test all-superclasses/one-arg
  "Test ALL-SUPERCLASSES."
  (is (equal (list (find-class 'x1))
             (all-superclasses (find-class 'x1))))
  (is (equal (mapcar #'find-class '(x1 x2))
             (all-superclasses (find-class 'x2))))
  (is (equal (mapcar #'find-class '(y1 x1 x2 x3))
             (all-superclasses (find-class 'x3)))))

(test all-specialized-methods
  "Test ALL-SPECIALIZED-METHODS."
  (is (subsetp (mapcar #'find-slot-reader-method
                       '(x1) (list #'slotx1-of))
               (all-specialized-methods (find-class 'x1))))
  (is (subsetp (mapcar #'find-slot-reader-method
                       '(x1 x2) (list #'slotx1-of #'slotx2-of))
               (all-specialized-methods (find-class 'x2))))
  (is (subsetp (mapcar #'find-slot-reader-method
                       '(x1 x2 x3 y1) (list #'slotx1-of #'slotx2-of
                                            #'slotx3-of #'sloty1-of))
               (all-specialized-methods (find-class 'x3)))))

(test all-specialized-generic-functions
  "Test ALL-SPECIALIZED-GENERIC-FUNCTIONS."
  (let ((mgf #+:sbcl #'sb-mop:method-generic-function
             #+:cmu #'mop:method-generic-function
             #+:lispworks #'clos:method-generic-function))
    (is (subsetp (mapcar mgf (all-specialized-methods (find-class 'x1)))
                 (all-specialized-generic-functions (find-class 'x1))))
    (is (subsetp (mapcar mgf (all-specialized-methods (find-class 'x2)))
                 (all-specialized-generic-functions (find-class 'x2))))
    (is (subsetp (mapcar mgf (all-specialized-methods (find-class 'x3)))
                 (all-specialized-generic-functions (find-class 'x3))))))

(test all-external-generic-functions
  (let ((external ()))
    (do-external-symbols (sym (find-package :common-lisp) external)
      (when (fboundp sym)
        (push (symbol-function sym) external)))
    (is-true (loop
                for gf in (all-external-generic-functions
                           (find-package :common-lisp))
                always (eql 'standard-generic-function (type-of gf))))
    (is-true (subsetp (all-external-generic-functions
                           (find-package :common-lisp)) external))))
