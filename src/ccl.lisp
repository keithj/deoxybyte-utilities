;;;
;;; Copyright (c) 2012 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-utilities)

;;; clos-utilities

;;; Introspection utility functions
(defun has-superclass-p (class superclass)
  "Returns T if CLASS has SUPERCLASS, or NIL otherwise."
  (member superclass (ccl:compute-class-precedence-list class)))

(defun direct-superclasses (class)
  "Returns a list of the direct superclasses of CLASS."
  (ccl:class-direct-superclasses class))

(defun direct-subclasses (class)
  "Returns a list of the direct subclasses of CLASS."
  (ccl:class-direct-subclasses class))

(defun all-superclasses (class &optional
                         (ceiling (find-class 'standard-object)))
  "Returns a list of all superclasses of CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  (set-difference (ccl:class-precedence-list class)
                  (ccl:class-precedence-list ceiling)))

(defun all-specialized-methods (class &optional
                                (ceiling (find-class 'standard-object)))
   "Returns a list of all methods specialized on CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
   (apply #'append (mapcar #'ccl:specializer-direct-methods
                           (all-superclasses class ceiling))))

(defun all-specialized-generic-functions (class &optional
                                          (ceiling (find-class
                                                    'standard-object)))
  "Returns a list of all generic functions specialized on CLASS, up to,
but not including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  (mapcar #'ccl:method-generic-function
          (all-specialized-methods class ceiling)))

(defun all-slots (class)
  "Returns a sorted list of all slots of CLASS."
  (flet ((slot-def-name (slot-def)
           (ccl:slot-definition-name slot-def)))
    (stable-sort (mapcar #'slot-def-name (all-slot-definitions class))
                 #'string< :key #'symbol-name)))

(defun slot-documentation (slot class)
  "Returns the documentation string for SLOT of CLASS."
  (check-arguments (and slot class) (slot class)
                   "neither slot nor class may be NIL")
  (flet ((slot-def-name (slot-def)
           (ccl:slot-definition-name slot-def)))
    (documentation (find slot (all-slot-definitions class)
                         :key #'slot-def-name) t)))

(defun all-slot-definitions (class)
  "Returns a list of all slot definitions of CLASS."
  (unless (ccl:class-finalized-p class)
    (ccl:finalize-inheritance class))
  (ccl:compute-slots class))
