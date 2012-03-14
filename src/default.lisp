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
  (error "IS-SUPERCLASS-P not supported on this implementation."))

(defun direct-superclasses (class)
  "Returns a list of the direct superclasses of CLASS."
  (error "DIRECT-SUPERCLASSES not supported on this implementation."))

(defun direct-subclasses (class)
  "Returns a list of the direct subclasses of CLASS."
  (error "DIRECT-SUBCLASSES not supported on this implementation."))

(defun all-superclasses (class &optional
                         (ceiling (find-class 'standard-object)))
  "Returns a list of all superclasses of CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  (error "ALL-SUPERCLASSES not supported on this implementation."))

(defun all-specialized-methods (class &optional
                                (ceiling (find-class 'standard-object)))
   "Returns a list of all methods specialized on CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
   (error (msg "ALL-SPECIALIZED-METHODS not supported"
               "on this implementation.")))

(defun all-specialized-generic-functions (class &optional
                                          (ceiling (find-class
                                                    'standard-object)))
  "Returns a list of all generic functions specialized on CLASS, up to,
but not including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  (error (msg "ALL-SPECIALIZED-GENERIC-FUNCTIONS not supported"
              "on this implementation.")))

(defun all-slots (class)
  "Returns a sorted list of all slots of CLASS."
  (error "ALL-SLOTS not supported on this implementation."))

(defun slot-documentation (slot class)
  "Returns the documentation string for SLOT of CLASS."
  (error "SLOT-DOCUMENTATION not supported on this implementation."))

(defun all-slot-definitions (class)
  "Returns a list of all slot definitions of CLASS."
  (error "ALL-SLOT-DEFINITIONS not supported on this implementation."))
