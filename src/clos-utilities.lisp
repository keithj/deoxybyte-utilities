;;;
;;; Copyright (c) 2008-2011 Keith James. All rights reserved.
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

;;; Introspection utility functions
(defun has-superclass-p (class superclass)
  "Returns T if CLASS has SUPERCLASS, or NIL otherwise."
  #-(or :sbcl :cmu :lispworks :ccl)
  (error "IS-SUPERCLASS-P not supported on this implementation.")
  #+:sbcl (member superclass (sb-mop:compute-class-precedence-list class))
  #+:cmu (member superclass (mop:compute-class-precedence-list class))
  #+:lispworks (member superclass (clos:compute-class-precedence-list class))
  #+:ccl (member superclass (ccl:compute-class-precedence-list class)))

(defun direct-superclasses (class)
  "Returns a list of the direct superclasses of CLASS."
  #-(or :sbcl :cmu :lispworks :ccl)
  (error "DIRECT-SUPERCLASSES not supported on this implementation.")
  #+:sbcl (sb-mop:class-direct-superclasses class)
  #+:cmu (pcl:class-direct-superclasses class)
  #+:lispworks (clos:class-direct-superclasses class)
  #+:ccl (ccl:class-direct-superclasses class))

(defun direct-subclasses (class)
  "Returns a list of the direct subclasses of CLASS."
  #-(or :sbcl :cmu :lispworks :ccl)
  (error "DIRECT-SUBCLASSES not supported on this implementation.")
  #+:sbcl (sb-mop:class-direct-subclasses class)
  #+:cmu (pcl:class-direct-subclasses class)
  #+:lispworks (clos:class-direct-subclasses class)
  #+:ccl (ccl:class-direct-subclasses class))

(defun all-superclasses (class &optional
                         (ceiling (find-class 'standard-object)))
  "Returns a list of all superclasses of CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks :ccl)
  (error "ALL-SUPERCLASSES not supported on this implementation.")
  #+:sbcl (set-difference (sb-mop:compute-class-precedence-list class)
                          (sb-mop:compute-class-precedence-list ceiling))
  #+:cmu (set-difference (mop:compute-class-precedence-list class)
                         (mop:compute-class-precedence-list ceiling))
  #+:lispworks (set-difference (clos:class-precedence-list class)
                               (clos:class-precedence-list ceiling))
  #+:ccl (set-difference (ccl:class-precedence-list class)
                         (ccl:class-precedence-list ceiling)))

(defun all-classes (package-name &optional (superclass
                                            (find-class 'standard-object)))
  "Returns a list of all classes defined in package PACKAGE-NAME,
optionally restricted to those classes that have SUPERCLASS."
  (let ((package (find-package package-name))
        (classes ()))
    (do-symbols (s package classes)
      (let ((c (find-class s nil)))
        (when (and c
                   (eql package (symbol-package (class-name c)))
                   (has-superclass-p c superclass))
          (push c classes))))))

(defun all-external-classes (package-name
                             &optional (superclass (find-class t)))
  "Returns a list of all exported classes defined in package
PACKAGE-NAME, optionally restricted to those classes that have
SUPERCLASS."
  (let ((package (find-package package-name))
        (classes ()))
    (do-external-symbols (s package classes)
      (let ((c (find-class s nil)))
        (when (and c
                   (eql package (symbol-package (class-name c)))
                   (has-superclass-p c superclass))
          (push c classes))))))

(defun all-specialized-methods (class &optional
                                (ceiling (find-class 'standard-object)))
   "Returns a list of all methods specialized on CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
   #-(or :sbcl :cmu :lispworks :ccl)
   (error (msg "ALL-SPECIALIZED-METHODS not supported"
               "on this implementation."))
   (apply #'append
          (mapcar #+:sbcl #'sb-mop:specializer-direct-methods
                  #+:cmu #'mop:specializer-direct-methods
                  #+:lispworks #'clos:specializer-direct-methods
                  #+:ccl #'ccl:specializer-direct-methods
                  (all-superclasses class ceiling))))

(defun all-specialized-generic-functions (class &optional
                                          (ceiling (find-class
                                                    'standard-object)))
  "Returns a list of all generic functions specialized on CLASS, up to,
but not including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks :ccl)
  (error (msg "ALL-SPECIALIZED-GENERIC-FUNCTIONS not supported"
              "on this implementation."))
  (mapcar #+:sbcl #'sb-mop:method-generic-function
          #+:cmu #'mop:method-generic-function
          #+:lispworks #'clos:method-generic-function
          #+:ccl #'ccl:method-generic-function
          (all-specialized-methods class ceiling)))

(defun all-external-generic-functions (package-name)
  "Returns a list of all the external generic functions in package
PACKAGE-NAME."
  (let ((generic-fns ()))
    (do-external-symbols (s (find-package package-name) generic-fns)
      (when (fboundp s)
        (let ((fn (symbol-function s)))
          (when (eql 'standard-generic-function (type-of fn))
            (push fn generic-fns)))))))

(defun all-slots (class)
  "Returns a sorted list of all slots of CLASS."
  #-(or :sbcl :ccl)
  (error "ALL-SLOTS not supported on this implementation.")
  (flet ((slot-def-name (slot-def)
           #+:sbcl (sb-mop:slot-definition-name  slot-def)
           #+:ccl (ccl:slot-definition-name slot-def)))
    (stable-sort (mapcar #'slot-def-name (all-slot-definitions class))
                 #'string< :key #'symbol-name)))

(defun slot-documentation (slot class)
  "Returns the documentation string for SLOT of CLASS."
    #-(or :sbcl :ccl)
  (error "SLOT-DOCUMENTATION not supported on this implementation.")
  (check-arguments (and slot class) (slot class)
                   "neither slot nor class may be NIL")
  (flet ((slot-def-name (slot-def)
           #+:sbcl (sb-mop:slot-definition-name slot-def)
           #+:ccl (ccl:slot-definition-name slot-def)))
    (documentation (find slot (all-slot-definitions class)
                         :key #'slot-def-name) t)))

(defun all-slot-definitions (class)
  "Returns a list of all slot definitions of CLASS."
  #-(or :sbcl :ccl)
  (error "ALL-SLOT-DEFINITIONS not supported on this implementation.")
  #+:sbcl (progn
            (unless (sb-mop:class-finalized-p class)
              (sb-mop:finalize-inheritance class))
            (sb-mop:compute-slots class))
  #+:ccl (progn
           (unless (ccl:class-finalized-p class)
             (ccl:finalize-inheritance class))
           (ccl:compute-slots class)))
