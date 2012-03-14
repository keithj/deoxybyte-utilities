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

(in-package :uk.co.deoxybyte-utilities)

;;; Introspection utility functions
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

(defun all-external-generic-functions (package-name)
  "Returns a list of all the external generic functions in package
PACKAGE-NAME."
  (let ((generic-fns ()))
    (do-external-symbols (s (find-package package-name) generic-fns)
      (when (fboundp s)
        (let ((fn (symbol-function s)))
          (when (eql 'standard-generic-function (type-of fn))
            (push fn generic-fns)))))))

