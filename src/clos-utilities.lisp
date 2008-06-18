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

(in-package :cl-gp-utilities)

;;; Introspection utility functions
(defun all-superclasses (class &optional
                         (ceiling (find-class 'standard-object)))
  "Returns a list of all superclasses of CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks)
  (error "ALL-SUPERCLASSES not supported on this implementation.")
  #+:sbcl (set-difference (sb-mop:compute-class-precedence-list class)
                          (sb-mop:compute-class-precedence-list ceiling))
  #+:cmu (set-difference (mop:compute-class-precedence-list class)
                         (mop:compute-class-precedence-list ceiling))
  #+:lispworks (set-difference (clos:class-precedence-list class)
                               (clos:class-precedence-list ceiling)))

(defun all-specialized-methods (class &optional
                                (ceiling (find-class 'standard-object)))
   "Returns a list of all methods specialized on CLASS, up to, but not
including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
   #-(or :sbcl :cmu :lispworks)
   (error msg("ALL-SPECIALIZED-METHODS not supported"
              "on this implementation."))
   (apply #'append
          (mapcar #+:sbcl #'sb-mop:specializer-direct-methods
                  #+:cmu #'mop:specializer-direct-methods
                  #+:lispworks #'clos:specializer-direct-methods
                  (all-superclasses class ceiling))))

(defun all-specialized-generic-functions (class &optional
                                          (ceiling (find-class
                                                    'standard-object)))
  "Returns a list of all generic functions specialized on CLASS, up to,
but not including, class CEILING. The class CEILING defaults to
STANDARD-OBJECT."
  #-(or :sbcl :cmu :lispworks)
  (error msg("ALL-SPECIALIZED-GENERIC-FUNCTIONS not supported"
             "on this implementation."))
  (mapcar #+:sbcl #'sb-mop:method-generic-function
          #+:cmu #'mop:method-generic-function
          #+:lispworks #'clos:method-generic-function
          (all-specialized-methods class ceiling)))

(defun all-external-generic-functions (package)
  "Returns a list of all the external generic functions in PACKAGE."
  (let ((generic-fns ()))
    (do-external-symbols (s (find-package package) generic-fns)
      (when (fboundp s)
        (let ((fn (symbol-function s)))
          (when (eql 'standard-generic-function (type-of fn))
            (push fn generic-fns)))))
    generic-fns))
