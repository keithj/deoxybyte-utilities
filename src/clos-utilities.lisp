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
(defun has-superclass-p (class superclass)
  "Returns T if CLASS has SUPERCLASS, or NIL otherwise."
  #-(or :sbcl :cmu :lispworks)
  (error "IS-SUPERCLASS-P not supported on this implementation.")
  #+:sbcl (member superclass (sb-mop:compute-class-precedence-list class))
  #+:cmu (member superclass (mop:compute-class-precedence-list class))
  #+:lispworks (member superclass (clos:class-precedence-list class)))

(defun direct-superclasses (class)
  "Returns a list of the direct superclasses of CLASS."
  #-(or :sbcl :cmu :lispworks)
  (error "DIRECT-SUPERCLASSES not supported on this implementation.")
  #+:sbcl (sb-mop:class-direct-superclasses class)
  #+:cmu (cmu:class-direct-superclasses class)
  #+:lispworks (clos:class-direct-superclasses class))

(defun direct-subclasses (class)
  "Returns a list of the direct subclasses of CLASS."
  #-(or :sbcl :cmu :lispworks)
  (error "DIRECT-SUBCLASSES not supported on this implementation.")
  #+:sbcl (sb-mop:class-direct-subclasses class)
  #+:cmu (cmu:class-direct-subclasses class)
  #+:lispworks (clos:class-direct-subclasses class))

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

(defun all-classes (package-name &optional (superclass (find-class 't)))
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
                             &optional (superclass (find-class 't)))
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

(defun all-external-generic-functions (package-name)
  "Returns a list of all the external generic functions in package
PACKAGE-NAME."
  (let ((generic-fns ()))
    (do-external-symbols (s (find-package package-name) generic-fns)
      (when (fboundp s)
        (let ((fn (symbol-function s)))
          (when (eql 'standard-generic-function (type-of fn))
            (push fn generic-fns)))))))

;; (in-package :cl-dot)

;; (defmethod graph-object-node ((graph (eql 'class-example))
;;                               (object class))
;;   (make-instance 'node
;;                  :attributes (list :label (class-name object)
;;                                    :shape :box
;;                                    :style :filled
;;                                    :fillcolor "#eeeeff"
;;                                    :fontname "Arial"
;;                                    :fontsize 6)))

;; (defmethod graph-object-pointed-to-by ((graph (eql 'class-example))
;;                                        (object class))
;;   (gpu:direct-subclasses object))


;; (let* ((data (gpu:all-classes :bs))
;;        (dgraph (generate-graph-from-roots 'class-example data
;;                                           '(:rankdir "BT"))))
;;   (dot-graph dgraph "/home/keith/test.png" :format :png))
