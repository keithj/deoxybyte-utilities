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

(in-package :cl-user)

(defpackage #:cl-gp-utilities-system
  (:use :common-lisp :asdf))


(in-package #:cl-gp-utilities-system)

(defsystem cl-gp-utilities
    :name "cl-gp-utilities"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL v3"
    :components
    ((:module :cl-gp-utilities
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "conditions")
                           (:file "cl-gp-utilities")
                           (:file "numeric-utilities")
                           (:file "cons-utilities")
                           (:file "vector-utilities")
                           (:file "string-utilities")
                           (:file "byte-array-utilities")
                           (:file "clos-utilities")))))


(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          :cl-gp-utilities))))
  (operate 'load-op :cl-gp-utilities-test)

  (let ((*default-pathname-defaults* (component-pathname c)))
    (funcall (intern (string :run-tests) (string :lift))
             :config "./cl-gp-utilities-test.config")))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   :cl-gp-utilities))))
  nil)

(defmethod perform ((op cldoc-op) (c (eql (find-system
                                           :cl-gp-utilities))))
  (unless (find-package :cl-gp-utilities)
    (operate 'load-op :cl-gp-utilities))

  (let ((*default-pathname-defaults* (component-pathname c))
        (fn-sym (intern (string :extract-documentation) (string :cldoc)))
        (op-sym (intern (string :html) (string :cldoc))))
    (funcall fn-sym op-sym "./doc/html" c)))
