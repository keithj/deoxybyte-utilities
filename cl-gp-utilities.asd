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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cl-system-utilities))

(defpackage #:cl-gp-utilities-system
  (:use :common-lisp :asdf :cl-system-utilities))


(in-package #:cl-gp-utilities-system)

(defsystem cl-gp-utilities
    :name "cl-gp-utilities"
    :author "Keith James"
    :version "0.2.0"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :cl-gp-utilities :cl-gp-utilities-test)))
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
                           (:file "clos-utilities")
                           (:file "finite-state-machine")))
     (:lift-test-config :lift-tests
                        :pathname "cl-gp-utilities-test.config"
                        :target-system :cl-gp-utilities)
     (:cldoc-config :cldoc-documentation
                    :pathname "doc/html"
                    :target-system :cl-gp-utilities)))
