;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :uk.co.deoxybyte-utilities-system
  (:use :common-lisp :asdf :deoxybyte-systems))

(in-package :uk.co.deoxybyte-utilities-system)

(defsystem deoxybyte-utilities
  :name "deoxybyte-utilities"
  :version "0.5.1"
  :author "Keith James"
  :licence "GPL v3"
  :in-order-to ((test-op (load-op :deoxybyte-utilities
                                  :deoxybyte-utilities-test)))
  :components
  ((:module :deoxybyte-utilities
            :serial t
            :pathname "src/"
            :components ((:file "package")
                         (:file "conditions")
                         (:file "deoxybyte-utilities")
                         (:file "numeric-utilities")
                         (:file "cons-utilities")
                         (:file "vector-utilities")
                         (:file "string-utilities")
                         (:file "byte-array-utilities")
                         (:file "clos-utilities")
                         (:file "finite-state-machine")
                         (:file "queue")))
   (:lift-test-config :lift-tests
                      :pathname "deoxybyte-utilities-test.config"
                      :target-system :deoxybyte-utilities)
   (:cldoc-config :cldoc-documentation
                  :pathname "doc/html/"
                  :target-system :deoxybyte-utilities)))
