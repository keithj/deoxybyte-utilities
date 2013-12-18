;;;
;;; Copyright (c) 2007-2013 Keith James. All rights reserved.
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

(asdf:load-system :deoxybyte-systems)

(in-package :uk.co.deoxybyte-systems)

(defsystem deoxybyte-utilities
  :name "deoxybyte-utilities"
  :version "0.11.0"
  :author "Keith James"
  :licence "GPL v3"
  :in-order-to ((test-op (load-op :deoxybyte-utilities
                                  :deoxybyte-utilities-test))
                (doc-op (load-op :deoxybyte-utilities
                                 :cldoc)))
  :depends-on ((:version :deoxybyte-systems "1.0.0"))
  :components
  ((:module :deoxybyte-utilities
            :serial t
            :pathname "src/"
            :components ((:file "package")
                         (:file "conditions")
                         (:file "type-utilities")
                         (:file "deoxybyte-utilities")
                         (:file "numeric-utilities")
                         (:file "cons-utilities")
                         (:file "set-utilities")
                         (:file "vector-utilities")
                         (:file "string-utilities")
                         (:file "octet-vector-utilities")
                         (:file "clos-utilities")
                         (:file "finite-state-machine")
                         (:file "queue")
                         #+:sbcl(:file "sbcl")
                         #+:ccl (:file "ccl")
                         #-(or :sbcl :ccl) (:file "default"))))
  :perform (test-op :after (op c)
                    (maybe-run-lift-tests :deoxybyte-utilities
                                          "deoxybyte-utilities-test.config"))
  :perform (doc-op :after (op c)
                   (maybe-build-cldoc-docs :deoxybyte-utilities "doc/html")))
