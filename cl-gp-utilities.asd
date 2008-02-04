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
  (:use :common-lisp :asdf)
  (:export #:testsuite))


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
                           (:file "cl-gp-utilities")))))


(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          'cl-gp-utilities))))
  (operate 'load-op :cl-gp-utilities-test)
  (funcall (intern (string :run!) (string :fiveam))
           'cl-gp-utilities-system:testsuite))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   'cl-gp-utilities))))
  nil)
