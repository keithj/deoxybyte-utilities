;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-utilities-test)

(deftestsuite deoxybyte-utilities-tests ()
  ())

(defclass x1 ()
  ((slotx1 :initarg :slotx1
           :reader slotx1-of)))

(defclass y1 ()
  ((sloty1 :initarg :sloty1
           :reader sloty1-of)))

(defclass x2 (x1)
  ((slotx2 :initarg :slotx2
           :reader slotx2-of)))

(defclass x3 (x2 y1)
  ((slotx3 :initarg :slotx3
           :reader slotx3-of)))

(defun find-slot-reader-method (class-name generic-function)
  (find-method generic-function '() (list (find-class class-name))))
