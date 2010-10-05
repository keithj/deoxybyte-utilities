;;;
;;; Copyright (C) 2008-2010 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-utilities-test)

(addtest (deoxybyte-utilities-tests) octets-to-string/1
  (let ((octets (make-array 2 :element-type 'octet :initial-contents '(65 65))))
    (ensure (subtypep (type-of (octets-to-string octets)) 'simple-base-string))
    (ensure (equal "AA" (octets-to-string octets)))
    (ensure-error
      (octets-to-string octets -1))
    (ensure-condition invalid-argument-error
      (octets-to-string octets 99))
    (ensure-condition invalid-argument-error
      (octets-to-string octets 1 0))
    (ensure-error
      (octets-to-string octets 0 99))))
