;;;
;;; Copyright (C) 2009-2010 Keith James. All rights reserved.
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

(deftype octet ()
  "Alias for unsigned-byte 8."
  '(unsigned-byte 8))

(deftype nibble ()
  "Alias for unsigned-byte 4"
  '(unsigned-byte 4))

(deftype uint8 ()
  "Alias for unsigned-byte 8."
  '(unsigned-byte 8))

(deftype int8 ()
  "Alias for signed-byte 8."
  '(signed-byte 8))

(deftype uint16 ()
  "Alias for unsigned-byte 16."
  '(unsigned-byte 16))

(deftype int16 ()
  "Alias for signed-byte 16."
  '(signed-byte 16))

(deftype uint32 ()
  "Alias for unsigned-byte 32."
  '(unsigned-byte 32))

(deftype int32 ()
  "Alias for signed-byte 32."
  '(signed-byte 32))

;; array-index is deprecated in favour of vector-index
(deftype array-index ()
  "Array index type."
  '(and fixnum (integer 0 *)))

(deftype vector-index ()
  "Vector index type."
  '(and fixnum (integer 0 *)))

(deftype simple-octet-vector ()
  "Alias for (simple-array (unsigned-byte 8) (*)), using a naming
  convention analagous to simple-base-string."
  '(simple-array (unsigned-byte 8) (*)))
