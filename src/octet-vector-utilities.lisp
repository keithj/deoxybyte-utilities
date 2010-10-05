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

(in-package :uk.co.deoxybyte-utilities)

(defun octets-to-string (octets &optional (start 0) (end nil end-supplied-p))
  "Returns a new simple-base-string created from the values in
OCTET-VECTOR, a simple-array of (unsigned-byte 8), between indices
START and END. The elements of the returned string are the result of
calling code-char on the respective elements of OCTET-VECTOR."
  (declare (optimize (speed 3)))
  (declare (type simple-octet-vector octets))
  (declare (type vector-index start))
  (cond ((and (zerop start) (not end-supplied-p))
         (map-into (make-array (length octets) :element-type 'base-char)
                   #'code-char octets))
        (t
         (let ((end (or end (length octets))))
           (declare (type vector-index end))
           (check-arguments (<= 0 start end) (start end)
                            "must satisfy (<= 0 start end)")
           (let ((len (- end start)))
             (map-into (make-array len :element-type 'base-char) #'code-char
                       (replace (make-array len :element-type 'octet) octets
                                :start2 start :end2 end)))))))

(defun string-to-octets (str &optional (start 0) (end nil end-supplied-p))
  "Returns a new vector of octets created from the simple-base-string
STR, between indices START and END. The elements of the returned
vector are the result of calling char-code on the respective
characters of STR."
  (declare (optimize (speed 3)))
  (declare (type simple-base-string str))
  (declare (type vector-index start))
  (cond ((and (zerop start) (not end-supplied-p))
         (map-into (make-array (length str) :element-type 'octet) #'char-code str))
        (t
         (let ((end (or end (length str))))
           (declare (type vector-index end))
           (check-arguments (<= 0 start end) (start end)
                            "must satisfy (<= 0 start end)")
           (let ((len (- end start)))
             (map-into (make-array len :element-type 'octet) #'char-code
                       (replace (make-array len :element-type 'base-char) str
                                :start2 start :end2 end)))))))

