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
             (map-into (make-string len :element-type 'base-char) #'code-char
                       (replace (make-array len :element-type 'octet) octets
                                :start2 start :end2 end)))))))

(defun string-to-octets (str &optional (start 0) (end nil end-supplied-p))
  "Returns a new vector of octets created from the simple-string
STR, between indices START and END. The elements of the returned
vector are the result of calling char-code on the respective
characters of STR."
  (declare (optimize (speed 3)))
  (declare (type vector-index start))
  ;; This is here because we can get a significant speed boost for the
  ;; cases where we can determine that a string is a simple-base-string
  (macrolet
      ((map-string (type s)
         `(locally (declare (type ,type ,s))
            (cond ((and (zerop start) (not end-supplied-p))
                   (map-into (make-array (length ,s) :element-type 'octet)
                             #'char-code ,s))
                  (t
                   (let ((end (or end (length ,s))))
                     (declare (type vector-index end))
                     (check-arguments (<= 0 start end) (start end)
                                      "must satisfy (<= 0 start end)")
                     (let* ((len (- end start))
                            (tmp (make-string len :element-type 'base-char)))
                       (map-into
                        (make-array len :element-type 'octet)
                        #'char-code
                        (replace tmp ,s :start2 start :end2 end)))))))))
    (etypecase str
      (simple-base-string (map-string simple-base-string str))
      (simple-string (map-string simple-string str)))))
