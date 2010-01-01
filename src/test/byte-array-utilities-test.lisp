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

(addtest (deoxybyte-utilities-tests) whitespace-bytes-p/1
  (let ((ws (mapcar #'char-code '(#\Space #\Tab #\Return
                                  #\Linefeed #\FormFeed)))
        (ct (loop
               for i from 65 to 90
               collect i)))
    (ensure (loop
               for c in ws
               always (whitespace-byte-p c)))
    (ensure (loop
               for c in ct
               never (whitespace-byte-p c)))
    (ensure (whitespace-bytes-p
             (make-array 5 :element-type 'octet :initial-contents ws)))
    (ensure (not (whitespace-bytes-p
                  (make-array 26 :element-type 'octet :initial-contents ct))))))

(addtest (deoxybyte-utilities-tests) content-bytes-p/1
  (let ((ws (mapcar #'char-code '(#\Space #\Tab #\Return
                                  #\Linefeed #\FormFeed)))
        (ct (loop
               for i from 65 to 90
               collect i)))
    (ensure (not (content-bytes-p
                  (make-array 5 :element-type 'octet :initial-contents ws))))
    (ensure (content-bytes-p
             (make-array 26 :element-type 'octet :initial-contents ct)))
    (ensure (content-bytes-p
             (make-array 31 :element-type 'octet
                         :initial-contents (append ws ct))))))


(addtest (deoxybyte-utilities-tests) make-sb-string/1
  (let ((bytes (make-array 2 :element-type 'octet :initial-contents '(65 65))))
    (ensure (subtypep (type-of (make-sb-string bytes)) 'simple-base-string))
    (ensure-error
      (make-sb-string bytes -1))
    (ensure-condition invalid-argument-error
      (make-sb-string bytes 99))
    (ensure-condition invalid-argument-error
      (make-sb-string bytes 1 0))
    (ensure-condition invalid-argument-error
      (make-sb-string bytes 0 99))))

(addtest (deoxybyte-utilities-tests) concat-into-sb-string/1
  (ensure (string=
           "AABBCC"
           (concat-into-sb-string
            (make-array 3 :initial-contents
                        (loop
                           for i from 65 to 67
                           collect (make-array 2 :element-type 'octet
                                               :initial-element i)))))))
