;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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

(in-package :cl-gp-utilities-test)

(addtest (cl-gp-utilities-tests) whitespace-bytes-p/1
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
             (make-array 5 :element-type '(unsigned-byte 8)
                         :initial-contents ws)))
    (ensure (not (whitespace-bytes-p
                  (make-array 26 :element-type '(unsigned-byte 8)
                              :initial-contents ct))))))

(addtest (cl-gp-utilities-tests) content-bytes-p/1
  (let ((ws (mapcar #'char-code '(#\Space #\Tab #\Return
                                  #\Linefeed #\FormFeed)))
        (ct (loop
               for i from 65 to 90
               collect i)))
    (ensure (not (content-bytes-p
                  (make-array 5 :element-type '(unsigned-byte 8)
                              :initial-contents ws))))
    (ensure (content-bytes-p
             (make-array 26 :element-type '(unsigned-byte 8)
                         :initial-contents ct)))
    (ensure (content-bytes-p
             (make-array 31 :element-type '(unsigned-byte 8)
                         :initial-contents (append ws ct))))))


(addtest (cl-gp-utilities-tests) make-sb-string/1
  (let ((bytes (make-array 2 :element-type '(unsigned-byte 8)
                           :initial-contents '(65 65))))
    (ensure (subtypep (type-of (make-sb-string bytes)) 'simple-base-string))
    (ensure-error
      (make-sb-string bytes -1))
    (ensure-condition invalid-argument-error
      (make-sb-string bytes 99))
    (ensure-condition invalid-argument-error
      (make-sb-string bytes 1 0))
    (ensure-condition invalid-argument-error
      (make-sb-string bytes 0 99))))

(addtest (cl-gp-utilities-tests) concat-into-sb-string/1
  (ensure (string=
           "AABBCC"
           (concat-into-sb-string
            (make-array 3 :initial-contents
                        (loop
                           for i from 65 to 67
                           collect
                             (make-array 2
                                         :element-type '(unsigned-byte 8)
                                         :initial-element i)))))))
