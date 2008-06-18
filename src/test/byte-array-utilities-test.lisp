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

(in-suite cl-gp-utilities-system:testsuite)

(test whitespace/content-bytes-p
  (let ((ws (mapcar #'char-code '(#\Space #\Tab #\Return
                                  #\Linefeed #\FormFeed)))
        (ct (loop
               for i from 65 to 90
               collect i)))
    (is-true (loop
                for c in ws
                always (whitespace-byte-p c)))
    (is-true (loop
                for c in ct
                never (whitespace-byte-p c)))
    (is-true (whitespace-bytes-p
              (make-array 5 :element-type '(unsigned-byte 8)
                          :initial-contents ws)))
    (is-false (whitespace-bytes-p
               (make-array 26 :element-type '(unsigned-byte 8)
                           :initial-contents ct)))
    (is-false (content-bytes-p
               (make-array 5 :element-type '(unsigned-byte 8)
                           :initial-contents ws)))
    (is-true (content-bytes-p
              (make-array 26 :element-type '(unsigned-byte 8)
                          :initial-contents ct)))
    (is-true (content-bytes-p
              (make-array 31 :element-type '(unsigned-byte 8)
                          :initial-contents (append ws ct))))))

(test make-sb-string
  (let ((bytes (make-array 2 :element-type '(unsigned-byte 8)
                           :initial-contents '(65 65))))
    (is (subtypep (type-of (make-sb-string bytes)) 'simple-base-string))
    (signals error
        (make-sb-string bytes -1))
    (signals invalid-argument-error
      (make-sb-string bytes 99))
    (signals invalid-argument-error
      (make-sb-string bytes 1 0))
    (signals invalid-argument-error
      (make-sb-string bytes 0 99))))

(test concat-into-sb-string
  (is (string= "AABBCC"
               (concat-into-sb-string
                (make-array 3 :initial-contents
                            (loop
                               for i from 65 to 67
                               collect
                                 (make-array 2
                                             :element-type '(unsigned-byte 8)
                                             :initial-element i)))))))
