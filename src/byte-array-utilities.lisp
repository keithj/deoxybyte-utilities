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

(defvar *whitespace-codes*
  (make-array 5 :element-type 'octet
              :initial-contents (mapcar #'char-code
                                        '(#\Space #\Tab #\Return
                                          #\Linefeed #\Page)))
  "Character codes of whitespace characters.")

;;; byte array utility functions
(defun whitespace-byte-p (byte)
  "Returns T if BYTE is one of the currently bound set of whitespace
codes (defaults to codes of #\Space #\Tab #\Return #\Linefeed and
#\FormFeed), or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-octet-vector *whitespace-codes*)
           (type octet byte))
  (loop for w across *whitespace-codes*
     thereis (= w byte)))

(defun whitespace-bytes-p (bytes)
  "Returns T if all the bytes in BYTES are whitespace codes as defined
by WHITESPACE-BYTE-P, or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-octet-vector bytes))
  (loop for b across bytes
     always (whitespace-byte-p b)))

(defun content-bytes-p (bytes)
  "Returns T if any of BYTES are not whitespace codes as defined by
WHITESPACE-BYTE-P, or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-octet-vector bytes))
  (loop for b across bytes
     thereis (not (whitespace-byte-p b))))

(defun has-byte-at-p (bytes byte index)
  "Returns T if array BYTES has BYTE at INDEX."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-octet-vector bytes)
           (type octet byte))
  (and (not (zerop (length bytes)))
       (= byte (aref bytes index))))

(defun starts-with-byte-p (bytes byte)
  "Returns T if array BYTES has BYTE at index 0."
  (has-byte-at-p bytes byte 0))

(defun make-sb-string (byte-array &optional (source-start 0) source-end)
  "Returns a new simple-base-string created from the values in
BYTE-ARRAY, a simple-array of (unsigned-byte 8), between indices
SOURCE-START and SOURCE-END, inclusive. SOURCE start defaults to 0 and
SOURCE-END defaults to NIL. The elements of the returned string are
the result of calling code-char on the respective elements of
BYTE-ARRAY."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-octet-vector byte-array))
  (let ((source-end (or source-end (1- (length byte-array)))))
    (declare (type fixnum source-start source-end))
    (let ((source-len (length byte-array)))
      (cond ((zerop source-len)
             (make-string 0 :element-type 'base-char))
            (t
             (check-arguments (and (<= 0 source-start)
                                   (> source-len source-start)) (source-start)
                                   "source-start must be >= 0 and be <= ~a"
                                   source-len)
             (check-arguments (<= source-start source-end)
                              (source-start source-end)
                              "source-start must be <= source-end")
             (check-arguments (<= source-end source-len) (source-end)
                              "source-end must be >= 0 and be <= ~a" source-len)
             (let ((dest-length (1+ (- source-end source-start))))
               (declare (type vector-index dest-length))
               (let ((string (make-string dest-length
                                          :element-type 'base-char)))
                 (copy-array byte-array source-start source-end
                             string 0 #'code-char)
                 string)))))))

(defun concat-into-sb-string (byte-arrays)
  "Returns a new simple-base-string created by concatenating, in the
order supplied, the simple-arrays of (unsigned-byte 8) contained in
the vector BYTE-ARRAYS. The elements of the returned string are the
result of calling code-char on the contents of the respective elements
of BYTE-ARRAYS."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type vector byte-arrays))
  (let ((new-str (make-string (reduce #'+ byte-arrays :key #'length)
                              :element-type 'base-char))
        (num-arrays (length byte-arrays)))
    (do ((i 0 (1+ i))
         (offset 0))
        ((= i num-arrays) new-str)
      (let ((byte-array (aref byte-arrays i)))
        (declare (type simple-octet-vector byte-array)
                 (type vector-index offset))
        (unless (zerop (length byte-array))
          (copy-array byte-array 0 (1- (length byte-array))
                      new-str offset #'code-char)
          (incf offset (length byte-array)))))))
