;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
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

(in-package :cl-gp-utilities)

(defvar *whitespace-codes*
  (make-array 5 :element-type '(unsigned-byte 8)
              :initial-contents (mapcar #'char-code
                                        '(#\Space #\Tab #\Return
                                          #\Linefeed #\FormFeed)))
  "Character codes of whitespace characters.")

;;; byte array utility functions
(defun whitespace-byte-p (byte)
  "Returns T if BYTE is one of the currently bound set of whitespace
codes (defaults to codes of #\Space #\Tab #\Return #\Linefeed and
#\FormFeed), or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) *whitespace-codes*)
           (type (unsigned-byte 8) byte))
  (loop for w across *whitespace-codes*
     thereis (= w byte)))

(defun whitespace-bytes-p (bytes)
  "Returns T if all the bytes in BYTES are whitespace codes as defined
by WHITESPACE-BYTE-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (loop for b across bytes
     always (whitespace-byte-p b)))

(defun content-bytes-p (bytes)
  "Returns T if any of BYTES are not whitespace codes as defined by
WHITESPACE-BYTE-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes))
  (loop for b across bytes
     thereis (not (whitespace-byte-p b))))

(defun has-byte-at-p (bytes byte index)
  "Returns T if array BYTES has BYTE at INDEX."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) bytes)
           (type (unsigned-byte 8) byte))
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
  (declare (optimize (speed 3) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) byte-array))
  (let ((source-end (or source-end (1- (length byte-array)))))
    (declare (type array-index source-start source-end))
    (let ((source-len (length byte-array)))
      (cond ((zerop source-len)
             (make-string 0 :element-type 'base-char))
            ((or (< source-start 0)
                 (>= source-start source-len))
             (error 'invalid-argument-error
                    :params 'source-start
                    :args source-start
                    :text
                    (format nil "source-start must be >= 0 and be <= ~a"
                            source-len)))
            ((> source-start source-end)
             (error 'invalid-argument-error
                    :params '(source-start source-end)
                    :args (list source-start source-end)
                    :text "source-start must be <= source-end"))
            ((>= source-end source-len)
             (error 'invalid-argument-error
                    :params 'source-end
                    :args source-end
                    :text
                    (format nil "source-end must be >= 0 and be <= ~a"
                            source-len)))
            (t
             (let ((dest-length (1+ (- source-end source-start))))
               (declare (type array-index dest-length))
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
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type vector byte-arrays))
  (let ((new-str (make-string (reduce #'+ byte-arrays :key #'length)
                              :element-type 'base-char))
        (num-arrays (length byte-arrays)))
    (do ((i 0 (1+ i))
         (offset 0))
        ((= i num-arrays) new-str)
      (let ((byte-array (aref byte-arrays i)))
        (declare (type (simple-array (unsigned-byte 8)) byte-array)
                 (type array-index offset))
        (unless (zerop (length byte-array))
          (copy-array byte-array 0 (1- (length byte-array))
                      new-str offset #'code-char)
          (incf offset (length byte-array)))))))
