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

(in-package :cl-gp-utilities)

(defvar *whitespace-chars*
  (make-array 5 :element-type 'character
              :initial-contents '(#\Space #\Tab #\Return
                                  #\Linefeed #\FormFeed))
  "Whitespace characters.")

(defun control-char-p (char)
  "Returns T if CHAR is an ASCII control character (all characters
with codes 0-31, inclusive, and the character with code 127), or NIL
otherwise."
  (or (<= (char-code char) 31)
      (= 127 (char-code char))))

(defun whitespace-char-p (char)
  "Returns T if CHAR is one of the currently bound set of whitespace
characters (defaults to #\Space #\Tab #\Return #\Linefeed and
#\FormFeed), or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string *whitespace-chars*))
  (loop for w across *whitespace-chars*
     thereis (char= w char)))

(defun whitespace-string-p (str)
  "Returns T if all the characters in STR are whitespace as defined by
WHITESPACE-CHAR-P, or NIL otherwise."
  (loop for c across str
       always (whitespace-char-p c)))

(defun content-string-p (str)
  "Returns T if any of the characters in STR are not whitespace as
defined by WHITESPACE-CHAR-P, or NIL otherwise."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (loop for c across str
     thereis (not (whitespace-char-p c))))

(defun empty-string-p (str)
  "Returns T if STR is a zero-length string or contains only
whitespace as defined by WHITESPACE-CHAR-P, or NIL otherwise."
  (or (zerop (length str))
      (whitespace-string-p str)))

(defun contains-char-p (str char)
  "Returns T if STR contains CHAR, or NIL otherwise"
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (loop for c across str
     thereis (char= char c)))

(defun has-char-at-p (str char index)
  "Returns T if STR has CHAR at INDEX."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str))
  (and (not (zerop (length str)))
       (char= char (char str index))))

(defun starts-with-char-p (str char)
  "Returns T if STR has CHAR at index 0."
  (has-char-at-p str char 0))

(defun every-char-p (str test &rest indices)
  "Applies predicate TEST to characters of string STR indicated by
INDICES and returns T if all those characters match TEST."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type string str)
           (type function test))
  (loop for i in indices
     always (funcall test (char str i))))

(defun concat-strings (strs)
  "Returns a new simple-string created by concatenating, in the order
supplied, the simple-strings contained in the vector STRS."
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector strs))
  (let ((new-str (make-string (reduce #'+ strs :key #'length)
                              :element-type 'character))
        (num-strs (length strs)))
    (do ((i 0 (1+ i))
         (offset 0))
        ((= i num-strs) new-str)
      (let ((str (aref strs i)))
        (declare (type simple-string str)
                 (type array-index offset))
        (unless (zerop (length str))
          (copy-array str 0 (1- (length str))
                      new-str offset)
          (incf offset (length str)))))))

(defun msg (&rest strings)
  (concat-strings
   (coerce (interleave strings (make-string 1 :initial-element #\space))
           'simple-vector)))
