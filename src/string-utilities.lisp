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

(defvar *whitespace-chars*
  (make-array 5 :element-type 'base-char
              :initial-contents '(#\Space #\Tab #\Return #\Linefeed #\Page))
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
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-base-string *whitespace-chars*)
           (type character char))
  (find char *whitespace-chars*))

(defun whitespace-string-p (str)
  "Returns T if all the characters in STR are whitespace as defined by
WHITESPACE-CHAR-P, or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str))
  (loop
     for c across str
     always (whitespace-char-p c)))

(defun content-string-p (str)
  "Returns T if any of the characters in STR are not whitespace as
defined by WHITESPACE-CHAR-P, or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type (simple-array character (*)) str))
  (flet ((fn (c)
           (not (whitespace-char-p c))))
    (find-if #'fn str)))

(defun empty-string-p (str)
  "Returns T if STR is a zero-length string or contains only
whitespace as defined by WHITESPACE-CHAR-P, or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str))
  (or (zerop (length str))
      (whitespace-string-p str)))

(defun contains-char-p (str char &key (test #'char=))
  "Returns T if STR contains CHAR, determined by TEST (defaults to
CHAR=) or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str)
           (type character char)
           (type function test))
  (find char str :test test))

(defun has-char-at-p (str char index &key (test #'char=))
  "Returns T if STR has CHAR at INDEX, determined by TEST (defaults to
CHAR=), or NIL otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str)
           (type function test))
  (and (not (zerop (length str)))
       (funcall test char (char str index))))

(defun starts-with-char-p (str char &key (test #'char=))
  "Returns T if STR has CHAR at index 0, determined by TEST (defaults
to CHAR=), or NIL otherwise."
  (has-char-at-p str char 0 :test test))

(defun ends-with-char-p (str char &key (test #'char=))
  "Returns T if STR has CHAR at its last index, determined by
TEST (defaults to CHAR=), or NIL otherwise."
  (has-char-at-p str char (1- (length str)) :test test))

(defun every-char-p (str test &rest indices)
  "Applies predicate TEST to characters of string STR indicated by
INDICES and returns T if all those characters match TEST."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str)
           (type function test))
  (loop
     for i in indices
     always (funcall test (char str i))))

(defun starts-with-string-p (str1 str2 &key (test #'string=))
  "Returns T if STR1 starts with STR2, determined by TEST (defaults to
STRING=), or NIL otherwise."
  (let ((len2 (length str2)))
    (and (>= (length str1) len2)
         (funcall test str1 str2 :end1 len2))))

(defun ends-with-string-p (str1 str2 &key (test #'string=))
  "Returns T if STR1 ends with STR2, determined by TEST (defaults to
STRING=), or NIL otherwise."
  (let ((len1 (length str1))
        (len2 (length str2)))
    (and (>= len1 len2)
         (funcall test str1 str2 :start1 (- len1 len2)))))

(defun concat-strings (strs)
  "Returns a new simple-string created by concatenating, in the order
supplied, the simple-strings contained in the vector STRS."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (vector simple-string) strs))
  (let ((new-str (make-string (reduce #'+ strs :key #'length)
                              :element-type 'character))
        (num-strs (length strs)))
    (do ((i 0 (1+ i))
         (offset 0))
        ((= i num-strs) new-str)
      (let ((str (aref strs i)))
        (declare (type simple-string str)
                 (type vector-index offset))
        (unless (zerop (length str))
          (copy-vector str 0 (length str)
                       new-str offset)
          (incf offset (length str)))))))

(defun txt (&rest strings)
  "Returns the result of concatenating STRINGS, separated by spaces."
  (apply #'concatenate 'string
         (intersperse strings (make-string 1 :initial-element #\Space))))

(defun str (&rest strings)
  "Returns the result of concatenating strings STRINGS."
  (apply #'concatenate 'string strings))

(defun string-positions (str char &key (start 0) end)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str))
  (let ((end (or end (length str))))
    (declare (type vector-index start end))
    (check-arguments (<= 0 start end (length str)) (start end)
                     "start must be >= 0 and be <= end")
    (loop
       for i from start below end
       when (char= char (char str i))
       collect i)))

(defun string-split-indices (str char &key (start 0) end)
  "Returns two values, a list of start indices and a list of end
indices into STR between START and END such that if used as start/end
arguments to subseq, STR will be split on CHAR. CHAR is compared with
elements in STR using TEST, which defaults to EQL."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-string str))
  (let ((end (or end (length str))))
    (declare (type vector-index start end))
    (check-arguments (<= 0 start end (length str)) (start end)
                     "start must be >= 0 and be <= end")
    (let ((positions (string-positions str char :start start :end end)))
      (if positions
          (loop
             with starts = ()
             with ends = ()
             for pos of-type fixnum in positions
             and prev = start then (the vector-index (1+ pos))
             maximize pos into last-pos
             do (progn
                  (push prev starts)
                  (push pos ends))
             finally (progn
                       (push (the vector-index (1+ last-pos)) starts)
                       (push end ends)
                       (return
                         (values (nreverse starts) (nreverse ends)))))
        nil))))

(defun string-split (str char &key (start 0) end remove-empty-substrings)
  "Returns a list of strings made by splitting simple-string STR at
CHAR, between START and END. If REMOVE-EMPTY-SUBSEQS is T, any empty
subsequences will be omitted from the returned list."
  (declare (optimize (speed 3) (safety 1)))
  (declare (type simple-string str))
  (let ((end (or end (length str))))
    (declare (type vector-index start end))
    (check-arguments (<= 0 start end (length str)) (start end)
                     "start must be >= 0 and be <= end")
    (multiple-value-bind (starts ends)
        (string-split-indices str char :start start :end end)
      (if (and starts ends)
          (loop
             for i of-type vector-index in starts
             for j of-type vector-index in ends
             when (not (and remove-empty-substrings
                            (= i j)))
             collect (subseq str i j))
        (list (subseq str start end))))))
