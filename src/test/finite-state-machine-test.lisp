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

(in-package :cl-gp-utilities-test)

(defsmfun char-triples/1 (str &key (start 0))
     ((i start)
      (len (length str))
      (triples ())
      (triple ()))
     ((c0 ((< i len) c1
           (setf triple (list (char str i)))
           (incf i))
          (t nil
             (reverse triples)))
      (c1 ((< i len) c2
           (push (char str i) triple)
           (incf i))
          (t nil
             (reverse triples)))
      (c2 ((< i len) c0
           (push (char str i) triple)
           (push (reverse triple) triples)
           (incf i))
          (t nil
             (reverse triples)))))

(addtest (cl-gp-utilities-tests) defsmfun/1
  (ensure (equalp '((#\a #\a #\a) (#\b #\b #\b) (#\c #\c #\c))
                  (char-triples/1 "aaabbbccc")))
  (ensure (equalp '((#\a #\a #\b) (#\b #\b #\c))
                  (char-triples/1 "aaabbbccc" :start 1)))
  (ensure (equalp '((#\c #\c #\c))
                  (char-triples/1 "aaabbbccc" :start 6)))
  (ensure-null (char-triples/1 "aaabbbccc" :start 7)))


(defun char-triples/2 (str)
  (let ((i 0)
        (len (length str))
        (triples ())
        (triple ()))
    (defsm ((c0 ((< i len) c1
                 (setf triple (list (char str i)))
                 (incf i))
                (t nil
                   (reverse triples)))
            (c1 ((< i len) c2
                 (push (char str i) triple)
                 (incf i))
                (t nil
                   (reverse triples)))
            (c2 ((< i len) c0
                 (push (char str i) triple)
                 (push (reverse triple) triples)
                 (incf i))
                (t nil
                   (reverse triples)))))))

(addtest (cl-gp-utilities-tests) defsm/1
  (ensure (equalp '((#\a #\a #\a) (#\b #\b #\b) (#\c #\c #\c))
                  (char-triples/2 "aaabbbccc"))))
