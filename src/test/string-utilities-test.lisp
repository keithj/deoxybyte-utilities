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

(test control-char-p
  (is-true (loop
         for i from 0 to 31
         always (control-char-p (code-char i))))
  (is-true (control-char-p (code-char 127)))
  (is-true (loop
              for c across "abcdefghijklmnopqrstuvwxyz"
              never (control-char-p c))))

(test whitespace-char-p
  (is-true (loop
              for c in '(#\Space #\Tab #\Return
                         #\Linefeed #\FormFeed)
              always (whitespace-char-p c)))
  (is-true (loop
              for c across "abcdefghijklmnopqrstuvwxyz"
              never (whitespace-char-p c))))

(test whitespace-string-p
  (is-true (whitespace-string-p
            (make-array 5 :element-type 'base-char
                        :initial-contents '(#\Space #\Tab #\Return
                                            #\Linefeed #\FormFeed))))
  (is-false (whitespace-string-p
             (make-array 6 :element-type 'base-char
                         :initial-contents '(#\Space #\Tab #\Return
                                             #\Linefeed #\FormFeed #\a)))))

(test content-string-p
  (is-true (content-string-p
            (make-array 6 :element-type 'base-char
                        :initial-contents '(#\Space #\Tab #\Return
                                            #\Linefeed #\FormFeed #\a))))
  (is-false (content-string-p
            (make-array 5 :element-type 'base-char
                        :initial-contents '(#\Space #\Tab #\Return
                                            #\Linefeed #\FormFeed)))))

(test empty-string-p
  (is-true (empty-string-p
            (make-array 5 :element-type 'base-char
                        :initial-contents '(#\Space #\Tab #\Return
                                            #\Linefeed #\FormFeed))))
  (is-true (empty-string-p ""))
  (is-false (empty-string-p
             (make-array 6 :element-type 'base-char
                         :initial-contents '(#\Space #\Tab #\Return
                                             #\Linefeed #\FormFeed #\a)))))

(test contains-char-p
  (is-true (contains-char-p "abc" #\a))
  (is-true (contains-char-p "abc" #\b))
  (is-true (contains-char-p "abc" #\c))
  (is-false (contains-char-p "abc" #\d)))

(test has-char-at-p
  (is-true (has-char-at-p "abc" #\a 0))
  (is-true (has-char-at-p "abc" #\b 1))
  (is-true (has-char-at-p "abc" #\c 2))
  (is-false (has-char-at-p "abc" #\a 1))
  (is-false (has-char-at-p "abc" #\c 0)))

(test starts-with-char-p
  (is-true (starts-with-char-p "abc" #\a))
  (is-false (starts-with-char-p "abc" #\b)))

(test every-char-p
  (let ((fn #'(lambda (c) (char= #\a c))))
    (is-true (every-char-p "aaa" fn 0 1 2))
    (is-true (every-char-p "aab" fn 0 1))
    (is-true (every-char-p "baa" fn 1 2))))

(test concat-strings
  (is (string= "aaabbbccc"
               (concat-strings (make-array 4 :initial-contents
                                           '("aaa" "bbb" "ccc" ""))))))
