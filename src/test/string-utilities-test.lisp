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

(in-package :uk.co.deoxybyte-utilities-test)

(addtest (deoxybyte-utilities-tests) control-char-p/1
  (ensure (loop
             for i from 0 to 31
             always (control-char-p (code-char i))))
  (ensure (control-char-p (code-char 127))))

(addtest (deoxybyte-utilities-tests) control-char-p/2
  (ensure (loop
             for c across "abcdefghijklmnopqrstuvwxyz"
             never (control-char-p c))))

(addtest (deoxybyte-utilities-tests) whitespace-char-p/1
  (ensure (loop
             for c in '(#\Space #\Tab #\Return
                        #\Linefeed #\FormFeed)
             always (whitespace-char-p c))))

(addtest (deoxybyte-utilities-tests) whitespace-char-p/2
  (ensure (loop
             for c across "abcdefghijklmnopqrstuvwxyz"
             never (whitespace-char-p c))))

(addtest (deoxybyte-utilities-tests) whitespace-string-p/1
  (ensure (whitespace-string-p
           (make-array 5 :element-type 'base-char
                       :initial-contents '(#\Space #\Tab #\Return
                                           #\Linefeed #\FormFeed)))))

(addtest (deoxybyte-utilities-tests) whitespace-string-p/2
  (ensure (not (whitespace-string-p
                (make-array 6 :element-type 'base-char
                              :initial-contents
                              '(#\Space #\Tab #\Return
                                #\Linefeed #\FormFeed #\a))))))

(addtest (deoxybyte-utilities-tests) content-string-p/1
  (ensure (content-string-p
           (make-array 6 :element-type 'base-char
                       :initial-contents '(#\Space #\Tab #\Return
                                           #\Linefeed #\FormFeed #\a)))))

(addtest (deoxybyte-utilities-tests) content-string-p/2
  (ensure (not (content-string-p
                (make-array 5 :element-type 'base-char
                            :initial-contents
                            '(#\Space #\Tab #\Return
                              #\Linefeed #\FormFeed))))))

(addtest (deoxybyte-utilities-tests) empty-string-p/1
  (ensure (empty-string-p
           (make-array 5 :element-type 'base-char
                       :initial-contents '(#\Space #\Tab #\Return
                                           #\Linefeed #\FormFeed)))))

(addtest (deoxybyte-utilities-tests) empty-string-p/2
  (ensure (empty-string-p "")))

(addtest (deoxybyte-utilities-tests) empty-string-p/3
  (ensure (not (empty-string-p
                (make-array 6 :element-type 'base-char
                              :initial-contents
                              '(#\Space #\Tab #\Return
                                #\Linefeed #\FormFeed #\a))))))

(addtest (deoxybyte-utilities-tests) contains-char-p/1
  (ensure (contains-char-p "abc" #\a))
  (ensure (contains-char-p "abc" #\b))
  (ensure (contains-char-p "abc" #\c))
  (ensure (not (contains-char-p "abc" #\d))))

(addtest (deoxybyte-utilities-tests) has-char-at-p/1
  (ensure (has-char-at-p "abc" #\a 0))
  (ensure (has-char-at-p "abc" #\b 1))
  (ensure (has-char-at-p "abc" #\c 2))
  (ensure (not (has-char-at-p "abc" #\a 1)))
  (ensure (not (has-char-at-p "abc" #\c 0))))

(addtest (deoxybyte-utilities-tests) starts-with-char-p/1
  (ensure (starts-with-char-p "abc" #\a))
  (ensure (not (starts-with-char-p "abc" #\b))))

(addtest (deoxybyte-utilities-tests) every-char-p/1
  (let ((fn #'(lambda (c) (char= #\a c))))
    (ensure (every-char-p "aaa" fn 0 1 2))
    (ensure (every-char-p "aab" fn 0 1))
    (ensure (every-char-p "baa" fn 1 2))))

(addtest (deoxybyte-utilities-tests) concat-strings/1
  (ensure (string=
           "aaabbbccc"
           (concat-strings (make-array 4 :initial-contents
                                       '("aaa" "bbb" "ccc" ""))))))
