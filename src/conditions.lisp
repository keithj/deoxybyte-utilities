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

(define-condition invalid-argument-error (error)
  ((parameters :initform "<not supplied>"
               :initarg :params
               :reader parameters-of
               :documentation "The invalid parameters")
   (arguments :initform "<not supplied>"
              :initarg :args
              :reader arguments-of
              :documentation "The invalid arguments.")
   (text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:report (lambda (condition stream)
             (format stream
                     "Invalid ~a argument~:[s~;~] ~@[~a~]~@[: ~a~]."
                     (parameters-of condition)
                     (atom (parameters-of condition))
                     (arguments-of condition)
                     (text-of condition))))
  (:documentation "An error that is raised when an invalid argument is
passed to a function."))

(define-condition invalid-operation-error (error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:report (lambda (condition stream)
             (format stream "Invalid operation~@[: ~a~]."
                     (text-of condition))))
  (:documentation "An error that is raised when an invalid operation
is attempted."))
