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

(define-condition invalid-argument-error (error)
  ((parameters :initform "<not supplied>"
               :initarg :parameters
               :reader parameters-of
               :documentation "The invalid parameters.")
   (arguments :initform "<not supplied>"
              :initarg :arguments
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
                     (or (atom (parameters-of condition))
                         (endp (rest (parameters-of condition))))
                     (arguments-of condition)
                     (text-of condition))))
  (:documentation "An error that is raised when an invalid argument is
passed to a function."))

(define-condition missing-argument-error (error)
  ((parameters :initform "<not supplied>"
               :initarg :parameters
               :reader parameters-of
               :documentation "The invalid parameters.")
   (text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:report (lambda (condition stream)
             (format stream
                     "Missing ~a argument~:[s~;~]~@[: ~a~]."
                     (parameters-of condition)
                     (atom (parameters-of condition))
                     (text-of condition))))
  (:documentation "An error that is raised when a required argument is
not passed to a function."))

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

(define-condition deprecation-warning (style-warning)
  ((feature :initform nil
            :initarg :feature
            :reader feature-of)
   (in-favour :initform nil
              :initarg :in-favour
              :reader in-favour-of))
  (:report (lambda (condition stream)
             (format stream "~a is deprecated~@[ in favour of ~a~]."
                     (feature-of condition) (in-favour-of condition))))
  (:documentation "A warning that is raised when a deprecated feature
is used."))
