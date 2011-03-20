;;;
;;; Copyright (c) 2008-2011 Keith James. All rights reserved.
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

;; CL's simple-condition and simple-error give some basic error
;; message formatting and I was tempted to use one of them as a
;; superclass. However, in view of this comment from Kent Pitman on
;; comp.lang.lisp (16 Apr 1999), it seems that this is not their
;; intended use:
;;
;; "There was never an intent that simple-condition could be or should be
;; extended.  They were intended for use with people too lazy to make
;; type-specific errors, but it was assumed that the people who did make
;; something easier to use would do so in a better way."

(define-condition formatted-condition (condition)
  ((format-control :initform nil
                   :initarg :format-control
                   :reader format-control-of
                   :documentation "The format control used to create
                   the condition message.")
   (format-arguments :initform nil
                     :initarg :format-arguments
                     :reader format-arguments-of
                     :documentation "The list of format arguments used
                   to create the condition message.")))

(define-condition simple-text-condition (condition)
  ((text :initform nil
         :initarg :text
         :reader text-of)))

(define-condition invalid-argument-error (error formatted-condition)
  ((parameters :initform "<not supplied>"
               :initarg :parameters
               :reader parameters-of
               :documentation "The invalid parameters.")
   (arguments :initform "<not supplied>"
              :initarg :arguments
              :reader arguments-of
              :documentation "The invalid arguments."))
  (:report (lambda (condition stream)
             (format stream
                     "invalid ~a argument~:[s~;~] ~@[~a~]~@[: ~a~]"
                     (parameters-of condition)
                     (or (atom (parameters-of condition))
                         (endp (rest (parameters-of condition))))
                     (arguments-of condition)
                     (message-of condition))))
  (:documentation "An error that is raised when an invalid argument is
passed to a function."))

(define-condition missing-argument-error (error formatted-condition)
  ((parameters :initform "<not supplied>"
               :initarg :parameters
               :reader parameters-of
               :documentation "The invalid parameters."))
  (:report (lambda (condition stream)
             (format stream
                     "missing ~a argument~:[s~;~]~@[: ~a~]"
                     (parameters-of condition)
                     (atom (parameters-of condition))
                     (message-of condition))))
  (:documentation "An error that is raised when a required argument is
not passed to a function."))

(define-condition invalid-operation-error (error formatted-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "invalid operation~@[: ~a~]"
                     (message-of condition))))
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
             (format stream "~a is deprecated~@[ in favour of ~a~]"
                     (feature-of condition) (in-favour-of condition))))
  (:documentation "A warning that is raised when a deprecated feature
is used."))

(defgeneric message-of (condition)
  (:documentation "Returns the condition message string."))

(defmethod message-of ((condition formatted-condition))
  (with-accessors ((str format-control-of) (args format-arguments-of))
      condition
    (cond ((and str args)
           (apply #'format nil str args))
          (str
           str)
          (t
           nil))))

(defmethod message-of ((condition simple-text-condition))
  (text-of condition))
