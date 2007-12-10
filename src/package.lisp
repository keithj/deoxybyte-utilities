
(defpackage #:cl-gp-utilities
  (:use #:common-lisp)
  (:nicknames #:gpu)
  (:documentation "General purpose utilities.")
  (:export
   ;; Conditions

   ;; Classes

   ;; Generics

   ;; Functions
   #:make-simple-base-string
   #:all-superclasses
   #:all-specialized-methods
   #:all-specialized-generic-functions
   #:all-external-generic-functions
   ;; Macros
   #:copy-array))
