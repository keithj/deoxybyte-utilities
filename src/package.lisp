
(defpackage #:cl-gp-utilities
  (:use #:common-lisp)
  (:nicknames #:gpu)
  (:documentation "General purpose utilities.")
  (:export
   ;; Conditions

   ;; Classes

   ;; Generics

   ;; Functions
   #:has-byte-at-p
   #:starts-with-byte-p
   #:make-sb-string
   #:concat-into-sb-string
   #:has-char-at-p
   #:starts-with-char-p
   #:every-char-p
   #:concat-strings
   #:write-wrapped-string
   #:all-superclasses
   #:all-specialized-methods
   #:all-specialized-generic-functions
   #:all-external-generic-functions
   ;; Macros
   #:copy-array
   #:assocdr
   #:rassocar
   ;;Types
   #:array-index))
