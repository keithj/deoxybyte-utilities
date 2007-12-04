
(defpackage #:cl-gp-utilities
  (:use #:common-lisp)
  (:nicknames #:gpu)
  (:documentation "General purpose utilities.")
  (:export
   ;; Conditions

   ;; Classes
   #:lazy-init-proxy
   ;; Generics
   #:reify
   #:initargs-of
   #:proxied-class-of
   ;; Functions
   #:all-superclasses
   #:all-specialized-methods
   #:all-specialized-generic-functions
   #:enable-lazy-init
   ;; Macros
   #:copy-array))
