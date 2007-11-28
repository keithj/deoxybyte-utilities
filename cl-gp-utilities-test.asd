
(defsystem cl-gp-utilities-test
    :depends-on (:cl-gp-utilities :lisp-unit)
    :components ((:module :cl-gp-utilities-test
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-gp-utilities-test"
                                              :depends-on ("package")))))
    :in-order-to ((test-op (load-op cl-gp-utilities-test))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'lisp-unit)
    (asdf:oos 'asdf:load-op 'lisp-unit))
  (unless (find-package 'cl-gp-utilities)
    (asdf:oos 'asdf:load-op 'cl-gp-utilities)))

(defmethod perform ((operation test-op)
                    (component (eql (find-system 'cl-gp-utilities-test))))
  (let ((*default-pathname-defaults* (component-pathname component)))
    (lisp-unit:run-all-tests :cl-gp-utilities-test)))

(defmethod operation-done-p ((operation test-op)
                             (component (eql (find-system
                                              'cl-gp-utilities-test))))
  (values nil))