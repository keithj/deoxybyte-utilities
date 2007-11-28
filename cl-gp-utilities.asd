
(defsystem cl-gp-utilities
    :name "cl-gp-utilities"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL"
    :components
    ((:module :cl-io-utilities
              :pathname "src/"
              :components ((:file "package")
                           (:file "cl-gp-utilities"
                                  :depends-on ("package"))))))
