;;;
;;; Copyright (c) 2007-2013 Keith James. All rights reserved.
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

(defsystem deoxybyte-utilities-test
  :depends-on ((:version :lift "1.7.0")
               :deoxybyte-utilities)
  :components ((:module :deoxybyte-utilities-test
                        :serial t
                        :pathname "test/"
                        :components ((:file "package")
                                     (:file "deoxybyte-utilities-test")
                                     (:file "cons-utilities-test")
                                     (:file "set-utilities-test")
                                     (:file "numeric-utilities-test")
                                     (:file "vector-utilities-test")
                                     (:file "octet-vector-utilities-test")
                                     (:file "string-utilities-test")
                                     (:file "clos-utilities-test")
                                     (:file "finite-state-machine-test")
                                     (:file "queue-test")))))
