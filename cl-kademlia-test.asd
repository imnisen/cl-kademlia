#|
  This file is a part of cl-kademlia project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-kademlia-test-asd
  (:use :cl :asdf))
(in-package :cl-kademlia-test-asd)

(defsystem cl-kademlia-test
  :author "Nisen"
  :license ""
  :depends-on (:cl-kademlia
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-kademlia"))))
  :description "Test system for cl-kademlia"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
