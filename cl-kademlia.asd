#|
  This file is a part of cl-kademlia project.
  Copyright (c) 2018 Nisen (imnisen@gmail.com)
|#

#|
  Author: Nisen (imnisen@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-kademlia-asd
  (:use :cl :asdf))
(in-package :cl-kademlia-asd)

(defsystem cl-kademlia
  :version "0.1"
  :author "Nisen"
  :license ""
  :depends-on (:log4cl
               :ironclad
               :usocket
               :flexi-streams
               :bit-smasher
               :lparallel)
  :components ((:module "src"
                :components
                ((:file "tools")
                 (:file "rpc")
                 (:file "node")
                 (:file "protocol")
                 (:file "routing")
                 (:file "storage")
                 (:file "network")
                 (:file "main")
                 )))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-kademlia-test))))
