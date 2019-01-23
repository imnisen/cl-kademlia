(in-package :cl-user)
(defpackage cl-kademlia-test
  (:use :cl
        :cl-kademlia
        :prove))
(in-package :cl-kademlia-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-kademlia)' in your Lisp.

(plan nil)



(defparameter s1 (make-node 9001 "s1" "s1"))


(defparameter s2 (make-node 9002 "s2" "s2"))

(bootstrap s1 :addrs '(("127.0.0.1" 9002)))

(get-key s1 "hi")

(set-key s1 "hi" "world")


(finalize)
