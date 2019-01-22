(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


;; (defvar *node* (make-node))

;; (listen-at *node* "127.0.0.1" 54321)

;; (bootstrap node '(("127.0.0.1" 10000)
;;                   ("127.0.0.1" 20000)
;;                   ("127.0.0.1" 30000)))

;; (set node "abc" "1234567")

;; (get node "abc") => "1234567"


;; ;; Interface
;; (defun make-node ()
;;   (let ((n (make-instance 'node
;;                           :id (generate-random-160bits-fun)
;;                           :server (make-instance 'kad-server)
;;                           :client (make-instance 'kad-client)
;;                           :storage (make-instance 'forgetful-storage))))
;;     (setf (server-node (node-server n)) n)) ;; binding each other
;;   )


(defun make-node (port name key-id)
  (let* ((n (make-instance 'node
                           :id (digest key-id)
                           :ip "127.0.0.1"
                           :port port
                           :long-id (byte-array-to-integer* (digest key-id))))
         (r (make-instance 'routing-table
                           :node n
                           :ksize 20
                           :protocol nil))
         (sto (make-instance 'forgetful-storage))
         (p (make-instance 'protocol
                           :node n
                           :router r
                           :storage sto))
         (s (make-instance 'server
                           :ksize 20
                           :alpha 3
                           :protocol p
                           :node n
                           :storage sto)))

    (setf (router-protocol r) p)
    (format t "~%Initial finished...~%")


    (bt:make-thread #'(lambda () (listen-on s "127.0.0.1" port))
                    :name name)
    s))



;; (ql:quickload :cl-kademlia)
;; (in-package :cl-kademlia)
;; (setf s1 (make-node 9001 "s1" "s1"))
;; (setf s2 (make-node 9002 "s2" "s2"))
;; (bootstrap s1 :addrs '(("127.0.0.1" 9002)))
;; (get-key s1 "hi")
;; (set-key s1 "hi" "world")

;; what if bootstrap twice
