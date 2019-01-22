(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


(defclass node ()
  ((id :initarg :id
       :accessor node-id
       :initform nil)
   (ip :initarg :ip
       :accessor node-ip)
   (port :initarg :port
         :accessor node-port)
   (long-id :initarg :long-id
            :initform nil)))


(defgeneric distance-to (node target-node)
  (:documentation "Get distance between this node and target node" ))

(defgeneric same-home-as (node target-node)
  (:documentation "Is node and target has same home?"))

(defgeneric node-long-id (node)
  (:documentation ""))


(defmethod distance-to ((node node) target-node)
  (bit-smasher:int<- (bit-xor (bit-smasher:bits<- (node-long-id node))
                              (bit-smasher:bits<- (node-long-id target-node)))))



(defmethod node-long-id ((node node))
  (when (node-id node)
    (byte-array-to-integer* (node-id node))))



(defmethod same-home-as ((node node) target-node)
  (and (equal (node-ip node) (node-ip target-node))
       (equal (node-port node) (node-port target-node))))

;; (defmethod (setf node-long-id) ((node node) long-id)
;;   ())




;; ;; To use latter
;; (defclass node-heap ()
;;   ((node :initarg :node
;;          :accessor heap-node)
;;    (heap :initarg :heap
;;          :accessor heap-node)
;;    (contacted :initarg :contacted
;;               :accessor heap-contacted)
;;    (maxsize :initarg :maxsize
;;             :accessor heap-maxsize)))


;; (defgeneric remove (node-heap node-ids)
;;   (:documentation ""))

;; (defgeneric get-node-by-id (node-heap node-id)
;;   (:documentation ""))

;; (defgeneric all-been-contacted (node-heap)
;;   (:documentation ""))

;; (defgeneric get-ids (node-heap)
;;   (:documentation ""))

;; (defgeneric mark-contacted(node-heap node)
;;   (:documentation ""))

;; (defgeneric popleft (node-heap)
;;   (:documentation ""))

;; (defgeneric push (node-heap nodes)
;;   (:documentation ""))

;; (defgeneric get-uncontacted (node-heap)
;;   (:documentation ""))
