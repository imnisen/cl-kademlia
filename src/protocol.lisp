(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


(defclass protocol (transport-udp-server transport-udp-client)
  ((node :initarg :node
         :accessor protocol-node
         :initform nil)
   (router :initarg :router
           :accessor protocol-router)
   (storage :initarg :storage
            :accessor protocol-storage)))




;; server part

;; Use macro to c wrapper expose and defmethod
;; Or change rpc inteface
(defmethod initialize-instance :after ((protocol protocol) &key)
  "Add rpc methods"
  (expose protocol "PING" (lambda (addr sender-id) (rpc-ping protocol addr sender-id)))
  (expose protocol "STORE" (lambda (addr sender-id key value) (rpc-store protocol addr sender-id key value)))
  (expose protocol "FIND_NODE" (lambda (addr sender-id key) (rpc-find-node protocol addr sender-id key)))
  (expose protocol "FIND_VALUE" (lambda (addr sender-id key) (rpc-find-value protocol addr sender-id key))))

;; TODO  RPC 的接收者可以把 PING 附带在 RPC 的回应中以进一步确认发送者网络地址的有效性。

(defmethod rpc-ping ((protocol protocol) addr sender-id)
  "Handle a PING request
return a randowm 160bits rpc id"
  ;; (generate-random-160bits-fun)   todo later

  (let ((source (make-instance 'node
                               :id sender-id
                               :ip (first addr)
                               :port (second addr)
                               )))
    (welcome-if-new-node protocol source) ;; TODO use macro to wrapper the let and welcome-**

    (node-id (protocol-node protocol))))


(defmethod rpc-store ((protocol protocol) addr sender-id key value)
  ""
  (let ((source (make-instance 'node
                               :id sender-id
                               :ip (first addr)
                               :port (second addr)
                               )))
    (welcome-if-new-node protocol source)

    (storage-store (protocol-storage protocol)
                   key
                   value)
    t))

(defmethod rpc-find-node ((protocol protocol) addr sender-id key)
  "response k nearest nodes from id I know.
id is 160bit; return data is '((ip,port,id) (ip,port,id) ...)"
  (let ((source (make-instance 'node
                               :id sender-id
                               :ip (first addr)
                               :port (second addr)
                               )))
    (welcome-if-new-node protocol source)

    (let* ((node-to-find (make-instance 'node :id key))
           (n (find-neighbors (protocol-router protocol) node-to-find :exclude source))
           (re (mapcar #'(lambda (x)
                           (with-slots (ip port id) x
                             (list ip port id)))
                       n)))
      re)))

(defmethod rpc-find-value ((protocol protocol) addr sender-id key)
  "same as find node, but if I have store the value of id, just return it"



  (let ((source (make-instance 'node
                               :id sender-id
                               :ip (first addr)
                               :port (second addr)
                               )))
    (welcome-if-new-node protocol source)
    (let ((result (storage-fetch (protocol-storage protocol) key)))
      ;; (break)
      (if result result
          (rpc-find-node protocol addr sender-id key)
          ;; (mapcar #'(lambda (x)
          ;;             (with-slots (ip port id) x
          ;;               (list ip port id)))
          ;;         (find-neighbors (protocol-router protocol) node-to-find))
          )))
  )





;; client part
(defmethod call-ping ((protocol protocol) node-to-ask &key bootstraped)
  (client-connect protocol :host (node-ip node-to-ask) :port (node-port node-to-ask))

  (prog1
      ;; real logic
      (let* ((call-result (call protocol "PING" (list (node-id (protocol-node protocol))) :timeout 5))
             (success (not (null call-result))))
        (when (not bootstraped)
          (handle-call-response protocol call-result node-to-ask))
        (list (node-id node-to-ask) success call-result))
    (client-stop protocol)))


(defmethod call-store ((protocol protocol) node-to-ask key value)
  (client-connect protocol :host (node-ip node-to-ask) :port (node-port node-to-ask))
  (prog1
      ;; real logic
      (let* ((call-result (call protocol "STORE" (list (node-id (protocol-node protocol)) key value) :timeout 5))
             (success (not (null call-result))))
        (handle-call-response protocol call-result node-to-ask)

        (list (node-id node-to-ask) success call-result))
    (client-stop protocol))
  )

(defmethod call-find-node ((protocol protocol) node-to-ask node-to-find)

  (client-connect protocol :host (node-ip node-to-ask) :port (node-port node-to-ask))

  (prog1
      ;; real logic
      (let* ((call-result (call protocol "FIND_NODE" (list (node-id (protocol-node protocol)) (node-id node-to-find)) :timeout 5))
             (success (not (null call-result))))
        (handle-call-response protocol call-result node-to-ask)
        (list (node-id node-to-ask) success call-result))
    (client-stop protocol))
  )



(defmethod call-find-value ((protocol protocol) node-to-ask node-to-find)


  (client-connect protocol :host (node-ip node-to-ask) :port (node-port node-to-ask))

  (prog1
      ;; real logic
      (let* ((call-result (call protocol "FIND_VALUE" (list (node-id (protocol-node protocol)) (node-id node-to-find)) :timeout 5))
             (success (not (null call-result))))
        (handle-call-response protocol call-result node-to-ask)
        (list (node-id node-to-ask) success call-result))
    (client-stop protocol))

  )

;; need refactor
(defmacro client-wrapper (&body body)
  `(progn
     (client-connect protocol :host (node-ip node-to-ask) :port (node-port node-to-ask))
     (prog1
         ;; real logic
         ,@body

       (client-stop protocol))))



;; TODO use macro to wrapper client-connect and client-stop
;; (client-connect protocol :host (host peer) :port (port peer))
;; (protocol-stop *protocol*)



(defgeneric welcome-if-new-node (protocol node)
  (:documentation "Given a new node, send it all the keys/values it should be storing,
then add it to the routing table."))

(defgeneric handle-call-response (protocol result node)
  (:documentation "If we get a response, add the node to the routing table.
If we get no response, make sure it's removed from the routing table."))



(defmethod welcome-if-new-node ((protocol protocol) node)
  (when (is-new-node (protocol-router protocol) node)
    (log:info "never seen ~a before, adding to router" node)

    ;; publish old key values todo



    ;; add contact
    (add-contact (protocol-router protocol) node)

    ))

(defmethod handle-call-response ((protocol protocol) result node)
  (if result
      (welcome-if-new-node protocol node)
      (remove-contact (protocol-router protocol) node)))
