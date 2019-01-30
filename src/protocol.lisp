(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


(defclass protocol (rpcudp:rpc-node)
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
  (rpcudp:expose protocol "PING" (lambda (sender-id) (rpc-ping protocol (ensure-vector sender-id))))
  (rpcudp:expose protocol "STORE" (lambda (sender-id key value) (rpc-store protocol (ensure-vector sender-id) (ensure-vector key) value)))
  (rpcudp:expose protocol "FIND_NODE" (lambda (sender-id key) (rpc-find-node protocol (ensure-vector sender-id) (ensure-vector key))))
  (rpcudp:expose protocol "FIND_VALUE" (lambda (sender-id key) (rpc-find-value protocol (ensure-vector sender-id) (ensure-vector key)))))

;; TODO  RPC 的接收者可以把 PING 附带在 RPC 的回应中以进一步确认发送者网络地址的有效性。
;; TODO use macro to wrapper the let and welcome-**

;; return my id
(defmethod rpc-ping ((protocol protocol) sender-id)
  "Handle a PING request return a randowm 160bits rpc id"
  (log:info "rpc-ping with args:" sender-id)
  (let ((source (make-node :id sender-id
                           :ip rpcudp:*remote-host*
                           :port rpcudp:*remote-port*)))
    (welcome-if-new-node protocol source)
    (node-id (protocol-node protocol))))

;; return me store sucess?
(defmethod rpc-store ((protocol protocol) sender-id key value)
  "Handle a store request"
  (log:info "rpc-ping with args:" sender-id key value)
  (let ((source (make-node :id sender-id
                           :ip rpcudp:*remote-host*
                           :port rpcudp:*remote-port*)))
    (welcome-if-new-node protocol source)

    (storage-store (protocol-storage protocol)
                   key
                   value)
    t))

;; return data is '((ip,port,id) (ip,port,id) ...)
(defmethod rpc-find-node ((protocol protocol) sender-id key)
  "response k nearest nodes from id I know. id is 160bit"
  (log:info "rpc-ping with args:" sender-id key)
  (let ((source(make-node :id sender-id
                          :ip rpcudp:*remote-host*
                          :port rpcudp:*remote-port*)))
    (welcome-if-new-node protocol source)

    (let* ((node-to-find (make-instance 'node :id key))
           (n (find-neighbors (protocol-router protocol) node-to-find :exclude source))
           (re (mapcar #'(lambda (x)
                           (with-slots (ip port id) x
                             (list ip port id)))
                       n)))
      re)))

;; return (find-it?    [ result |  ((ip,port,id) (ip,port,id) ...)   ])
(defmethod rpc-find-value ((protocol protocol) sender-id key)
  "same as find node, but if I have store the value of id, just return it"
  (log:info "rpc-ping with args:" sender-id key)
  (let ((source (make-node :id sender-id
                           :ip rpcudp:*remote-host*
                           :port rpcudp:*remote-port*)))
    (welcome-if-new-node protocol source)
    (let ((result (storage-fetch (protocol-storage protocol) key)))
      (if result
          (list t result)
          (list nil (rpc-find-node protocol sender-id key)))))
  )



;; client part

;; return (node-to-ask-id, call-success?, remote-return-id)
(defmethod call-ping ((protocol protocol) node-to-ask &key bootstraped)
  (let* ((call-result (rpcudp:call protocol
                                   "PING"
                                   (list (node-id (protocol-node protocol)))
                                   (node-ip node-to-ask)
                                   (node-port node-to-ask)
                                   :timeout 5))
         (success (not (null call-result))))
    (when (not bootstraped)
      (handle-call-response protocol call-result node-to-ask))
    (list (node-id node-to-ask)
          success
          (ensure-vector call-result))))

;; return (node-to-ask-id, call-success?, store-success?)
(defmethod call-store ((protocol protocol) node-to-ask key value)
  (let* ((call-result (rpcudp:call protocol
                                   "STORE"
                                   (list (node-id (protocol-node protocol)) key value)
                                   (node-ip node-to-ask)
                                   (node-port node-to-ask)
                                   :timeout 5))
         (success (not (null call-result))))
    (handle-call-response protocol call-result node-to-ask)

    (list (node-id node-to-ask) success call-result)))

;; return (node-to-ask-id, call-success?, ((ip,port,id) (ip,port,id), ... )    )
(defmethod call-find-node ((protocol protocol) node-to-ask node-to-find)
  (let* ((call-result (rpcudp:call protocol
                                   "FIND_NODE"
                                   (list (node-id (protocol-node protocol)) (node-id node-to-find))
                                   (node-ip node-to-ask)
                                   (node-port node-to-ask)
                                   :timeout 5))
         (success (not (null call-result)))
         (result '()))
    (handle-call-response protocol call-result node-to-ask)

    ;;transfer call-result to correct type
    (dolist (o call-result)
      (format t "ip: ~a~%" (first o))
      (format t "port: ~a~%" (second o))
      (format t "id: ~a~%" (third o))
      (push (list (ensure-vector (first o))
                  (second o)
                  (ensure-vector (third o)))
            result))
    (setf result (nreverse result))

    (list (node-id node-to-ask) success result)))


;; return
(defmethod call-find-value ((protocol protocol) node-to-ask node-to-find)
  (let* ((call-result (rpcudp:call protocol
                                   "FIND_VALUE"
                                   (list (node-id (protocol-node protocol)) (node-id node-to-find))
                                   (node-ip node-to-ask)
                                   (node-port node-to-ask)
                                   :timeout 5))
         (success (not (null call-result)))
         (result nil))
    (handle-call-response protocol call-result node-to-ask)

    (if (first call-result)
        (setf result (second call-result))

        (progn
          (dolist (o (second call-result))
            (format t "call-find-value")
            (format t "ip: ~a~%" (first o))
            (format t "port: ~a~%" (second o))
            (format t "id: ~a~%" (third o))
            (push (list (ensure-vector (first o))
                        (second o)
                        (ensure-vector (third o)))
                  result))
          (setf result (nreverse result)))
        )
    (list (node-id node-to-ask) success result)))



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
