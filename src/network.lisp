(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


(defclass server ()
  ((ksize :initarg :ksize
          :reader server-ksize
          :initform 20)
   (alpha :initarg :alpha
          :reader server-alpha
          :initform 3)
   (protocol :initarg :protocol
             :reader server-protocol)
   (node :initarg :node
         :accessor server-node)
   (storage :initarg :storage
            :accessor server-storage)))




(defgeneric listen-on (server host port)
  (:documentation "Listen to host and port start to work"))

(defgeneric bootstrap (server &key addrs)
  (:documentation "Start kad node, if addrs provides, connect to them, else start a lonely node"))

(defgeneric get-key (server key)
  (:documentation "Get a value of key"))

(defgeneric set-key (server key value)
  (:documentation "Set a value of key"))

(defgeneric stop (server)
  (:documentation "Stop the kademlia node"))




(defmethod listen-on ((server server) host port)
  "Load storage; start kad-server"
  ;; (storage-load (server-sotrage))
  (rpcudp:start (server-protocol server) host port))


(defmethod bootstrap ((server server) &key addrs)
  "start connecting to other servers connecting to addrs given
loop through the addrs, ping each, if it replys, call FIND_NODE(self) to it;
addrs format: '((\"127.0.0.0\" 8001) (\"127.0.0.0\" 8002) (\"127.0.0.0\" 8003))"
  (let ((useful-nodes (make-bootstrap-nodes server :addrs addrs))
        (find-node-result '()))

    ;; call FIND_NODE to useful-nodes
    (recursive-process server useful-nodes #'call-find-node (server-node server)
                       #'bootstrap-handle-result nil)))

(defmethod get-key ((server server) key)
  "First check local storage, if not found, then find neighbors of key,
call FIND_VALUE to them.
"
  (let* ((node-to-find (make-node-from-key key))
         (result (storage-fetch (server-storage server) (node-id node-to-find))))
    ;; (break)
    (if result result
        (let ((nearest (find-server-neighbors server node-to-find)))
          (if (= 0 (length nearest))
              (log:info "There are no known neighbors to get key ~a" key)

              (let* ((r (recursive-process server nearest #'call-find-value node-to-find
                                           #'get-key-handle-result #'get-key-stop-case :find-value-p t))
                     (value (car (first (make-counter r)))))
                ;; (break)
                (storage-store (server-storage server) (node-id node-to-find) value)
                value))))))

(defmethod set-key ((server server) key value)
  "Find in my route table neighbors to key. Call FIND_NODE(key) to them, getting the nearest nodes to key.
Call STORE(key value) to them. If self node is nearer than nodes found, self node also store the key value.
if any remote SRORE finished return success."
  (check-dht-value-type value)

  (let* ((node-to-find (make-node-from-key key))
         (nearest (find-server-neighbors server node-to-find)))

    (if (= 0 (length nearest))
        (log:info "There are no known neighbors to get key ~a" key)
        (let ((nodes-to-call-store (get-k-network-nearest-nodes server nearest node-to-find)))

          ;; if self is nearer than found in the network, self store it.
          (when (or (null nodes-to-call-store)
                    (< (distance-to (server-node server) node-to-find)
                       (apply #'max (mapcar #'(lambda (n) (distance-to n node-to-find))
                                            nodes-to-call-store))))
            ;; (break)
            (storage-store (server-storage server) (node-id node-to-find) value))


          ;; this part should be concurrent;
          ;; if any of up call-sotre success, return t
          (magic-any1 (magic-concurrent-async
                        (loop :for n :in nodes-to-call-store
                              :do (call-store (server-protocol server) n (node-id node-to-find) value))))
          t)
        )))

;; TODO
(defmethod stop ((server server))
  ())


(defun make-bootstrap-nodes (server &key addrs)
  "Make useful bootstrap nodes from addrs passed"
  ;;Not async now, because it's hard to know which ping reponse from which boot-node, TODO
  (let ((init-nodes (mapcar #'(lambda (addr) (make-node :ip (first addr) :port (second addr)))
                            addrs))
        (bootstrap-nodes '()))
    (loop :for node :in init-nodes
          :do
          (let* ((res (call-ping (server-protocol server) node :bootstraped t))
                 (success (second res))
                 (node-id (third res)))
            (when success
              (setf (node-id node) node-id)
              (push node bootstrap-nodes))))
    bootstrap-nodes))

(defun not-in (each l)
  (if (member (node-id each) (mapcar #'(lambda (x) (node-id x)) l) :test #'equalp) nil t))


(defun find-server-neighbors (server node-to-find)
  (find-neighbors (protocol-router (server-protocol server)) node-to-find))

(defun make-node-from-key (key)
  (make-instance 'node :id (digest key)))

;; TODO make it concurrent-async
(defun magic-concurrent-async-call (method alpha-nodes node-to-find protocol)
  (let ((result '()))
    (loop :for node-to-ask :in alpha-nodes
          :do (let ((x (funcall method protocol node-to-ask node-to-find)))
                (nright-push x
                             result)))
    result))

(defun get-shortest-k-by-distance (node-to-find nodes k)
  "list-nodes: ((ip port id) (ip port id) (ip port id))"
  (when nodes
    (subseq (sort nodes #'(lambda (x y) (< (distance-to x node-to-find) (distance-to y node-to-find)))) 0 (min k (length nodes)))))


(defun get-k-network-nearest-nodes (server nearest node-to-find)
  (let ((r (recursive-process server nearest #'call-find-node node-to-find #'set-key-handle-result nil)))
    ;; (break)
    (get-shortest-k-by-distance node-to-find r (server-ksize server))))

(defun recursive-process (server nearest rpc-method node-to-find handle-result-method stop-case &key find-value-p) ;; ugly params
  (let ((result (if find-value-p '() nearest)))
    (with-slots (protocol ksize alpha) server
      (loop
        :with nearest-uncontacted := nearest
        :with nearest-contacted := '()
        :for alpha-nodes := (nleft-pop-n nearest-uncontacted alpha)
        :while alpha-nodes
        :do (progn
              (nright-batch-push alpha-nodes nearest-contacted)
              (let ((rr (magic-concurrent-async-call rpc-method alpha-nodes node-to-find protocol)))
                (loop :for res
                      :in rr
                      :do (multiple-value-bind (nearest-uncontacted1 result1)
                              (funcall handle-result-method res nearest-contacted server)
                            (setf nearest-uncontacted (append nearest-uncontacted nearest-uncontacted1))
                            (setf result (append result result1))
                            ))
                (when find-value-p
                  (when (funcall stop-case result)
                    (return result)))))
        :finally (return result)))))

(defun get-key-handle-result (res nearest-contacted server)
  (let ((nearest-uncontacted '())
        (result '()))
    (destructuring-bind (peer-id success r) res
      (declare (ignore peer-id))
      (if success
          (if (listp r)
              (loop :for (ip port id) :in r
                    :do
                    (when (and (not-in (make-instance 'node :ip ip :port port :id id)
                                       nearest-contacted)
                               (not (equalp id (node-id (server-node server)))))
                      (nright-push (make-instance 'node :ip ip :port port :id id)
                                   nearest-uncontacted))) ;; return list of nodes may know the value
              (push r result))))
    (values nearest-uncontacted result)))

(defun bootstrap-handle-result (res nearest-contacted server)
  (let ((nearest-uncontacted '()))
    (destructuring-bind (peer-id success r) res
      (declare (ignore peer-id))
      (if success
          (loop :for (ip port id) :in r
                :do
                (when (and (not-in (make-instance 'node :ip ip :port port :id id)
                                   nearest-contacted)
                           (not (equalp id (node-id (server-node server)))))

                  (nright-push (make-instance 'node :ip ip :port port :id id)
                               nearest-uncontacted)))))
    (values nearest-uncontacted nil)))

(defun set-key-handle-result (res nearest-contacted server)
  (let ((nearest-uncontacted '())
        (result '()))
    ;; (break)
    (destructuring-bind (peer-id success r) res
      (if success
          (loop :for (ip port id) :in r
                :do
                (when (and (not-in (make-instance 'node :ip ip :port port :id id)
                                   nearest-contacted)
                           (not (equalp id (node-id (server-node server)))))

                  (nright-push (make-instance 'node :ip ip :port port :id id)
                               nearest-uncontacted)

                  (push (make-instance 'node :ip ip :port port :id id)
                        result)))))
    (values nearest-uncontacted result)))

(defun get-key-stop-case (result)
  (when result t ))
