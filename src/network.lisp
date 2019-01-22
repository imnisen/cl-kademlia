(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)

;; (setf lparallel:*kernel* (lparallel:make-kernel 10))

(defun check-dht-value-type (value)
  (etypecase value
    (integer t)
    (float t)
    (string t)))

(defmacro magic-concurrent-async (&body body)
  (let ((g (gensym)))
    `(let ((,g (progn ,@body)))
       (log:info "magic result:" ,g)
       ,g)))


(defmacro magic-any1 (&body body)
  `(progn ,@body))


(defmacro nright-batch-push (to-be-pushed-list l)
  (let ((g (gensym)))
    `(progn
       (dolist (,g ,to-be-pushed-list)
         (nright-push ,g ,l))
       ,l)))

(defmacro nright-push (to-be-pushed l)
  `(progn
     (if ,l
         (push ,to-be-pushed (cdr (last ,l)))
         (setf ,l (list ,to-be-pushed))
         )
     ,l))

;; Definition
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



;; Defgenerics

(defgeneric listen-on (server host port)
  (:documentation "listen to host and port start to work"))

(defgeneric get-key (server key)
  (:documentation "get a value from server"))

(defgeneric set-key (server key value)
  (:documentation "set a value from server"))

(defgeneric bootstrap (server &key addrs)
  (:documentation "start a server, if addrs provides, connect to them, else start a lonely server"))



(defgeneric ask-for-nearest-peers (server peer)
  (:documentation "Find neighbors to peer in server's route-table "))


(defgeneric get-distance (server peer)
  (:documentation " "))


;; Defmethod
(defmethod make-bootstrap-nodes ((server server) &key addrs)
  "Make useful bootstrap nodes from addrs passed"
  ;;Not async now, because it's hard to know which ping reponse from which boot-node
  (let ((init-nodes (mapcar #'(lambda (x) (make-instance 'node
                                                    :ip (first x)
                                                    :port (second x)))
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

(defmethod bootstrap ((server server) &key addrs)
  "start connecting to other servers connecting to addrs given
loop through the addrs, ping each, if it replys, call FIND_NODE(self) to it;
addrs format: '((\"127.0.0.0\" 8001) (\"127.0.0.0\" 8002) (\"127.0.0.0\" 8003))"
  (let ((useful-nodes (make-bootstrap-nodes server :addrs addrs))
        (find-node-result '()))

    ;; call FIND_NODE to useful-nodes
    (recursive-process server useful-nodes #'call-find-node (server-node server)
                       #'bootstrap-handle-result nil)))


(defun not-in (each l)
  (if (member (node-id each) (mapcar #'(lambda (x) (node-id x)) l) :test #'equalp) nil t))

(defmethod listen-on ((server server) host port)
  "Load storage; start kad-server"
  ;; (storage-load (server-sotrage))
  (server-listen (server-protocol server) :host host :port port))



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


(defmethod get-key ((server server) key)
  "First check local storage, if not found, then find neighbors of key,
call FIND_VALUE to them.
"
  (check-dht-value-type key)
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


(defun get-shortest-k-by-distance (node-to-find nodes k)
  "list-nodes: ((ip port id) (ip port id) (ip port id))"
  (when nodes
    (subseq (sort nodes #'(lambda (x y) (< (distance-to x node-to-find) (distance-to y node-to-find)))) 0 (min k (length nodes)))))

;; (defun handle-alpha-nodes-once ())


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
                            ;; (break)
                            (setf nearest-uncontacted (append nearest-uncontacted nearest-uncontacted1))
                            (setf result (append result result1))
                            ))
                (when find-value-p
                  ;; (break)
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

(defmethod set-key ((server server) key value)
  "Find in my route table neighbors to key. Call FIND_NODE(key) to them, getting the nearest nodes to key.
Call STORE(key value) to them. If self node is nearer than nodes found, self node also store the key value.
if any remote SRORE finished return success."
  (check-dht-value-type key)

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


;; (defun c-dis (node-to-find nodes-to-call-store)
;;   (mapcar #'(lambda (n) (distance-to n node-to-find)) nodes-to-call-store ))
