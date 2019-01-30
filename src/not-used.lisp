;; Latter
;; (defgeneric ask-for-nearest-peers (server peer)
;;   (:documentation "Find neighbors to peer in server's route-table "))
;; (defgeneric get-distance (server peer)
;;   (:documentation ""))


;;; crawling.lisp
;; (defclass spider-crawl ()
;;   ((protocol :initarg :protocol
;;              :accessor spider-protocol)
;;    (ksize :initarg :ksize
;;           :accessor spider-ksize)
;;    (alpha :initarg :alpha
;;           :accessor spider-alpha)
;;    (node :initarg :node
;;          :accessor spider-node)
;;    (nearest :initarg :nearest
;;             :accessor spider-nearest)
;;    (last-ids-crawled :initarg :last-ids-crawled
;;                      :accessor spider-node-last-ids-crawled)))

;; (defgeneric _find (spider rpcmethod)
;;   (:documentation ""))

;; (defgeneric nodes-found (spider responses)
;;   (:documentation ""))

;; ;; -----------------

;; (defmethod _find ((spider spider-crawl) rpcmethod)
;;   (with-slots (nearest alpha node) spider
;;     (log:info "crawling network with nearest" nearest)

;;     (let ((count alpha)
;;           (ds '()))
;;       (loop :for peer :in (subseq (get-uncontacted nearest) 0 count)
;;             :do (progn (mark-contacted peer)
;;                        (push (node-id peer)
;;                              (apply rpcmethod (list peer node)))))
;;       (nodes-found spider (magic-gather ds)) ;; can here dispatch successfully?
;;       ))

;;   )




;; 

;; (defclass value-spider-crawl (spider-crawl)
;;   ((nearest-without-value :initarg :nearest-without-value
;;                           :accessor spider-nearest-without-value)))

;; (defgeneric find (value-spider)
;;   (:documentation ""))

;; (defgeneric nodes-found (value-spider)
;;   (:documentation ""))

;; (defgeneric handle-found-values (value-spider values)
;;   (:documentation ""))

;; ;; -----------------

;; (defmethod find ((value-spider value-spider))
;;   (_find value-spider call-find-value))

;; (defmethod nodes-found ((value-spider value-spider) responses)
;;   (let ((to-move '())
;;         (found-values '()))
;;     (loop :for (peer-id . response) :in responses
;;           :do (cond ((not (happened responsed)) (push peer-id to-move))
;;                     ((has-value response) (push (get-value response) found-values))
;;                     (t (let ((peer (get-node-by-id peer-id)))
;;                          (with-slots (nearest-without-value nearest) spider
;;                            (push peer nearest-without-value)
;;                            (push peer nearest))))))
;;     (remove (spider-nearest spider) to-move)

;;     (cond ((> (length found-values) 0) (handle-found-values spider found-values))
;;           ((all-been-contacted (spider-nearest spider)) nil)
;;           (t (find spider)))

;;     ))

;; (defmethod handle-found-values ((value-spider value-spider) values)
;;   (todo))












;; 

;; (defclass node-spider-crawl (spider-crawl)
;;   ((nearest-without-value :initarg :nearest-without-value
;;                           :accessor spider-nearest-without-value)))

;; (defgeneric find (node-spider)
;;   (:documentation ""))

;; (defgeneric nodes-found (node-spider)
;;   (:documentation ""))


;; 

;; (defclass rpc-find-response ()
;;   ((response :initarg :response
;;              :accessor rpc-response)))


;; (defgeneric happened (response)
;;   (:documentation ""))

;; (defgeneric has-value (response)
;;   (:documentation ""))

;; (defgeneric get-value (response)
;;   (:documentation ""))

;; (defgeneric get-node-list (response)
;;   (:documentation ""))
