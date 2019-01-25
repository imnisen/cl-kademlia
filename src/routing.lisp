(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)



;; Kbucket
(defclass kbucket ()
  ((range-lower :initarg :range-lower
                :accessor kbucket-range-lower)
   (range-upper :initarg :range-upper
                :accessor kbucket-range-upper) ;; kbucket range
   (nodes :initarg :nodes
          :accessor kbucket-nodes
          :initform '())  ;; format (#(126 36 13 231 79 177 237 8 250 8 211 128 99 246 166 169 20 98 168 21) . #<NODE #x302001BB4CDD>)
   (replacement-nodes :initarg :replacement-nodes
                      :accessor kbucket-replacement-nodes
                      :initform '())
   (ksize :initarg :ksize
          :accessor kbucket-ksize))
  )


(defgeneric get-nodes (kbucket)
  (:documentation ""))


(defgeneric add-node (kbucket node)
  (:documentation "Add a C{Node} to the C{KBucket}.  Return True if successful,
False if the bucket is full. If the bucket is full, keep track of node in a replacement list,
per section 4.1 of the paper."))



(defgeneric split (kbucket)
  (:documentation ""))

(defgeneric remove-node (kbucket node)
  (:documentation ""))


(defgeneric has-in-range (kbucket node)
  (:documentation ""))

(defgeneric is-new-node (kbucket node)
  (:documentation ""))

(defgeneric depth (kbucket)
  (:documentation ""))

(defgeneric head (kbucket)
  (:documentation ""))

;; -----------------------------

(defmethod get-nodes ((kbucket kbucket))
  (mapcar #'cdr (kbucket-nodes kbucket)))

(defmethod has-in-range ((kbucket kbucket) node)
  (<= (kbucket-range-lower kbucket) (node-long-id node) (kbucket-range-upper kbucket)))

(defmethod is-new-node ((kbucket kbucket) node)
  (if (find (node-id node) (mapcar #'car (kbucket-nodes kbucket)) :test 'equalp) nil t))


(defmethod add-node ((kbucket kbucket) node)
  (with-slots (id) node
    (with-slots (nodes ksize replacement-nodes) kbucket
      (cond ((assoc id nodes :test #'equalp)
             (progn (setf (cdr (assoc id nodes :test #'equalp)) node) nil))
            ((< (length nodes) ksize)
             (progn (push (cons id node) nodes) t))
            (t
             (progn (push node replacement-nodes) nil))))))


(defmethod split ((kbucket kbucket))
  (with-slots (range-lower range-upper ksize nodes) kbucket
    (let* ((midpoint (round (/ (+ range-lower range-upper) 2)))
           (one (make-instance 'kbucket :range-lower range-lower
                                        :range-upper midpoint
                                        :ksize ksize))
           (two (make-instance 'kbucket :range-lower (+ 1 midpoint)
                                        :range-upper range-upper
                                        :ksize ksize)))
      (loop :for node :in (mapcar #'cdr  nodes)
            :do (push (cons (node-id node) node)
                      (kbucket-nodes (if (<= (node-long-id node)) one two))))
      (values one two))))

;; TODO The proper way is when use push to nodes, keep them in order, then, when head, get the first one. But now ,since push appears many places, let's take the order in the head
(defmethod head ((kbucket kbucket))
  (with-slots (nodes) kbucket
    (car (first (sort nodes #'(lambda (x y) (< (car x) (car y))))))))


(defmethod depth ((kbucket kbucket))
  (length (shared-prefix (mapcar #'(lambda (x) (bytes-to-bit-string (car x)))) (kbucket-nodes kbucket))))



(defmethod remove-node ((kbucket kbucket) node)
  (let ((x (remove-if #'(lambda (x) (equalp (node-id (cdr x)) (node-id node)))
                      (kbucket-nodes kbucket))))
    (setf (kbucket-nodes kbucket)
          x))

  ;; todo replacement nodes logic goes here

  )


;; Routing table

(defclass routing-table ()
  ((node :initarg :node
         :accessor router-node) ;; whose routing-table
   (ksize :initarg :ksize
          :accessor router-ksize)
   (buckets :initarg :buckets
            :accessor router-buckets)
   (protocol :initarg :protocol
             :accessor router-protocol))  )



(defgeneric find-neighbors (routing-table node-to-find  &key k exclude)
  (:documentation ""))



(defgeneric split-bucket (routing-table index)
  (:documentation ""))

(defgeneric get-lonely-buckets (routing-table)
  (:documentation ""))


(defgeneric remove-contact (routing-table node)
  (:documentation ""))

(defgeneric add-contact (routing-table node)
  (:documentation ""))

(defgeneric is-new-node (routing-table node)
  (:documentation ""))

(defgeneric get-bucket-for (routing-table target-id)
  (:documentation ""))


;; ---------------------

(defmethod initialize-instance :after ((routing-table routing-table) &key)
  (setf (router-buckets routing-table)
        (list (make-instance 'kbucket
                             :range-lower 0
                             :range-upper (expt 2 160)
                             ;; :nodes (list (cons (node-id (router-node routing-table))
                             ;;                    (router-node routing-table)))
                             :nodes '()
                             :ksize (router-ksize routing-table)))))


(defmethod find-neighbors ((routing-table routing-table) node-to-find &key k exclude)
  "return a list of nodes"
  (let ((buckets (router-buckets routing-table))
        (index (get-bucket-for routing-table node-to-find))
        (neighbors '())
        (k (or k (router-ksize routing-table))))
    (loop :with currerent-nodes := (get-nodes (nth index buckets))
          :with left-buckets := (subseq buckets 0 index)
          :with right-buckets := (subseq buckets (+ 1 index))
          :with left := t
          :for k1 := k then (- k1 1)
          :while (> k1 0)
          :do (cond (currerent-nodes (let ((n (pop-right currerent-nodes)))
                                       (log:info (node-id n) (node-id node-to-find))
                                       (when (and (not (equal (node-id n) (node-id node-to-find)))
                                                  (or (null exclude)
                                                      (not (same-home-as exclude n))))
                                         (push n neighbors))))
                    ((and left left-buckets) (setf currerent-nodes (get-nodes (pop-right left-buckets))
                                                   left nil))
                    (right-buckets (setf currerent-nodes (get-nodes (pop-left right-buckets))
                                         left t))
                    (t (return))))
    ;; TODO sort by distance?
    neighbors))




(defmethod get-bucket-for ((routing-table routing-table) node)
  (let ((buckets (router-buckets routing-table)))
    (loop :for bucket :in buckets
          :for upper := (kbucket-range-upper bucket)
          :for index := 0 then (+ index 1)
          :while (> upper (node-long-id node))
          :finally (return index))))


(defmethod is-new-node ((routing-table routing-table) node)
  (let ((index (get-bucket-for routing-table node)))
    (is-new-node (nth index (router-buckets routing-table)) node)))


(defmethod add-contact ((routing-table routing-table) node)
  ;; (break)
  (let* ((index (get-bucket-for routing-table node))
         (bucket (nth index (router-buckets routing-table))))
    (when (not (add-node bucket node))
      (if (or (has-in-range bucket (router-node routing-table))
              (not (= 0 (mod (depth bucket) 5))))
          (progn
            (split-bucket routing-table index)
            (add-contact routing-table node))
          ;; this call ping maybe need do async
          (call-ping (router-protocol routing-table) (head bucket))
          ))))

(defmethod split-bucket ((routing-table routing-table) index)
  (with-slots (buckets) routing-table
    (multiple-value-bind (one two) (split (nth index buckets))
      (setf (nth index buckets) one)
      (setf (nth (+ 1 index) buckets) two))))


(defmethod remove-contact (routing-table node)
  (let* ((index (get-bucket-for routing-table node))
         (bucket (nth index (router-buckets routing-table))))
    (remove-node bucket node)))
