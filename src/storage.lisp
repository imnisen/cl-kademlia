(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


;; storage

(defclass storage () ())

(defgeneric storage-store (storage key value)
  (:documentation ""))

(defgeneric storage-fetch (storage key)
  (:documentation ""))

;; (defgeneric storage-dump ()
;;   (:documentation ""))

;; (defgeneric storage-load ()
;;   (:documentation ""))

(defgeneric iter-tiems-older-than ()
  (:documentation ""))

(defgeneric items ()
  (:documentation ""))



(defclass forgetful-storage (storage)
  ((data :initarg :data
         :accessor storage-data
         :initform (make-hash-table :test #'equalp))
   (ttl :initarg :ttl
        :accessor storage-ttl
        :initform 604800)))

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defmethod storage-store ((storage forgetful-storage) key value)
  (setf (gethash key (storage-data storage)) (list value (now))))

(defmethod storage-fetch ((storage forgetful-storage) key)
  (first (gethash key (storage-data storage))))


(defmethod cull ((storage forgetful-storage))
  "delete expired data"
  (todo))

;; (defmethod storage-dump ((storage forgetful-storage))
;;   (:documentation ""))

;; (defmethod storage-load ((storage forgetful-storage))
;;   (:documentation ""))
