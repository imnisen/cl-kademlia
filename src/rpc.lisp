(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


;;;
;;; Need to clean this code and package it.
;;;

(defvar data-max-length 65507)

(defclass transport-udp-server ()
  (;; (host :accessor host
   ;;       :initarg :host
   ;;       :initform (error "No host given"))
   ;; (port :accessor port
   ;;       :initarg :port
   ;;       :initform (error "No port given"))
   ;; (socket :accessor :socket
   ;;         :initform nil)
   (dispatch-router :accessor dispatch-router
                    :initform nil)))



;; Need refactor, too unstable
(defun load-msg (str)
  (log:info "Try load-msg" str)
  (handler-case
      (first (multiple-value-list (read-from-string str)))
    (error (e)
      (log:error "load-msg error" e))))

(defun dump-msg (msg)
  (log:info "Try dump-msg" msg)
  (handler-case
      (write-to-string msg
                       :array t :base 10 :case :downcase :circle t
                       :escape t :gensym t :length nil :level nil
                       :lines nil :pretty nil :radix nil :readably nil)
    (error (e)
      (log:error "dump-msg error" e))))



;;; server api
(defun make-server ()
  (make-instance 'transport-udp-server))

(defgeneric expose (server rpc-name lambda-fun)
  (:documentation ""))

(defmethod expose ((server transport-udp-server) rpc-name lambda-fun)
  (push (cons rpc-name lambda-fun) (dispatch-router server)))

(defun server-listen (server &key host port)
  (log:info "Starting listen on address ~a ~a" host port)
  (let ((socket (usocket:socket-connect nil nil
                                        :protocol :datagram
                                        :local-host host
                                        :local-port port)))
    (unwind-protect
         (loop :for (msg . addr)
               := (multiple-value-bind (return-buffer return-length remote-host remote-port)
                      (usocket:socket-receive socket nil data-max-length)
                    (let ((msg (load-msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))))
                      (log:info "Server receive msg: ~a from client ~a ~a " msg remote-host remote-port)
                      (list msg remote-host remote-port)))
               :do (progn

                     ;; send result back to client
                     (let* ((result (progn
                                      ;; recved msg format : (msg-id rpc-name args-in-list)
                                      (apply (cdr (assoc (second msg) (dispatch-router server) :test #'equalp))
                                             (cons (list (when (stringp (first addr))
                                                           (ipaddr-to-dotted (first addr)))
                                                         (second addr)) (third msg))
                                             ;; (append (list (first addr) (second addr)) (third msg))
                                             )))
                            (packet (dump-msg (append msg (list result)))) ;; construct the return msg: (msg-id rpc-name args-in-list result)
                            (buffer (flexi-streams:string-to-octets packet :external-format :utf-8)))
                       (log:info "Server reply msg: ~a to  client ~a ~a" packet (first addr) (second addr))
                       (usocket:socket-send socket buffer (length buffer)
                                            :host (first addr)
                                            :port (second addr))))
               )
      (usocket:socket-close socket))))




(defclass transport-udp-client ()
  ((host :accessor client-host
         :initarg :host
         :initform nil)
   (port :accessor client-port
         :initarg :port
         :initform nil)
   (outstanding :accessor client-outstanding
                :initform nil)
   (outstanding-lock :accessor client-outstanding-lock
                     :initform (bt:make-lock "outstanding-lock"))
   (socket :accessor client-socket
           :initform nil)
   (backgroud-thread :accessor client-background-thread
                     :initform nil)

   ))


;;; Client api
(defun make-client ()
  (make-instance 'transport-udp-client))


(defun generate-msg-id ()
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence
                                      :Sha1
                                      (crypto:random-data 32 (crypto:make-prng :fortuna)))))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(bt:with-lock-held (,lock) ,@body))

;; At separate thread
(defvar check-time-interval 0.2)
(defun call (client rpc-name args &key timeout)
  (let* ((msg-id (generate-msg-id))
         (msg (list msg-id rpc-name args))  ;; construct the send msg: (msg-id rpc-name args-in-list)
         (packet (dump-msg msg))
         (buffer (flexi-streams:string-to-octets packet :external-format :utf-8))
         (result nil)
         (end-time (and timeout (+ (get-internal-real-time) (* internal-time-units-per-second timeout)))))
    (log:info "Client send msg: ~a to server ~a ~a" msg (client-host client) (client-port client))
    (with-lock-held ((client-outstanding-lock client))
      (push (cons msg-id nil) (client-outstanding client)))

    (handler-case
        (usocket:socket-send (client-socket client) buffer (length buffer))
      (error (c)
        (log:info "Notice: connecting error" c " to"
                  (client-host client)
                  (client-port client))))

    ;; fetch result from outstanding
    (loop :while (or (null end-time)
                     (< (get-internal-real-time) end-time))
          :do (progn
                (with-lock-held ((client-outstanding-lock client))
                  (when (cdr (assoc msg-id (client-outstanding client) :test #'equalp))
                    (setf result  (cdr (assoc msg-id (client-outstanding client) :test #'equalp)))
                    (log:info "Result is" result)
                    (return result)))
                (sleep check-time-interval)
                (log:info "once")))))


;; In backgroung thread
(defun handle-msg (client)
  (loop
    (multiple-value-bind (return-buffer return-length)
        (usocket:socket-receive (client-socket client) nil data-max-length)
      (let* ((msg (load-msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))) ;; recved msg: (msg-id rpc-name args-in-list result)
             (msg-id (first msg))
             (result-value (fourth msg)))
        (with-lock-held ((client-outstanding-lock client))
          (if (assoc msg-id (client-outstanding client) :test #'equalp)
              (progn
                (log:info "Client get a msg" msg-id result-value)
                (setf (cdr (assoc msg-id (client-outstanding client) :test #'equalp))
                      result-value))
              (log:info "Unknown msg ~a" msg)))
        (log:info "Client receive msg: ~a from server" msg)))))


(defun client-connect (client &key host port)
  (log:info "Client start listen on address" host port)
  (let ((socket (usocket:socket-connect host port
                                        :protocol :datagram)))
    (setf (client-socket client) socket
          (client-host client) host
          (client-port client) port
          (client-background-thread client) (bt:make-thread (lambda () (handle-msg client))
                                                            :name "Background client"))

    ))


(defun client-stop (client)
  (bt:destroy-thread (client-background-thread client)) ;;very rude now
  (sleep 1)
  (usocket:socket-close (client-socket client))
  (log:info "client stop"))




;; ;; use demo

;; ;; server
;; (defvar *server* (make-server))

;; (expose *server* "sum" (lambda (a b) (reduce #'+ (list a b))))
;; (expose *server* "mul" (lambda (args) (reduce #'* args)))
;; (expose *server* "sub" (lambda (&rest args) (reduce #'- args)))

;; (server-listen *server* :host "127.0.0.1" :port 50879)


;; 
;; ;; client

;; (defvar *client* (make-client))

;; (client-connect *client* :host "127.0.0.1" :port 50879)

;; (call *client* "sum" '(10 20))
;; (call *client* "mul" '((10 20 30)))
;; (call *client* "sub" '(10 20 30) :timeout 10)

;; (client-stop *client*)
;; ;; add timeout
