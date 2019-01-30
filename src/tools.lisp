(in-package :cl-user)
(defpackage cl-kademlia
  (:use :cl))
(in-package :cl-kademlia)


(defmacro pop-left (l)
  "Notice l need to be setf"
  `(pop ,l))

(defmacro pop-right (l)
  "Notice l need to be setf"
  (let ((g (gensym)))
    `(prog1 (car (last ,l))
       (let ((,g (length ,l)))
         (cond ((= ,g 0) nil)
               ((= ,g 1) (setf ,l nil))
               (t (nbutlast ,l)))))))

(defmacro nleft-pop-n (l n)
  "Pop n items from l, if l is not enough, pop all of it."
  (let ((g (gensym))
        (i (gensym)))
    `(let ((,g '() ))
       (loop :repeat ,n
             for ,i := (pop ,l)
             :do (when ,i (push ,i ,g)))
       (nreverse ,g))))


;; TODO latter
(defun left-push ())

(defun nleft-push ())

(defun nleft-batch-push ())

(defun left-batch-push ())


(defun right-push ())

(defun right-batch-push ())


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



;; def digest(s):
;;     if not isinstance(s, bytes):
;;     s = str(s).encode('utf8')
;;     return hashlib.sha1(s).digest()

;; temp use hex string not byte array as node id
;; (defun digest (key)
;;   (ironclad:byte-array-to-hex-string
;;    (ironclad:digest-sequence
;;     :Sha1
;;     (ironclad:ascii-string-to-byte-array key))))

;; real solution
(defun digest (key)
  (ironclad:digest-sequence
   :Sha1
   (ironclad:ascii-string-to-byte-array key)))

;; node_id.hex()
;; (ironclad:byte-array-to-hex-string **)

;; self.long_id = int(node_id.hex(), 16)
(defun hex-string-to-integer (s)
  (parse-integer s :radix 16))

(defun byte-array-to-integer* (b)
  (setf b (coerce b 'vector))  ;; temp solutions
  #+sbcl
  (setf b (make-array (length b) :element-type '(unsigned-byte 8)  :initial-contents
                      (loop for x across b collect x)))
  (parse-integer (ironclad:byte-array-to-hex-string b) :radix 16))


(defun make-counter (l)
  (let ((r '()))
    (loop :for x :in l
          :do (if (assoc x r :test #'equalp)
                  (setf (cdr (assoc x r :test #'equalp)) (1+ (cdr (assoc x r :test #'equalp))) )
                  (push (cons x 1) r)))
    (sort r  #'(lambda (x y) (> (cdr x) (cdr y))))))


;; def sharedPrefix(args):
;;     """
;;     Find the shared prefix between the strings.

;;     For instance:

;;         sharedPrefix(['blahblah', 'blahwhat'])

;;     returns 'blah'.
;;     """
;;     i = 0
;;     while i < min(map(len, args)):
;;     if len(set(map(operator.itemgetter(i), args))) != 1:
;;     break
;;     i += 1
;;     return args[0][:i]


(defun shared-prefix (args)
  (let ((min-length (apply #'min (mapcar #'length args))))
    (subseq (first args)
            0
            (loop :for i :below min-length
                  :while (apply #'char-equal (mapcar #'(lambda (x) (aref x i)) args))
                  :finally (return i)))))



;; def bytesToBitString(bites):
;;     bits = [bin(bite)[2:].rjust(8, '0') for bite in bites]
;;     return "".join(bits)
(defun bytes-to-bit-string (b)
  (format nil "~{~8,'0B~}" (loop for x across b collect x)))

;; copy from ccl
(defun ipaddr-to-dotted (addr &key values)
  "Convert a 32-bit unsigned IP address into octets."
  (let* ((a (ldb (byte 8 24) addr))
         (b (ldb (byte 8 16) addr))
         (c (ldb (byte 8  8) addr))
         (d (ldb (byte 8  0) addr)))
    (if values
        (values a b c d)
        (format nil "~d.~d.~d.~d" a b c d))))


(defun ensure-vector (l)
  (coerce l 'vector))

(defun check-dht-value-type (value)
  "value type checker"
  (etypecase value
    (integer t)
    (float t)
    (string t)
    (bool t)
    ;; (vector t)
    ))

;; TODO latter
(defmacro magic-concurrent-async (&body body)
  (let ((g (gensym)))
    `(let ((,g (progn ,@body)))
       (log:info "magic result:" ,g)
       ,g)))

(defmacro magic-any1 (&body body)
  `(progn ,@body))
