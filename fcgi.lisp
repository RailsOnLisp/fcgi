
(in-package :fcgi)

(defvar *fcgi-web-server-addrs* nil)

(defconstant +fcgi-version-1+ 1)

(let ((fcgi-types
       #(nil
         begin-request
         abort-request
         end-request
         params
         stdin
         stdout
         stderr
         data
         get-values
         get-values-result
         unknown)))
  (defun fcgi-type-code (type)
    (position type fcgi-types :test 'eq))
  (defun fcgi-code-type (code)
    (when (< code (length fcgi-types))
      (svref fcgi-types code))))

;;  unsigned bytes

(deftype ub8 (&optional length) `(array octet (,length)))

(defun ub8 (&optional (contents ()) (length (length contents)))
  (apply #'make-array length :element-type 'octet
         (when contents
           `(:initial-contents ,contents))))

;;  FastCGI record

(defstruct fcgi-record
  (header (ub8 `(,+fcgi-version-1+ 0 0 0 0 0 0 0)) :type (ub8 8))
  (content (ub8) :type ub8)
  (padding (ub8) :type ub8))

(defgeneric fcgi-record-version (record))
(defgeneric (setf fcgi-record-version) (value record))
(defgeneric fcgi-record-type (record))
(defgeneric (setf fcgi-record-type) (value record))
(defgeneric fcgi-record-request-id (record))
(defgeneric (setf fcgi-record-request-id) (value record))
(defgeneric fcgi-record-content-length (record))
(defgeneric (setf fcgi-record-content-length) (value record))
(defgeneric fcgi-record-padding-length (record))
(defgeneric (setf fcgi-record-padding-length) (value record))

(defmethod fcgi-record-version ((a array))
  (aref a 0))

(defmethod (setf fcgi-record-version) (value (a array))
  (setf (aref a 0) value))

(defmethod fcgi-record-type ((a array))
  (fcgi-code-type (aref a 1)))

(defmethod (setf fcgi-record-type) (value (a array))
  (setf (aref a 1) (fcgi-type-code value)))

(defmethod fcgi-record-request-id ((a array))
  (logior (ash (aref a 2) 8)
          (aref a 3)))

(defmethod (setf fcgi-record-request-id) (value (a array))
  (setf (aref a 2) (logand #xFF (ash value -8))
        (aref a 3) (logand #xFF value)))

(defmethod fcgi-record-content-length ((a array))
  (logior (ash (aref a 4) 8)
          (aref a 5)))

(defmethod (setf fcgi-record-content-length) (value (a array))
  (setf (aref a 4) (logand #xFF (ash value -8))
        (aref a 5) (logand #xFF value)))

(defmethod fcgi-record-padding-length ((a array))
  (aref a 6))

(defmethod (setf fcgi-record-padding-length) (value (a array))
  (setf (aref a 6) value))

(defun padding-length (x)
  (- (* (ceiling x 8) 8) x))

(defun fcgi-record (&key (version +fcgi-version-1+)
                      (type 0)
                      (request-id 0)
                      (content (ub8))
                      (content-length (length content))
                      (padding-length (padding-length content-length))
                      (padding (ub8 () padding-length)))
  (let ((header (ub8 () 8)))
    (setf (fcgi-record-version header) version
          (fcgi-record-type header) type
          (fcgi-record-request-id header) request-id
          (fcgi-record-content-length header) content-length
          (fcgi-record-padding-length header) padding-length)
    (make-fcgi-record :header header :content content :padding padding)))

(defmethod print-object ((obj fcgi-record) stream)
  (prin1 `(fcgi-record :version ,(fcgi-record-version obj)
                       :type ,(fcgi-record-type obj)
                       :request-id ,(fcgi-record-request-id obj)
                       :content ,(fcgi-record-content obj)
                       :padding-length ,(fcgi-record-padding-length obj))
         stream))

(defmethod fcgi-record-version ((r fcgi-record))
  (fcgi-record-version (fcgi-record-header r)))

(defmethod (setf fcgi-record-version) (value (r fcgi-record))
  (setf (fcgi-record-version (fcgi-record-header r)) value))

(defmethod fcgi-record-type ((r fcgi-record))
  (fcgi-record-type (fcgi-record-header r)))

(defmethod (setf fcgi-record-type) (value (r fcgi-record))
  (setf (fcgi-record-type (fcgi-record-header r)) value))

(defmethod fcgi-record-request-id ((r fcgi-record))
  (fcgi-record-request-id (fcgi-record-header r)))

(defmethod (setf fcgi-record-request-id) (value (r fcgi-record))
  (setf (fcgi-record-request-id (fcgi-record-header r)) value))

(defmethod fcgi-record-content-length ((r fcgi-record))
  (fcgi-record-content-length (fcgi-record-header r)))

(defmethod (setf fcgi-record-content-length) (value (r fcgi-record))
  (setf (fcgi-record-content-length (fcgi-record-header r)) value))

(defmethod fcgi-record-padding-length ((r fcgi-record))
  (fcgi-record-padding-length (fcgi-record-header r)))

(defmethod (setf fcgi-record-padding-length) (value (r fcgi-record))
  (setf (fcgi-record-padding-length (fcgi-record-header r)) value))

(defun recv-fcgi-record (fd)
  (let ((header (ub8 () 8)))
      (recv-sequence fd header 0)
      (let ((content (ub8 () (fcgi-record-content-length header)))
            (padding (ub8 () (fcgi-record-padding-length header))))
        (recv-sequence fd content 0)
        (recv-sequence fd padding 0)
        (make-fcgi-record :header header
                          :content content
                          :padding padding))))

(defun send-fcgi-record (fd record)
  (send-sequence fd (fcgi-record-header record) 0)
  (send-sequence fd (fcgi-record-content record) 0)
  (send-sequence fd (fcgi-record-padding record) 0))

;;  Streams

(defclass fcgi-stream (trivial-gray-stream-mixin)
  ())

(define-condition fcgi-stream-error (stream-error)
  ()
  (:documentation "Superclass for all errors related to
FastCGI streams."))

(define-condition fcgi-stream-closed-error (fcgi-stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is closed."
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when someone is trying
to read from or write to a closed FastCGI stream."))

(defmethod check-if-open ((stream fcgi-stream))
  "Checks if STREAM is open and signals an error otherwise."
  (unless (open-stream-p stream)
    (error 'fcgi-stream-closed-error
           :stream stream)))

(defmethod stream-element-type ((stream fcgi-stream))
  'octet)

(defclass fcgi-input-stream (fcgi-stream fundamental-binary-input-stream)
  ((buffers :initform ()
            :accessor fcgi-input-stream-buffers
            :type list)
   (index :initform 0
          :accessor fcgi-input-stream-index
          :type (unsigned-byte 16))
   (complete :initform nil
             :accessor fcgi-input-stream-complete
             :type boolean)))

(defmethod fcgi-input-stream-add-buffer ((stream fcgi-stream) buffer)
  (with-slots (buffers complete) stream
    (if (endp buffers)
        (setf buffers (list buffer))
        (labels ((add (list)
                   (if (endp (rest list))
                       (setf (rest list) (list buffer))
                       (add (rest list)))))
          (add buffers)))))

(defmethod stream-read-byte ((stream fcgi-input-stream))
  (check-if-open stream)
  (with-accessors ((buffers fcgi-input-stream-buffers)
                   (index fcgi-input-stream-index)) stream
    (if (endp buffers)
        :eof
        (let ((b (first buffers)))
            (cond ((>= index (length b))
                   (setf buffers (rest buffers)
                         index 0)
                   (stream-read-byte))
                  (t
                   (aref b index)
                   (incf index)))))))

(defclass fcgi-output-stream (fcgi-stream fundamental-binary-output-stream)
  ((buffer :initform (make-array 4000 :element-type 'octet
                                 :fill-pointer 0)
           :reader fcgi-output-stream-buffer
           :type array)
   (type :initarg :type
         :reader fcgi-output-stream-type
         :type symbol)
   (request-id :initarg :request-id
               :reader fcgi-output-stream-request-id
               :type (unsigned-byte 16))
   (sockfd :initarg :sockfd
           :reader fcgi-output-stream-sockfd
           :type file-descriptor)))

(defmethod stream-flush ((stream fcgi-output-stream))
  (with-slots (buffer type request-id sockfd) stream
    (let ((record (fcgi-record :type type
                               :request-id request-id
                               :content buffer)))
      (send-fcgi-record sockfd record)
      (setf (fill-pointer buffer) 0))))

(defmethod stream-write-byte ((stream fcgi-output-stream) byte)
  (check-if-open stream)
  (with-slots (buffer) stream
    (setf (aref buffer (fill-pointer buffer)) byte
          (fill-pointer buffer) (1+ (fill-pointer buffer)))
    (when (= (array-dimension buffer 0) (fill-pointer buffer))
      (stream-flush stream)))
  byte)

(defmethod stream-write-sequence ((stream fcgi-output-stream) seq start end
                                  &key &allow-other-keys)
  (check-if-open stream)
  (with-slots (buffer) stream
    (loop
       while (< start end)
       for e = (min (- end start)
                    (- (array-dimension buffer 0) (fill-pointer buffer)))
       do (dotimes (i e)
            (setf (aref buffer (fill-pointer buffer)) (aref seq start))
            (incf (fill-pointer buffer))
            (incf start))
       do (when (= (array-dimension buffer 0) (fill-pointer buffer))
            (stream-flush stream)))))
      
(defmethod fcgi-close ((stream fcgi-output-stream))
  (stream-flush stream)
  (with-slots (type request-id sockfd) stream
    (let ((record (fcgi-record :type type :request-id request-id)))
      (send-fcgi-record sockfd record))))

;;  Requests

(defparameter *request* nil)

(defvar *fcgi-requests*)

(defun make-fcgi-requests-array ()
  (make-array 16 :adjustable t :initial-element nil))

(defstruct fcgi-request
  (id    0 :type (unsigned-byte 16))
  (role  0 :type (unsigned-byte 16))
  (flags 0 :type (unsigned-byte 8))
  (params (make-instance 'fcgi-input-stream)  :type fcgi-input-stream)
  (stdin  (make-instance 'fcgi-input-stream)  :type fcgi-input-stream)
  (stdout (make-instance 'fcgi-output-stream) :type fcgi-output-stream)
  (stderr (make-instance 'fcgi-output-stream) :type fcgi-output-stream))

(defun fcgi-request (&key id role flags sockfd)
  (make-fcgi-request :id id :role role :flags flags
                     :params (make-instance 'fcgi-input-stream)
                     :stdin  (make-instance 'fcgi-input-stream)
                     :stdout (make-instance 'fcgi-output-stream
                                            :sockfd sockfd
                                            :request-id id
                                            :type 'stdout)      
                     :stderr (make-instance 'fcgi-output-stream
                                            :sockfd sockfd
                                            :request-id id
                                            :type 'stderr)))

;;  Name value pairs

(defun read-1-or-4-bytes (stream)
  (let ((b (read-byte stream)))
    (if (zerop (ash b -7))
        b
        (+ (ash (logand b #x7F) 24)
           (ash (read-byte stream) 16)
           (ash (read-byte stream) 8)
           (read-byte stream)))))

(defun read-name-value (stream)
  (let* ((name-length (read-1-or-4-bytes stream))
         (value-length (read-1-or-4-bytes stream))
         (name (make-array name-length :element-type '(unsigned-byte 8)))
         (value (make-array value-length :element-type '(unsigned-byte 8))))
    (read-sequence name stream)
    (read-sequence value stream)
    (cons name value)))

(defun write-1-or-4-bytes (value stream)
  (cond ((< value #x80)
         (write-byte value stream))
        (t
         (write-byte (logior #x80 (logand #x7F (ash value -24))) stream)
         (write-byte (logand #xFF (ash value -16)) stream)
         (write-byte (logand #xFF (ash value -8)) stream)
         (write-byte (logand #xFF value) stream)))
  (force-output stream))

(defun write-name-value (cons stream)
  (let ((name (car cons))
        (value (cdr cons)))
    (write-1-or-4-bytes (length name) stream)
    (write-1-or-4-bytes (length value) stream)
    (write-sequence name stream)
    (write-sequence value stream)))

;;  Variables

(defvar *fcgi-max-conn* 1)

(defvar *fcgi-max-reqs* 1)

(defvar *fcgi-mpxs-conns* 0)

;;  Management record types

(defun fcgi-unknown-type-record (type)
  (let ((header (make-array 8 :element-type '(unsigned-byte 8)))
        (content (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (fcgi-record-version header) +fcgi-version-1+
          (fcgi-record-type header) 'unknown
          (fcgi-record-request-id header) 0
          (fcgi-record-content-length header) 8
          (fcgi-record-padding-length header) 0
          (aref content 0) type)
    (make-fcgi-record :header header :content content)))

(defun fcgi-unknown-type (sockfd record)
  (let ((r (fcgi-unknown-type-record (fcgi-record-type record))))
    (send-fcgi-record sockfd r)))

;;  Application record types

(defun fcgi-begin-request (sockfd record)
  (let* ((id (fcgi-record-request-id record))
         (content (fcgi-record-content record))
         (role (logior (ash (aref content 0) 8)
                       (aref content 1)))
         (flags (aref content 2))
         (req (fcgi-request :id id :role role :flags flags :sockfd sockfd)))
    (setf (aref *fcgi-requests* id) req)))

(defun fcgi-params (request record)
  (let ((params (fcgi-request-params request))
        (content (fcgi-record-content record)))
    (if (zerop (length content))
        (setf (fcgi-input-stream-complete params) t)
        (fcgi-input-stream-add-buffer params (fcgi-record-content record)))))

(defconstant +fcgi-request-complete+ 0)

(defun fcgi-end-request (sockfd request status
                         &optional (protocol-status +fcgi-request-complete+))
  (let* ((status (typecase status
                   (integer status)
                   (t 0)))
         (content (ub8 (list (logand #xFF (ash status -24))
                             (logand #xFF (ash status -16))
                             (logand #xFF (ash status -8))
                             (logand #xFF status)
                             protocol-status
                             0 0 0)))
         (record (fcgi-record :type 'end-request
                              :request-id (fcgi-request-id request)
                              :content content)))
    (send-fcgi-record sockfd record)))

(defun fcgi-call (sockfd func request)
  (let ((status (funcall func request)))
    (fcgi-close (fcgi-request-stdout request))
    (fcgi-close (fcgi-request-stderr request))
    (fcgi-end-request sockfd request status)))

(defun fcgi-stdin (sockfd request record func)
  (let ((stdin (fcgi-request-stdin request))
        (content (fcgi-record-content record)))
    (cond ((zerop (length content))
           (setf (fcgi-input-stream-complete stdin) t)
           (fcgi-call sockfd func request))
          (t
           (fcgi-input-stream-add-buffer stdin (fcgi-record-content record))))))

;;  Handler

(defun handle-fcgi-record (sockfd record func)
  (assert (= +fcgi-version-1+ (fcgi-record-version record)))
  (let ((*request* (aref *fcgi-requests* (fcgi-record-request-id record))))
    (if (null *request*)
        (case (fcgi-record-type record)
          ((begin-request) (fcgi-begin-request sockfd record)))
        (case (fcgi-record-type record)
          ((begin-request) ())
          ((params) (fcgi-params *request* record))
          ((stdin) (fcgi-stdin sockfd *request* record func))
          (:otherwise (fcgi-unknown-type sockfd record))))))

;;  Sockets

(defun server-on-socket (func bound-socket)
  (loop for sockfd = (ignore-errors (accept bound-socket))
     while sockfd
     do
       (unwind-protect
            (loop for record = (ignore-errors (recv-fcgi-record sockfd))
               while record
               do (handle-fcgi-record sockfd record func))
         ;;    do (format t "~&~S~%" record))
         (close-sock sockfd))))

(defvar *sockets* ())

(defun close-all-sockets ()
  (unless (endp *sockets*)
    (close-sock (pop *sockets*))
    (close-all-sockets)))

(defun socket-server (func &key
                             (inet-addr "127.0.0.1")
                             (port 9000))
  (let ((s (socket +af-inet+ +sock-stream+ 0)))
    (unwind-protect (progn
                      (push s *sockets*)
                      (bind-inet s inet-addr port)
                      (listen-sock s 128)
                      (let ((*fcgi-requests* (make-fcgi-requests-array)))
                        (server-on-socket func s)))
      (ignore-errors (close-sock s))
      (setf *sockets* (remove s *sockets*)))))

(untrace socket-server fcgi-begin-request fcgi-params fcgi-stdin)
(trace socket-server fcgi-begin-request fcgi-params fcgi-stdin)

(defun test-func (req)
  (format t "~&TEST ~S~%" req)
  (let ((out (make-flexi-stream (fcgi-request-stdout req)
                                :external-format :utf-8))
        (err (make-flexi-stream (fcgi-request-stderr req)
                                :external-format :utf-8))
        (crlf #.(coerce '(#\Return #\Newline) 'string)))
    (declare (ignore err))
    (format out "HTTP/1.0 200 OK~A" crlf)
    (format out "Content-Type: text/plain; charset=UTF-8~A" crlf)
    (format out "Content-Length: 11")
    (format out "~A" crlf)
    (format out "Hello world~A" crlf)))

#+test
(close-all-sockets)

#+test
(socket-server 'test-func :inet-addr "0.0.0.0" :port 4207)

#+test
(ub8 '(1 2 3))
