(asdf:oos 'asdf:load-op :trivial-gray-streams)

(use-package :trivial-gray-streams)

(defclass stream-manipulator ()
  ((action :accessor action-of
:initarg :action
:initform (lambda (manip-stream) (declare (ignore manip-stream)) nil))))

(defclass manip-stream ()
  ((stream :accessor stream-of :initarg :stream :initform (error "Required :stream"))
   (base-field :accessor base-field-of :initform 10)))

(defclass manip-output-stream (manip-stream trivial-gray-streams:fundamental-character-output-stream)
  ((adjust-field :accessor adjust-field-of :initform :right)
   (showbase :accessor showbase-of :initform nil)
   (unit-buf :accessor unit-buf-of :initform nil) ;; 出力処理の旅に出力バッファをフラッシュするか
   (width :accessor width-of :initform 0) ;; 最低の出力幅
   (fill-char :accessor fill-char-of :initform #\Space)))

(defgeneric set-flag (manip-stream flag &rest args))
(defgeneric clear-flags (manip-stream))

(defmethod set-flag ((manip-stream manip-stream) flag &rest args)
  (case flag
    ((base-field) (setf (base-field-of manip-stream) (car args)))
    (T (error "Invalid flag: ~A" flag))))

(defmethod clear-flags ((manip-stream manip-stream))
  nil)

(defmethod set-flag ((manip-stream manip-output-stream) flag &rest args)
  (case flag
    ((adjust-field) (setf (adjust-field-of manip-stream) (car args)))
    ((showbase) (setf (showbase-of manip-stream) t))
    ((unit-buf) (setf (unit-buf-of manip-stream) t))
    ((no-unit-buf) (setf (unit-buf-of manip-stream) nil))
    ((width) (setf (width-of manip-stream) (car args)))
    ((fill-char) (setf (fill-char-of manip-stream) (car args)))
    (T (call-next-method))))

(defmethod clear-flags ((manip-stream manip-output-stream))
  (set-flag manip-stream 'width 0)
  (set-flag manip-stream 'fill-char #\space))

(defun set-fill (char)
  (make-instance
   'stream-manipulator
   :action
   #'(lambda (manip-stream)
       (set-flag manip-stream 'fill-char char))))

(defun set-width (n)
  (make-instance
   'stream-manipulator
   :action
   #'(lambda (manip-stream)
       (set-flag manip-stream 'width n))))

(defun set-base-field (n)
  (make-instance
   'stream-manipulator
   :action
   #'(lambda (manip-stream)
       (set-flag manip-stream 'base-field n))))

(defparameter +endl+
  (make-instance
   'stream-manipulator
   :action
   #'(lambda (manip-stream)
       (format (stream-of manip-stream) "~%")
       (force-output (stream-of manip-stream)))))

(defparameter +left+
  (make-instance
   'stream-manipulator
   :action
   #'(lambda (manip-stream)
       (set-flag manip-stream 'adjust-field :left))))

(defparameter +right+
  (make-instance
   'stream-manipulator
   :action
   #'(lambda (manip-stream)
       (set-flag manip-stream 'adjust-field :right))))

(defun integer->string (n radix)
  (format nil "~vR" radix n))

(defun make-formatter (manip-output-stream)
  (concatenate
   'string
   "~"
   ;; mincol
   (format nil "~A," (width-of manip-output-stream))
   ;; colinc
   ","
   ;; minpad
   ",'"
   ;; padchar
   (string (fill-char-of manip-output-stream))
   ;; adjust
   (if (eq (adjust-field-of manip-output-stream) :left)
       "@"
       "")
   "A"))

(defmethod trivial-gray-streams:stream-write-string
    ((stream manip-output-stream) (seq stream-manipulator) &optional start end)
  (declare (ignore start end))
  (funcall (action-of seq) stream)
  stream)

(defmethod trivial-gray-streams:stream-write-string
    ((stream manip-output-stream) val &optional start end)
  (let ((fmt (make-formatter stream)))
    (format (stream-of stream) fmt val)
    (when (unit-buf-of stream)
      (force-output stream))
    (clear-flags stream)
    stream))


(defmethod trivial-gray-streams:stream-write-string
    ((stream manip-output-stream) (val integer) &optional start end)
  (let ((num-str (integer->string val (base-field-of stream))))
    (trivial-gray-streams:stream-write-string stream num-str start end)))


(defmacro << (manip-stream &rest args)
  (if args
      (let ((sym (gensym)))
`(let ((,sym ,manip-stream))
(write-string ,(car args) ,sym)
(<< ,sym ,@(cdr args))))
      manip-stream))



;; (<< (make-instance 'manip-output-stream :stream *standard-output*)
;; (set-fill #\-)
;; (set-width 10)
;; 12)
