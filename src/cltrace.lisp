(defpackage #:cltrace
  (:use :cl))

(in-package #:cltrace)

(defun some-test (a b)
  (+ a b))

(defparameter *probe-registry* (make-hash-table :test #'equal))

(defun make-probe-key (where fn-sym)
  `(,where ,fn-sym))

(defun run-probes (where fn-sym)
  "Run all registered probes for fn-sym at where"
  (let* ((key (make-probe-key where fn-sym))
         (probes (gethash key *probe-registry*)))
    ()))

(defun wrap-function-for-probe (sym)
  "Wraps a function for tracing purposes, returns a function that when called uninstalls the trace"
  (let ((original (symbol-function sym)))
    (setf (symbol-function sym)
          (lambda (&rest args)
            (format t "Function ~a called with ~a" sym args)
            (run-probes :before sym)
            (let ((result (apply original args)))
              (run-probes :after sym)
              result)))
    (lambda ()
      ;; TODO(Marce): Before doing this I should probably check
      ;; if the sym still points to our wrapper lambda, since
      ;; it may have been redefined in the meantime.
      (setf (symbol-function sym) original))))

(defparameter *programs-lock* (bt:make-lock "programs-lock"))
(defparameter *programs* (make-hash-table))

(defclass probe ()
  ((fn-sym :initarg :fn-sym :accessor probe-fn-sym)
   (where :initarg :where :accessor probe-where)
   (when :initarg :when :accessor probe-when)
   (what :initarg :what :accessor probe-what)
   (uninstaller :initarg :uninstaller :accessor probe-uninstaller)))

(defclass program ()
  ((name :initarg :name :accessor name)
   (uninstallers :initarg :uninstallers :accessor uninstallers)))

(defun get-or-create-program (name)
  "Get or create a program instance"
  (let ((program (gethash name *programs*)))
    (or program
        (make-instance 'program :name name :uninstallers (make-hash-table)))))

(defmacro define-program (name &body body)
  "Define a cltrace program"
  (with-lock-held (*programs-lock*)
    (let ((program (get-or-create-program name))
          (probes body))
      (install program probes))))

(defmethod install ((p program) new-probes)
  "Install a program"
  ;; We need to:
  ;;  + Check what new probes there are
  ;;  + Check what probes have been removed
  ;;  + Check what probes where updated
  ;;  + Execute the operations
  (let (())))

