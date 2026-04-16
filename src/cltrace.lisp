(defpackage #:cltrace
  (:use :cl :rove))

(in-package #:cltrace)

(defun some-test (a b)
  (+ a b))

;; Control Globals
(defparameter *cltrace-lock* (bt:make-lock))
(defparameter *probe-registry* (make-hash-table :test #'equal))
(defparameter *programs* (make-hash-table))
(defparameter *installed-programs* ()
  "Installed programs is a list of currently installed programs.")

(defvar *args* nil "Special variable that holds the args when in a probe execution context")
(defvar *result* nil "Special variable that holds the return value when in a probe execution context")

(defclass probed-function (c2mop:funcallable-standard-object)
  ((function-sym :initarg :function-sym :reader probed-function-sym)
   (original :initarg :original :reader probed-function-original))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((pf probed-function) &key &allow-other-key)
  (with-accessors ((original probed-function-original)
                   (sym probed-function-sym)) pf
    (let* ((argnames (trivial-arguments:arglist original)))
      (c2mop:set-funcallable-instance-function pf
       (lambda (&rest args)
         (let ((*args* (loop for argname in argnames for argval in args collect (cons argname argval)))
               (count-probes-run 0))
           ;; TODO(Marce): Handle errors around this
           (incf count-probes-run (run-probes :entry sym))
           (let* ((result (apply original args))
                  (*result* result))
             ;; TODO(Marce): Handle errors around this
             (incf count-probes-run (run-probes :exit sym))
             ;; We count then lock to reduce the amount of times we lock, we use
             ;; the number of probes run as a heuristic
             (when (= count-probes-run 0)
               ;; If no probes fired, lock the registry to make sure we don't uninstall
               ;; ourselves while another thread installs itself
               (bt:with-lock-held (*cltrace-lock*)
                 (let ((num-probes (get-count-of-probes-for-fn sym)))
                   (when (= num-probes 0)
                     (format t "Uninstalling from ~a~%" sym)
                     ;; Restore original function
                     (setf (symbol-function sym) original)))))
             result)))))))

(defun unwrap-probed-fn (fn-sym)
  (let ((fn (symbol-function fn-sym)))
    (when (probed-p fn)
      (setf (symbol-function fn-sym)
            (probed-function-original fn)))))

(defun probed-p (fn)
  (typep fn 'probed-function))

(defun make-probe-key (where fn-sym)
  `(,where ,fn-sym))

(defun run-probes (where fn-sym)
  "Run all registered probes for fn-sym at where. Returns the number of probes found."
  (format t "Function ~a called with ~a (returning ~a)~%" fn-sym *args* *result*)
  (let ((key (make-probe-key where fn-sym))
        (num-probes 0))
    (dolist (p *installed-programs*)
      (with-accessors ((probes program-probes))
          p
        (multiple-value-bind (probe present-p)
            (gethash key probes)
          (when present-p
            (execute probe)
            (incf num-probes)))))
    num-probes))

(defun get-count-of-probes-for-fn (fn-sym)
  (let ((num-probes 0)
        (entry-key (make-probe-key :entry fn-sym))
        (exit-key (make-probe-key :exit fn-sym)))
    (dolist (p *installed-programs*)
      (with-accessors ((probes program-probes))
          p
        (multiple-value-bind (entry-probe entry-present-p)
            (gethash entry-key probes)
          (declare (ignore entry-probe))
          (multiple-value-bind (exit-probe exit-present-p)
              (gethash exit-key probes)
            (declare (ignore exit-probe))
            (when entry-present-p (incf num-probes))
            (when exit-present-p (incf num-probes))))))
    num-probes))
  
(defun wrap-function-for-probing (sym)
  "Wraps a function for tracing purposes, if no probes run before or after we uninstall ourselves"
  (let* ((original (symbol-function sym)))
    (unless (probed-p original)
      (let ((wrapper (make-instance 'probed-function :original original :function-sym sym)))
        (setf (symbol-function sym) wrapper)))))

(defclass probe ()
  ((fn-sym :initarg :fn-sym :accessor probe-fn-sym)
   (where :initarg :where :accessor probe-where)
   (when :initarg :when :accessor probe-when)
   (what :initarg :what :accessor probe-what)))

(defmethod install-probe ((p probe))
  ())

(defmethod execute ((probe probe))
  (with-accessors ((pwhen probe-when)
                   (pwhat probe-what))
      probe
    (when (funcall pwhen)
      (funcall pwhat))))

(defclass program ()
  ((name :initarg :name :accessor name)
   (probes :initform (make-hash-table :test #'equal) :accessor program-probes)))

(defun get-or-create-program (name)
  "Get or create a program instance"
  (let ((program (gethash name *programs*)))
    (if (null program)
        (setf (gethash name *programs*) (make-instance 'program :name name))
        program)))

(defun is-valid-when-expr-fun (fn)
  (member fn '(+ - * / and or = eq eql equal)))

(defun compile-when-funcall (when-expr)
  (destructuring-bind (fn &rest args)
      when-expr
    (if (is-valid-when-expr-fun fn)
        (let ((args (mapcar #'compile-when-expr args)))
          `(,fn ,@args)))))

(defun get-arg (name)
  (cdr (assoc name *args*)))

(defun compile-when-expr (when-expr)
  (cond
    ((consp when-expr) (compile-when-funcall when-expr))
    ((symbolp when-expr) `(get-arg ',when-expr))
    ((numberp when-expr) when-expr)
    (t (error "Invalid ~a in when expression" when-expr))))

(defun compile-when-into-lambda (when-expr)
  "Compiles a when expression into a lambda that can later be executed as a probe guard.

When expressions are limited in what they can do, any symbol is extracted from "
  (let ((when-cl-expr (compile-when-expr when-expr)))
    (compile nil `(lambda () ,when-cl-expr))))

(defun compile-body-expr (body-expr)
  (cond
    ((consp body-expr)
     (case (car body-expr)
       ((print) `(format t ,@(mapcar #'compile-body-expr (cdr body-expr))))
       (t (error "Unknown function in body expression"))))
    ((symbolp body-expr) `(get-arg ',body-expr))
    ;; Cannot be a symbol
    ((atom body-expr) body-expr)
    (t (error "Unkown expression"))))

(defun compile-body-exprs (body-exprs)
  (mapcar #'compile-body-expr body-exprs))

(defun compile-body-into-lambda (body-exprs)
  "Compiles a body expression into a lambda that can later be executed when a probe fires."
  (let ((body-cl-expr (compile-body-exprs body-exprs)))
    (compile nil `(lambda () ,@body-cl-expr))))

(defun parse-probe (probe-def)
  (let ((header (car probe-def))
        (body (cdr probe-def)))
    (destructuring-bind (where fn-sym &key when)
        header
      (let ((when-lambda (compile-when-into-lambda when))
            (body-lambda (compile-body-into-lambda body)))
        (make-instance 'probe
                       :where where
                       :fn-sym fn-sym
                       :when when-lambda
                       :what body-lambda)))))

(defun %define-program (name body)
  (bt:with-lock-held (*cltrace-lock*)
    (let* ((program (get-or-create-program name))
           (parsed-probes (mapcar #'parse-probe body))
           (probes (alexandria:plist-hash-table
                    (loop for p in parsed-probes
                          nconc (list (make-probe-key
                                       (probe-where p)
                                       (probe-fn-sym p))
                                      p))
                    :test #'equal)))
      (uninstall-program name)
      (install program probes)
      nil)))

(defmacro define-program (name &body body)
  "Define a cltrace program. This reinstalls the program with the new definition."
  (%define-program name body))

(defun hash-keys-difference (a b)
  "Keys a - Keys b"
  (let ((a-keys (alexandria:hash-table-keys a))
        (b-keys (alexandria:hash-table-keys b))
        (test-fn (hash-table-test b)))
    (remove-if
     (lambda (k)
       (member k b-keys :test test-fn))
     a-keys)))

(defmethod install ((p program) probes)
  (with-accessors ((existing-probes program-probes))
      p
    (let* ((functions-keys (alexandria:hash-table-keys probes))
           (function-syms (mapcar #'second functions-keys)))
      ;; Setup functions for triggering probes
      (dolist (f function-syms)
        (wrap-function-for-probing f))
      (setf (program-probes p) probes)
      (push p *installed-programs*))))

(defun installed-programs ()
  "Returns list of installed programs in the image"
  (mapcar #'name *installed-programs*))

(defun uninstall-program (name)
  "Uninstalls a program.

Probes will no longer trigger probes from that program."
  (setf *installed-programs*
        (remove-if
         (lambda (p)
           (eq (name p) name))
         *installed-programs*)))

(define-program things
  ((:entry some-test)
   (print "~a" a)))

(deftest when-expr
  (testing "compile-when-expr"
    (let ((compiled-expr (compile-when-expr '(= a 2))))
      (ok (equal compiled-expr '(= (get-arg 'a) 2)))))
  (testing "compile-when-into-lambda"
    (let ((when-lambda (compile-when-into-lambda '(= a 2)))
          (*args* '((a . 2))))
      (ok (eq (funcall when-lambda) t)))))

(deftest body-expr
  (testing "print"
    (ok (equal
         (compile-body-expr '(print "~a" a))
         '(format t "~a" (get-arg 'a)))))
  (testing "compile to lambda"
    (let ((body-lambda (compile-body-into-lambda '((print "~a" a))))
          (*args* '((a . 2))))
      (ok (string= (funcall body-lambda) "2")))))

(deftest parse-probe
  (testing "parse-probe"
    (let ((probe (parse-probe '((:entry some-test :when (= a 2))
                                (print "~a" a)))))
      (with-accessors ((pwhen probe-when)
                       (pwhere probe-where)
                       (pwhat probe-what)
                       (fn-sym probe-fn-sym))
          probe
        (ok (eq fn-sym 'some-test))
        (ok (eq pwhere :entry))
        (ok (functionp pwhen))
        (ok (functionp pwhat))
        (let ((*args* '((a . 2) (b . 4))))
          (ok (eq (funcall pwhen) t))
          (ok (string= (funcall pwhat) "2")))
        (let ((*args* '((a . 3) (b . 12))))
          (ok (eq (funcall pwhen) nil))
          (ok (string= (funcall pwhat) "3")))))))
