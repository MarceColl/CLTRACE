(asdf:defsystem #:cltrace
  :description "A Dtrace inspired observability framework for live Lisp systems"
  :depends-on (:trivial-arguments :bordeaux-threads :rove)
  :components ((:module "src"
                :components
                ((:file "cltrace")))))
