(defprocess account
  ((balance))

  ;; When the process starts, set balance to 0
  (init () (= (balance self) 100))

  (transfer
   (to amount)
   :check-balance
   (>= balance amount)
   :do-transfer
   (~> (balance self) (- (balance self) amount))
   (~> (balance to) (+ (balance to) amount))))


(defspec cltrace
  :processes ((alice account)
              (bob account)))
  


(defstep init ()
  (and (= alice 0)
       (= bob 0)))

  
  

