(defun approximate (a b &optional gap)
  (or gap (setq gap 1.0e-6))
  (if (< (- (abs a) (abs b)) gap)
      t
    nil)
  )


(defun genpass (len &optional d l u s)
  "Password generator"
  (let ((pass "")
        (base "")
        (baselen 0)
        (dig "0123456789")
        (lo "abcdefghijklmnopqrstuvwxyz")
        (up "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (spe "+-*%/!@#$%^&:<>;?.,'")
        )
    (or s (or u (or l  (or d (and (setq l t) (setq d t))))))
    (and l (setq base lo))
    (and d (setq base (concat base dig)))
    (and u (setq base (concat base up)))
    (and s (setq base (concat base spe)))
    (setq baselen (length base))
    (while (> len 0)
      (setq pass (concat pass (string (aref base (random baselen)))))
      (setq len (1- len))
      )
    pass)
  )







