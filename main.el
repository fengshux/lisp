(defun approximate (a b &optional gap)
  (or gap (setq gap 1.0e-6))
  (if (< (- (abs a) (abs b)) gap)
      t
    nil)
  )


(defun genpass (len &optional d a A @)
  "Generator password"
  (let ((pass "")
        (base "")
        (baselen 0)
        (dig "0123456789")
        (lo "abcdefghijklmnopqrstuvwxyz")
        (up "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (spe "+-*%/!@#$%^&:<>;?.,'")
        )
    (or @ (or A (or a  (or d (and (setq a t) (setq d t))))))
    (and a (setq base lo))
    (and d (setq base (concat base dig)))
    (and A (setq base (concat base up)))
    (and @ (setq base (concat base spe)))
    (setq baselen (length base))
    (while (> len 0)
      (setq pass (concat pass (string (aref base (random baselen)))))
      (setq len (1- len))
      )
    pass)
  )


(progn
  (generate-new-buffer "abc")
  (kill-buffer "abc")
  )

