(defun moves (coins)
  (remove-if (lambda (v) (> v coins))
             '(1 2 3)))

(defun end-gamep (coins)
  (= coins 0))

(defun utility (coins)
  (if (end-gamep coins)
      1
      0))

(defun best-value (coins)
  (cond
    ((end-gamep coins) (utility coins))
    (t
     (let ()
       (loop for move in (moves coins)
          do (let ((next-state (- coins move)))
               (se