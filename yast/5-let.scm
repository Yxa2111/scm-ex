(let ((i 1) (j 2))
  (+ i j))
(let ((i 1))
  (let ((j (+ i 2)))
    (* i j) (- i j)))

(let* ((i 3) (j (+ i 2)))
  (* i j))
