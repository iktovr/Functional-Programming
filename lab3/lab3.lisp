; 3.45

(defun fill-side-diag (matr j i m n idx up)
    (if (< j (min (+ m 1) (- (* 2 n) m 1)))
        (progn
            (setf (aref matr i (- m i)) idx)
            (fill-side-diag matr (+ j 1) (+ i (if up -1 1)) m n (+ idx 1) up))
        idx))

(defun fill-matrix (matr i m n idx up)
    (if (< m (- (* 2 n) 1))
        (let
            ((next-i 
                (let 
                    ((next-i (+ i (* (min m (- (* 2 n) m 2)) (if up -1 1)))))
                    (+ next-i 
                       (if (and (/= next-i (- n 1)) 
                                (or (= (- m next-i) 0) 
                                    (= (- m next-i) (- n 1)))) 
                           1 0))))
            (next-idx (fill-side-diag matr 0 i m n idx up)))
            (fill-matrix matr next-i (+ m 1) n next-idx (not up)))
        matr))

(defun matrix-1l-2l (n)
    (fill-matrix (make-array (list n n)) 0 0 n 1 NIL))

(defun print-matrix (matrix &optional (chars 3) stream)
    (let ((*print-right-margin* (+ 6 (* (1+ chars)
                                       (array-dimension matrix 1)))))
        (pprint matrix stream)
        (values)))