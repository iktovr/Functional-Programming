; 2.36

(defun reverse-iter (a b)
        (if (null a)
                b
                (reverse-iter (rest a) (cons (first a) b))))

(defun myreverse (a)
        (reverse-iter a NIL))

(defun decrease-p (l)
        (if (null (rest l))
                T
                (if (> (first l) (first (rest l))) 
                        (decrease-p (rest l))
                        NIL)))

(defun decreasing (l)
        (if (decrease-p l)
                l
                (myreverse l)))