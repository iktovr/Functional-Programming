; 1.45

(defun cube (x)
    (* x x x))

(defun square (x)
    (* x x))

(defun improve (guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defconstant eps 0.0001)

(defun good-enough-p (guess x)
    (< (abs (- (cube guess) x)) eps))

(defun cuberoot-iter (guess x)
    (if (good-enough-p guess x)
        guess
        (cuberoot-iter (improve guess x) x)))

(defun cuberoot (x)
    (cuberoot-iter 1.0 x))
