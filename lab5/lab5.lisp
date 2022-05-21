; 5.43

(defun make-term (&key order coeff)
    (list order coeff))

(defun order (term) (first term))
(defun coeff (term) (second term))

(defclass polynom ()
    ((var-symbol :initarg :var :reader var)
    (term-list :initarg :terms :reader terms)))

(defgeneric zerop1 (arg)
    (:method ((n number))
        (zerop n)))

(defgeneric minusp1 (arg)
    (:method ((n number))
        (minusp n)))

(defmethod print-object ((p polynom) stream)
    (format stream "[МЧ (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
        (var p)
        (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var p)
                          (order term)))
            (terms p))))

(defun der-terms (terms)
    (loop for x in terms
        when (> (order x) 0)
        collect (make-term 
                    :order (1- (order x)) 
                    :coeff (* (coeff x) (order x)))))

(defun der-polynom (p)
    (make-instance 'polynom
        :var (var p)
        :terms (der-terms (terms p))))
