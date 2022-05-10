; 4.38

(defvar *digits* #("один" "два" "три" "четыре" "пять" "шесть" "семь" "восемь" "девять"))
(defvar *digits-for-thousand* #("одна" "две"))
(defvar *two-digits* #("одиннадцать" "двенадцать" "тринадцать" "четырнадцать" "пятнадцать" "шестнадцать" "семнадцать" "восемнадцать" "девятнадцать"))
(defvar *tens* #("десять" "двадцать" "тридцать" "сорок" "пятьдесят" "шестьдесят" "семьдесять" "восемьдесят" "девяносто"))
(defvar *hundreds* #("сто" "двести" "триста" "четыреста" "пятьсот" "шестьсот" "семьсот" "восемьсот" "девятьсот"))
(defvar *case* #(#("тысяча" "тысячи" "тысяч") #("миллион" "миллиона" "миллионов")))

(defun three-digits-sentence (num pos)
    (let ((res NIL))
        (if (= num 0)
            res
            (let 
                ((digit (mod num 10))
                (ten (mod (floor num 10) 10))
                (hundred (mod (floor num 100) 10)))

                (if (/= hundred 0) ; 100 - 900
                    (setq res (concatenate 'string 
                                  res 
                                  (svref *hundreds* (1- hundred)) 
                                  " ")))

                (if (and (= ten 1) (/= digit 0)) ; 11 - 19
                    (setq res (concatenate 'string 
                                  res 
                                  (svref *two-digits* (1- digit)) 
                                  " "))
                    (progn 
                        (if (/= ten 0) ; 10 - 90
                            (setq res (concatenate 'string 
                                          res 
                                          (svref *tens* (1- ten)) 
                                          " ")))
                        (cond
                            ((and (/= digit 0) (or (/= pos 1) (> digit 2))) ; 1 - 9
                                (setq res (concatenate 'string 
                                              res 
                                              (svref *digits* (1- digit)) 
                                              " ")))
                            ((/= digit 0) ; 1 - 2 thousand
                                (setq res (concatenate 'string 
                                              res 
                                              (svref *digits-for-thousand* 
                                                     (1- digit)) 
                                              " "))))))
                
                (if (> pos 0)
                    (cond
                        ((or (= digit 0) (= ten 1) (> digit 4)) 
                            (setq res (concatenate 'string 
                                          res 
                                          (svref (svref *case* (1- pos)) 2) 
                                          " ")))
                        ((= digit 1) 
                            (setq res (concatenate 'string 
                                          res 
                                          (svref (svref *case* (1- pos)) 0) 
                                          " ")))
                        (T 
                            (setq res (concatenate 'string 
                                          res 
                                          (svref (svref *case* (1- pos)) 1) 
                                          " ")))))
                res))))

(defun number-sentence-iter (num pos res)
    (if (= num 0)
        res
        (number-sentence-iter
            (floor num 1000)
            (1+ pos)
            (concatenate 'string
                (three-digits-sentence (mod num 1000) pos)
                res))))

(defun number-sentence (num)
    (string-right-trim " " (number-sentence-iter num 0 NIL)))