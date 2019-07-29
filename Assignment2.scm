#lang scheme
;;Leong Mary 15360789
;;Written and ran in DrRacket version 7.3 R6RS. Downloaded DrRacket from racket-lang.org

(display "please enter a credit card number with no spaces.")
(newline)
(define val (read-line))
(newline)
(display "Number entered : " )


;;convert string number to int number
(string->number val)

;; function to convert int number to a list
(define (num->list num)
  (if (< num 10)
      (list num)
      (append (num->list (floor (/ num 10)))
              (list (- num (* 10 (floor (/ num 10))))))))

;;creates new list for cc #
(define numlist (num->list (string->number val)))


;;function to determine if cc # is valid (13 <= # of digits <= 19)
(define (valid-num? numlist)
            (if (and (>= (length numlist) 13) (<= (length numlist) 19)) #t #f))

;remove to show if num of digits is valid
;(valid-num? numlist)

;;function to determine if # of digits in cc# is even
(define (even-num? numlist)
           (if (= 0 (modulo (length numlist) 2)) #t #f))


;remove to show id list has even num of digits
;(even-num? numlist)





;;function name: multiple-even --> multiples every other number in credit car by 2 starting from 2nd to last digit.
;; takes in length of credit card number and the list of numbers in reverse(starting from end)
;;if the number of digits is even, length of card % 2 = 0 will remain the same and alternate numbers are multiplied by 2
(define multiple-even (lambda (x y)
                   (if (null? y) '()
                       (if (= 1  (modulo x 2))
                           (cons (* (car y ) 2) ( multiple-even (- x 1) (cdr y)))
                           (cons (car y) ( multiple-even (- x 1) (cdr y)))))))

;;function name: multiple-odd --> multiples every other number in credit car by 2 starting from 2nd to last digit.
;; takes in length of credit card number and the list of numbers in reverse(starting from end)
;;if the number of digits is even, length of card % 2 = 1 will remain the same and alternate numbers are multiplied by 2
(define multiple-odd (lambda (x y)
                   (if (null? y) '()
                       (if (= 0  (modulo x 2))
                           (cons (* (car y ) 2) ( multiple-odd (- x 1) (cdr y)))
                           (cons (car y) ( multiple-odd (- x 1) (cdr y)))))))




;;adds all 2 digits numbers together to form 1 digit
(define (addition listn) (map (lambda (x)
            (if (>= x 10)
                (+ (modulo x 10) 1)
                 x)) listn)  )

;;adds all numbers in credit card together
(define (totals listn)
         (if (null? listn) 0
         (+ (car listn) (totals (cdr listn)))
           ))

;;tells you if the card number is valid. It is valid if total is divisible by 10
(define (valid? total)
         (if (= 0 (modulo total 10)) #t #f))


(newline)
(display "credit card number you entered is ")


;checks if number entered is a valid cc#
(if (and (valid-num? numlist) (even-num? numlist))

    (valid? (totals (addition (reverse (multiple-even (length numlist) (reverse numlist))) )))
    ( if (and (valid-num? numlist) (not (even-num? numlist) ))
         (valid? (totals (addition (reverse (multiple-odd (length numlist) (reverse numlist))))))
         (display "number not valid")))


(display "#t = valid #f = not valid")
(newline)

;;remove to show numbers doubled
;;(display "every other number doubled starting from last number: ")
;;(if (and (valid-num? numlist) (even-num? numlist))
;;(reverse (multiple-even (length numlist) (reverse numlist)))
;;( if (and (valid-num? numlist) (not (even-num? numlist) ))
;;    (reverse (multiple-odd (length numlist) (reverse numlist)))
;;     null))
