; chapter 1 learning note
; exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (>= (/ n d) 0)
      (cons (/ (abs n) g) (/ (abs d) g))
      (cons (/ (* -1 (abs n)) g) (/ (abs d) g)))))
(define numer car)
(define denom cdr)
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define neg-one-half (make-rat -1 2))
(define one-third (make-rat 1 -3))
(define neg-one-third (make-rat -1 3))
(print-rat (add-rat one-half one-third))
(print-rat (add-rat neg-one-half one-third))
(print-rat (make-rat 2 -6))

; exercise 2.2
(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p-1 p-2)
  (cons p-1 p-2))
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2) (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define sgmt-1 (make-segment (make-point 1 2) (make-point 2 3)))
(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 2 4))))

; exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
(define (append items1 items2)
  (if (null? items1)
    items2
    (append (cdr items1) (cons (car items1) items2))))
(define (last-pair items)
  (cond ((null? items)
      (error "empty list"))
    ((null? (cdr items)) ;递归终结条件
      items)
    (else
      (last-pair (cdr items))))) ;递归序列
(define (reverse items)
  (itr items '()))
(define (itr items res)
  (if (null? items)
    res
    (itr (cdr items) (cons (car items) res))))
(define l (list 1 2 3 4))
(define squarts (list 1 4 9 16))
;(length squarts)
(list-ref squarts 2)
(length squarts)
(append l squarts)
(last-pair l)
(last-pair squarts)
(reverse l)

;exercise 2.20
(define (same-parity sample . others)
  (filter (if (even? sample)
              even?
              odd?)
          (cons sample others)))

(same-parity 1 3 4 5)
