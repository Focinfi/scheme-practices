; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf object)
  (cadr object))

(define (weight-leaf object)
  (caddr object))
; test leaf
(define leaf (make-leaf 'A 1))
(eq? (leaf? leaf) true)
(eq? (symbol-leaf leaf) 'A)
(eq? (weight-leaf leaf) 1)

; code-tree
(define (make-code-tree left right)
  (list 
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))
; test tree
(define tree (make-code-tree (make-leaf 'A 1) (make-leaf 'B 2)))
(eq? (leaf? (left-branch tree)) true)
(eq? (symbol-leaf (right-branch tree)) 'B)
(and (eq? (car (symbols tree)) 'a) 
  (eq? (cadr (symbols tree)) 'b))

; adjoin
(define (adjoin-set x set)
  (cond ((null? set)
      (list x))
    ((> (weight (car set)) (weight x))
      (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
        (make-leaf-set (cdr pairs))))))

(define (make-tree leaves)
  (cond ((or (null? (car leaves)) (null? (cadr leaves)))
      (error "leaves is not enough"))
    ((null? (cddr leaves))
      (make-code-tree (car leaves) (cadr leaves)))
    (else (make-tree (adjoin-set (make-code-tree (car leaves) (cadr leaves)) (cddr leaves))))))

; test make leaf set
(define leaf-set (make-leaf-set '((A 1) (B 3) (C 2))))
(and (eq? (caddar leaf-set) 1)
  (eq? (caddr (cadr leaf-set)) 2)
  (eq? (caddr (caddr leaf-set)) 3)
  (null? (cdddr leaf-set)))

; test make tree
(define tree (make-tree leaf-set))
(weight tree)
(eq? (weight tree) 6)

; encode
(define (encode tree)
  (define (visit n bits)
    (if (leaf? n)
      (cons (symbol-leaf n) bits)
      (cons (visit (left-branch n) (cons 0 bits))
        (visit (right-branch n) (cons 1 bits)))))
  (visit tree '()))

; test encode
(encode tree) ; outputs: ((b 0) (a 0 1) c 1 1)



