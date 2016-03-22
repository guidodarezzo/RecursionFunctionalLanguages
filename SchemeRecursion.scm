
;; Problem 1

;; (fromto k n) returns a list with all integers from k (inclusive) and n
;; (inclusive). When k and n are equal, fromto will return n. The size of this
;; problem is abs(n-k).

;; Base Case: when k = n, the size of the problem is 0. Therefore, fromto will
;; return n.

;; Hypothesis: (fromto k n) will return (k k+1 k+2...n)

;; Recursive Step: (fromto k n) = (cons k (fromto (+ k 1) n))

(define (fromto k n)
    (cond ((> k n) '())
          (else (cons k (fromto (+ k 1) n)))))

;; Problem 2

;; (removeMults m L) returns a list without the multiples of m in list L, such
;; that any integer % m = 0 is removed from the list. The size of this problem
;; is the length of list L.

;; Base Case: if the list L's size = 0, the size of our problem will be 0 and an
;; empty list will be returned.

;; Hypothesis: (removeMults m (cdr L)) will return a list containing the elements of
;; (cdr L) without multiples of m.

;; Recursive Step: (removeMults m L) = (cons car L
;; (removeMults m (cdr L)))


(define (removeMults m L)
  (cond ((null? L) '())
        ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
        (else (cons (car L) (removeMults m (cdr L))))))


;;Problem 3

;; (removeAllMults L) returns a list without any multiples in list L, such
;; that any integer % any other integer = 0 is removed from the list. The size
;;of this problem is the length of list L.

;; Base case: if the list L's size = 0, the size of our problem will be 0 and an
;; empty list will be returned.

;; Hypothesis: (removeAllMults m (cdr L)) will return a list containing the elements of
;; (cdr L) without multiples of (car L).

;; Recursive Step: (removeAllMults L) = (cons (car L) (removeAllMults
;; (removeMults (car L) (cdr L))))



(define (removeAllMults L)
  (cond ((null? L) '())
        ((= (car L) 1) (removeAllMults (cdr L)))
        (else (cons (car L) (removeAllMults (removeMults (car L) (cdr L)))))))


;;Problem 4

;; (primes n) returns a list with all the prime integers in 2 through n, such
;; that any integer evenly divisble by another is removed from the list. The size
;; of this problem is n because we will use fromto to create a list
;; of size n. 

;; Base case: if n = 0 or 1, the size of our problem will be 0 and an
;; empty list will be returned.

;; Hypothesis: 
;; (primes (- n 1)) will return a list without prime numbers between 1 and n - 1.

;; Recursive Step: (primes n) = (removeAllMults (fromto 1 n)) = (cons (car (fromto 1 n))
;;(removeAllMults (removeMults (car (fromto 1 n)) (cdr (fromto 1 n))))

(define (primes n)
  (removeAllMults (fromto 1 n)))

;; Problem 5

;; (maxDepth L) returns the maximum nesting depth of list L. 

;; Base case: a list with no nesting, such as '(1 2 3) or an empty list
;; will return 0. The size of the problem is length of list L.

;; Hypothesis: (maxDepth (cdr L)) will return the maximum nesting depth of
;; (cdr L).

;; Recursive Step: (maxDepth L) = (max (+ 1 (maxDepth (car L)) (maxDepth (cdr L))))

(define (maxDepth L)
  (cond
    ((null? L) 0)
    ((not (pair? L)) (- 1))
    (else (max (+ 1 (maxDepth (car L))) (maxDepth (cdr L))))))

            

;; Problem 6

;; (prefix exp) returns a prefix version of the infix after conversion. Size of
;; the problem is the length of exp.

;; Base case: when exp is 1 element long, testprefix will return exp.

;; Hypothesis: (prefix (cddr exp)) will return a prefix expression of (cddr exp).

;; Recursive step: (prefix exp) = (cons (cadr exp) (list (prefix (car exp))
;; (prefix (cddr exp))))


(define (prefix exp)
  (cond
    ((not (list? exp)) exp)
    ((null? (cdr exp)) (car exp))
    (else
     (cons (cadr exp) (list (prefix (car exp)) (prefix (cddr exp)))))))


;; Problem 7

;; (composition fns) takes a composition of the list of functions (fns) and subsequently
;; returns the result of these functions. The size of the problem is the length of the list
;; of functions.

;; Base case: when fns is null, then there are no functions to execute, so it returns
;; the original value (in my program exp) without modifications.

;; Hypothesis: (composition (cdr fns)) returns the composition of functions in (cdr fns).

;; Recursive step: (composition fns) = (define (deconstruct fns exp) ((car fns)
;; (deconstruct (cdr fns) exp))) (lambda (exp) (deconstruct fns exp))

(define (composition fns)
  (define (deconstruct fns exp)
    (cond
      ((null? fns) exp)
      (else
       ((car fns) (deconstruct (cdr fns) exp)))))
    (lambda (exp) (deconstruct fns exp)))




  


