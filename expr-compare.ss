;#lang racket

; combine two symbols (eg. a!b)
(define (combine-var a b)  
  (string->symbol(string-append (symbol->string a) "!" (symbol->string b)))
)

;check expr belongs to list
(define (keyword expr)
  (member expr '(cons if lambda let quote))
)

;replace l(list) elements with z if element equal to e
(define (replace z e l)
  (if (not(list? l))
      (if (equal? l e)
          z
          l
       )
      (if (empty? l)
          '()
          (if (not(equal? (car l) e))
              (cons  (car l) (replace z e (cdr l)))
              (cons z (replace z e (cdr l)))
           )
       )
   )
)

;;LET
;check each pair if y is l1 do num==0
(define (check-list-pair l1 l2 num)
  ;check l1 is empty l1 and l2 are same length
  (if (empty? l1)
      '()
      ;check if two are equal
      (if (not(equal? (car(car l1))(car(car l2))))
          ;to change the order of x and y if y is l1
          (if (equal? num 0)
              (let ((v1 (car (car l2)))(v2 (car (car l1))))
                 (let ((v (combine-var v1 v2))) (cons (cons v (cdr(car l1))) (check-list-pair (cdr l1)(cdr l2)num))))
              (let ((v (combine-var(car(car l1))(car(car l2))))) (cons (cons v (cdr(car l1))) (check-list-pair (cdr l1)(cdr l2) num)))
          )
          (cons (car l1) (check-list-pair (cdr l1)(cdr l2) num))
      )
   )
)

;new list with changed variable names
(define (new_l l1 l2 tail_l)
  (if (empty? l1)
      tail_l
      (if (not(equal? (car l1) (car l2)))
         (new_l (cdr l1) (cdr l2) (replace (car (car l1)) (car(car l2)) tail_l))
         (new_l (cdr l1) (cdr l2) tail_l)
       );eg. (car l1) == (a b)
   )      
)

(define (let-compare x y)
  ;(write(car x))
  (let ((x1 (check-list-pair (car x)(car y) 1)))
    (let ((y1 (check-list-pair (car y) (car x) 0)))
      (expr-compare (list x1 (new_l x1 (car x) (car(cdr x))))(list y1 (new_l y1 (car y)(car(cdr y)))))
    )
  )
)




;;LAMBDA

(define (inner-replace e1 e2 l)
  (if (empty? l)
      '()
      (if (equal? e2 (car(cdr(car l))))
          (cons (list (car(car l)) e1) (inner-replace e1 e2 (cdr l)))
          (cons (car l) (inner-replace e1 e2 (cdr l)))
       )
   )
)

;for inner-let if l1 and l2 are different change the value in l3
;l1: (a!b c d!e) l2:(a c d) l3: ((a a)(h d))   
(define (inner-let l1 l2 l3)
  (if (empty? l1)
      l3
      (if (equal? (car l1) (car l2))
          (inner-let (cdr l1) (cdr l2) l3)
          (inner-let (cdr l1) (cdr l2) (inner-replace (car l1) (car l2) l3))
       )
   )
)

;new lambda list creator
(define (lambda-new l1 l2 l)
      (if (empty? l1)
          l
          (if (not(equal? (car l1) (car l2)))
              (lambda-new (cdr l1) (cdr l2) (replace (car l1) (car l2)  l))   
              (lambda-new (cdr l1) (cdr l2) l)
           )
       )
)

;check lambda variables
(define (lambda-list l1 l2)
 (if (empty? l1)
     '()
     (if (not(equal? (car l1) (car l2)))
         (let ((v (combine-var (car l1)(car l2)))) (cons v (lambda-list (cdr l1)(cdr l2))))
         (cons (car l1) (lambda-list (cdr l1)(cdr l2)))
     )
  )
)

;compare lambda x and y
(define (lambda-compare x y)
  ;z1: first lambda list with new vaiable
  (let ((z1 (lambda-list (car x) (car y))))
    ;z2 and z3 are new-lambda list for x and y respectfully
    (let ((z2  (lambda-new z1 (car x)(car(cdr x)))))
     (let ((z3  (lambda-new z1 (car y)(car(cdr y)))))
     (expr-compare
         ;if inner let, fix the values accordingly
        (cons  z1 (if(and(list? z2) (equal? (car z2) 'let))
                    (cons 'let (cons(inner-let z1 (car x) (car(cdr z2))) (cdr(cdr z2))))
                     (list z2)
                   )
         )
        (cons z1 (if(and(list? z3) (equal? (car z3) 'let))                    
                   (cons 'let (cons(inner-let z1 (car y) (car(cdr z3))) (cdr(cdr z3))))
                    (list z3)
                  )
         )
       )
      )
    )
  )
)


(define (expr-compare x y)
 (if (equal? x y)
     ;if equal
     x
     ;check if list
     (if (and (list? x) (list? y))
         ;if list check length
         (if (equal? (length x) (length y))
             ;if list and same length, check if each belongs to keywords
             (if (not(and (keyword(car x)) (keyword (car y))))
                 ;check if one of them is keyword
                 (if (keyword(car x))
                    (list 'if '% x y)
                    (cons (expr-compare (car x)(car y)) (expr-compare(cdr x)(cdr y)))
                  )
                 ;if keywords match
                 (case (first x)
                   ('quote
                    (list 'if '% x y )
                    )
                   ('let
                    (cons 'let (let-compare (cdr x)(cdr y)))
                    )
                   ('lambda
                    (cons 'lambda (lambda-compare  (cdr x) (cdr y)))
                    )
                   (else  (cons (expr-compare(first x)(first y))(expr-compare(rest x)(rest y))))
                 )
             )
             ;if list but not same length
             (list  'if '% x y)
         )
         ;not a list check for boolean
         (if (and (boolean? x)(boolean? y))
             ;if not a list but boolean
             (if x
                 '%
                 '(not %)
             )
             ;if neither list nor boolean
             (list  'if '% x y)
          )
      )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;https://stackoverflow.com/questions/20778926/mysterious-racket-error-define-unbound-identifier-also-no-app-syntax-trans
;using eval helped from this website when using racket
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;replace % with #t or #f
(define (replace-bool bool l) 
    (if (empty? l)
        '()
        (if (list? (car l))
            (cons (replace-bool bool (car l)) (replace-bool bool (cdr l)))
            (if (eqv? '% (car l))
                (cons bool (replace-bool bool (cdr l)))
                (cons (car l) (replace-bool bool (cdr l)))
             )
        )
    )
)

(define (test-expr-compare x y)
  (if 
   (and
     (equal? (eval x ns)(eval(replace-bool #t (expr-compare x y)) ns))
     (equal? (eval y ns)(eval(replace-bool #f (expr-compare x y)) ns))
    )
    #t
    #f
  )  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;testing case:

(define test-expr-x 
  '((lambda (x y) (cons x y))
     (if (equal? #t #t)
         (quote (1 2 4))
         (list 3 6 7)
     )
     (cons (let ((a 2)) a) (list 1 2))
  )	
)


(define test-expr-y
  '((lambda (x y) (cons x y))
     (if (equal? #t #f)
         (quote (1 2 4))
         (list 3 6 7)
     )
     (cons (let ((b 2)) b) (list 5 6))
  )	
)
(test-expr-compare test-expr-x test-expr-y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-expr-compare '(cons 2 (quote (1 5))) '(cons 2 (quote (1 2))))
(test-expr-compare test-expr-x test-expr-y)
(expr-compare 12 12)  ;  12
(expr-compare 12 20)  ;  (if % 12 20)
(expr-compare #t #t)  ;  #t
(expr-compare #f #f)  ;  #f
(expr-compare #t #f)  ;  %
(expr-compare #f #t)  ;  (not %)
(expr-compare 'a '(cons a b))  ;  (if % a (cons a b))
(expr-compare '(cons a b) '(cons a b))  ;  (cons a b)
(expr-compare '(cons a b) '(cons a c))  ;  (cons a (if % b c))
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
  ; (cons (cons a (if % b c)) (cons (if % b a) c))
(expr-compare '(cons a b) '(list a b))  ;  ((if % cons list) a b)
(expr-compare '(list) '(list a))  ;  (if % (list) (list a))
(expr-compare ''(a b) ''(a c))  ;  (if % '(a b) '(a c))
(expr-compare '(quote (a b)) '(quote (a c)))  ;  (if % '(a b) '(a c))
(expr-compare '(quoth (a b)) '(quoth (a c)))  ;  (quoth (a (if % b c)))
(expr-compare '(if x y z) '(if x z z))  ;  (if x (if % y z) z)
(expr-compare '(if x y z) '(g x y z))
  ; (if % (if x y z) (g x y z))
(expr-compare '(let ((a 1)) (f a)) '(let ((a 2)) (g a)))
  ; (let ((a (if % 1 2))) ((if % f g) a))
1
(expr-compare '(let ((a c)) a) '(let ((b d)) b))
  ; (let ((a!b (if % c d))) a!b)//
(expr-compare ''(let ((a c)) a) ''(let ((b d)) b))
  ; (if % '(let ((a c)) a) '(let ((b d)) b))
(expr-compare '(+ #f (let ((a 1) (b 2)) (f a b)))
              '(+ #t (let ((a 1) (c 2)) (f a c))))
  ; (+
  ;   (not %)
   ;  (let ((a 1) (b!c 2)) (f a b!c)))///
(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
  ; ((lambda (a) ((if % f g) a)) (if % 1 2))
(expr-compare '((lambda (a b) (f a b)) 1 2)
              '((lambda (a b) (f b a)) 1 2))
  ; ((lambda (a b) (f (if % a b) (if % b a))) 1 2)
(expr-compare '((lambda (a b) (f a b)) 1 2)
              '((lambda (a c) (f c a)) 1 2))
  ; ((lambda (a b!c) (f (if % a b!c) (if % b!c a)))///
  ;   1 2)
(expr-compare '(let ((a (lambda (b a) (b a))))
                 (eq? a ((lambda (a b) (let ((a b) (b a)) (a b)))
                         a (lambda (a) a))))
              '(let ((a (lambda (a b) (a b))))
                 (eqv? a ((lambda (b a) (let ((a b) (b a)) (a b)))
                          a (lambda (b) a)))))
  ; (let ((a (lambda (b!a a!b) (b!a a!b))))////
  ;    ((if % eq? eqv?)
  ;     a
  ;     ((lambda (a!b b!a) (let ((a (if % b!a a!b)) (b (if % a!b b!a))) (a b)))
  ;      a (lambda (a!b) (if % a!b a))))

(expr-compare '((lambda (b a) (let ((a b) (b a)) a b ))1 2) '((lambda (a b) (let ((c b) (d a)) a b ))1 2))


(lambda-new '(b a) '(b a) '(let ((a b) (b a)) a b ))
