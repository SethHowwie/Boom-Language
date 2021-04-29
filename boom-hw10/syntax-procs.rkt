#lang racket
;; File Name: Syntax-procs.rkt
;; Author: Seth Howard
;; Description: Syntax procedures for the Boom language

(provide boom-exp?
         number-exp?
         unary?        make-unary       unary->arg     unary->op
         binary?       make-binary      binary->arg1   binary->op   binary->arg2
         let?          make-let         let->var       let->val     let->body
         varref?       make-varref      varref->val
         primitive?    zero             two            ten)

;;--------General Type Predicate---------

(define boom-exp?
  (lambda (arg)
    (or (varref? arg)
        (number? arg)
        (unary? arg)
        (binary? arg)
        (let? arg))))

;; ------- Numbers? -------
(define number-exp?
  (lambda (number-exp)
    (and (number? number-exp)
         (exact? number-exp))))


(define make-number identity)
(define number->val identity)


;; ------ Unary ------

(define unary?
  (lambda (exp)
    (if (number-exp? exp)
        #f
        (if (eq? (length exp) 2)
            (or (eq? (first exp) '-)
                (eq? (first exp) 'sq))
            #f))))
            

(define make-unary
  (lambda (op exp)
    (list op exp)))

(define unary->arg second)
(define unary->op first)

;; ------ Binary ------

(define binary?
  (lambda (exp)
    (if (number-exp? exp)
        #f
        (if (eq? (length exp) 3)
            (or (eq? (second exp) '+)
                (eq? (second exp) '-)
                (eq? (second exp) '*)
                (eq? (second exp) '/)
                (eq? (second exp) '%)
                (eq? (second exp) '@))
            #f))))

(define make-binary
  (lambda (arg1 op arg2)
    (list arg1 op arg2)))

(define binary->arg1 first)
(define binary->op second)
(define binary->arg2 third)

;; ------ Varrefs -------

(define varref?
  (lambda (s)
    (and (symbol? s)
         (not (keyword? s)))))

(define make-varref identity)
(define varref->val identity)

(define keyword?
  (lambda (s)
    (member s '(let in =))))

;; ------ Let --------

(define let?
  (lambda (env)
    (and (eq? (first env) 'let)
         (eq? (third env) '=)
         (eq? (fifth env) 'in))))

(define make-let
  (lambda (var exp exp2)
    (list 'let var '= exp 'in exp2)))

(define let->var
  (lambda (let-exp)
    (second let-exp)))

(define let->val
  (lambda (let-exp)
    (fourth let-exp)))

(define let->body
  (lambda (let-exp)
    (sixth let-exp)))

;; ------ primitives --------

(define primitive?
  (lambda (exp)
    (if (primitive-val? exp)
        #t
        #f)))


 (define primitive-val?
  (lambda (s)
    (member s '(two zero ten))))
(define zero 0)
(define two 2)
(define ten 10)








  
