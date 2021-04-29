#lang racket
;; File Name: Interpreter.rkt
;; Author: Seth Howard
;; Description: The interpretor part of the Boom language

(require "syntax-procs.rkt")
(require "datatypes.rkt")
(provide preprocess
         eval-exp)

(define preprocess
  (lambda (sugared-exp)
    (cond
          ((number-exp? sugared-exp)
           sugared-exp)
          ((varref? sugared-exp)
           sugared-exp)
          ((binary? sugared-exp)
           (if (eq? (binary->op sugared-exp) '@)
               (make-binary (make-binary(preprocess (binary->arg1 sugared-exp))
                                        '+
                                        (preprocess (binary->arg2 sugared-exp)))
                            '/
                            2)
               (make-binary (preprocess (binary->arg1 sugared-exp))
                            (binary->op sugared-exp)
                            (preprocess (binary->arg2 sugared-exp)))))
          ((unary? sugared-exp)
           (if (eq? (unary->op sugared-exp) 'sq)
               (make-binary (preprocess (unary->arg sugared-exp))
                            '*
                            (preprocess (unary->arg sugared-exp)))
               (make-unary (unary->op sugared-exp)
                           (preprocess (unary->arg sugared-exp)))))
          ((let? sugared-exp)
           (make-let (preprocess (let->var sugared-exp))
                     (preprocess (let->val sugared-exp))
                     (preprocess (let->body sugared-exp)))))))

(define eval-exp
  (lambda (exp)
    (cond ((boom-exp? exp)
           (eval-help (preprocess exp) (initial-env)))
          (else (error 'boom "Illegal Expression ~a" exp)))))


(define initial-env
  (lambda ()
    (bind 'zero 0 (bind 'two 2 (bind 'ten 10 (make-bindings))))))

(define eval-help
  (lambda (boom-exp env)
    (cond ((varref? boom-exp)
           (if (var-exists? boom-exp env)
               (look-up boom-exp env)
               (error 'eval-help "undefined variable ~a" boom-exp)))
          ((number-exp? boom-exp) boom-exp)
          ((let? boom-exp)
           (eval-help (let->body boom-exp)
           (bind (let->var boom-exp)
                 (eval-help (let->val boom-exp) env)
                 env)))
                              
          ((number-exp? boom-exp) boom-exp)
          ((unary? boom-exp)
           (cond ((eq? (unary->op boom-exp) '-)
                 (* (eval-help (unary->arg boom-exp) env) -1))))
          ((binary? boom-exp)
           (let ((arg1 (eval-help (binary->arg1 boom-exp) env))
                 (arg2 (eval-help (binary->arg2 boom-exp) env)))
           (cond ((eq? (binary->op boom-exp) '+)
                  (+ arg1 arg2))
                 ((eq? (binary->op boom-exp) '-)
                  (- arg1 arg2))
                 ((eq? (binary->op boom-exp) '%)
                  (remainder arg1 arg2))
                 ((eq? (binary->op boom-exp) '*)
                  (* arg1 arg2))
                 ((eq? (binary->op boom-exp) '/)
                  (quotient arg1 arg2))))))))
                                     
;(define eval-let
;  (lambda (let-exp bindings)
;   (begin
;     (bind (let->var let-exp) (eval-exp (list (look-up (first (let->val let-exp)) bindings) (second (let->val let-exp))
;                                              (look-up (third (let->val let-exp)) bindings))) bindings)
;     ;; I couldn't quite finish this one. Or the next clearly, I do intend to try and finish just ran out of time. 
;     ))) Irrelevant


(define run-boom
  (lambda ()
    (display "Welcome to my implementation of the boom language.")
    (display "Insert a boom expression: ")
    (let ((exp (read)))
      (if (boom-exp? exp)
          (write (eval-exp exp))
          (begin
            (display "Sorry, that isn't a valid boom expression. Try again.")
            (run-boom))))))
    
    ;; Also couldn't get this one, I couldn't get the idea of checking the input to be a valid boom-exp before
    ;; passing it to eval-exp. 
                 
                  