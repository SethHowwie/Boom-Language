#lang racket

;; File Name: datatypes.rkt
;; Author: Seth Howard
;; Description: Data types extending the boom language.

(provide bind
         make-bindings
         look-up
         var-exists?)


(define make-bindings
  (lambda ()
    (list)))

(define bind
  (lambda (var val make-bindings)
       (append make-bindings (list (cons var val)))))

(define look-up
  (lambda (var env)
    (if (var-exists? var env)
        (cdr (assoc var env))
        (error 'look-up "undefined variable ~a" var))))


(define var-exists?
  (lambda (var env)
    (if (null? env)
        #f
        (if (eq? (car (first env)) var)
                 #t
                 (var-exists? var (rest env))))))

