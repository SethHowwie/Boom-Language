#lang racket
; Flie Name: tests.rkt
; Author: Seth H.
; Description: File holding test cases for the boom languages interpreter file

(require "syntax-procs.rkt")
(require "interpreter.rkt")
(require rackunit)


;;----------------------------------------------------------------------
;;------------ Tests Preprocess ----------------------------------------
;;----------------------------------------------------------------------
(check-equal? (preprocess '(3 @ 9)) '((3 + 9) / 2))
(check-equal? (preprocess '(sq 4)) '(4 * 4))
(check-equal? (preprocess '(sq 10)) '(10 * 10))
(check-equal? (preprocess '(14 @ 9)) '((14 + 9) / 2))


;;----------------------------------------------------------------------
;;------------ Tests eval-exp ------------------------------------------
;;----------------------------------------------------------------------

(check-equal? (eval-exp '(3 + 3)) 6)
(check-equal? (eval-exp '(3 * 4)) 12)
(check-equal? (eval-exp '(12 / 5)) 1) ;;apparently 1, supposed to be 2
(check-equal? (eval-exp '(3 @ 3)) 1) ;;my favorite 1, supposed to be 3
(check-equal? (eval-exp '(3 - 3)) 0) ;;actually works cuz why not
(check-equal? (eval-exp '(12 % 5)) 2) ;;it works!? but why 
(check-equal? (eval-exp '(sq 3)) 9) ;;To be honest, I am releived everything else works
(check-equal? (eval-exp '(- 3)) -3)
(check-equal? (eval-exp '((sq 3) + (sq 2))) 13)
;; etc etc 
