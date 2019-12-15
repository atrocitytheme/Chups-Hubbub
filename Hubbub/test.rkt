#lang racket #| ★ CSC324 Fall 2019: Assignment 1 Sample Tests ★ |#
#|
Module: hubbub_test_sample
Description: Sample Tests for Assignment 1
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019

Warning: as usual, these sample tests are very incomplete, and are meant to
give you a sense of the test structure we'll use, but NOT to verify the
complete correctness of your work on this assignment! Please add your own
tests here
|#

(require rackunit)
(require "hubbub.rkt")
(require "hubbub_errors.rkt")


(module+ test
  #;(test-equal? "Numeric literal"
                 (run-interpreter '(30))
                 30)

  #;(test-equal? "Multiple independent defines"
                 (run-interpreter '((define a 1)
                                    (define b #t)
                                    (define c #f)
                                    b))
                 #t)

  #;(test-exn "Identifier with unused define (unbound-name error)"
              (regexp (format (hash-ref error-strings 'unbound-name) 'b))
              (thunk (run-interpreter '((define a 10)
                                        b))))

  #;(test-equal? "Simple +"
                 (run-interpreter '((+ 30 40)))
                 70)

  #;(test-equal? "Unary function call"
                 (run-interpreter '(((lambda (x) (+ x 1)) 1)))
                 2)

  #;(test-equal? "make-adder (like lecture)"
                 (run-interpreter '((define make-adder
                                      (lambda (n)
                                        (lambda (m)
                                          (+ n m))))
                                    (define add-one (make-adder 1))
                                    (define add-two (make-adder 2))
                                    (+ (add-one 5) (add-two 10))))
                 ; We write out explicitly the computation produced using
                 ; correct substitution.
                 (+ (+ 1 5) (+ 2 10)))

  #;(test-equal? "Contract: (integer? -> boolean?), valid call"
                 (run-interpreter '((define f (lambda (x) (< x 3)))
                                    (define-contract f (integer? -> boolean?))
                                    (f 1)))
                 #t))
