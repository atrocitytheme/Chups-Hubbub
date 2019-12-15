#lang racket #| ★ CSC324 Fall 2019: Assignment 1 ★ |#
#|
Module: hubbub_errors
Description: Assignment 1: Building an Interpreter
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2019
The assignment handout can be found at
https://www.cs.toronto.edu/~david/csc324/assignments/a1/handout.html
|#
(provide run-interpreter)

(require racket/hash)  ; You may use the functions imported from this module.
(require "hubbub_errors.rkt")


;-----------------------------------------------------------------------------------------
; Main functions (skeleton provided in starter code)
;-----------------------------------------------------------------------------------------
#|
(run-interpreter prog) -> any
  prog: datum?
    A syntactically-valid Hubbub program.
  Evaluates the Hubbub program and returns its value, or raises an error if the program is
  not semantically valid.
|#
(define (run-interpreter prog)
  (let ([env (build-env prog)]
        [last-expr (last prog)])
    (interpret env last-expr)))

#|
(interpret env expr) -> any
  env: hash?
    The environment with which to evaluate the expression.
  expr: datum?
    A syntactically-valid Hubbub expression.
  Returns the value of the Hubbub expression under the given environment.
|#
(define (interpret env expr)
  (match expr
    ;If expr is boolean or number, then evaluate directly
    [(? number?) expr]
    [(? boolean?) expr]
    
    ;If expr is ID, then evaluate its value by searching environment
    [(? symbol?) (if (hash-has-key? env expr)
                     ;A symbol can refer to a expr that save in environment, can interpret this expression recursively
                     (interpret env (hash-ref env expr))
                     ;When symbol is not in hash, unbound-name error
                     (report-error 'unbound-name expr))]
    
    ;If expr is lambda function, Evaluate lambda and create clousure,
    ;if there is a duplicate in parameters, raise an error
    ;notice check-dup only return true or raise an error, so else is never reached, similar to check-rtype and check-contract
    [(list 'lambda params body) (if (check-dup params)
                                    (closure params body env null)
                                    (void))]
    
    ;If expr is a function call, then evaluate function call
    [(list func params ...) (if (check-procedure env func)
                                (if (check-contract env expr)
                                    (let([return-v (evaluate-function env func params)]
                                         )
                                      (if (check-rtype env expr return-v)
                                          return-v
                                          (void)))
                                    (void)
                                    )
                                (report-error 'unbound-name func))]
    )
  )

;----------------------------------------------------------------------------------------
; Self-defined Helpers
;----------------------------------------------------------------------------------------

#|
(evaluate-function env func params) -> any
Evaluate the function
|#
(define (evaluate-function env func params)
  
  (match func
    ;If function is a name binding
    [(? symbol?) (if (hash-has-key? env func)
                     (let* ([func-closure (hash-ref env func)]
                            [func-param-id (closure-params func-closure)]
                            [func-body (closure-body func-closure)]
                            ;parent function scope
                            [func-env (closure-inner-env func-closure)]
                            ;combine parent scope and current top-scope
                            [combined-env (combine-env env func-env)]
                            ;Delete reference to func in the environment to avoid recursion
                            [combined-env-final (hash-set combined-env func '())]
                            ;Final closure environment
                            [closure-env (bind-param combined-env-final func-param-id (length func-param-id)
                                                     params (length params))]
                            )
                       (interpret closure-env func-body))
                     ;If function is a buildin
                     (if (builtin? func)
                         (if (equal? func 'procedure?)
                             (check-procedure env (first params))
                             (apply (hash-ref builtins func) (evaluate-params env params)))
                         (report-error 'not-a-function func)
                         ))]
    ;If function is a lambda
    [(list 'lambda param-id body) (let* ([closure-env (bind-param env param-id (length param-id)
                                                                  params (length params))]
                                         )
                                    (interpret closure-env body))]
    ;If function is calling to another function
    [(list inner-func inner-func-params ...) (if (check-procedure env inner-func)
                                                 (let ([inner-func-eval (evaluate-function env inner-func inner-func-params)])
                                                   (if (closure? inner-func-eval)
                                                       (let*([inner-func-body (closure-body inner-func-eval)]
                                                             [inner-func-param-id (closure-params inner-func-eval)]
                                                             [inner-func-env (closure-inner-env inner-func-eval)]
                                                             [inner-func-env-with-param (bind-param inner-func-env inner-func-param-id
                                                                                                    (length inner-func-param-id) params (length params))])
                                                         (interpret inner-func-env-with-param inner-func-body))
                                                       (report-error 'not-a-function func)))
                                                 (report-error 'not-a-function inner-func))]
    ))


#|
(combine-env env func-env) -> hash
Helper for evaluate-function, combine env and func-env. If they has same key k,
then k's value in func-env has higher priority
|#
(define (combine-env env func-env)
  (foldl (lambda (func-env-key env)
           (hash-set env func-env-key (hash-ref func-env func-env-key))
           )
         env (hash-keys func-env)))

#|
(evaluate-params env params) -> list
Helper for evaluate-function, interpret the function parameter expressions
|#
(define (evaluate-params env params)
  (map (lambda (param) (interpret env param)) params))


#|
(check-dup lst) -> boolean
If there is a duplicate, report duplicate-name error
|#
(define (check-dup lst)
  (foldl (lambda (e hash)
           (if (hash-has-key? hash e)
               (report-error 'duplicate-name e)
               (hash-set hash e 0)))
         (hash) lst))



#|
(bind-param env func params) -> hash
Helper function for interpret, create hash table with function parameter's id as key,
each key refer to their input value.
|#
(define (bind-param env param-id length-param-id params length-params)
  (let* ([first-param (interpret env (first params))]
         [rest-params (rest params)]
         [first-param-id (first param-id)]
         [rest-param-id (rest param-id)]
         [new-env (hash-set env first-param-id first-param)])
    (if (and (equal? rest-params null) (equal? rest-param-id null))
        new-env
        (if (equal? length-param-id length-params)
            (bind-param new-env rest-param-id length-param-id rest-params length-params)
            (report-error 'arity-mismatch length-param-id  length-params)
            )
        )
    )
  )




#|
(build-env prog) -> hash
prog: datum?
Helper for run-interpreter, returns a hash table representing the environment constructed from this
hubbub program
|#
(define (build-env prog)
  (foldl
   (lambda(id cur-env)
     (save-bindings-or-contract id cur-env)
     )
   (hash)
   prog
   )
  )

#|
(save-bindings-or-contract definition-expr env) -> hash
Helper for build-env, returns a hash table with definition-expr included, definition-expr could be a name binding or contract in hubbub
|#
(define (save-bindings-or-contract definition-expr env)
  (match definition-expr
    [(list 'define id expr)
     (if (hash-has-key? env id) (report-error 'duplicate-name id) (hash-set env id (interpret env expr)))]
    [(list 'define-contract id contract) (if

                                          (hash-has-key? env id) (hash-set env id (process-contract env id contract))
                                          (report-error 'unbound-name id))]
    ;only case is when we call the last line of the program, which is a hubbub expression
    ;do nothing and return the environment
    [else env]
    )
  )
;-----------------------------------------------------------------------------------------
; Implement contract checking for hubbub
;-----------------------------------------------------------------------------------------
#|
Contract evaluation for Hubbub when defining, throw invalid-contract
****** used only when defined
|#
(define (process-contract env id contract)
  (let ([target (hash-ref env id)])
    (if (closure? target) (closure (closure-params target)
                                   (closure-body target)
                                   (closure-inner-env target)
                                   contract) (report-error 'invalid-contract id))))
#|
Should be callled in conditions where there're no unbound errors
double map
return work/report-error contract-violation
*******  used only when interpret, in if statement
|#
(define (check-contract env expr)
  (match expr [(list func args ...) (if (not (hash-has-key? env func))
                                        #t
                                        (cond [(null? (closure-contract
                                                       (hash-ref env func))) #t]
                                              [else (let* ([cur-closure (hash-ref env func)]
                                                           [check-arity (if (equal? (length (closure-params cur-closure)) (length args))
                                                                            #t
                                                                            (report-error 'arity-mismatch (length args) (length (closure-params cur-closure))))]
                                                           [cur-contract (closure-contract cur-closure)]
                                                           [expected-body (extract-contract-body cur-contract)]
                                                           [actual-body args]
                                                           )
                                                      (map (curry-compare-contract env) (zip expected-body actual-body)))]))]))

#|interface used|#
(define (check-rtype env expr value)
  (match expr [(list func args ...)
               (if (not (hash-has-key? env func))
                   #t
                   (cond [(null? (closure-contract
                                  (hash-ref env func))) #t]
                         [else (let* ([cur-closure (hash-ref env func)]
                                      [cur-contract (closure-contract cur-closure)]
                                      [expected-rtype (extract-contract-rtype cur-contract)]
                                      )
                                 (cond [(equal? expected-rtype 'any) #t]
                                       [
                                        (let* ([output (interpret env (list expected-rtype value))]
                                               
                                           
                                               ) output)
                                        #t]
                                       [else (report-error 'contract-violation)]))]))]))

(define (curry-compare-contract env)
  (lambda (contract)
    (match contract [(list 'any other) #t]
      [other (if (equal? (interpret env contract) #t)
                 #t
                 (report-error 'contract-violation))]
      )))

(define (extract-contract-body expr)
  (match expr [(list args ... '-> rtype) args]))

(define (extract-contract-rtype expr)
  (match expr [(list args ... '-> rtype) rtype]))

#|
define the zip utility function, s.t we can compare the result respectively
|#
(define zip (lambda (lst1 lst2)(map list lst1 lst2)))
;----------------------------------------------------------------------------------------
; Implement procedure? function for hubbub
;----------------------------------------------------------------------------------------
#|
(check-procedure env expr) -> boolean
(implementation of procedure? for hubbub)
Returns true if expr is a procedure in hubbub
|#
(define (check-procedure env expr)
  (match expr
    [(list 'lambda params body) #t]
    [(? symbol?) (if (binded-to-procedure-or-builtin? env expr) #t #f)]
    [(list func params ...) (check-procedure env func)]
    [else #f]))

#|
(binded-to-procedure? expr) -> boolean
Return true if id refers to a function in the environment
|#
(define (binded-to-procedure-or-builtin? env id)
  (if (hash-has-key? env id)
      ;Check if the id is saved as a closure in the environment.
      (closure? (hash-ref env id))
      ;When symbol is not in hash, unbound-name error
      (if(builtin? id) #t (report-error 'unbound-name id))
      )
  )
;-----------------------------------------------------------------------------------------
; Helpers: Builtins and closures
;-----------------------------------------------------------------------------------------
; A hash mapping symbols for Hubbub builtin functions to their corresponding Racket value.
(define builtins
  (hash
   '+ +
   'equal? equal?
   '< <
   'integer? integer?
   'boolean? boolean?
   ; Note: You'll almost certainly need to replace procedure? here to properly return #t
   ; when given your closure data structure at the end of Task 1!
   'procedure? check-procedure
   ))

; Returns whether a given symbol refers to a builtin Hubbub function.
(define (builtin? identifier) (hash-has-key? builtins identifier))

#|
Starter definition for a closure "struct". Racket structs behave similarly to
C structs (contain fields but no methods or encapsulation).
Read more at https://docs.racket-lang.org/guide/define-struct.html.
You can and should modify this as necessary. If you're having trouble working with
Racket structs, feel free to switch this implementation to use a list/hash instead.
|#
(struct closure (params body inner-env contract))