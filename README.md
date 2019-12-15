# Chups-Hubbub
A programming language design

## Hubbub Features
The Hubbub grammar is defined as follows (<prog> indicates the form of an entire Hubbub program):

<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=6 orderedList=false} -->

```
<prog> = <binding_or_contract> ... <expr> ;

<binding_or_contract> = <binding>    (* Name-value binding *)
                      | <contract>   (* Function contract (more on this later) *)
                      ;

<binding> = "(" "define" ID <expr> ")" ;

<expr> = ID                                      (* Identifier *)
       | INTEGER                                 (* Integer literal *)
       | BOOLEAN                                 (* Boolean literal, #t or #f *)
       | "(" "lambda" "(" ID ... ")" <expr> ")"  (* Function value *)
       | "(" <expr> <expr> ... ")"               (* Function call *)
       ;

<contract> = "(" "define-contract" ID "(" <con-expr> ... "->" <con-expr> ")" ")" ;
<con-expr> = <expr> | "any" ;
```

## Contract definition
e.g1: 
```
(define-contract f (integer? boolean? any -> procedure?)))
```

e.g2:
```
(define-contract f2
  ((lambda (x) (< 0 x)) (lambda (x) (< 0 x)) -> (lambda (x) (< 0 x))))
```

contracts are checked dynamically every time a function is called