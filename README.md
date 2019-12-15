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

## Chups Features
* Chups core (the haskell language form to turn into)
```
data Prog = Prog [Binding] Expr     -- ^ A whole Chups program

data Binding = Binding String Expr  -- ^ A name binding

data Expr
    -- The "core" Chups language
    = IntLiteral Integer      -- ^ Integer literal
    | BoolLiteral Bool        -- ^ Boolean literal
    | Identifier String       -- ^ Identifier
    | Lambda [String] Expr    -- ^ Function value
    | Call Expr [Expr]        -- ^ Function call
    | If Expr Expr Expr       -- ^ if expressions

    -- Additional control flow expressions
    | Shift String Expr       -- ^ `shift` expression in Racket
    | Reset Expr              -- ^ `reset` expression in Racket
    | Error String            -- ^ an error value (with a given message)
    | Raise Expr              -- ^ "raise" an error
    | Try Expr String Expr    -- ^ try-except expression
```
### Implemented Features:
* turn a normal state to CPS (continuation passing style)
* Shift, error, reset, try catch statement
