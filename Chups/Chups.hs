{-|
Module: Chups
Description: Chups language for Assignment 2
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

The assignment handout can be found at
https://www.cs.toronto.edu/~david/csc324/assignments/a2/handout.html
-}
module Chups
    (
    -- Task 1 (CPS transform, stateless)
      cpsTransformProg
    , cpsTransform
    -- Tasks 2 & 3 (CPS transform, stateful; manipulating control flow)
    , cpsTransformProgS
    , cpsTransformS
    )
where

import qualified Data.List as List
import qualified Control.Monad.State as State
import ChupsTypes (Prog(..), Binding(..), Expr(..))

-------------------------------------------------------------------------------
-- |
-- * Task 1: Implementing CPS transformation (stateless)
-------------------------------------------------------------------------------

-- | The main transformation function. This should take a Chups program in the
-- core language, and return a CPS-transformed version of this program.
-- Remember to use pass the `_id` continuation for top-level expressions.
cpsTransformProg :: Prog -> Prog
cpsTransformProg (Prog bindings expr) = (Prog (foldl extractBinding [] bindings) (cpsTransform expr (Identifier "_id")))
-- helper to return the cps transformed version of bindings
extractBinding acc (Binding a expr) = acc ++ [(Binding a (cpsTransform expr (Identifier "_id")))]


-- | The main transformation function, which takes two Expr values:
-- the first is the expression to transform, and the second is (an Expr representation of)
-- a unary function representing the continuation to apply to the expression.
-- The resulting expression should be semantically equivalent to
-- calling the continuation on the expression.
cpsTransform :: Expr -> Expr -> Expr
cpsTransform (IntLiteral num) k = Call k [(IntLiteral num)]
cpsTransform (BoolLiteral bool) k = Call k [(BoolLiteral bool)]
cpsTransform (Identifier id) k = Call k [(Identifier id)]
cpsTransform (Lambda params body) k = Call k [(Lambda (params ++ ["_k"]) (cpsTransform body (Identifier "_k")))]
cpsTransform (Call (Identifier f) params) k = if (all checkLiteral params) then (Call (Identifier f) (params ++ [k]))
                                   else 
                                    let bottomList = Call (Identifier f) ((foldl (\a x-> a ++ [(collectExpr x)]) [] params) ++ [k])
                                    in foldr expandExpr bottomList params
cpsTransform (Call (Lambda param body) params) k = cpsTransform (Lambda param body)  (Lambda ["_v"] (Call (Identifier "_v") (params ++ [k])))
cpsTransform (If (BoolLiteral bool) thenExpr elseExpr) k = (If (BoolLiteral bool) (cpsTransform thenExpr k) (cpsTransform elseExpr k))
cpsTransform (If (Identifier r) thenExpr elseExpr) k = (If (Identifier r) (cpsTransform thenExpr k) (cpsTransform elseExpr k))
cpsTransform (If cond thenExpr elseExpr) k = (If (cpsTransform cond (Identifier "_id")) (cpsTransform thenExpr k) (cpsTransform elseExpr k))


-- Remember that for Task 1, you only need to handle the core Chups expression types.
-- You can leave this pattern-match line to prevent an "unmatched pattern" compiler warning.
cpsTransform _ _ =
    error "For Task 1, you do not need to handle other expression types."
-- check if the expression is a identifier or literal
checkLiteral (IntLiteral _) = True
checkLiteral (BoolLiteral _) = True
checkLiteral (Identifier _) = True
checkLiteral other = False
-- collect the expression for the original expression, output e.g: (cps: _v0 _v1 k)
collectExpr (IntLiteral num) = (IntLiteral num)
collectExpr (BoolLiteral bool)  = (BoolLiteral bool)
collectExpr (Identifier id) = (Identifier id)
collectExpr other = (Identifier "_v")
-- expand the expression to get a lambda outer function
expandExpr x a = (case x of 
                    IntLiteral num -> a
                    BoolLiteral bool -> a
                    Identifier id -> a
                    other -> (cpsTransform x (Lambda ["_v"] a)))
-------------------------------------------------------------------------------
-- |
-- * Task 2: Implementing CPS transformation (stateful)
-------------------------------------------------------------------------------
-- | This is similar to cpsTransformProg, except that it uses a stateful
-- version of cpsTransform to generate unique names and avoid name collisions.
cpsTransformProgS :: Prog -> Prog
cpsTransformProgS (Prog bindings expr) = (Prog (foldl extractBindingS [] bindings) (State.evalState (cpsTransformS expr (Identifier "_id")) 0))
-- helper to return the cps transformed version of bindings
extractBindingS acc (Binding a expr) = acc ++ [(Binding a (State.evalState (cpsTransformS expr (Identifier "_id")) 0))]

-- | Stateful version of cpsTransform, which uses its Integer state as a counter
-- to generate fresh variable names. See assignment handout for details.
cpsTransformS :: Expr -> Expr -> State.State Integer Expr
cpsTransformS (IntLiteral num) k = State.state ( \count -> (Call k [(IntLiteral num)], count))
cpsTransformS (BoolLiteral bool) k = State.state ( \count -> (Call k [(BoolLiteral bool)], count))
cpsTransformS (Identifier id) k = State.state ( \count -> (Call k [(Identifier id)], count))
-- First increase state by one, then transform all subequations (note: state passed to subequation was increased by 1)
cpsTransformS (Lambda params body) k = do count <- State.get 
                                          _ <- (State.put (count+1))
                                          transformedBody <- (cpsTransformS body (Identifier ("_k"++(show count))))
                                          return (Call k [(Lambda (params ++ ["_k"++(show count)]) transformedBody)])
-- First when called with identifier 
cpsTransformS (Call (Identifier f) params) k = -- if all the params are atomic, then do the tranform 
                                              if (all checkLiteral params) then 
                                                do starter <- State.get
                                                   _ <- (State.put starter)
                                                   return (Call (Identifier f) (params ++ [k]))
                                              else -- if any of the params is not atomic, do the following steps 
                                                 do
                                                  starter <- State.get
                                                  _ <- State.put (starter + 1)
                                                  bottomList <- (let bt = Call (Identifier f) (((State.evalState (foldl (
                                                                                  \a x -> let (val, curCounter) = State.runState a 0
                                                                                          in collectExprS x val curCounter
                                                                                  ) (State.state $ \s-> ([], 0)) params)) 0) ++ [k])
                                                                in (return bt)) -- we get the bottom expression
                                                  -- expand lambda(a) (...)
                                                  foldr expandExprS (State.state $ \s-> (bottomList, (foldl (\a x->
                                                                                                              getNonLiteral x a) 0 params) - 1) ) params
-- when called with lambda
cpsTransformS (Call (Lambda param body) params) k = if (all checkLiteral params) then 
                                                      do  -- same as the pattern matching above, but a more generalized version involving lambda function
                                                        count <- State.get
                                                        _ <- (State.put (count + 1))
                                                        cpsTransformS (Lambda param body) (Lambda [("_v" ++ (show count))] (Call (Identifier ("_v" ++ (show count))) (params ++ [k])))
                                                      else
                                                        do
                                                          starter <- State.get
                                                          _ <- State.put (starter + 1)
                                                          bottomList <- (let bt = Call (State.evalState (cpsTransformS (Lambda param body) (Identifier "_id")) 0) (((State.evalState (foldl (
                                                                                          \a x -> let (val, curCounter) = State.runState a 0
                                                                                                  in collectExprS x val curCounter
                                                                                          ) (State.state $ \s-> ([], 0)) params)) 0) ++ [k])
                                                                        in (return bt))
                                                                    
                                                          cur <- State.get
                                                          foldr expandExprS (State.state $ \s-> (bottomList, (foldl (\a x->
                                                                                                              getNonLiteral x a) 0 params) - 1) ) params
-- Transform if statements
cpsTransformS (If (Identifier r) thenExpr elseExpr) k = do
                                                        thenEx <- cpsTransformS thenExpr k
                                                        elseEx <- cpsTransformS elseExpr k
                                                        return (If (Identifier r) thenEx elseEx) -- if the if statement is only an Identifier 
-- if the cond is an expr 
cpsTransformS (If cond thenExpr elseExpr) k = do
                                                thenEx <- cpsTransformS thenExpr k
                                                elseEx <- cpsTransformS elseExpr k
                                                condEx <- cpsTransformS cond (Identifier "_id")
                                                return (If condEx thenEx elseEx)

-- | Task3
-- cpsTransformS for shift expression, first transform expr then wrape continuation to a binary function with parameter "value", "last" 
-- then call transformed expr with binary function
cpsTransformS (Shift kName expr) k = 
  do
    transformedExpr <- (cpsTransformS expr (Identifier "_id"))
    return (Call (Lambda [kName] transformedExpr) [(Lambda ["value", "last"] (Call (Identifier "last") [(Call k [(Identifier "value")])]) )])
-- cpsTransformS for rest expression, first transform expr with _id as continuation, then call k on it
cpsTransformS (Reset expr) k = 
  do
    transformedBody <- (cpsTransformS expr (Identifier "_id"))
    return (Call k [transformedBody])
-- Similar as transform a literal
cpsTransformS (Error str) k = State.state ( \count -> (Call k [(Error str)], count))
-- Assume expr is always an error, transform expr with _id as continuation
cpsTransformS (Raise expr) k = (cpsTransformS  expr (Identifier "_id"))
-- Evaluate body then transform if conditions as a call to cps functions, and transform then and else statements for both if expressions
-- then return an nested if statement by constructing all evaluated expressions together.
cpsTransformS (Try body msg handler) k = 
  do
    v <- cpsTransformS body (Identifier "_id")
    --check whether a value is an error or not, if not an error evaluate it
    isErrorCond <- State.state ( \count -> ((Call (Identifier "cps:_error?") [v, (Identifier "_id")]), count))
    ifNotErr <- cpsTransformS body k
    --check whether errors are equal, if equal to the msg, evaluate handler otherwise return the error
    isEqualCond <- State.state ( \count -> ((Call (Identifier "cps:equal?") [v, (Error msg), (Identifier "_id")]), count))
    ifIsErrAndEqual <- cpsTransformS handler k
    return (If isErrorCond (If isEqualCond ifIsErrAndEqual v) ifNotErr)
    
-- collect the state of the expression to get the expression, output state value e.g: (cps:* _v0 _v1 ...)
collectExprS (IntLiteral num) prev curCounter = State.state (\s-> ((prev ++ [(IntLiteral num)]), curCounter))
collectExprS (BoolLiteral bool) prev curCounter = State.state (\s-> ((prev ++ [(BoolLiteral bool)]), curCounter))
collectExprS (Identifier id) prev curCounter = State.state (\s-> ((prev ++ [(Identifier id)]), curCounter))
collectExprS other prev curCounter = State.state (\s->((prev ++ [(Identifier ("_v"++(show curCounter)))]), curCounter + 1))

-- expand expression with lambda(a) (...) container
expandExprS x a = (case x of 
                      IntLiteral num -> a
                      BoolLiteral bool -> a
                      Identifier id -> a
                      other -> let (v, counter) = State.runState a 0
                                   curVal = State.evalState (cpsTransformS x (Lambda [("_v" ++ (show counter))] v)) counter
                               in State.state (\s-> (curVal, counter - 1)))
                               
-- if expr is a literal, increment the counter, otherwise remains the same counter
getNonLiteral expr init = (case expr of 
                              IntLiteral num -> init
                              BoolLiteral bool -> init
                              Identifier id -> init
                              other -> init + 1)
