{-|
Module: ChupsTypes
Description: Types for Assignment 2
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

This module provides the public types required for Assignment 2.
You should review the data types carefully, but do not change anything
in this file! We will use a fresh copy of this file for testing purposes.
-}
module ChupsTypes
    ( Prog(..)
    , Binding(..)
    , Expr(..)
    )
where

import qualified Data.List as List

-- | A program consists of zero or more name bindings followed by a single
-- expression.
data Prog = Prog [Binding] Expr deriving (Eq)

-- | A binding consists of an identifier an expression.
data Binding = Binding String Expr deriving (Eq)

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
    deriving (Eq)

-------------------------------------------------------------------------------
-- |
-- * We use a custom instance of "Show" for these data types to turn them
-- into valid Racket expressions. This will be useful for testing purposes
-- later on, when we try to evaluate these expressions.
-------------------------------------------------------------------------------

instance Show Prog where
    -- show :: Prog -> String
    show (Prog bindings expr) =
        List.intercalate "\n"
            $
            -- Global bindings available to all Chups programs
              [ "(define/match (cps:+ . _) [((list args ... k)) (k (apply + args))])"
              , "(define/match (cps:* . _) [((list args ... k)) (k (apply * args))])"
              , "(define/match (cps:equal? . _) [((list args ... k)) (k (apply equal? args))])"
              , "(define (_id v) v)"  -- Identity continuation

              , "(define (cps:_error? x k) (k (string? x)))"
              ]
            ++ map show bindings
            ++ [show expr]

instance Show Binding where
    -- show :: Binding -> String
    show (Binding name expr) = "(define " ++ name ++ " " ++ show expr ++ ")"

instance Show Expr where
    -- show :: Expr -> String
    show (IntLiteral  n    ) = show n
    show (BoolLiteral True ) = "#t"
    show (BoolLiteral False) = "#f"
    show (Identifier  ident) = ident
    show (Lambda params body) =
        "(lambda (" ++ (List.unwords params) ++ ") " ++ show body ++ ")"
    show (Call func args) =
        "(" ++ show func ++ " " ++ (List.unwords (map show args)) ++ ")"
    show (If cond thenExpr elseExpr) =
        "(if "
            ++ show cond
            ++ " "
            ++ show thenExpr
            ++ " "
            ++ show elseExpr
            ++ ")"
    show (Error msg            ) = "\"" ++ msg ++ "\""

-- These can be useful for debugging purposes, but the final programs
-- your CPS transformer returns should not contain any of these expression types.
    show (Shift identifier body) = "(_Shift " ++ identifier ++ show body ++ ")"
    show (Reset body           ) = "(_Reset " ++ show body ++ ")"
    show (Raise error          ) = "(_Raise " ++ show error ++ ")"
    show (Try body msg handler) =
        "(_Try " ++ show body ++ " " ++ msg ++ " " ++ show handler ++ ")"
