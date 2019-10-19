-- Authors:
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp 
  deriving Eq

--------------------------------------------------------------------------------
-- * A1
-- Data type for expressions

data Expr = N Int 
          | X Int     
          | BExp BinOp Expr Expr



--------------------------------------------------------------------------------
-- * A2
-- Checks that exponents are never negative in an expression
prop_Expr :: Expr -> Bool
prop_Expr (BExp op e1 e2) = (prop_Expr e1) && (prop_Expr e2)
prop_Expr (X n)            = not(n < 0)
prop_Expr (N n)            = True

--------------------------------------------------------------------------------
-- * A3
-- Shows an expresion

instance Show Expr where
  show (X 1)                  = "x"
  show (X n)                  = "(x^" ++ (show n) ++ ")"
  show (N n)                  = (show n)
  show (BExp op e1 e2)        = (show e1) ++ (opString op) ++ (show e2)
    where opString AddOp      = "+"
          opString MulOp      = "*"

--------------------------------------------------------------------------------
-- * A4
-- Generates random expressions

rSingle :: Gen Expr
rSingle = do exp <- choose(0,9)
             n <- choose(0,100)
             t <- elements[(X 0),(N 0)]
             case t of
              (X _) -> (return(X exp))
              (N _) -> (return (N n))

rOperEx :: Int -> Gen Expr
rOperEx 0 = rSingle
rOperEx n | n > 0 = do op <- elements [AddOp,MulOp]
                       e1 <- rSingle
                       e2 <- (rOperEx (n-1))
                       case (BExp op e1 e2) of
                        (BExp MulOp (N 1) e2) -> return(BExp op (N 2) e2)
                        (BExp MulOp e1 (N 1)) -> return(BExp op e1 (N 2))
                        (BExp op     e1   e2)  -> return(BExp op e1 e2)

instance Arbitrary Expr where arbitrary = do r <- choose (0,5)
                                             rOperEx r

--------------------------------------------------------------------------------
-- * A5
-- Takes a value for x and an expression and evaluates it
eval :: Int -> Expr -> Int
eval x (X n)           = x ^ n 
eval x (N n)           = n 
eval x (BExp op e1 e2) = (opA op) (eval x e1) (eval x e2)
  where opA AddOp      = (+)
        opA MulOp      = (*)
  
--------------------------------------------------------------------------------
-- * A6
-- Converts expression to polynomial
exprToPoly :: Expr -> Poly
exprToPoly (N n)                      = fromList [n]
exprToPoly (X n)                      = fromList (1 : replicate n 0)
exprToPoly (BExp AddOp e1 e2)         = (exprToPoly e1) + (exprToPoly e2)
exprToPoly (BExp MulOp e1 e2)         = (exprToPoly e1) * (exprToPoly e2)
        

-- Checks that evaluating the polynomial you get from exprToPoly gives
-- the same answer as evaluating the expression
prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly e x = (eval x e) == evalPoly x (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
-- Creates a singular expression, for example 4x^2, 4, x^2 
sExpr :: Int -> Int -> Expr
sExpr i 1 = N i
sExpr 1 l = X (l - 1)
sExpr i l = (BExp MulOp (N i) (X (l - 1)))

-- Iterates through list and builds an expression
listP :: [Int] -> Expr 
listP []          = N 0
listP (0:(x1:[])) = (N x1)
listP (1:(0:[]))  = (X 1)
listP (0:xs)      = listP xs
listP (x:xs)      | allZero   = sExpr x l
                  | otherwise = (BExp AddOp (sExpr x l) (listP xs))
  where 
    l = length (x:xs)
    allZero = and [ i == 0 | i <- xs]

-- Converts polynomial into expression
polyToExpr :: Poly -> Expr
polyToExpr p = listP (toList p) 
-- polyToExpr :: Poly -> Expr
-- polyToExpr p | (or [ t == 0 | t <- (drop 1 lP)])  = sPoly
--              | length(lP) > 1                     = (BExp AddOp (sPoly) (polyToExpr (fromList (drop 1 lP))))
--   where
--       lP = (toList p)
--       sPoly | (length lP) == 0 = N 0
--             | otherwise = (BExp MulOp (X ((length lP) - 1)) (N (head lP)))

-- Checks that evaluated polynomial converted to expr gives the same value 
--as before
prop_polyToExpr x p = (eval x (polyToExpr p)) == (evalPoly x p)

--------------------------------------------------------------------------------
-- * A8
-- Simplifies an expression by converting it to a polynomial and back again
simplify :: Expr -> Expr
simplify e = polyToExpr(exprToPoly e)

--------------------------------------------------------------------------------

-- * A9
-- QuickCheck property that makes sure a simplified
-- expression does not contain any "junk"
prop_noJunk :: Expr -> Bool
prop_noJunk e = noJunk(simplify e)

noJunk :: Expr -> Bool
noJunk (X 0)                      = False
noJunk (N _)                      = True
noJunk (X _)                      = True
noJunk (BExp AddOp (N 0) _)       = False
noJunk (BExp AddOp _ (N 0))       = False
noJunk (BExp MulOp (N 1) _)       = False
noJunk (BExp MulOp _ (N 1))       = False
noJunk (BExp MulOp (N 0) _)       = False
noJunk (BExp MulOp _ (N 0))       = False
noJunk (BExp AddOp (N e1) (N e2)) = False
noJunk (BExp MulOp (N e1) (N e2)) = False
noJunk (BExp op e1 e2)          = (noJunk e1) && (noJunk e2)

--------------------------------------------------------------------------------