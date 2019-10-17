-- Authors:
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp 
  deriving Eq

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = N Int 
          | X Int     
          | BExp BinOp Expr Expr



--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (BExp op e1 e2) = (prop_Expr e1) && (prop_Expr e2)
prop_Expr (X n)            = not(n < 0)
prop_Expr (N n)            = True

--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x. 

instance Show Expr where
  show (X 0)                  = "1"
  show (X 1)                  = "x"
  show (X n)                  = "(x^" ++ (show n) ++ ")"
  show (N n)                  = (show n)
  show (BExp AddOp e1 (N 0))  = (show e1)
  show (BExp AddOp (N 0) e2)  = (show e2)
  show (BExp MulOp e1 (N 1))  = (show e1)
  show (BExp MulOp (N 1) e2)  = (show e2)
  show (BExp op e1 e2)        = (show e1) ++ (opString op) ++ (show e2)
    where opString AddOp      = "+"
          opString MulOp      = "*"

--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests

--not final
--maybe negative nums 
-- rSingle :: Gen Expr
-- rSingle = do r <- choose(0,10)
--              n <- elements[N,X]
--              return (n r)

rSingle :: Gen Expr
rSingle = do exp <- choose(1,9)
             n <- choose(1,100)
             t <- elements[(X 0),(N 0)]
             case t of
              (X _) -> (return(X exp))
              (N _) -> (return (N n))

rOperEx :: Int -> Gen Expr
rOperEx 0 = rSingle
rOperEx n | n > 0 = do op <- elements [AddOp,MulOp]
                       e1 <- rSingle
                       e2 <- (rOperEx (n-1))
                       return(BExp op e1 e2)

instance Arbitrary Expr where arbitrary = do r <- choose (0,5)
                                             rOperEx r

--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval x (X n)           = x ^ n 
eval x (N n)           = n 
eval x (BExp op e1 e2) = (opA op) (eval x e1) (eval x e2)
  where opA AddOp      = (+)
        opA MulOp      = (*)
  
--------------------------------------------------------------------------------
-- * A6
-- Define
exprToPoly :: Expr -> Poly
e1 = (BExp AddOp (BExp MulOp (X 3) (N 3)) (BExp MulOp (X 3) (N 5)))

exprToPoly (N n)                      = fromList [n]
exprToPoly (X n)                      = fromList (1 : replicate n 0)
exprToPoly (BExp AddOp e1 e2)         = (exprToPoly e1) + (exprToPoly e2)
exprToPoly (BExp MulOp e1 e2)         = (exprToPoly e1) * (exprToPoly e2)
        
-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly e x = (eval x e) == evalPoly x (exprToPoly e)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction, 
polyToExpr :: Poly -> Expr
polyToExpr p | (length lP) <= 1   = eld
             | (length lP) > 1    = (BExp AddOp (eld) (polyToExpr (fromList (drop 1 lP))))
  where
      lP = (toList p)
      eld | (length lP) == 0 = N 0
          | otherwise = (BExp MulOp (X ((length lP) - 1)) (N (head lP)))


-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr x p = (eval x (polyToExpr p)) == (evalPoly x p)

--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify e = polyToExpr(exprToPoly e)

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

--------------------------------------------------------------------------------