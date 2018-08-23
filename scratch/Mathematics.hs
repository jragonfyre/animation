--
-- Mathematics.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Mathematics
  ( module Mathematics
  ) where

import Data.Function (on)

import Data.List (intersperse)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

data MType
  = Int
  | Real
  | Null
  | Func MType MType
  | Prod MType MType
  -- | Union MType MType
  deriving (Eq,Ord,Show,Read)

data Expr = Expr
  { freeVars :: Set Variable
  , eType :: MType
  , rawExp :: ExprF
  }

--data FDef 
  -- = UFunc Variable Expr -- variable should be free in the expression
  -- | RFunc String (Double -> Expr) -- haskell function
  -- | IFunc String (Int -> Expr) -- haskell function

data Literal
  = IFLiteral String (Int -> Expr)
  | RFLIteral String (Double -> Expr)
  | ILiteral Int
  | RLiteral Double
  | NLiteral

data ExprF
  = FuncAppExp Expr Expr -- expression that evaluates to a function and the expression it is applied to
  | LitExp Lit
  | ProdExp Expr Expr
  | LambdaExp Variable Expr
  | VarExp Variable
  | UVarExp String
  -- | Binding Variable Expr

type Context = Map String Expr

checkTypesAndNormalize :: Expr -> Maybe Expr
checkTypesAndNormalize (FuncApp expf exparg) = do
  
  

-- variables are equivalent to their names and types, tho this isn't good at all.
type Variable = (String,MType)

data Op2 = 
  { opName :: String
  , in1T :: MType
  , in2T :: MType
  , outT :: MType
  }



-- should be a.e. differentiable function of a single variable with fDeriv a math expression with a single
-- free variable which is a.e. equal to the derivative of the sfunc
data SFuncType = SFuncT
  { fName :: String
  , fDeriv :: MExpr
  , fEval :: Double -> Double
  }

instance Eq SFuncType where
  (==) = (==) `on` fName

instance Ord SFuncType where
  compare = compare `on` fName

instance Show SFuncType where
  show sf = "<sFunc:"++ fName sf ++">"

data MExpr
  = Sum [MExpr]
  | Product MExpr MExpr
  | Negation MExpr
  | Inverse MExpr
  | RLiteral Double
  | ILiteral Int
  | Zero
  | One
  | Variable String
  | IPower MExpr Int
  | RPower MExpr Double
  | GPower MExpr MExpr
  | SFunc SFuncType MExpr
  deriving (Eq,Ord)

--data CExpr = CExpr MExpr (Set String)

compileMExpr :: MExpr -> CExpr
compileMExpr (Sum

wrapParens :: String -> String
wrapParens str = '(':(str++")")

instance Show MExpr where
  show (Sum exprs) = wrapParens . concat . intersperse "+" $ map show exprs
  show (Product expr1 expr2) = wrapParens $ show expr1 ++ "*" ++ show expr2
  -- wrapParens . concat . intersperse "*" $ map show exprs
  show (Negation expr) = wrapParens . ('-':) . show $ expr
  show (Inverse expr) = wrapParens . ("1/"++) . show $ expr
  show (RLiteral val) = if val < 0 then wrapParens $ show val else show val
  show (ILiteral val) = if val < 0 then wrapParens $ show val else show val
  show Zero = "0"
  show One = "1"
  show (Variable vname) = "<var:"++vname++">"
  show (IPower expr int) = wrapParens $ (show expr) ++ "^" ++ (show $ ILiteral int)
  show (RPower expr val) = wrapParens $ (show expr) ++ "^" ++ (show $ RLiteral val)
  show (GPower expr expr2) = wrapParens $ (show expr) ++ "^" ++ (show expr2)
  show (SFunc ft expr) = wrapParens $ show ft ++ show expr

makeProduct :: MExpr -> MExpr -> MExpr
makeProduct Zero _ = Zero
makeProduct _ Zero = Zero
makeProduct One a = a
makeProduct a One = a
makeProduct x y = Product x y

makeSum :: [MExpr] -> MExpr
makeSum exprs = Sum $ filter (/=Zero) exprs

makeIPower :: MExpr -> Int -> MExpr
makeIPower _ Zero = One
makeIPower x One = x
makeIPower x n = IPower x n

makeRPower :: MExpr -> Double -> MExpr
makeRPower = RPower

makeGPower :: MExpr -> MExpr -> MExpr
makeGPower _ Zero = One
makeGPower x One = x
makeGPower x n = GPower x n


diff :: String -> MExpr -> MExpr
diff vn (Sum exprs) = Sum $ map diff vn exprs
diff vn (Product expr1 expr2) = Sum
  [ Product (diff vn expr1) expr2
  , Product expr1 (diff vn expr2)
  ]
--diff (Product exprs) = Sum 
diff vn (Negation expr) = Negation $ diff vn expr
diff vn (Inverse expr) = Negation $ Product
  (diff vn expr)
  (Inverse (IPower expr 2))

diff vn (RLiteral val) = Zero
diff vn (ILiteral val) = Zero
diff vn Zero = Zero
diff vn One = Zero
diff vn (Variable vname) = if vn == vname then One else Zero
diff vn (IPower expr pow) = Product
   (ILiteral pow)
   $ Product 
       (diff vn expr)
       (IPower expr (pow-1))
diff vn (RPower expr pow) = Product
   (RLiteral pow)
   $ Product 
       (diff vn expr)
       (RPower expr (pow-1))
diff vn (RPower expr pow) = Product pow
   $ Product 
       (diff vn expr)
       (RPower expr (Sum [pow,ILiteral (-1)]))
diff vn (SFunc ft expr)


