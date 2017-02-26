module Lib where

import AbsLambda
import Data.List

type Context = [(String,Binding)]
data Binding = NameBind deriving Eq
data Result  = NoRuleApplies | Step Term
data Term
    = TmVar String Integer
    | TmInt Integer
    | TmApp Term Term
    | TmLet Ident Term Term
    | TmAbs Ident Term
    | TmAdd Term Term
  deriving (Eq, Ord, Show, Read)

rmNames :: Context -> NmTerm -> Term
rmNames c (NmTmVar (Ident str))    = case (elemIndex (str,NameBind) c) of
                                        Just a  -> TmVar str $ fromIntegral a
                                        Nothing -> error "Undefined"
rmNames _ (NmTmInt n)              = TmInt $ (fromIntegral n)
rmNames c (NmTmApp tm1 tm2)        = TmApp (rmNames c tm1) (rmNames c tm2)
rmNames c (NmTmAbs (Ident str) tm) = let c' = (str,NameBind):c in TmAbs (Ident str) $ rmNames c' tm
rmNames c (NmTmAdd tm1 tm2)        = TmAdd (rmNames c tm1) (rmNames c tm2)

prettyTerm :: Term -> String
prettyTerm (TmVar str n)          = "x@" ++ show n
prettyTerm (TmInt n)              = show n
prettyTerm (TmApp tm1 tm2)        = (prettyTerm tm1) ++ " " ++ (prettyTerm tm2)
prettyTerm (TmAbs str tm)         = "λ " ++ prettyTerm tm
prettyTerm (TmAdd tm1 tm2)        = (prettyTerm tm1) ++ " + " ++  (prettyTerm tm2)

shift :: Integer -> Term -> Term
shift n tm =
    let walk c (TmVar str n1)
          | n1 >= c   = TmVar str (n1 + n)
          | otherwise = TmVar str n1
        walk c (TmAbs idt tm)  = TmAbs idt $ walk (succ c) tm
        walk c (TmApp tm1 tm2) = TmApp (walk c tm1) (walk c tm2)
        walk c (TmAdd tm1 tm2) = TmAdd (walk c tm1) (walk c tm2)
        walk _ x               = x
    in walk 0 tm

subst' :: Integer -> Term -> Term -> Term
subst' j s t =
  let walk c term
        = case term of
            TmVar str x   -> if x == j + c
                            then shift c s
                            else TmVar str x
            TmAbs x t1    -> TmAbs x (walk (c + 1) t1)
            TmApp t1 t2   -> TmApp (walk c t1) (walk c t2)
            TmAdd tm1 tm2 -> TmAdd (walk c tm1) (walk c tm2)
            tm            -> tm
  in walk 0 t

subst :: Term -> Term -> Term
subst s t = shift (-1) (subst' 0 (shift 1 s) t)

evalStep :: Context -> Term -> Result
evalStep c (TmAdd (TmInt n1) (TmInt n2)) = Step $ TmInt $ n1 + n2
evalStep c (TmAdd tm1 tm2)               | isVal tm1 = Step $ tm1 `TmAdd` eval tm2
                                         | isVal tm2 = Step $ eval tm1 `TmAdd` tm2
evalStep c (TmApp (TmAbs id tm1) tm2)    | isVal tm2 = Step $ subst tm2 tm1
                                         | otherwise = Step $ TmApp tm1 (extractRes $ evalStep c tm2)
evalStep c (TmApp tm1 tm2)               = Step $ TmApp (extractRes $ evalStep c tm1) tm2
evalStep _ a                             = NoRuleApplies

extractRes :: Result -> Term
extractRes (Step tm) = tm

eval :: Term -> Term
eval t = let mt = evalStep [] t in
            case mt of
                NoRuleApplies -> t
                Step t' -> eval t'

isVal :: Term -> Bool
isVal (TmInt _)   = True
isVal (TmAbs _ _) = True
isVal _           = False
