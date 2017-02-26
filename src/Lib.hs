module Lib where

import AbsLambda
import Data.List

data Binding = NameBind deriving Eq
type Context = [(String,Binding)]
data Term
    = TmVar String Int
    | TmInt Integer
    | TmApp Term Term
    | TmAbs Ident Term
    | TmAdd Term Term
  deriving (Eq, Ord, Show, Read)

rmNames :: Context -> NmTerm -> Term
rmNames c (NmTmVar (Ident str))    = case (elemIndex (str,NameBind) c) of
                                        Just a  -> TmVar str a
                                        Nothing -> error "Undefined"
rmNames _ (NmTmInt n)              = TmInt n
rmNames c (NmTmApp tm1 tm2)        = TmApp (rmNames c tm1) (rmNames c tm2)
rmNames c (NmTmAbs (Ident str) tm) = let c' = (str,NameBind):c in TmAbs (Ident str) $ rmNames c' tm
rmNames c (NmTmAdd tm1 tm2)        = TmAdd (rmNames c tm1) (rmNames c tm2)
