{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.TH (gen, ErrType (..)) where

import Control.Monad.Extra (concatMapM)
import Data.Char (toLower)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude hiding (All, First, Type)

data ErrType = All | First

gen :: ErrType -> String -> Q [Dec]
gen errType str = do
    name' <- lookupTypeName str
    name <- maybe (error $ "Data type not found " <> toText str) return name'
    reify name >>= \case
        TyConI (DataD _ _ _ _ cons _) -> do
            case errType of
                All -> concatMapM (validateCon str) cons
                First -> concatMapM (errorCon str) cons
        _ -> error "Not a type constructor"

errorCon :: String -> Con -> Q [Dec]
errorCon errName con@(NormalC nm _) = do
    ty <- reifyType nm
    let constraint = ForallT [] [AppT (AppT (ConT (mkName "MonadError")) (ConT (mkName errName))) (VarT (mkName "m"))]
    let ty' = delinearize (genLast ty)
    let args = nArgs ty'
        name = getName con
        pats = map (VarP . mkName . return) $ take args ['a' ..]
        vars = map (VarE . mkName . return) $ take args ['a' ..]
        exp =
            InfixE
                (Just $ VarE (mkName "throwError"))
                (VarE $ mkName "$")
                (Just $ foldl' AppE (ConE (mkName name)) vars)
    return
        [ SigD (mkName (small name)) (constraint ty')
        , FunD (mkName (small name)) [Clause pats (NormalB exp) []]
        ]
errorCon _ _ = error "Not a normal constructor"

validateCon :: String -> Con -> Q [Dec]
validateCon errName con@(NormalC nm _) = do
    ty <- reifyType nm
    let constraint =
            ForallT
                []
                [ AppT (AppT (ConT (mkName "MonadValidate")) (AppT ListT (ConT (mkName errName)))) (VarT (mkName "m"))
                ]
    let ty' = delinearize (genLast ty)
    let ty'' = delinearize (unitLast ty)
    let args = nArgs ty'
        name = getName con
        nameRefute = name <> "'"
        nameDispute = name
        pats = map (VarP . mkName . return) $ take args ['a' ..]
        vars = map (VarE . mkName . return) $ take args ['a' ..]
        refute =
            InfixE
                (Just $ VarE (mkName "refute"))
                (VarE $ mkName "$")
                ( Just
                    $ InfixE
                        (Just $ VarE (mkName "return"))
                        (VarE $ mkName "$")
                        (Just $ foldl' AppE (ConE (mkName name)) vars)
                )
        dispute =
            InfixE
                (Just $ VarE (mkName "dispute"))
                (VarE $ mkName "$")
                ( Just
                    $ InfixE
                        (Just $ VarE (mkName "return"))
                        (VarE $ mkName "$")
                        (Just $ foldl' AppE (ConE (mkName name)) vars)
                )
        dispute' =
            InfixE
                (Just dispute)
                (VarE $ mkName ">>")
                (Just $ AppE (VarE $ mkName "pure") (AppE (ConE (mkName "TypeX")) (ConE (mkName "UnsolvableX"))))
    return
        [ SigD (mkName (small nameRefute)) (constraint ty')
        , FunD (mkName (small nameRefute)) [Clause pats (NormalB refute) []]
        , SigD (mkName (small nameDispute)) (constraint ty'')
        , FunD (mkName (small nameDispute)) [Clause pats (NormalB dispute) []]
        ]
validateCon _ _ = error "Not a normal constructor"

small :: String -> String
small [] = []
small (x : xs) = toLower x : xs

getName :: Con -> String
getName (NormalC nm _) = case nm of
    Name occNm _ -> coerce occNm
getName _ = error "Can not generate function for non-normal constructor"

genLast :: Type -> Type
genLast = \case
    AppT t1 t2 -> AppT t1 (genLast t2)
    ConT {} -> (AppT (VarT . mkName $ "m") (VarT . mkName $ "a"))
    x -> delinearize x

unitLast :: Type -> Type
unitLast = \case
    AppT t1 t2 -> AppT t1 (unitLast t2)
    ConT {} -> (AppT (VarT . mkName $ "m") (TupleT 0))
    x -> delinearize x

delinearize :: Type -> Type
delinearize = \case
    AppT MulArrowT _ -> ArrowT
    AppT t1 t2 -> AppT (delinearize t1) (delinearize t2)
    ty -> ty

nArgs :: (Num a) => Type -> a
nArgs = \case
    AppT MulArrowT ty -> 1 + nArgs ty
    AppT ArrowT ty -> 1 + nArgs ty
    AppT l r -> nArgs l + nArgs r
    ForallT {} -> 0
    ForallVisT {} -> 0
    AppKindT {} -> 0
    SigT {} -> 0
    VarT _ -> 0
    ConT _ -> 0
    PromotedT {} -> 0
    InfixT {} -> 0
    UInfixT {} -> 0
    PromotedInfixT {} -> 0
    PromotedUInfixT {} -> 0
    ParensT _ -> 0
    TupleT _ -> 0
    UnboxedTupleT _ -> 0
    UnboxedSumT _ -> 0
    ArrowT -> 0
    MulArrowT -> 0
    EqualityT -> 0
    ListT -> 0
    PromotedTupleT _ -> 0
    PromotedNilT -> 0
    PromotedConsT -> 0
    StarT -> 0
    ConstraintT -> 0
    LitT _ -> 0
    WildCardT -> 0
    ImplicitParamT {} -> 0
