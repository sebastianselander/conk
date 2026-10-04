{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Tc2 where

import Control.Lens (use)
import Control.Lens.Getter (uses, view, views)
import Control.Lens.Setter (assign, locally, modifying, (+=))
import Control.Lens.TH
import Control.Monad.Validate (MonadValidate, ValidateT, runValidateT)
import Control.Monad.Writer (Writer, runWriter)
import Data.Data (Data)
import Relude hiding (Any, Type, intercalate)
import Relude.Unsafe (fromJust)

import Data.Map.Strict qualified as Map

import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Substitution (Substitute (apply), Substitution)
import Frontend.Typechecker.Ctx (Ctx, defTable)
import Frontend.Typechecker.Polytype (instantiate)
import Frontend.Typechecker.Types
import Frontend.Typechecker.Unify (typeOf, unifies, unify)
import Frontend.Types
import Names (Ident, Names, Namespace, getOriginalName')
import Table (DefTable, builtIns, functions)
import Utils (listify')

import Frontend.Builtin qualified as Builtins
import Frontend.Substitution qualified as Sub
import Frontend.Typechecker.Ctx qualified as Ctx
import Table qualified as DefTable


data Env = Env
    { _variables :: Map Ident (Type Tc, SourceInfo)
    , _fresh :: Int
    }
    deriving (Show)


$(makeLenses ''Env)


newtype TcM a = Tc
    { runTc ::
        StateT Env (ReaderT Ctx (ValidateT [TcError] (Writer [TcWarning]))) a
    }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadReader Ctx
        , MonadState Env
        , MonadValidate [TcError]
        )


run :: Ctx -> Env -> TcM a -> (Either [TcError] a, [TcWarning])
run ctx env = runWriter . runValidateT . flip runReaderT ctx . flip evalStateT env . runTc


getFuns :: (Data a) => a -> [(Ident, (PolyType Tc, SourceInfo))]
getFuns = listify' f
  where
    f :: FnRn -> Maybe (Ident, (PolyType Tc, SourceInfo))
    f (Fn info name (Params _ tyvars) args returnType _) =
        let funTy = PolyType tyvars (TyFun NoExtField (fmap typeOf args) (typeOf returnType))
         in Just (name, (funTy, info))


data TypeCons = TypeCons
    { types :: [(Ident, (PolyType Tc, SourceInfo))]
    , cons :: [(Ident, (PolyType Tc, SourceInfo))]
    }


getTypesAndCons :: (Data a) => a -> TypeCons
getTypesAndCons a = TypeCons {types = listify' h a, cons = concat (listify' f a)}
  where
    h :: AdtRn -> Maybe (Ident, (PolyType Tc, SourceInfo))
    -- TODO: Add type params to polytype
    h (Adt loc name _) = Just (name, (PolyType [] $ TyCon NoExtField name, loc))

    f :: AdtRn -> Maybe [(Ident, (PolyType Tc, SourceInfo))]
    f (Adt _ name cons) =
        -- TODO: Add type params to polytype
        let returnType = TyCon NoExtField name
         in Just $ fmap (g returnType) cons
      where
        g :: TypeTc -> ConstructorRn -> (Ident, (PolyType Tc, SourceInfo))
        g returnType = \case
            EnumCons loc name -> (name, (PolyType [] returnType, loc))
            FunCons loc name argTys ->
                (name, (PolyType [] $ TyFun NoExtField (fmap typeOf argTys) returnType, loc))


check_expr ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    TypeTc -> ExprRn -> m (Substitution Tc, ExprTc)
check_expr expectedType currentExpr = undefined


infer_expr ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    ExprRn -> m (Substitution Tc, ExprTc)
infer_expr currentExpr = Ctx.push currentExpr $ case currentExpr of
    Lit info lit -> do
        let (ty, lit') = infLit lit
        pure (Sub.empty, Lit (info, ty) lit')
    Var (info, namespace, boundedness) name -> do
        (ty, info) <- case boundedness of
            Free -> lookupVar name
            Bound -> lookupVar name
            Toplevel; Imported -> do
                (polytype, info) <- lookupFun namespace name
                ty <- instantiateTc polytype
                pure (ty, info)
            Constructor -> do
                (polytype, info) <- lookupCon namespace name
                ty <- instantiateTc polytype
                pure (ty, info)
            Builtin -> do
                builtins <- view (defTable . builtIns)
                case Builtins.lookup namespace name builtins of
                    Just (ty, info) -> (,info) <$> instantiateTc ty
                    _ -> error "INTERNAL ERROR: Missing builtin"
        pure (Sub.empty, Var (info, ty, boundedness) name)
    Prefix info Neg expr -> do
        (sub, expr) <- check_expr (TyLit NoExtField Int) expr
        pure (sub, Prefix (info, TyLit NoExtField Int) Neg expr)
    Prefix info Not expr -> do
        (sub, expr) <- check_expr (TyLit NoExtField Bool) expr
        pure (sub, Prefix (info, TyLit NoExtField Bool) Neg expr)
    BinOp info left op right -> do
        let expectedType = operatorType op
        (sub1, left) <- check_expr expectedType left
        (sub2, right) <- check_expr expectedType right
        let sub3 = Sub.compose sub2 sub1
        let returnType = operatorReturnType expectedType op
        pure (sub3, apply sub3 (BinOp (info, returnType) left op right))
    App info lExpr rExprs -> undefined
    Let (info, mbty) name expr -> undefined
    Ass (info, bind, _namespace) name op expr -> undefined
    Ret info mbExpr -> undefined
    EBlock NoExtField block -> undefined
    Break info expr -> undefined
    If info condition true false -> undefined
    While info expr block -> undefined
    Loop info block -> undefined
    Lam info args body -> undefined
    Match loc scrutinee matchArms -> undefined


hasInfo :: ExprTc -> SourceInfo
hasInfo = \case
    Lit info _ -> fst info
    Var (info, _, _) _ -> info
    Prefix info _ _ -> fst info
    BinOp info _ _ _ -> fst info
    App info _ _ -> fst info
    Let info _ _ -> view stmtInfo info
    Ass (info, _) _ _ _ -> view stmtInfo info
    Ret info _ -> fst info
    EBlock _ (Block info _ _) -> fst info
    Break info _ -> fst info
    If info _ _ _ -> fst info
    While info _ _ -> fst info
    Loop info _ -> fst info
    Lam info _ _ -> fst info
    Match info _ _ -> fst info


operatorReturnType :: TypeTc -> BinOp -> TypeTc
operatorReturnType inputTy = \case
    Mul -> inputTy
    Div -> inputTy
    Add -> inputTy
    Sub -> inputTy
    Mod -> inputTy
    Or -> TyLit NoExtField Bool
    And -> TyLit NoExtField Bool
    Lt -> TyLit NoExtField Bool
    Gt -> TyLit NoExtField Bool
    Lte -> TyLit NoExtField Bool
    Gte -> TyLit NoExtField Bool
    Eq -> TyLit NoExtField Bool
    Neq -> TyLit NoExtField Bool


operatorType :: BinOp -> TypeTc
operatorType = \case
    Mul -> TyLit NoExtField Int
    Div -> TyLit NoExtField Int
    Add -> TyLit NoExtField Int
    Sub -> TyLit NoExtField Int
    Mod -> TyLit NoExtField Int
    Or -> TyLit NoExtField Bool
    And -> TyLit NoExtField Bool
    Lt -> TyLit NoExtField Int
    Gt -> TyLit NoExtField Int
    Lte -> TyLit NoExtField Int
    Gte -> TyLit NoExtField Int
    Eq -> TyLit NoExtField Int
    Neq -> TyLit NoExtField Int


infLit :: LitRn -> (TypeTc, LitTc)
infLit = \case
    IntLit info n -> (TyLit NoExtField Int, IntLit info n)
    DoubleLit info n -> (TyLit NoExtField Double, DoubleLit info n)
    StringLit info s -> (TyLit NoExtField String, StringLit info s)
    CharLit info c -> (TyLit NoExtField Char, CharLit info c)
    BoolLit info b -> (TyLit NoExtField Bool, BoolLit info b)
    UnitLit info -> (TyLit NoExtField Unit, UnitLit info)


insertVar :: (MonadState Env m) => Ident -> Type Tc -> SourceInfo -> m ()
insertVar name ty info = modifying variables (Map.insert name (ty, info))


lookupVar :: (MonadState Env m) => Ident -> m (Type Tc, SourceInfo)
lookupVar name =
    uses
        variables
        ( fromMaybe (error $ "INTERNAL ERROR: Could not find variable: " <> show name)
            . Map.lookup name
        )


lookupCon :: (MonadReader Ctx m) => Namespace -> Ident -> m (PolyType Tc, SourceInfo)
lookupCon namespace name =
    views
        Ctx.defTable
        ( fromJust
            . Map.lookup name
            . fromJust
            . Map.lookup namespace
            . view DefTable.constructors
        )


lookupFun :: (MonadReader Ctx m) => Namespace -> Ident -> m (PolyType Tc, SourceInfo)
lookupFun namespace name =
    views
        (Ctx.defTable . DefTable.functions)
        ( fromMaybe (error ("INTERNAL ERROR: Unable to find name: " <> show name))
            . (Map.lookup name <=< Map.lookup namespace)
        )


instantiateTc :: (MonadState Env m) => PolyType Tc -> m TypeTc
instantiateTc ty = do
    fr <- use fresh
    let (ty', fr') = instantiate fr ty
    assign fresh fr'
    pure ty'
