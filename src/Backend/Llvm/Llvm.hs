{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Llvm.Llvm where

import Control.Lens.Getter (view)
import Control.Lens.Setter (locally)
import Control.Monad.Extra (concatMapM)
import Data.Tuple.Extra (fst3, snd3)
import Relude hiding (Type, and, div, exitFailure, null, or, rem)

import Data.List.NonEmpty qualified as NonEmpty

import Backend.Llvm.Monad
import Backend.Llvm.Prelude (exitFailure, printString)
import Backend.Llvm.Types
import Backend.Types
import Impossible (__IMPOSSIBLE__)
import Names (ExplicitName (EName, name), Ident (..))
import Utils (mapWithIndexM)

import Backend.Core.Types qualified as Core
import Origin qualified

assemble :: Core.Program -> NonEmpty Module
assemble (Core.Program mods) = fmap assembleModule mods

assembleModule :: Core.Module -> Module
assembleModule (Core.Module namespace defs) =
    Module
        . sortBy (comparing Down)
        <$> runAssembler namespace
        $ concatMapM assembleDecl defs

assembleDecl :: Core.Def -> IRBuilder [Definition]
assembleDecl (Core.Decl _namespace ty name args) = pure [Declare ty name args NoEllipsis]
assembleDecl (Core.StaticString name ty text) = pure [GlobalString name ty text]
assembleDecl (Core.Main namespace block) = do
    clearBlocks
    mapM_ assembleExpr block
    emitBlock
    pure . Function Origin.Function (EName namespace "main" "main") [] (I 1) <$> getBlocks
assembleDecl (Core.Fn origin name arguments returnType block) = do
    clearBlocks
    args <- mapM assembleArg arguments
    mapM_ assembleExpr block
    emitBlock
    pure . Function origin name args returnType <$> getBlocks
assembleDecl (Core.TypeSyn name ty) = pure [TypeDefinition name ty]
assembleDecl (Core.Con index name ty tyArgs) = do
    clearBlocks
    assembleCon index name ty tyArgs

assembleCon :: Int -> ExplicitName -> Type -> [Type] -> IRBuilder [Definition]
assembleCon index name ty = \case
    [] -> do
        name' <- fresh
        alloced <- alloca name' ty
        store (struct [LInt Int64 (fromIntegral index)]) alloced
        loaded <- load ty alloced
        ret loaded
        emitBlock
        blocks <- getBlocks
        pure [Function Origin.ConstructorFn name [] ty blocks]
    tys -> do
        -- NOTE: Can we just inline `mkName`??
        let constructorFun = name {name = Ident "mk" <> name.name}
        operands <- mapM (\ty -> fmap (LocalReference ty) fresh) tys
        let retty = getReturnType ty
        name' <- fresh
        alloced <- alloca name' retty
        tag <- gep alloced [i32 @Integer 0, i32 @Integer 0]
        store (i64 index) tag
        void $ flip mapWithIndexM operands $ \index argument -> do
            value <- gep alloced [i32 @Integer 0, i32 @Integer 1, i32 @Integer index]
            -- TODO: Do one malloc for all arguments?
            malloced <- malloc (ptr (typeOf argument)) (i64 (sizeOf (typeOf argument)))
            -- TODO(sebsel): Is this GEP needed?
            mallocedPtr <- gep malloced [i32 @Integer 0]
            store argument mallocedPtr
            store malloced value
        struct <- load retty alloced
        ret struct
        emitBlock
        blocks <- getBlocks
        pure
            [ Function
                Origin.ConstructorFn
                constructorFun
                operands
                retty
                blocks
            , Function
                Origin.Function
                name
                []
                ty
                [BasicBlock entryLabel [] (Nameless $ Ret $ Just (global ty constructorFun))]
            ]

assembleArg :: Core.Arg -> IRBuilder Operand
assembleArg (Core.EnvArg namespace ty) = do
    pure $ LocalReference ty (EName namespace (Ident "env") (Ident "env"))
assembleArg (Core.Arg name ty) = do
    let arg = LocalReference ty (name {name = mkArgName name.name})
    fakeArg <- alloca name ty
    store arg fakeArg
    pure arg

mkArgName :: Ident -> Ident
mkArgName (Ident name) = Ident $ name <> ".arg"

assembleLoadExpr :: Core.TyExpr -> IRBuilder Operand
assembleLoadExpr expr = do
    operand <- assembleExpr expr
    case operand of
        LocalReference {} -> load (Core.typeOf expr) operand
        ConstantOperand {} -> pure operand

assembleExpr :: Core.TyExpr -> IRBuilder Operand
assembleExpr (Core.Typed taggedType expr) =
    case expr of
        Core.Constant lit -> comment "Lit expression" >> assembleLit lit
        Core.Var binding name -> do
            comment $ "Var expression : " <> show binding
            case binding of
                Core.Constructor ->
                    case taggedType of
                        TyFun _ _ -> call taggedType (global taggedType name) []
                        _ -> call taggedType (global taggedType name) []
                Core.Free -> pure $ LocalReference (ptr taggedType) name
                Core.Bound -> pure $ LocalReference (ptr taggedType) name
                Core.Function -> pure $ global taggedType name
                Core.GlobalConstant -> gep (global (ptr taggedType) name) [i32 @Integer 0]
                Core.Argument -> pure $ LocalReference taggedType name
                Core.Lambda -> pure $ LocalReference (ptr taggedType) name
        Core.BinOp leftExpr operator rightExpr -> do
            comment "BinOp expression"
            left <- assembleLoadExpr leftExpr
            right <- assembleLoadExpr rightExpr
            llvmBinOp operator taggedType left right
        Core.PrefixOp Core.Deref expr -> do
            comment "Deref"
            assembleLoadExpr expr
        Core.PrefixOp Core.Ref expr -> do
            comment "Ref"
            e <- assembleExpr expr
            name <- fresh
            operand <- alloca name (ptr taggedType)
            store e operand
            comment "Ref over"
            pure operand
        Core.PrefixOp Core.Not expr -> do
            comment "PrefixOp expression"
            expr <- assembleLoadExpr expr
            eq Bool (ConstantOperand (LBool taggedType False)) expr
        Core.PrefixOp Core.Neg expr -> do
            comment "PrefixOp expression"
            expr <- assembleLoadExpr expr
            sub taggedType (ConstantOperand (LInt taggedType 0)) expr
        Core.App appExpr argExprs -> do
            comment "App expression"
            app <- assembleLoadExpr appExpr
            args <- mapM assembleLoadExpr argExprs
            call taggedType app args
        Core.Let name varType Nothing -> comment "Let expression" >> alloca name varType
        Core.Let name varType (Just expr) -> do
            comment "Let expression"
            decl <- alloca name varType
            expr <- assembleLoadExpr expr
            store expr decl
            pure decl
        Core.DerefAss name varType expr -> do
            comment "DerefAss"
            let operand = LocalReference (ptr varType) name
            value <- assembleLoadExpr expr
            mem <- load varType operand
            store value mem
            comment "DerefAss over"
            unit
        Core.Ass name varType expr -> do
            comment "Ass expression"
            expr <- assembleLoadExpr expr
            let operand = LocalReference (ptr varType) name
            store expr operand
            unit
        -- FIXME: Rethink terminators
        Core.Return expr -> do
            comment "Return expression"
            expr <- assembleLoadExpr expr
            ret expr
            unit
        Core.Break -> do
            comment "Break expression"
            lbl <- view breakLabel
            jump lbl
            unit
        Core.If condition trueBlk falseBlk -> do
            comment "If expression"
            true <- mkLabel "if_true"
            false <- mkLabel "if_false"
            end <- mkLabel "if_cont"
            condition <- assembleLoadExpr condition
            br condition true (maybe end (const false) falseBlk)
            block true
            mapM_ assembleExpr trueBlk
            jump end
            block false
            maybe (pure ()) (mapM_ assembleExpr) falseBlk
            jump end
            block end
            unit -- hacky
        Core.While condition blk -> do
            comment "While expression"
            start <- mkLabel "while_start"
            continue <- mkLabel "while_continue"
            exit <- mkLabel "while_exit"
            jump continue
            block continue
            expr <- assembleLoadExpr condition
            br expr start exit
            block start
            locally breakLabel (const exit) $ mapM_ assembleExpr blk
            jump continue
            block exit
            unit
        Core.Closure fun env -> do
            comment "Closure expression"
            name <- fresh
            mem <- case env of
                [] -> pure $ null (ptr opaquePtr)
                _ -> do
                    mem <- malloc (ptr opaquePtr) (i64 (length env * 8))
                    forM_ (zip [0 ..] env) $ \(index, Core.Typed ty expr) -> do
                        gepOperand <- gep mem [i32 @Integer index]
                        alloced <- malloc (ptr ty) (i32 $ sizeOf ty)
                        operand <- assembleExpr (Core.Typed ty expr)
                        store operand alloced
                        store alloced gepOperand
                    pure mem
            declaration <- alloca name taggedType
            functionPointer <- gep declaration [i32 @Integer 0, i32 @Integer 0]
            environmentPointer <- gep declaration [i32 @Integer 0, i32 @Integer 1]
            functionOperand <- assembleExpr fun
            store functionOperand functionPointer
            store mem environmentPointer
            load taggedType declaration
        Core.StructIndexing expr n -> do
            comment "StructIndexing expression"
            operand <- assembleExpr expr
            extractValue Nothing operand [fromInteger n]
        Core.ExtractFree bindName envName index -> do
            comment "ExtractFree expression"
            operand <- gep (localRef (ptr opaquePtr) envName) [i32 index]
            operand <- gep operand [i32 @Integer 0]
            operand <- load (ptr taggedType) operand
            operand <- load taggedType operand
            variable <- alloca bindName taggedType
            store operand variable
            pure variable
        Core.Match scrutinee@(Core.Typed _ _) matchArms (Core.Catch _name catchExpr) -> do
            comment "Match expression"
            resultOperand <- fresh
            result <- alloca resultOperand taggedType
            scrutOperand <- assembleLoadExpr scrutinee
            tag <- extractValue (Just Int64) scrutOperand [0]
            doneLbl <- mkLabel "Done"
            catchLbl <- mkLabel "Catch"
            switchCases <- forM matchArms
                $ \(Core.MatchArm pat _) ->
                    (LInt Int64 (indexOf pat),)
                        <$> mkLabel ("Case_" <> show (indexOf pat))
            switch tag catchLbl switchCases
            forM_ (zip switchCases matchArms)
                $ \((constructorTag, lbl), Core.MatchArm (Core.PCon _ vars) body) -> do
                    block lbl
                    comment ("Constructor tag: " <> show constructorTag)
                    forM_ (zip [0 ..] vars) $ \(index, (name, ty)) -> do
                        comment "var"
                        var <- alloca name ty
                        ptr <- extractValue (Just (ptr ty)) scrutOperand [1, index]
                        ptr <- gep ptr [i32 @Int 0]
                        val <- load ty ptr
                        store val var
                    operand <- NonEmpty.last <$> mapM assembleLoadExpr body
                    comment "storing into result"
                    store operand result
                    jump doneLbl
            () <- do
                block catchLbl
                comment "catchExpr"
                comment (show catchExpr)
                mapM_ assembleExpr catchExpr
                unreachable
            block doneLbl
            pure result
        Core.ToStderrExit var -> do
            void
                $ call
                    (I 1)
                    (global (TyFun [opaquePtr, ptr Char] Unit) (fst $ fst3 printString))
                    [constant (LNull opaquePtr), global opaquePtr var]
            void
                $ call
                    (I 1)
                    (global (TyFun [] Void) (fst $ fst3 exitFailure))
                    []
            pure (poison taggedType)

indexOf :: Core.Pattern -> Integer
indexOf = \case
    Core.PCon n _ -> fromIntegral n

assembleLit :: (Monad m) => Core.Literal -> m Operand
assembleLit = \case
    Core.IntLit int -> pure $ ConstantOperand (LInt Int64 int)
    Core.DoubleLit double -> pure $ ConstantOperand (LDouble Float double)
    Core.CharLit char -> pure $ ConstantOperand (LChar Char char)
    Core.BoolLit bool -> pure $ ConstantOperand (LBool Bool bool)
    Core.UnitLit -> pure $ ConstantOperand LUnit
    Core.NullLit -> pure $ ConstantOperand (LNull (PointerType Void))

unit :: (Monad m) => m Operand
unit = pure $ ConstantOperand LUnit

llvmBinOp :: Core.BinOp -> (Type -> Operand -> Operand -> IRBuilder Operand)
llvmBinOp op = case op of
    Core.MulOp -> mul
    Core.DivOp -> div
    Core.AddOp -> add
    Core.SubOp -> sub
    Core.ModOp -> rem
    Core.LtOp -> lt
    Core.OrOp -> or
    Core.AndOp -> and
    Core.GtOp -> gt
    Core.LteOp -> le
    Core.GteOp -> ge
    Core.EqOp -> eq
    Core.NeqOp -> neq

llvmLit :: Type -> Core.Literal -> Constant
llvmLit ty = \case
    Core.IntLit int -> LInt ty int
    Core.DoubleLit double -> LDouble ty double
    Core.CharLit char -> LChar ty char
    Core.BoolLit bool -> LBool ty bool
    Core.UnitLit -> LUnit
    Core.NullLit -> LNull ty

i32 :: (Integral a) => a -> Operand
i32 = ConstantOperand . LInt Int32 . fromIntegral

i64 :: (Integral a) => a -> Operand
i64 = ConstantOperand . LInt Int64 . fromIntegral

derefType :: Type -> Type
derefType (PointerType ty) = ty
derefType _ = __IMPOSSIBLE__

getReturnType :: Type -> Type
getReturnType = \case
    TyFun _ ty -> ty
    _ -> __IMPOSSIBLE__
