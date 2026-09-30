{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Llvm.Llvm where

import Backend.Core.Types qualified as Core
import Backend.Llvm.Monad
import Backend.Llvm.Prelude (exitFailure, printString)
import Backend.Llvm.Types
import Backend.Types
import Control.Lens.Getter (use, view)
import Control.Lens.Setter (locally)
import Control.Monad.Extra (concatMapM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Tuple.Extra (fst3)
import Names (Ident (..))
import Origin (Origin (..))
import Relude hiding (Type, and, div, exitFailure, null, or, rem)
import Utils (catMaybesSnd, mapWithIndexM)

assemble :: Core.Program -> Ir
assemble (Core.Program defs) =
    IrMain
        . sortBy (comparing Down)
        <$> runAssembler
        $ concatMapM assembleDecl defs

assembleDecl :: Core.Def -> IRBuilder [Decl]
assembleDecl (Core.Decl _namespace ty name args) = pure [Declare ty name args NoEllipsis]
assembleDecl (Core.StaticString name ty text) = pure [GlobalString name ty text]
assembleDecl (Core.Main block) = do
    clearInstructions
    mapM_ assembleExpr block
    pure . LlvmMain <$> extractInstructions
assembleDecl (Core.Fn origin name arguments returnType block) = do
    clearInstructions
    args <- mapM assembleArg arguments
    mapM_ assembleExpr block
    pure . Define origin name args returnType <$> extractInstructions
assembleDecl (Core.TypeSyn name ty) = pure [TypeDefinition name ty]
assembleDecl (Core.Con index name ty tyArgs) = do
    clearInstructions
    assembleCon index name ty tyArgs

assembleCon :: Int -> Ident -> Type -> Maybe [Type] -> IRBuilder [Decl]
assembleCon index name ty = \case
    Nothing -> do
        instrs <- fmap snd $ inContext $ do
            name <- fresh
            alloced <- alloca name ty
            store (struct [LInt Int64 (fromIntegral index)]) alloced
            loaded <- load ty alloced
            ret loaded
        pure [Define ConstructorFn name [] ty instrs]
    Just tys -> do
        let constructorFun = Ident "mk" <> name
        operands <- mapM (\name -> fmap (LocalReference name) fresh) tys
        let retty = getReturnType ty
        instrs <- fmap snd $ inContext $ do
            name <- fresh
            alloced <- alloca name retty
            tag <- gep alloced [i32 @Integer 0, i32 @Integer 0]
            store (i64 index) tag
            void $ flip mapWithIndexM operands $ \index argument -> do
                value <- gep alloced [i32 @Integer 0, i32 @Integer 1, i32 @Integer index]
                malloced <- malloc (ptr (typeOf argument)) (i64 @Int 10)
                mallocedPtr <- gep malloced [i32 @Integer 0]
                store argument mallocedPtr
                store malloced value
            struct <- load retty alloced
            ret struct
        pure
            [ Define
                ConstructorFn
                constructorFun
                (localRef opaquePtr (Ident "env") : operands)
                retty
                instrs
            , Define Top name [] ty [Nameless $ Ret $ global ty constructorFun]
            ]

assembleArg :: Core.Arg -> IRBuilder Operand
assembleArg (Core.EnvArg ty) = pure $ LocalReference ty (Ident "env")
assembleArg (Core.Arg name ty) = do
    let arg = LocalReference ty (mkArgName name)
    fakeArg <- alloca name ty
    store arg fakeArg
    pure arg

mkArgName :: Ident -> Ident
mkArgName (Ident name) = Ident $ name <> ".arg"

assembleExpr :: Core.TyExpr -> IRBuilder Operand
assembleExpr (Core.Typed taggedType expr) =
    case expr of
        Core.Constant lit -> comment "Lit expression" >> assembleLit lit
        Core.Var binding name -> do
            comment "Var expression"
            case binding of
                Core.Constructor ->
                    case taggedType of
                        TyFun _ _ -> do
                            fun <- call taggedType (global taggedType name) []
                            name <- fresh
                            let structType = StructType [taggedType, opaquePtr]
                            alloced <- alloca name structType
                            funPtr <- gep alloced [i32 @Integer 0, i32 @Integer 0]
                            envPtr <- gep alloced [i32 @Integer 0, i32 @Integer 1]
                            store fun funPtr
                            store (null opaquePtr) envPtr
                            load structType alloced
                        _ -> call taggedType (global taggedType name) []
                Core.Free -> load taggedType $ LocalReference (ptr taggedType) name
                Core.Bound -> load taggedType $ LocalReference (ptr taggedType) name
                Core.Toplevel -> pure $ global taggedType name
                Core.GlblConst -> do
                    -- NOTE: This will not work with global strings
                    op <- gep (global (ptr taggedType) name) [i32 @Integer 0]
                    load taggedType op
                Core.Argument -> pure $ LocalReference taggedType name
                Core.Lambda -> load taggedType $ LocalReference (ptr taggedType) name
        Core.BinOp leftExpr operator rightExpr -> do
            comment "BinOp expression"
            left <- assembleExpr leftExpr
            right <- assembleExpr rightExpr
            llvmBinOp operator taggedType left right
        Core.PrefixOp Core.Not expr -> do
            comment "PrefixOp expression"
            expr <- assembleExpr expr
            eq Bool (ConstantOperand (LBool taggedType False)) expr
        Core.PrefixOp Core.Neg expr -> do
            comment "PrefixOp expression"
            expr <- assembleExpr expr
            sub taggedType (ConstantOperand (LInt taggedType 0)) expr
        Core.App appExpr argExprs -> do
            comment "App expression"
            app <- assembleExpr appExpr
            args <- mapM assembleExpr argExprs
            call taggedType app args
        Core.Let name varType Nothing -> comment "Let expression" >> alloca name varType
        Core.Let name varType (Just expr) -> do
            comment "Let expression"
            decl <- alloca name varType
            expr <- assembleExpr expr
            store expr decl
            pure decl
        Core.Ass name varType expr -> do
            comment "Ass expression"
            expr <- assembleExpr expr
            let operand = LocalReference (ptr varType) name
            store expr operand
            pure operand
        Core.Return expr -> do
            comment "Return expression"
            expr <- assembleExpr expr
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
            end <- mkLabel "if_after"
            condition <- assembleExpr condition
            -- jump to end if no else branch exist
            br condition true (maybe end (const false) falseBlk)
            -- true case
            label true
            mapM_ assembleExpr trueBlk
            jump end

            -- false case, if there is one
            maybe
                (pure ())
                ( \falseBlk -> do
                    label false
                    mapM_ assembleExpr falseBlk
                    jump end
                )
                falseBlk

            label end

            unit
        Core.While condition blk -> do
            comment "While expression"
            start <- mkLabel "while_start"
            continue <- mkLabel "while_continue"
            exit <- mkLabel "while_exit"
            jump continue
            label continue
            expr <- assembleExpr condition
            br expr start exit
            label start
            locally breakLabel (const exit) $ mapM_ assembleExpr blk
            jump continue
            label exit
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
        -- TODO: Rewrite without phi-node, break-expressions cause some pain atm
        Core.Match scrutinee@(Core.Typed _ _) matchArms (Core.Catch name catchExpr) -> do
            comment "Match expression"
            scrutOperand <- assembleExpr scrutinee
            tag <- extractValue (Just Int64) scrutOperand [0]
            doneLbl <- mkLabel "Done"
            catchLbl <- mkLabel "Catch"
            switchCases <- forM matchArms
                $ \(Core.MatchArm pat _) ->
                    (LInt Int64 (indexOf pat),)
                        <$> mkLabel ("Case_" <> show (indexOf pat))
            switch tag catchLbl switchCases
            phiArgs <- forM (zip (fmap snd switchCases) matchArms) $ \(lbl, Core.MatchArm (Core.PCon _ vars) body) -> do
                label lbl

                forM_ (zip [0 ..] vars) $ \(index, (name, ty)) -> do
                    var <- alloca name ty
                    ptr <- extractValue (Just (ptr ty)) scrutOperand [1, index]
                    ptr <- gep ptr [i32 @Int 0]
                    val <- load ty ptr
                    store val var
                let isBreak = case NonEmpty.last body of
                        Core.Typed _ Core.Break -> True
                        _ -> False
                operand <- NonEmpty.last <$> mapM assembleExpr body
                comeFrom <-
                    if isBreak
                        then pure Nothing
                        else do
                            jump doneLbl
                            Just <$> use predBlock
                pure (operand, comeFrom)
            label catchLbl
            comment (show catchExpr)
            operand <- NonEmpty.last <$> mapM assembleExpr catchExpr
            alloced <- alloca name taggedType
            store operand alloced
            jump doneLbl
            label doneLbl

            phiArgs <- pure $ catMaybesSnd phiArgs

            phi (phiArgs <> [(operand, catchLbl)])
        Core.ToStderrExit var -> do
            void
                $ call
                    (I 1)
                    (global (TyFun [opaquePtr, ptr Char] Unit) (fst3 printString))
                    [constant (LNull opaquePtr), global opaquePtr var]
            void
                $ call
                    (I 1)
                    (global (TyFun [] Void) (fst3 exitFailure))
                    []
            pure (undef taggedType)

indexOf :: Core.Pattern -> Integer
indexOf = \case
    Core.PCon n _ -> fromIntegral n

assembleLit :: (Monad m) => Core.Constant -> m Operand
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
    Core.Mul -> mul
    Core.Div -> div
    Core.Add -> add
    Core.Sub -> sub
    Core.Mod -> rem
    Core.Lt -> lt
    Core.Or -> or
    Core.And -> and
    Core.Gt -> gt
    Core.Lte -> le
    Core.Gte -> ge
    Core.Eq -> eq
    Core.Neq -> neq

llvmLit :: Type -> Core.Constant -> Constant
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

getReturnType :: Type -> Type
getReturnType = \case
    TyFun _ ty -> ty
    ty -> error $ "can not extract return type of non-function type: " <> show ty

isFunType :: Type -> Bool
isFunType (TyFun {}) = True
isFunType _ = False
