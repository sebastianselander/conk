{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend.Renamer.Pretty where

import Frontend.Renamer.Types
import Frontend.Types
import Names (Ident (..))
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
import Relude hiding (intercalate)

prettyRenamer :: (Pretty a) => a -> Text
prettyRenamer = show . Pretty.pretty

instance Pretty ProgramRn where
    pretty (Program _ defs) =
        Pretty.concatWith
            (Pretty.surround (Pretty.hardline <> Pretty.hardline))
            (fmap Pretty.pretty defs)

instance Pretty DefRn where
    pretty (DefFn fn) = Pretty.pretty fn
    pretty (DefAdt adt) = Pretty.pretty adt
    pretty (DefImport imp) = Pretty.pretty imp

instance Pretty ImportRn where
    pretty (ImportExplicit _ namespace imports) =
        "import"
            <+> Pretty.pretty namespace
            <+> Pretty.parens
                (Pretty.concatWith (Pretty.surround (Pretty.comma <> Pretty.space)) (fmap pretty imports))

instance Pretty AdtRn where
    pretty (Adt _ name cons) =
        "type"
            <+> Pretty.pretty name
            <+> Pretty.braces
                ( Pretty.hardline
                    <> Pretty.indent
                        4
                        ( Pretty.concatWith
                            (Pretty.surround Pretty.hardline)
                            (fmap (\x -> Pretty.pretty x <> Pretty.comma) cons)
                        )
                    <> Pretty.hardline
                )

instance Pretty ConstructorRn where
    pretty = \case
        EnumCons _ name -> Pretty.pretty name
        FunCons _ name types ->
            Pretty.pretty name
                <> Pretty.parens
                    ( Pretty.concatWith
                        (Pretty.surround (Pretty.comma <> Pretty.space))
                        (fmap Pretty.pretty types)
                    )

instance Pretty FnRn where
    pretty (Fn _ (Ident name) args ty block) =
        "def"
            <+> Pretty.pretty name
                <> Pretty.parens
                    ( Pretty.concatWith
                        (Pretty.surround Pretty.comma)
                        (fmap Pretty.pretty args)
                    )
            <+> "->"
            <+> Pretty.pretty ty
            <+> Pretty.pretty block

instance Pretty BlockRn where
    pretty (Block _ stmts tail) =
        Pretty.braces
            ( Pretty.hardline
                <> Pretty.indent
                    4
                    ( Pretty.concatWith
                        (Pretty.surround Pretty.hardline)
                        (fmap Pretty.pretty stmts)
                        <> maybe Pretty.emptyDoc (\x -> Pretty.hardline <> Pretty.pretty x) tail
                    )
                <> Pretty.hardline
            )

instance Pretty DataConCantHappen where
    pretty _ = error "absurd"

instance Pretty StmtRn where
    pretty (SExpr NoExtField expr) = Pretty.pretty expr <> Pretty.semi

instance Pretty ArgRn where
    pretty (Arg (_, namespace) name ty) = Pretty.pretty namespace <> "." <> Pretty.pretty name <> ":" <+> Pretty.pretty ty

instance Pretty TypeRn where
    pretty = prettyType1

prettyType1 :: TypeRn -> Doc ann
prettyType1 (TyFun _ l r) =
    "fn" <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty l))
        <+> "->"
        <+> Pretty.pretty r
prettyType1 ty = prettyType2 ty

prettyType2 :: TypeRn -> Doc ann
prettyType2 = \case
    TyCon _ name -> Pretty.pretty name
    TyLit _ tylit -> case tylit of
        Unit -> "()"
        String -> "string"
        Int -> "int"
        Double -> "double"
        Char -> "char"
        Bool -> "bool"
    ty@TyFun {} -> Pretty.parens (Pretty.pretty ty)

instance Pretty ExprRn where
    pretty = prettyExpr1

prettyExpr1 :: ExprRn -> Doc ann
prettyExpr1 (BinOp _ l Or r) = Pretty.pretty l <+> "||" <+> Pretty.pretty r
prettyExpr1 e = prettyExpr2 e

prettyExpr2 :: ExprRn -> Doc ann
prettyExpr2 (BinOp _ l And r) = Pretty.pretty l <+> "&&" <+> Pretty.pretty r
prettyExpr2 e = prettyExpr3 e

prettyExpr3 :: ExprRn -> Doc ann
prettyExpr3 (BinOp _ l Eq r) = Pretty.pretty l <+> "==" <+> Pretty.pretty r
prettyExpr3 (BinOp _ l Neq r) = Pretty.pretty l <+> "!=" <+> Pretty.pretty r
prettyExpr3 e = prettyExpr4 e

prettyExpr4 :: ExprRn -> Doc ann
prettyExpr4 (BinOp _ l Lt r) = Pretty.pretty l <+> "<" <+> Pretty.pretty r
prettyExpr4 (BinOp _ l Lte r) = Pretty.pretty l <+> "<=" <+> Pretty.pretty r
prettyExpr4 (BinOp _ l Gt r) = Pretty.pretty l <+> ">" <+> Pretty.pretty r
prettyExpr4 (BinOp _ l Gte r) = Pretty.pretty l <+> ">=" <+> Pretty.pretty r
prettyExpr4 e = prettyExpr5 e

prettyExpr5 :: ExprRn -> Doc ann
prettyExpr5 (BinOp _ l Add r) = Pretty.pretty l <+> "+" <+> Pretty.pretty r
prettyExpr5 (BinOp _ l Sub r) = Pretty.pretty l <+> "-" <+> Pretty.pretty r
prettyExpr5 e = prettyExpr6 e

prettyExpr6 :: ExprRn -> Doc ann
prettyExpr6 (BinOp _ l Mod r) = Pretty.pretty l <+> "%" <+> Pretty.pretty r
prettyExpr6 (BinOp _ l Div r) = Pretty.pretty l <+> "/" <+> Pretty.pretty r
prettyExpr6 (BinOp _ l Mul r) = Pretty.pretty l <+> "*" <+> Pretty.pretty r
prettyExpr6 (Prefix _ Not r) = "!" <+> Pretty.pretty r
prettyExpr6 (Prefix _ Neg r) = "-" <+> Pretty.pretty r
prettyExpr6 e = prettyExpr7 e

prettyExpr7 :: ExprRn -> Doc ann
prettyExpr7 e@BinOp {} = Pretty.parens (Pretty.pretty e)
prettyExpr7 e@Prefix {} = Pretty.parens (Pretty.pretty e)
prettyExpr7 (Lit _ lit) = Pretty.pretty lit
prettyExpr7 (Var (_,namespace,_) name) = Pretty.pretty namespace <> "." <> Pretty.pretty name
prettyExpr7 (App _ l rs) =
    Pretty.pretty l
        <> Pretty.parens (Pretty.concatWith (Pretty.surround Pretty.comma) (fmap Pretty.pretty rs))
prettyExpr7 (Let (_, Nothing) name e) = "let" <+> Pretty.pretty name <+> "=" <+> Pretty.pretty e
prettyExpr7 (Let (_, Just ty) name e) = "let" <+> Pretty.pretty name <> ":" <+> Pretty.pretty ty <+> "=" <+> Pretty.pretty e
prettyExpr7 (Ass _ (Ident name) op e) = Pretty.pretty name <+> Pretty.pretty op <+> Pretty.pretty e
prettyExpr7 (Ret _ Nothing) = "return"
prettyExpr7 (Ret _ (Just e)) = "return" <+> Pretty.pretty e
prettyExpr7 (EBlock _ block) = Pretty.pretty block
prettyExpr7 (Break _ Nothing) = "break"
prettyExpr7 (Break _ (Just e)) = "break" <+> Pretty.pretty e
prettyExpr7 (If _ cond thenB Nothing) = "if" <+> Pretty.pretty cond <+> Pretty.pretty thenB
prettyExpr7 (If a cond thenB (Just elseB)) =
    Pretty.pretty (If a cond thenB Nothing) <+> "else" <+> Pretty.pretty elseB
prettyExpr7 (While _ cond block) = "while" <+> Pretty.pretty cond <+> Pretty.pretty block
prettyExpr7 (Loop _ block) = "loop" <+> Pretty.pretty block
prettyExpr7 (Lam _ args body) =
    "\\" <> Pretty.concatWith (Pretty.surround Pretty.space) (fmap Pretty.pretty args)
        <+> "->"
        <+> Pretty.pretty body
prettyExpr7 (Match _ scrutinee arms) =
    "match"
        <+> Pretty.pretty scrutinee
        <+> Pretty.braces
            ( Pretty.hardline
                <> Pretty.indent
                    4
                    ( Pretty.concatWith
                        (Pretty.surround (Pretty.comma <> Pretty.hardline))
                        (fmap Pretty.pretty arms)
                    )
                <> Pretty.hardline
            )

instance Pretty MatchArmRn where
    pretty (MatchArm _ pat expr) = Pretty.pretty pat <+> "=>" <+> Pretty.pretty expr

instance Pretty PatternRn where
    pretty = \case
        PVar _ varName -> Pretty.pretty varName
        PEnumCon _ conName -> Pretty.pretty conName
        PFunCon _ conName pats ->
            Pretty.pretty conName
                <> Pretty.parens
                    ( Pretty.concatWith
                        (Pretty.surround (Pretty.comma <> Pretty.space))
                        (fmap Pretty.pretty pats)
                    )

instance Pretty LamArgRn where
    pretty (LamArg (_, Nothing, namespace) name) =
        Pretty.pretty namespace <> "." <> Pretty.pretty name
    pretty (LamArg (_, Just ty, namespace) name) =
        Pretty.parens $ Pretty.pretty namespace <> "." <> Pretty.pretty name <> ":" <+> Pretty.pretty ty

instance Pretty LitRn where
    pretty lit = case lit of
        IntLit _ l -> Relude.show l
        DoubleLit _ l -> Relude.show l
        StringLit _ l -> Relude.show l
        CharLit _ l -> Relude.show l
        BoolLit _ True -> "true"
        BoolLit _ False -> "false"
        UnitLit _ -> "()"

--
-- tcPrettyStmt :: StmtRn -> Doc ann
-- tcPrettyStmt (SExpr _ e) = prettyExpr1 e

instance Pretty AssignOp where
    pretty = \case
        AddAssign -> "+="
        SubAssign -> "-="
        MulAssign -> "*="
        DivAssign -> "/="
        ModAssign -> "%="
        Assign -> "="
