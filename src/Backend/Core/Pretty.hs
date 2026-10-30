{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Core.Pretty where

import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Relude hiding (Text, Type)

import Backend.Core.Types
import Backend.Types
import Names

import Origin qualified

prettyCore :: Program -> Text
prettyCore = renderStrict . layoutPretty defaultLayoutOptions . pProgram

pType :: Type -> Doc ann
pType = \case
    Void -> "void"
    Unit -> "()"
    String -> "string"
    Char -> "char"
    Int32 -> "int32"
    Int64 -> "int64"
    Float -> "double"
    Bool -> "bool"
    Mut ty -> pType ty <> "?"
    I n -> "i" <> show n
    TyCon name -> pretty name.name
    PointerType ty -> pType ty <> "*"
    OpaquePointer -> "ptr"
    StructType tys -> braces (concatWith (surround (comma <> space)) (fmap pType tys))
    ArrayType size ty -> brackets $ show size <+> "x" <+> pType ty
    TyFun args ret ->
        sep
            [ "fn" <> parens (hcat (punctuate comma (fmap pType args)))
            , "->"
            , pType ret
            ]

pLit :: Literal -> Doc ann
pLit = \case
    IntLit lit -> show lit
    DoubleLit lit -> show lit
    CharLit lit -> show lit
    BoolLit True -> "true"
    BoolLit False -> "false"
    UnitLit -> "()"
    NullLit -> "null"

pBinOp :: BinOp -> Doc ann
pBinOp = \case
    MulOp -> "*"
    DivOp -> "/"
    AddOp -> "+"
    SubOp -> "-"
    ModOp -> "%"
    OrOp -> "||"
    AndOp -> "&&"
    LtOp -> "<"
    GtOp -> ">"
    LteOp -> "<="
    GteOp -> ">="
    EqOp -> "=="
    NeqOp -> "!="

pPrefixOp :: PrefixOp -> Doc ann
pPrefixOp = \case
    Not -> "!"
    Neg -> "-"
    Ref -> "&"
    Deref -> "*"

pProgram :: Program -> Doc ann
pProgram (Program mods) = concatWith (surround (hardline <> hardline)) (fmap pModule mods)

pModule :: Module -> Doc ann
pModule (Module _ defs) = hcat (punctuate (hardline <> hardline) (fmap pDef defs))

pDef :: Def -> Doc ann
pDef (Decl namespace ty name tys) =
    "declare"
        <+> pType ty
        <+> pretty namespace
        <> "."
        <> pretty name.name
        <> parens (concatWith (surround (comma <> space)) (fmap pType tys))
pDef (TypeSyn name ty) = "type" <+> pretty name.name <+> "=" <+> pType ty
pDef (Con index name ty arguments) =
    show index
        <> "#"
        <> pretty name.name
        <> parens (concatWith (surround (comma <> space)) $ fmap pType arguments)
        <> ":"
        <+> pType ty
pDef (StaticString name ty str) = "const" <+> pretty name.name <> ":" <+> pType ty <+> "=" <+> pretty str
pDef (Main namespace exprs) = pDef (Fn Origin.Function (EName namespace (Ident "main") (Ident "main")) [] Unit exprs)
pDef (Fn _ name args typ exprs) =
    concatWith
        (<+>)
        [ "def"
        , pIdent name.name
        , parens (cat (punctuate comma (fmap pArg args)))
        , "->"
        , pType typ
        , pBlock exprs
        ]

pBlock :: Block -> Doc ann
pBlock (Block statements) =
    "{"
        <> hardline
        <> indent 4 (hcat $ punctuate hardline $ fmap pStatement statements)
        <> hardline
        <> "}"

pStatement :: Statement -> Doc ann
pStatement stmt = case stmt of
    Match scrutinee arms catch ->
        "match"
            <+> pExpr scrutinee
            <+> braces (hardline <> prettyArms arms <> hardline <> pCatch catch <> hardline)
    Let name ty expr -> "let" <+> pIdent name.name <> ":" <+> pType ty <+> maybe emptyDoc (\e -> "=" <+> pExpr e) expr
    Ass name ty expr -> pIdent name.name <> ":" <+> pType ty <+> "=" <+> pExpr expr
    DerefAss name ty expr -> "*" <> pIdent name.name <> ":" <+> pType ty <+> "=" <+> pExpr expr
    Return expr -> "return" <+> pExpr expr
    Break -> "break"
    If cond true mbfalse ->
        let iff =
                "if"
                    <+> pExpr cond
                    <+> "{"
                    <> hardline
                    <> pBlock true
                    <> hardline
                    <> "}"
            els e =
                "else"
                    <+> "{"
                    <> hardline
                    <> pBlock e
                    <> hardline
                    <> "}"
         in case mbfalse of
                Nothing -> iff
                Just false -> iff <+> els false
    While cond block ->
        "while"
            <+> pExpr cond
            <+> "{"
            <> hardline
            <> pBlock block
            <> hardline
            <> "}"
    Expr expr -> pExpr expr

pIdent :: Ident -> Doc ann
pIdent (Ident name) = pretty name

pArg :: Arg -> Doc ann
pArg (EnvArg namespace ty) = "env:" <+> pType ty
pArg (Arg name ty) = pIdent name.name <> ":" <+> pType ty

pExpr :: TyExpr -> Doc ann
pExpr (Typed _ expr) = go expr
  where
    go = \case
        Constant lit -> pLit lit
        Var _ ident -> pIdent ident.name
        BinOp l op r -> pExpr l <+> pBinOp op <+> pExpr r
        PrefixOp op expr -> pPrefixOp op <> pExpr expr
        App l rs -> pExpr l <> parens (hcat (punctuate comma $ fmap pExpr rs))
        Closure fun freeVars ->
            braces
                ( pExpr fun
                    <> ","
                    <+> brackets (concatWith (surround (comma <> space)) (fmap pExpr freeVars))
                )
        StructIndexing expr n ->
            "get" <> parens (pExpr expr <> "," <+> show n)
        ExtractFree bindName envName index ->
            "let" <+> pretty bindName.name <+> "=" <+> pretty envName.name <> brackets (show index)
        ToStderrExit txt -> "exit" <> parens (pretty txt.name)

pCatch :: Catch -> Doc ann
pCatch (Catch name block) = pretty name.name <+> "=>" <+> pBlock block

prettyArms :: [MatchArm] -> Doc ann
prettyArms = concatWith (surround hardline) . fmap prettyArm

prettyArm :: MatchArm -> Doc ann
prettyArm (MatchArm pat block) = prettyPat pat <+> "=>" <+> pBlock block

prettyShowPat :: Pattern -> Text
prettyShowPat = renderStrict . layoutPretty defaultLayoutOptions . prettyPat

prettyPat :: Pattern -> Doc ann
prettyPat = \case
    PCon name args ->
        pretty name <> parens (concatWith (surround (comma <> space)) (fmap (pretty . (.name) . fst) args))
