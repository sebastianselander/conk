{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Desugar.Pretty where

import Backend.Desugar.Types
import Backend.Types
import Data.Text (Text)
import Names
import Origin (Origin (Top))
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Relude hiding (Text, Type)

prettyDesugar :: Program -> Text
prettyDesugar = renderStrict . layoutPretty defaultLayoutOptions . pProgram

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
    TyCon name -> pretty name
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

pLit :: Lit -> Doc ann
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
    Mul -> "*"
    Div -> "/"
    Add -> "+"
    Sub -> "-"
    Mod -> "%"
    Or -> "||"
    And -> "&&"
    Lt -> "<"
    Gt -> ">"
    Lte -> "<="
    Gte -> ">="
    Eq -> "=="
    Neq -> "!="

pPrefixOp :: PrefixOp -> Doc ann
pPrefixOp = \case
    Not -> "!"
    Neg -> "-"

pProgram :: Program -> Doc ann
pProgram (Program defs) = hcat (punctuate hardline (fmap pDef defs))

pDef :: Def -> Doc ann
pDef (TypeSyn name ty) = "type" <+> pretty name <+> "=" <+> pType ty
pDef (Con index name ty arguments) =
    show index
        <> "#"
        <> pretty name
        <> maybe "" (\xs -> parens (concatWith (surround (comma <> space)) $ fmap pType xs)) arguments
        <> ":"
        <+> pType ty
pDef (StaticString name ty str) = "const" <+> pretty name <> ":" <+> pType ty <+> "=" <+> pretty str
pDef (Main exprs) = pDef (Fn Top (Ident "main") [] Unit exprs)
pDef (Fn _ name args typ exprs) =
    concatWith
        (<+>)
        [ "def"
        , pIdent name
        , parens (cat (punctuate comma (fmap pArg args)))
        , "->"
        , pType typ
        , "{"
        , hardline
        , indent 4 $ hcat $ punctuate hardline (fmap pExpr exprs)
        , hardline
        , "}"
        ]

pIdent :: Ident -> Doc ann
pIdent (Ident name) = pretty name

pArg :: Arg -> Doc ann
pArg (EnvArg ty) = "env:" <+> pType ty
pArg (Arg name ty) = pIdent name <> ":" <+> pType ty

pExpr :: TyExpr -> Doc ann
pExpr (Typed _ expr) = go expr
  where
    go = \case
        Lit lit -> pLit lit
        Var _ ident -> pIdent ident
        BinOp l op r -> pExpr l <+> pBinOp op <+> pExpr r
        PrefixOp op expr -> pPrefixOp op <+> pExpr expr
        App l rs -> pExpr l <> parens (hcat (punctuate comma $ fmap pExpr rs))
        Let name ty expr -> "let" <+> pIdent name <> ":" <+> pType ty <+> maybe emptyDoc (\e -> "=" <+> pExpr e) expr
        Ass name ty expr -> pIdent name <> ":" <+> pType ty <+> "=" <+> pExpr expr
        Return expr -> "return" <+> pExpr expr
        Break -> "break"
        If cond true mbfalse ->
            let iff =
                    "if"
                        <+> pExpr cond
                        <+> "{"
                        <> hardline
                        <> indent 4 (hcat $ punctuate hardline (fmap pExpr true))
                        <> hardline
                        <> "}"
                els e =
                    "else"
                        <+> "{"
                        <> hardline
                        <> indent 4 (hcat $ punctuate hardline (fmap pExpr e))
                        <> hardline
                        <> "}"
             in case mbfalse of
                    Nothing -> iff
                    Just false -> iff <+> els false
        While cond exprs ->
            "while"
                <+> pExpr cond
                <> "{"
                <> indent 4 (hcat $ punctuate hardline (fmap pExpr exprs))
                <> "}"
        Closure fun freeVars ->
            braces
                ( pExpr fun
                    <> ","
                    <+> brackets (concatWith (surround (comma <> space)) (fmap pExpr freeVars))
                )
        StructIndexing expr n ->
            "get" <> parens (pExpr expr <> "," <+> show n)
        ExtractFree bindName envName index ->
            "let" <+> pretty bindName <+> "=" <+> pretty envName <> brackets (show index)
        Match scrutinee arms catch ->
            "match"
                <+> pExpr scrutinee
                <+> braces (hardline <> prettyArms arms <> hardline <> pCatch catch <> hardline)
        ToStderrExit txt -> "exit: " <> pretty txt

pCatch :: Catch -> Doc ann
pCatch (Catch name expr) = pretty name <+> "=>" <+> concatWith (surround (semi <> hardline)) (fmap pExpr (toList expr))

prettyArms :: [MatchArm] -> Doc ann
prettyArms = concatWith (surround hardline) . fmap prettyArm

prettyArm :: MatchArm -> Doc ann
prettyArm (MatchArm pat body) = case body of
    (x :| []) -> prettyPat pat <+> "=>" <+> pExpr x
    (x :| xs) ->
        prettyPat pat
            <+> "=>"
            <+> braces
                (hardline <> indent 4 (concatWith (surround (semi <> hardline)) (fmap pExpr (x : xs))) <> hardline)

prettyShowPat :: Pattern -> Text
prettyShowPat = renderStrict . layoutPretty defaultLayoutOptions . prettyPat

prettyPat :: Pattern -> Doc ann
prettyPat = \case
    PCon name args -> pretty name <> parens (concatWith (surround (comma <> space)) (fmap (pretty . fst) args))
