{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Frontend.Parser.Parse (parse) where

import Data.Map qualified as Map
import Data.Tuple.Extra (uncurry3)
import Frontend.Parser.Types
import Frontend.Parser.Utils
import Frontend.Types
import Relude hiding (break, span)
import Text.Megaparsec (ParseErrorBundle, (<?>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Utils (File(..))
import Names (mkNamespace, Ident (Ident), Namespace (Namespace))
import System.FilePath (dropExtension)
import Data.Maybe (fromJust)

parse' ::
    BindingPowerTable PrefixOp BinOp Void ->
    File ->
    Either (ParseErrorBundle Text CustomParseError) ProgramPar
parse' table file =
    flip runReader table
        $ P.runParserT (Program (mkNamespace (dropExtension file.name)) <$> (lexeme (return ()) *> P.many definition <* P.eof)) file.name file.content

parse :: File -> Either (ParseErrorBundle Text CustomParseError) ProgramPar
parse = parse' defaultBindingPowerTable

import_ :: Parser ImportPar
import_ = do
    gs <- spanStart
    keyword "import"
    ns <- namespace
    P.choice
        [ (\asName loc -> XImport $ ImportAs ns asName loc) <$> (keyword "as" *> identifier) <*> spanEnd gs
        , ((\symbols loc -> ImportExplicit loc ns symbols) <$> parens (commaSepEnd identifier)) <*> spanEnd gs
        , XImport . ImportQualified ns <$> spanEnd gs
        ]
        <* semicolon

namespace :: Parser Namespace
namespace = Namespace . fromList . fmap (\(Ident name) -> name) <$> lexeme (P.sepBy identifier (P.hidden $ char '.'))

datatype :: Parser AdtPar
datatype = do
    gs <- spanStart
    keyword "type"
    adtName <- lexeme upperIdentifier
    constructors <- curlyBrackets (commaSepEnd constructor)
    loc <- spanEnd gs
    pure (Adt loc adtName constructors)

constructor :: Parser ConstructorPar
constructor = do
    gs <- spanStart
    constructorName <- upperIdentifier
    argumentTypes <- P.optional (parens (commaSep type_))
    loc <- spanEnd gs
    case argumentTypes of
        Nothing -> pure $ EnumCons loc constructorName
        Just tys -> pure $ FunCons loc constructorName tys

definition :: Parser DefPar
definition = DefImport <$> import_ <|> DefFn <$> function <|> DefAdt <$> datatype

function :: Parser FnPar
function = do
    gs <- spanStart
    lexeme (keyword "def")
    name <- lexeme identifier
    args <- parens (commaSep argument)
    ty <- P.option (TyLit NoExtField Unit) (lexeme (keyword "->") *> type_)
    expressions <- block
    info <- spanEnd gs
    pure (Fn info name args ty expressions)

argument :: Parser ArgPar
argument = do
    gs <- spanStart
    name <- lexeme identifier
    lexeme (keyword ":")
    (ty, info) <- span gs type_
    pure (Arg info name ty)

type_ :: Parser TypePar
type_ = P.choice [typeAtom, pFunTy, pTyCon, parens type_] <?> "type"
  where
    pTyCon :: Parser TypePar
    pTyCon = do
        name <- lexeme upperIdentifier
        pure $ TyCon NoExtField name

    pFunTy :: Parser TypePar
    pFunTy = do
        lexeme $ keyword "fn"
        argTys <- parens (commaSep type_)
        lexeme $ keyword "->"
        TyFun NoExtField argTys <$> type_

    typeAtom :: Parser TypePar
    typeAtom =
        P.choice
            [ primtype Int "int"
            , primtype Double "double"
            , primtype Char "char"
            , primtype String "string"
            , primtype Unit "()"
            , primtype Bool "bool"
            ]

primtype :: TyLit -> Text -> Parser TypePar
primtype lit text = TyLit NoExtField lit <$ lexeme (keyword text)

-- TODO: Remove needing semicolon after if, loop, while!
pStmtColon :: Parser (Maybe StmtPar)
pStmtColon =
    lexeme
        $ P.choice
            [ Just <$> sExpression <* semicolon
            , Nothing <$ P.hidden semicolon
            ]

if_ :: Parser ExprPar
if_ = P.label "if" $ do
    gs <- spanStart
    lexeme (keyword "if")
    cond <- expression
    thenB <- block
    elseB <- P.optional (lexeme (keyword "else") *> block)
    info <- spanEnd gs
    pure (If info cond thenB elseB)

while :: Parser ExprPar
while = P.label "while" $ do
    gs <- spanStart
    lexeme (keyword "while")
    cond <- expression
    loopBody <- block
    info <- spanEnd gs
    pure (While info cond loopBody)

loop :: Parser ExprPar
loop = P.label "loop" $ do
    gs <- spanStart
    lexeme (keyword "loop")
    body <- block
    info <- spanEnd gs
    pure $ Loop info body

ret :: Parser ExprPar
ret = P.label "return" $ do
    gs <- spanStart
    lexeme (keyword "return")
    expr <- P.optional expression
    info <- spanEnd gs
    pure (Ret info expr)

break :: Parser ExprPar
break = P.label "break" $ do
    gs <- spanStart
    lexeme (keyword "break")
    expr <- P.optional expression
    info <- spanEnd gs
    pure $ Break info expr

let_ :: Parser ExprPar
let_ = P.label "let" $ do
    gs <- spanStart
    lexeme (keyword "let")
    name <- lexeme identifier
    ty <- P.optional $ lexeme (keyword ":") *> type_
    lexeme (keyword "=")
    expr <- expression
    info <- spanEnd gs
    pure (Let (info, ty) name expr)

assignment :: Parser ExprPar
assignment = P.label "assignment" $ do
    gs <- spanStart
    (name, op) <- P.try $ do
        name <- lexeme identifier
        assignOp <- lexeme assignmentOp
        pure (name, assignOp)
    info <- spanEnd gs
    Ass info name op <$> expression

match :: Parser ExprPar
match = do
    gs <- spanStart
    keyword "match"
    scrutinee <- expression
    matchArms <- curlyBrackets $ commaSepEnd pMatchArm
    loc <- spanEnd gs
    pure $ Match loc scrutinee matchArms
  where
    pMatchArm :: Parser MatchArmPar
    pMatchArm = do
        gs <- spanStart
        pat <- pPattern
        keyword "=>"
        body <- expression
        loc <- spanEnd gs
        pure $ MatchArm loc pat body
      where
        pPattern :: Parser PatternPar
        -- NOTE: Must parse wildcard before normal variable or it will be tried as a variable
        pPattern = P.choice [pPCon, pPVar]
          where
            pPCon :: Parser PatternPar
            pPCon = do
                gs <- spanStart
                name <- upperIdentifier
                arguments <- P.optional (parens (commaSep pPattern))
                loc <- spanEnd gs
                case arguments of
                    Nothing -> pure $ PEnumCon loc name
                    Just args -> pure $ PFunCon loc name args
            pPVar :: Parser PatternPar
            pPVar = do
                gs <- spanStart
                name <- identifier
                loc <- spanEnd gs
                pure $ PVar loc name

assignmentOp :: Parser AssignOp
assignmentOp =
    P.choice
        [ keyword "=" <* P.notFollowedBy (keyword "=") $> Assign
        , keyword "+=" $> AddAssign
        , keyword "-=" $> SubAssign
        , keyword "*=" $> MulAssign
        , keyword "/=" $> DivAssign
        , keyword "%=" $> ModAssign
        ]

block :: Parser BlockPar
block = uncurry3 Block <$> go
  where
    go :: Parser (SourceInfo, [StmtPar], Maybe ExprPar)
    go = do
        gs <- spanStart
        (stmts, tail) <-
            curlyBrackets
                (optionallyEndedBy pStmtColon (expression <* P.notFollowedBy semicolon) <|> return ([], Nothing))
        info <- spanEnd gs
        pure (info, catMaybes stmts, tail)

sExpression :: Parser StmtPar
sExpression = SExpr NoExtField <$> expression

lambda :: Parser ExprPar
lambda = do
    gs <- spanStart
    keyword "\\"
    args <- lexeme $ P.many lamArg
    keyword "->"
    info <- spanEnd gs
    Lam info args <$> expression
  where
    lamArg :: Parser LamArgPar
    lamArg =
        ( do
            Arg info name ty <- parens argument
            pure (LamArg (info, Just ty) name)
        )
            <|> ( do
                    gs <- spanStart
                    name <- lexeme identifier
                    info <- spanEnd gs
                    pure $ LamArg (info, Nothing) name
                )

call :: Parser ExprPar
call = do
    gs <- spanStart
    expr <- atom
    args <- P.many $ (,) <$> spanEnd gs <*> parens (commaSep expression)
    let res = foldl' (\l (info, rs) -> App info l rs) expr args
    pure res

expression :: Parser ExprPar
expression = lambda <|> assignment <|> prattExpr prefixOp infixOp call

atom :: Parser ExprPar
atom =
    P.choice
        [ match
        , if_
        , while
        , ret
        , loop
        , break
        , let_
        , EBlock NoExtField <$> block
        , literal
        , variable
        , parens expression
        ]
        <?> "expression"
  where
    variable :: Parser ExprPar
    variable = do
        gs <- spanStart
        names <- lexeme (P.sepBy1  (identifier <|> upperIdentifier) (P.hidden (char '.')))
        let name = fromJust (viaNonEmpty last names)
        let namespace = Namespace . fmap (\(Ident name) -> name) . fromList <$> viaNonEmpty init names
        let namespaceOpt = case viaNonEmpty init names of
                Nothing -> Nothing
                Just [] -> Nothing
                Just xs -> Just (Namespace (fmap (\(Ident name) -> name) (fromList xs)))

        info <- spanEnd gs
        pure (Var (info, namespaceOpt) name)

literal :: Parser ExprPar
literal =
    P.choice
        [ bool
        , number
        , char
        , string
        , unit
        ]
  where
    unit :: Parser ExprPar
    unit = do
        gs <- spanStart
        res <- UnitLit NoExtField <$ keyword "()"
        info <- spanEnd gs
        pure (Lit info res)

    bool :: Parser ExprPar
    bool = do
        gs <- spanStart
        res <- BoolLit NoExtField <$> (True <$ keyword "true" <|> False <$ keyword "false")
        info <- spanEnd gs
        pure (Lit info res)

    number :: Parser ExprPar
    number = do
        gs <- spanStart
        res <- DoubleLit NoExtField <$> P.try P.float <|> IntLit NoExtField <$> P.decimal
        info <- spanEnd gs
        pure (Lit info res)

    string :: Parser ExprPar
    string = do
        gs <- spanStart
        res <- StringLit NoExtField <$> stringLiteral
        info <- spanEnd gs
        pure (Lit info res)

    char :: Parser ExprPar
    char = do
        gs <- spanStart
        res <- CharLit NoExtField <$> charLiteral
        info <- spanEnd gs
        pure (Lit info res)

prefixOp :: Parser PrefixOp
prefixOp =
    P.choice
        [ keyword "!" $> Not
        , keyword "-" $> Neg
        ]

infixOp :: Parser BinOp
infixOp =
    P.choice
        [ keyword "+" $> Add
        , keyword "-" $> Sub
        , keyword "*" $> Mul
        , keyword "/" $> Div
        , keyword "%" $> Mod
        , keyword "<=" $> Lte
        , keyword ">=" $> Gte
        , keyword "<" $> Lt
        , keyword ">" $> Gt
        , keyword "==" $> Eq
        , keyword "!=" $> Neq
        , keyword "&&" $> And
        , keyword "||" $> Or
        ]

defaultBindingPowerTable :: BindingPowerTable PrefixOp BinOp Void
defaultBindingPowerTable =
    let _infixTable =
            Map.fromList
                [ (Add, (6, 7))
                , (Sub, (6, 7))
                , (Mul, (7, 8))
                , (Div, (7, 8))
                , (Mod, (7, 8))
                , (Lte, (4, 4))
                , (Gte, (4, 4))
                , (Lt, (4, 4))
                , (Gt, (4, 4))
                , (Eq, (4, 4))
                , (Neq, (4, 4))
                , (And, (4, 3))
                , (Or, (3, 2))
                ]
        _prefixTable = Map.fromList [(Not, 9), (Neg, 9)]
        _postfixTable = Map.empty
     in BindingPowerTable
            { _prefixTable
            , _infixTable
            , _postfixTable
            }

prattExpr :: Parser PrefixOp -> Parser BinOp -> Parser ExprPar -> Parser ExprPar
prattExpr prefixParser infixParser atomParser = exprbp 0
  where
    exprbp :: Int -> Parser ExprPar
    exprbp minBp = do
        gs <- spanStart
        l <- P.eitherP atomParser prefixParser
        case l of
            Left l -> go gs minBp l
            Right r -> do
                bp <- prefixBindingPower r
                rhs <- exprbp bp
                info <- spanEnd gs
                gs <- spanStart
                go gs minBp $ Prefix info r rhs
      where
        go :: GhostSpan Before -> Int -> ExprPar -> Parser ExprPar
        go gs minBp l = P.withRecovery (\_ -> pure l) $ do
            operator <- P.lookAhead infixParser
            (lbp, rbp) <- infixBindingPower operator
            if lbp < minBp
                then pure l
                else do
                    _ <- infixOp
                    r <- exprbp rbp
                    info <- spanEnd gs
                    go gs minBp (BinOp info l operator r)
