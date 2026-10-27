{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Llvm.Prelude where

import Data.String.Interpolate (i)
import Data.Text (pack)
import Relude hiding (exitFailure, exitSuccess)

import Prettyprinter qualified as Pretty

import Backend.Llvm.Lower (llvmOut)
import Backend.Llvm.Types
    ( Constant (GlobalReference),
      Definition (..),
      Ellipsis (NoEllipsis),
      Operand (ConstantOperand),
    )
import Backend.Types (Type (..))
import Names (ExplicitName (..), Ident (Ident), Namespace (Namespace))

prelude :: ([((ExplicitName, Operand), Definition)], Text)
prelude =
    second (prologue <>)
        $ foldl'
            (\(decls, acc) ((operand, decl), body) -> ((operand, decl) : decls, acc <> "\n" <> addTxt decl body))
            ([], "")
        $ [ ((a, b), c)
          | (a, b, c) <-
                [ printString
                , printChar
                , exitSuccess
                , exitFailure
                , printInt
                ]
          ]
  where
    addTxt decl Nothing = llvmOut decl
    addTxt _ (Just body) = body

prologue :: Text
prologue =
    [i|
target triple = "x86_64-pc-linux-gnu"

@#{globalUnit} = internal constant i1 0

declare void @exit(i64)

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)
|]

globalUnit :: Text
globalUnit = "$internal_global_unit"

printString :: ((ExplicitName, Operand), Definition, Maybe Text)
printString =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
        args = [OpaquePointer, PointerType (I 8)]
        retty = I 1
     in ( (ename, ConstantOperand (GlobalReference (TyFun args retty) ename))
        , Declare retty ename args NoEllipsis
        , Just
            [i|
@snl = internal constant [3 x i8] c"%s\\00"
define i1 @#{Pretty.pretty ename}(i8* %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @snl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8* %x)
	ret i1 1
}
|]
        )
  where
    name = "printString"

printChar :: ((ExplicitName, Operand), Definition, Maybe Text)
printChar =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
        args = [OpaquePointer, PointerType (I 8)]
        retty = I 1
     in ( (ename, ConstantOperand (GlobalReference (TyFun args retty) ename))
        , Declare retty ename args NoEllipsis
        , Just
            [i|
@cnl = internal constant [3 x i8] c"%c\\00"
define i1 @#{Pretty.pretty ename}(i8 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @cnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8 %x)
	ret i1 1
}
|]
        )
  where
    name = "printChar"

exitSuccess :: ((ExplicitName, Operand), Definition, Maybe Text)
exitSuccess =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
        args = []
        retty = I 1
     in ( (ename, ConstantOperand (GlobalReference (TyFun args retty) ename))
        , Declare retty ename args NoEllipsis
        , Just
            [i|
define i1 @#{Pretty.pretty ename}() {
    call void @exit(i64 0)
    ret i1 1
}
|]
        )
  where
    name = "exit_success"

exitFailure :: ((ExplicitName, Operand), Definition, Maybe Text)
exitFailure =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
        args = []
        retty = I 1
     in ( (ename, ConstantOperand (GlobalReference (TyFun args retty) ename))
        , Declare retty ename args NoEllipsis
        , Just
            [i|
define i1 @#{Pretty.pretty ename}() {
    call void @exit(i64 1)
    ret i1 1
}
|]
        )
  where
    name = "exit_failure"

printInt :: ((ExplicitName, Operand), Definition, Maybe Text)
printInt =
    let ename = EName (Namespace $ "std" :| []) (Ident name) (Ident name)
        args = [OpaquePointer, I 8]
        retty = I 1
     in ( (ename, ConstantOperand (GlobalReference (TyFun args retty) ename))
        , Declare retty ename args NoEllipsis
        , Just
            [i|
@dnl = internal constant [3 x i8] c"%d\\00"
define i1 @#{Pretty.pretty ename}(i64 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @dnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i64 %x)
	ret i1 1
}
|]
        )
  where
    name = pack "printInt"
