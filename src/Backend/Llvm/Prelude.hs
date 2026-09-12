{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.Llvm.Prelude where

import Backend.Llvm.Lower (llvmOut)
import Backend.Llvm.Types (Decl (..), Ellipsis (Ellipsis, NoEllipsis))
import Backend.Types (Type (I, OpaquePointer, PointerType, Void))
import Data.String.Interpolate (i)
import Data.Text (pack)
import Names (Ident (..))
import Relude hiding (exitFailure, exitSuccess)

prelude :: ([Decl], Text)
prelude =
    foldl'
        ( \(decls, acc) (decl, body) -> (decl : decls, acc <> "\n" <> addTxt decl body )
        )
        ([], "")
        $ fmap
            (\(_, b, c) -> (b, c))
            [ exit
            , printf
            , malloc
            , printString
            , printChar
            , exitSuccess
            , exitFailure
            , printInt
            ]

  where
    addTxt decl Nothing = llvmOut decl
    addTxt _ (Just body) = body

prologue :: Text
prologue =
    [i|
target triple = "x86_64-pc-linux-gnu"

@#{globalUnit} = internal constant i1 0
|]

exit :: (Ident, Decl, Maybe Text)
exit = (Ident name, Declare Void (Ident name) [I 64] NoEllipsis, Nothing)
  where
    name = "exit"

printf :: (Ident, Decl, Maybe Text)
printf = (Ident name, Declare (I 32) (Ident name) [OpaquePointer] Ellipsis, Nothing)
  where
    name = "printf"

malloc :: (Ident, Decl, Maybe Text)
malloc = (Ident name, Declare OpaquePointer (Ident name) [I 64] NoEllipsis, Nothing)
  where
    name = "malloc"

globalUnit :: Text
globalUnit = "internal_global_unit"

printString :: (Ident, Decl, Maybe Text)
printString =
    ( Ident name
    , Declare (I 1) (Ident name) [OpaquePointer, PointerType (I 8)] NoEllipsis
    , Just
        [i|
@snl = internal constant [3 x i8] c"%s\\00"
define i1 @#{name}(ptr %env, i8* %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @snl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8* %x)
	ret i1 1
}
|]
    )
  where
    name = "printString"

printChar :: (Ident, Decl, Maybe Text)
printChar =
    ( Ident name
    , Declare (I 1) (Ident name) [OpaquePointer, PointerType (I 8)] NoEllipsis
    , Just
        [i|
@cnl = internal constant [3 x i8] c"%c\\00"
define i1 @#{name}(ptr %env, i8 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @cnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8 %x)
	ret i1 1
}
|]
    )
  where
    name = "printChar"

exitSuccess :: (Ident, Decl, Maybe Text)
exitSuccess =
    ( Ident name
    , Declare (I 1) (Ident name) [] NoEllipsis
    , Just
        [i|
define i1 @#{name}() {
    call void @exit(i64 0)
    ret i1 1
}
|]
    )
  where
    name = "exit_success"

exitFailure :: (Ident, Decl, Maybe Text)
exitFailure =
    ( Ident name
    , Declare (I 1) (Ident name) [] NoEllipsis
    , Just
        [i|
define i1 @#{name}() {
    call void @exit(i64 1)
    ret i1 1
}
|]
    )
  where
    name = "exit_failure"

printInt :: (Ident, Decl, Maybe Text)
printInt =
    ( Ident name
    , Declare (I 1) (Ident name) [OpaquePointer, I 8] NoEllipsis
    , Just
        [i|
@dnl = internal constant [3 x i8] c"%d\\00"
define i1 @#{name}(ptr %env, i64 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @dnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i64 %x)
	ret i1 1
}
|]
    )
  where
    name = pack "printInt"
