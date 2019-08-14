module Compiler.Rust.RustExpr

import Control.Monad.State
import Data.SortedMap


%default covering


public export
data RustName
  = UN String
  | MN Nat

public export
data RustConstant : Type where
  CInt : Int -> RustConstant
  CInteger : Integer -> RustConstant
  CDouble : Double -> RustConstant
  CChar : Char -> RustConstant
  CStr : String -> RustConstant

public export
data RustType = TInt | TInteger | TDouble | TChar | TStr

public export
data RustExpr : Type where
  PrimVal : RustConstant -> RustExpr
  Ref : RustName -> RustExpr
  Let : RustName -> RustExpr -> RustExpr -> RustExpr
  Lam : RustName -> RustExpr -> RustExpr
  App : RustExpr -> List RustExpr -> RustExpr
  Con : Int -> List RustExpr -> RustExpr
  BinOp : RustType -> String -> RustExpr -> RustExpr -> RustExpr
  Erased : RustExpr
  Crash : String -> RustExpr

public export
data RustDecl : Type where
  MkFun : String -> List RustName -> RustExpr -> RustDecl



showSep : String -> List String -> String
showSep sep [] = ""
showSep sep [x] = x
showSep sep (x :: xs) = x ++ sep ++ showSep sep xs

unwrapName : RustType -> String
unwrapName TInt = "int"
unwrapName TInteger = "integer"
unwrapName TDouble = "double"
unwrapName TChar = "char"
unwrapName TStr = "str"

dataConstructor : RustType -> String
dataConstructor TInt = "Int"
dataConstructor TInteger = "Integer"
dataConstructor TDouble = "Double"
dataConstructor TChar = "Char"
dataConstructor TStr = "Str"

genRustName : RustName -> String
genRustName (UN x) = x
genRustName (MN x) = "v" ++ show x

genConstant : RustConstant -> String
genConstant (CInt x) = "Arc::new(Int(" ++ show x ++ "))"
genConstant (CInteger x) = "Arc::new(Int(" ++ show x ++ "))" -- TODO: Should be `Integer`
genConstant (CDouble x) = "Arc::new(Double(" ++ show x ++ "))"
genConstant (CChar x) = "Arc::new(Char('\\u{" ++ toHex x ++ "}'))"
  where
    toHex : Char -> String
    toHex c = substr 2 6 (b32ToHexString (fromInteger (cast (ord c))))
genConstant (CStr x) = "Arc::new(Str(\"" ++ x ++ "\".to_string()))" -- TODO: Does not handle Unicode characters

genLet : String -> String -> String -> String
genLet n val scope =
  "{ let " ++ n ++  " = " ++ val ++ "; " ++ scope ++ " }"

genClone : String -> String
genClone varName = "Arc::clone(&" ++ varName ++ ")"

genVariableClones : Nat -> RustName -> String -> String
genVariableClones Z name scope = scope
genVariableClones (S k) name scope =
  genLet (genRustName name ++ "_" ++ show k) (genClone (genRustName name)) (genVariableClones k name scope)

genArgsClones : List RustName -> SortedMap Nat Nat -> String -> String
genArgsClones [] usedIds scope = scope
genArgsClones (varRustName@(MN x) :: xs) usedIds scope =
  let Just cloneCount = SortedMap.lookup x usedIds
    | Nothing => genArgsClones xs usedIds scope
  in genVariableClones cloneCount varRustName scope
genArgsClones (_ :: xs) usedIds scope = genArgsClones xs usedIds scope

deleteArgs : List RustName -> SortedMap Nat Nat -> SortedMap Nat Nat
deleteArgs [] usedIds = usedIds
deleteArgs ((MN x) :: xs) usedIds = deleteArgs xs (SortedMap.delete x usedIds)
deleteArgs (_ :: xs) usedIds = deleteArgs xs usedIds

genExpr : RustExpr -> State (SortedMap Nat Nat) String
genExpr (PrimVal val) = pure $ genConstant val
genExpr (Ref n@(UN x)) = pure $ genRustName n
genExpr (Ref n@(MN x)) = do
  usedIds <- get
  let Just nextIndex = SortedMap.lookup x usedIds
    | Nothing => do
      put $ insert x 0 usedIds
      pure $ genClone (genRustName n)
  put $ insert x (nextIndex + 1) usedIds
  pure $ genClone (genRustName n ++ "_" ++ show nextIndex)
genExpr (Let n val scope) = do
  innerScope <- genExpr scope
  usedIds <- get
  let newScope = genArgsClones [n] usedIds innerScope
  put (deleteArgs [n] usedIds)
  pure $ genLet (genRustName n) !(genExpr val) newScope
genExpr (Lam n scope) = do
  innerScope <- genExpr scope
  usedIds <- get
  let newScope = genArgsClones [n] usedIds innerScope
  put (deleteArgs [n] usedIds)
  pure $ "Arc::new(Lambda(Box::new(move |" ++ genRustName n ++ ": Arc<IdrisValue>| { " ++ newScope ++ " })))"
genExpr (App expr args) = do
  outArgs <- traverse genExpr args
  outExpr <- genExpr expr
  let calledExpr = case expr of
    Ref (UN fnRustName) => fnRustName
    _ => "(" ++ outExpr ++ ".unwrap_lambda())"
  pure $ calledExpr ++ "(" ++ showSep ", " outArgs ++ ")"
genExpr (Con tag args) = do
  outArgs <- traverse genExpr args
  pure $ "Arc::new(DataCon { tag: " ++ show tag ++ ", args: vec![" ++ showSep ", " outArgs ++ "]})"
genExpr (BinOp ty fnName val1 val2) = do
  let callUnwrapFn = ".unwrap_" ++ unwrapName ty ++ "()"
  pure $ "Arc::new(" ++ dataConstructor ty ++ "(" ++ !(genExpr val1) ++ callUnwrapFn ++ " " ++ fnName ++ " " ++ !(genExpr val2) ++ callUnwrapFn ++ "))"
genExpr Erased = pure $ "Arc::new(Erased)"
genExpr (Crash msg) = pure $ "panic!(\"" ++ msg ++ "\")"

export
genExprNoArgs : RustExpr -> String
genExprNoArgs expr =
  let (code, _) = State.runState (genExpr expr) empty
  in code

export
genDecl : RustDecl -> String
genDecl (MkFun name args scope) =
    let (code, usedIds) = State.runState (genExpr scope) empty
    in let newScope = genArgsClones args usedIds code
    in "fn " ++ name ++ "(" ++ showSep ", " (map showArg args) ++ ") -> Arc<IdrisValue> { " ++ newScope ++ " }\n"
  where
    showArg : RustName -> String
    showArg arg = genRustName arg ++ ": Arc<IdrisValue>"
