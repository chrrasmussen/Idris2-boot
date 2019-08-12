module Compiler.Rust.RustExpr

import Control.Monad.State
import Data.SortedMap


%default covering


public export
data RustName
  = UN String
  | MN Nat

public export
data IdrisValue : Type where
  LInt : Int -> IdrisValue
  LDouble : Double -> IdrisValue

public export
data RustExpr : Type where
  Value : IdrisValue -> RustExpr
  Ref : RustName -> RustExpr
  Let : RustName -> RustExpr -> RustExpr -> RustExpr
  Lam : RustName -> RustExpr -> RustExpr
  App : RustExpr -> List RustExpr -> RustExpr
  Crash : String -> RustExpr

public export
data RustDecl : Type where
  MkFun : String -> List RustName -> RustExpr -> RustDecl



showSep : String -> List String -> String
showSep sep [] = ""
showSep sep [x] = x
showSep sep (x :: xs) = x ++ sep ++ showSep sep xs

genRustName : RustName -> String
genRustName (UN x) = x
genRustName (MN x) = "v_" ++ show x

genIdrisValue : IdrisValue -> String
genIdrisValue (LInt x) = "Arc::new(Int(" ++ show x ++ "))"
genIdrisValue (LDouble x) = "Arc::new(Double(" ++ show x ++ "))"

genLet : String -> String -> String -> String
genLet n val scope =
  "{ let " ++ n ++  " = " ++ val ++ "; " ++ scope ++ " }"

genVariableClones : Nat -> RustName -> String -> String
genVariableClones Z name scope = scope
genVariableClones (S k) name scope =
  genLet (genRustName name ++ "_" ++ show k) ("Arc::clone(&" ++ genRustName name ++ ")") (genVariableClones k name scope)

genArgsClones : List RustName -> SortedMap Nat Nat -> String -> String
genArgsClones [] usedIds scope = scope
genArgsClones (varRustName@(MN x) :: xs) usedIds scope =
  let Just cloneCount = SortedMap.lookup x usedIds
    | Nothing => genArgsClones xs usedIds scope
  in genVariableClones cloneCount varRustName scope
genArgsClones (_ :: xs) usedIds scope = genArgsClones xs usedIds scope

genExpr : RustExpr -> State (SortedMap Nat Nat) String
genExpr (Value val) = pure $ genIdrisValue val
genExpr (Ref n@(UN x)) = pure $ genRustName n
genExpr (Ref n@(MN x)) = do
  usedIds <- get
  let Just nextIndex = SortedMap.lookup x usedIds
    | Nothing => do
      put $ insert x 0 usedIds
      pure $ genRustName n
  put $ insert x (nextIndex + 1) usedIds
  pure $ genRustName n ++ "_" ++ show nextIndex
genExpr (Let n val scope) = do
  innerScope <- genExpr scope
  usedIds <- get
  let newScope = genArgsClones [n] usedIds innerScope
  pure $ genLet (genRustName n) !(genExpr val) newScope
genExpr (Lam n scope) = do
  innerScope <- genExpr scope
  usedIds <- get
  let newScope = genArgsClones [n] usedIds innerScope
  pure $ "(move |" ++ genRustName n ++ ": Arc<IdrisValue>| { " ++ newScope ++ " })"
genExpr (App expr args) = do
  outArgs <- traverse genExpr args
  outExpr <- genExpr expr
  let calledExpr = case expr of
    Ref (UN fnRustName) => fnRustName
    _ => "(" ++ outExpr ++ ".unwrap_lambda())"
  pure $ calledExpr ++ "(" ++ showSep ", " outArgs ++ ")"
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
