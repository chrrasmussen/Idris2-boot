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

mutual
  public export
  data RustExpr : Type where
    PrimVal : RustConstant -> RustExpr
    Ref : RustName -> RustExpr
    Let : RustName -> RustExpr -> RustExpr -> RustExpr
    Lam : RustName -> RustExpr -> RustExpr
    App : RustExpr -> List RustExpr -> RustExpr
    Con : Int -> List RustExpr -> RustExpr
    BinOp : RustType -> String -> RustExpr -> RustExpr -> RustExpr
    ConCase : RustExpr -> List RustConAlt -> Maybe RustExpr -> RustExpr
    ConstCase : RustExpr -> List RustConstAlt -> Maybe RustExpr -> RustExpr
    Erased : RustExpr
    Crash : String -> RustExpr

  public export
  data RustConAlt : Type where
    MkConAlt : (tag : Int) -> (args : List RustName) -> RustExpr -> RustConAlt

  public export
  data RustConstAlt : Type where
    MkConstAlt : RustConstant -> RustExpr -> RustConstAlt

public export
data RustDecl : Type where
  MkFun : String -> List RustName -> RustExpr -> RustDecl



-- STRING HELPER FUNCTIONS

showSep : String -> List String -> String
showSep sep [] = ""
showSep sep [x] = x
showSep sep (x :: xs) = x ++ sep ++ showSep sep xs

wrapLet : String -> String -> String -> String
wrapLet n val scope =
  "{ let " ++ n ++  " = " ++ val ++ "; " ++ scope ++ " }"

wrapClone : String -> String
wrapClone varName = "Arc::clone(&" ++ varName ++ ")"


-- RUST EXPR TO STRING

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
genConstant (CInt x) = "Int(" ++ show x ++ ")"
genConstant (CInteger x) = "Int(" ++ show x ++ ")" -- TODO: Should be `Integer`
genConstant (CDouble x) = "Double(" ++ show x ++ "f64)"
genConstant (CChar x) = "Char('\\u{" ++ toHex x ++ "}')"
  where
    toHex : Char -> String
    toHex c = substr 2 6 (b32ToHexString (fromInteger (cast (ord c))))
genConstant (CStr x) = "Str(\"" ++ x ++ "\".to_string())" -- TODO: Does not handle Unicode characters

genVariableClones : Nat -> RustName -> String -> String
genVariableClones Z name scope = scope
genVariableClones (S k) name scope =
  wrapLet (genRustName name ++ "_" ++ show k) (wrapClone (genRustName name)) (genVariableClones k name scope)

genArgsClones : List RustName -> SortedMap Nat Nat -> String -> String
genArgsClones [] usedIds scope = scope
genArgsClones (varRustName@(MN x) :: xs) usedIds scope =
  let Just cloneCount = SortedMap.lookup x usedIds
    | Nothing => genArgsClones xs usedIds scope
  in genVariableClones cloneCount varRustName scope
genArgsClones (_ :: xs) usedIds scope = genArgsClones xs usedIds scope

repeatClones : List Nat -> SortedMap Nat Nat -> List (String, String)
repeatClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard (n `elem` keepIds)
  index <- [0..count]
  let varName = "v" ++ show n ++ (if count >= 1 then "_" ++ show (index `minus` 1) else "")
  pure (varName, varName)

freshClones : List Nat -> SortedMap Nat Nat -> List (String, String)
freshClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard $ (n `elem` keepIds) && (count >= 1)
  index <- [0..(count `minus` 1)]
  pure ("v" ++ show n, "v" ++ show n ++ "_" ++ show index)

genClones : List (String, String) -> String -> String
genClones [] scope = scope
genClones ((from, to) :: xs) scope =
  wrapLet to (wrapClone from) (genClones xs scope)

deleteArgs : List RustName -> SortedMap Nat Nat -> SortedMap Nat Nat
deleteArgs [] usedIds = usedIds
deleteArgs ((MN x) :: xs) usedIds = deleteArgs xs (SortedMap.delete x usedIds)
deleteArgs (_ :: xs) usedIds = deleteArgs xs usedIds

mutual
  genConAlt : RustConAlt -> State (SortedMap Nat Nat) String
  genConAlt (MkConAlt tag args scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let newScope = genArgsClones args usedIds innerScope
    put (deleteArgs args usedIds)
    let assignments = map genAssignment (zip [0..(length args `minus` 1)] args)
    pure $ "DataCon { tag: " ++ show tag ++ ", ref args } => { " ++ showSep "; " (assignments ++ [newScope]) ++ " }"
  where
    genAssignment : (Nat, RustName) -> String
    genAssignment (index, name) =
      "let " ++ genRustName name ++ " = Arc::clone(&args[" ++ show index ++ "])"

  genConstAlt : RustConstAlt -> State (SortedMap Nat Nat) String
  genConstAlt (MkConstAlt constant scope) =
    pure $ genConstant constant ++ " => { " ++ !(genExpr scope) ++ " }"

  genAltDef : Maybe RustExpr -> State (SortedMap Nat Nat) (List String)
  genAltDef Nothing = pure $ []
  genAltDef (Just def) =
    pure $ ["_ => " ++ !(genExpr def)]

  genExpr : RustExpr -> State (SortedMap Nat Nat) String
  genExpr (PrimVal val) = pure $ "Arc::new(" ++ genConstant val ++ ")"
  genExpr (Ref n@(UN x)) = pure $ genRustName n
  genExpr (Ref n@(MN x)) = do
    usedIds <- get
    let Just nextIndex = SortedMap.lookup x usedIds
      | Nothing => do
        put $ insert x 0 usedIds
        pure $ wrapClone (genRustName n)
    put $ insert x (nextIndex + 1) usedIds
    pure $ wrapClone (genRustName n ++ "_" ++ show nextIndex)
  genExpr (Let n val scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let newScope = genArgsClones [n] usedIds innerScope
    put (deleteArgs [n] usedIds)
    pure $ wrapLet (genRustName n) !(genExpr val) newScope
  genExpr (Lam n@(UN x) scope) =
    genExpr (Crash ("Invalid name in lambda: " ++ genRustName n))
  genExpr (Lam n@(MN x) scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let previousClones = repeatClones (delete x [0..(x `minus` 1)]) usedIds
    let newClones = freshClones [x] usedIds
    let newScope = genClones (previousClones ++ newClones) innerScope
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
  genExpr (ConCase expr alts def) = do
    outExpr <- genExpr expr
    outAlts <- traverse genConAlt alts
    outDef <- genAltDef def
    let catchAllCase = ["_ => panic!(\"No matches\")"]
    pure $ "match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }"
  genExpr (ConstCase expr alts def) = do
    outExpr <- genExpr expr
    outAlts <- traverse genConstAlt alts
    outDef <- genAltDef def
    let catchAllCase = ["_ => panic!(\"No matches\")"]
    pure $ "match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }"
  genExpr Erased = pure $ "Arc::new(Erased)"
  genExpr (Crash msg) = pure $ "{ let x: Arc<IdrisValue> = panic!(\"" ++ msg ++ "\"); x }"

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
