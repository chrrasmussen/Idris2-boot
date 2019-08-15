module Compiler.Rust.RustExpr

import Control.Monad.State
import Data.SortedMap


%default covering


public export
data RustUN = UN String

public export
data RustMN = MN Nat

public export
data RustConstant : Type where
  CInt : Int -> RustConstant
  CInteger : Integer -> RustConstant
  CDouble : Double -> RustConstant
  CChar : Char -> RustConstant
  CStr : String -> RustConstant
  CWorld : RustConstant

public export
data RustType = TInt | TInteger | TDouble | TChar | TStr

mutual
  public export
  data RustExpr : Type where
    PrimVal : RustConstant -> RustExpr
    RefUN : RustUN -> RustExpr
    RefMN : RustMN -> RustExpr
    Let : RustMN -> RustExpr -> RustExpr -> RustExpr
    Lam : RustMN -> RustExpr -> RustExpr
    App : RustExpr -> List RustExpr -> RustExpr
    Con : Int -> List RustExpr -> RustExpr
    BinOp : RustType -> String -> RustExpr -> RustExpr -> RustExpr
    ConCase : RustExpr -> List RustConAlt -> Maybe RustExpr -> RustExpr
    ConstCase : RustExpr -> List RustConstAlt -> Maybe RustExpr -> RustExpr
    Erased : RustExpr
    Crash : String -> RustExpr

  public export
  data RustConAlt : Type where
    MkConAlt : (tag : Int) -> (args : List RustMN) -> RustExpr -> RustConAlt

  public export
  data RustConstAlt : Type where
    MkConstAlt : RustConstant -> RustExpr -> RustConstAlt

public export
data RustDecl : Type where
  MkFun : RustUN -> List RustMN -> RustExpr -> RustDecl



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

genRustUN : RustUN -> String
genRustUN (UN x) = x

genRustMN : RustMN -> String
genRustMN (MN x) = "v" ++ show x

genRustMNIndex : RustMN -> Maybe Nat -> String
genRustMNIndex mn Nothing = genRustMN mn
genRustMNIndex mn (Just index) = genRustMN mn ++ "_" ++ show index

genConstant : RustConstant -> String
genConstant (CInt x) = "Int(" ++ show x ++ ")"
genConstant (CInteger x) = "Int(" ++ show x ++ ")"
--genConstant (CInteger x) = "Integer(BigInt::from_str(\"" ++ show x ++ "\").unwrap())" -- TODO: `Integer` solution
genConstant (CDouble x) = "Double(" ++ show x ++ "f64)"
genConstant (CChar x) = "Char('\\u{" ++ toHex x ++ "}')"
  where
    toHex : Char -> String
    toHex c = substr 2 6 (b32ToHexString (fromInteger (cast (ord c))))
genConstant (CStr x) = "Str(\"" ++ x ++ "\".to_string())" -- TODO: Does not handle Unicode characters
genConstant CWorld = "World"

genVariableClones : Nat -> RustMN -> String -> String
genVariableClones Z name scope = scope
genVariableClones (S k) name scope =
  wrapLet (genRustMN name ++ "_" ++ show k) (wrapClone (genRustMN name)) (genVariableClones k name scope)

genArgsClones : List RustMN -> SortedMap Nat Nat -> String -> String
genArgsClones [] usedIds scope = scope
genArgsClones (n@(MN x) :: xs) usedIds scope =
  let Just cloneCount = SortedMap.lookup x usedIds
    | Nothing => genArgsClones xs usedIds scope
  in genVariableClones cloneCount n scope

repeatClones : List Nat -> SortedMap Nat Nat -> List (String, String)
repeatClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard (n `elem` keepIds)
  index <- [0..count]
  let varName = genRustMNIndex (MN n) (if count >= 1 then Just (index `minus` 1) else Nothing)
  pure (varName, varName)

freshClones : List Nat -> SortedMap Nat Nat -> List (String, String)
freshClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard $ (n `elem` keepIds) && (count >= 1)
  index <- [0..(count `minus` 1)]
  pure (genRustMN (MN n), genRustMNIndex (MN n) (Just index))

genClones : List (String, String) -> String -> String
genClones [] scope = scope
genClones ((from, to) :: xs) scope =
  wrapLet to (wrapClone from) (genClones xs scope)

deleteArgs : List RustMN -> SortedMap Nat Nat -> SortedMap Nat Nat
deleteArgs [] usedIds = usedIds
deleteArgs ((MN x) :: xs) usedIds = deleteArgs xs (SortedMap.delete x usedIds)

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
    genAssignment : (Nat, RustMN) -> String
    genAssignment (index, name) =
      "let " ++ genRustMN name ++ " = Arc::clone(&args[" ++ show index ++ "])"

  genConstAlt : RustConstAlt -> State (SortedMap Nat Nat) String
  genConstAlt (MkConstAlt constant scope) =
    pure $ genConstant constant ++ " => { " ++ !(genExpr scope) ++ " }"

  genAltDef : Maybe RustExpr -> State (SortedMap Nat Nat) (List String)
  genAltDef Nothing = pure $ []
  genAltDef (Just def) =
    pure $ ["_ => " ++ !(genExpr def)]

  genExpr : RustExpr -> State (SortedMap Nat Nat) String
  genExpr (PrimVal val) = pure $ "Arc::new(" ++ genConstant val ++ ")"
  genExpr (RefUN n) = pure $ genRustUN n
  genExpr (RefMN n@(MN x)) = do
    usedIds <- get
    let Just nextIndex = SortedMap.lookup x usedIds
      | Nothing => do
        put $ insert x 0 usedIds
        pure $ wrapClone (genRustMN n)
    put $ insert x (nextIndex + 1) usedIds
    pure $ wrapClone (genRustMNIndex n (Just nextIndex))
  genExpr (Let n val scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let newScope = genArgsClones [n] usedIds innerScope
    put (deleteArgs [n] usedIds)
    pure $ wrapLet (genRustMN n) !(genExpr val) newScope
  genExpr (Lam n@(MN x) scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let previousClones = repeatClones (delete x [0..(x `minus` 1)]) usedIds
    let newClones = freshClones [x] usedIds
    let newScope = genClones (previousClones ++ newClones) innerScope
    put (deleteArgs [n] usedIds)
    pure $ "Arc::new(Lambda(Box::new(move |" ++ genRustMN n ++ ": Arc<IdrisValue>| { " ++ newScope ++ " })))"
  genExpr (App expr args) = do
    outArgs <- traverse genExpr args
    outExpr <- genExpr expr
    let calledExpr = case expr of
      RefUN (UN fnName) => fnName
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
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    pure $ "match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }"
  genExpr (ConstCase expr alts def) = do
    outExpr <- genExpr expr
    outAlts <- traverse genConstAlt alts
    outDef <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
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
    in "fn " ++ genRustUN name ++ "(" ++ showSep ", " (map showArg args) ++ ") -> Arc<IdrisValue> { " ++ newScope ++ " }\n"
  where
    showArg : RustMN -> String
    showArg arg = genRustMN arg ++ ": Arc<IdrisValue>"
