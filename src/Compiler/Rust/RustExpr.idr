module Compiler.Rust.RustExpr

import Control.Monad.State
import Data.SortedMap


%default covering


public export
data RustUN = UN String

public export
data RustMN = MN Nat

Eq RustMN where
  (MN x) == (MN y) = x == y

Ord RustMN where
  compare (MN x) (MN y) = compare x y

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
    RPrimVal : RustConstant -> RustExpr
    RRefUN : RustUN -> RustExpr
    RRefMN : RustMN -> RustExpr
    RLet : RustMN -> RustExpr -> RustExpr -> RustExpr
    RLam : RustMN -> RustExpr -> RustExpr
    RApp : RustExpr -> List RustExpr -> RustExpr
    RCon : Int -> List RustExpr -> RustExpr
    RBinOp : RustType -> String -> RustExpr -> RustExpr -> RustExpr
    RBoolBinOp : RustType -> String -> RustExpr -> RustExpr -> RustExpr
    RConCase : RustExpr -> List RustConAlt -> Maybe RustExpr -> RustExpr
    RConstCase : RustExpr -> List RustConstAlt -> Maybe RustExpr -> RustExpr
    RStrConstCase : RustExpr -> List RustConstAlt -> Maybe RustExpr -> RustExpr
    RDelay : RustExpr -> RustExpr
    RForce : RustExpr -> RustExpr
    RErased : RustExpr
    RCrash : String -> RustExpr

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

genStringLiteral : String -> String
genStringLiteral str = "\"" ++ str ++ "\"" -- TODO: Does not handle Unicode characters

genConstant : RustConstant -> String
genConstant (CInt x) = "Int(" ++ show x ++ ")"
genConstant (CInteger x) = "Int(" ++ show x ++ ")"
--genConstant (CInteger x) = "Integer(BigInt::from_str(\"" ++ show x ++ "\").unwrap())" -- TODO: `Integer` solution
genConstant (CDouble x) = "Double(" ++ show x ++ "f64)"
genConstant (CChar x) = "Char('\\u{" ++ toHex x ++ "}')"
  where
    toHex : Char -> String
    toHex c = substr 2 6 (b32ToHexString (fromInteger (cast (ord c))))
genConstant (CStr x) = "Str(" ++ genStringLiteral x ++ ".to_string())"
genConstant CWorld = "World"

repeatAllClones : SortedMap RustMN Nat -> List (String, String)
repeatAllClones usedIds = do
  (n, count) <- toList usedIds
  index <- [0..count]
  let varName = genRustMNIndex n (if count >= 1 then Just (index `minus` 1) else Nothing)
  pure (varName, varName)

repeatClones : List RustMN -> SortedMap RustMN Nat -> List (String, String)
repeatClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard (n `elem` keepIds)
  index <- [0..count]
  let varName = genRustMNIndex n (if count >= 1 then Just (index `minus` 1) else Nothing)
  pure (varName, varName)

freshClones : List RustMN -> SortedMap RustMN Nat -> List (String, String)
freshClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard $ (n `elem` keepIds) && (count >= 1)
  index <- [0..(count `minus` 1)]
  pure (genRustMN n, genRustMNIndex n (Just index))

genClones : List (String, String) -> String -> String
genClones [] scope = scope
genClones ((from, to) :: xs) scope =
  wrapLet to (wrapClone from) (genClones xs scope)

deleteArgs : List RustMN -> SortedMap RustMN Nat -> SortedMap RustMN Nat
deleteArgs [] usedIds = usedIds
deleteArgs (x :: xs) usedIds = deleteArgs xs (SortedMap.delete x usedIds)

mutual
  genConAlt : RustConAlt -> State (SortedMap RustMN Nat) String
  genConAlt (MkConAlt tag args scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let newClones = freshClones args usedIds
    let newScope = genClones newClones innerScope
    put (deleteArgs args usedIds)
    let assignments = map genAssignment (zip [0..(length args `minus` 1)] args)
    pure $ "DataCon { tag: " ++ show tag ++ ", ref args } => { " ++ showSep "; " (assignments ++ [newScope]) ++ " }"
  where
    genAssignment : (Nat, RustMN) -> String
    genAssignment (index, name) =
      "let " ++ genRustMN name ++ " = Arc::clone(&args[" ++ show index ++ "])"

  genConstAlt : RustConstAlt -> State (SortedMap RustMN Nat) String
  genConstAlt (MkConstAlt constant scope) =
    pure $ genConstant constant ++ " => { " ++ !(genExpr scope) ++ " }"

  genStrConstAlt : RustConstAlt -> State (SortedMap RustMN Nat) String
  genStrConstAlt (MkConstAlt (CStr str) scope) =
    pure $ genStringLiteral str ++ " => { " ++ !(genExpr scope) ++ " }"
  genStrConstAlt (MkConstAlt _ scope) =
    pure $ "_ => panic!(\"Expected CStr in ConstAlt\")"

  genAltDef : Maybe RustExpr -> State (SortedMap RustMN Nat) (List String)
  genAltDef Nothing = pure $ []
  genAltDef (Just def) =
    pure $ ["_ => " ++ !(genExpr def)]

  genExpr : RustExpr -> State (SortedMap RustMN Nat) String
  genExpr (RPrimVal val) = pure $ "Arc::new(" ++ genConstant val ++ ")"
  genExpr (RRefUN n) = pure $ genRustUN n
  genExpr (RRefMN n) = do
    usedIds <- get
    let Just nextIndex = SortedMap.lookup n usedIds
      | Nothing => do
        put $ insert n 0 usedIds
        pure $ wrapClone (genRustMN n)
    put $ insert n (nextIndex + 1) usedIds
    pure $ wrapClone (genRustMNIndex n (Just nextIndex))
  genExpr (RLet n val scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let newClones = freshClones [n] usedIds
    let newScope = genClones newClones innerScope
    put (deleteArgs [n] usedIds)
    pure $ wrapLet (genRustMN n) !(genExpr val) newScope
  genExpr (RLam n@(MN index) scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let previousClones = repeatClones (delete n (map MN [0..(index `minus` 1)])) usedIds
    let newClones = freshClones [n] usedIds
    let newScope = genClones (previousClones ++ newClones) innerScope
    put (deleteArgs [n] usedIds)
    pure $ "Arc::new(Lambda(Box::new(move |" ++ genRustMN n ++ ": Arc<IdrisValue>| { " ++ newScope ++ " })))"
  genExpr (RApp expr args) = do
    outArgs <- traverse genExpr args
    outExpr <- genExpr expr
    let calledExpr = case expr of
      RRefUN (UN fnName) => fnName
      _ => "(" ++ outExpr ++ ".unwrap_lambda())"
    pure $ calledExpr ++ "(" ++ showSep ", " outArgs ++ ")"
  genExpr (RCon tag args) = do
    outArgs <- traverse genExpr args
    pure $ "Arc::new(DataCon { tag: " ++ show tag ++ ", args: vec![" ++ showSep ", " outArgs ++ "]})"
  genExpr (RBinOp ty fnName val1 val2) = do
    let callUnwrapFn = ".unwrap_" ++ unwrapName ty ++ "()"
    pure $ "Arc::new(" ++ dataConstructor ty ++ "(" ++ !(genExpr val1) ++ callUnwrapFn ++ " " ++ fnName ++ " " ++ !(genExpr val2) ++ callUnwrapFn ++ "))"
  genExpr (RBoolBinOp ty fnName val1 val2) = do
    let callUnwrapFn = ".unwrap_" ++ unwrapName ty ++ "()"
    pure $ "Arc::new(Int((" ++ !(genExpr val1) ++ callUnwrapFn ++ " " ++ fnName ++ " " ++ !(genExpr val2) ++ callUnwrapFn ++ ") as i64))"
  genExpr (RConCase expr alts def) = do
    outExpr <- genExpr expr
    outAlts <- traverse genConAlt alts
    outDef <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    pure $ "match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }"
  genExpr (RConstCase expr alts def) = do
    outExpr <- genExpr expr
    outAlts <- traverse genConstAlt alts
    outDef <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    pure $ "match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }"
  genExpr (RStrConstCase expr alts def) = do
    outExpr <- genExpr expr
    outAlts <- traverse genStrConstAlt alts
    outDef <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    pure $ "match " ++ outExpr ++ ".unwrap_str().as_ref() { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }"
  genExpr (RDelay scope) = do
    innerScope <- genExpr scope
    usedIds <- get
    let previousClones = repeatAllClones usedIds
    let newScope = genClones previousClones innerScope
    pure $ "Arc::new(Delay(Box::new(move || {" ++ newScope ++ "})))"
  genExpr (RForce scope) = do
    pure $ "(" ++ !(genExpr scope) ++ ".unwrap_delay())()"
  genExpr RErased = pure $ "Arc::new(Erased)"
  genExpr (RCrash msg) = pure $ "{ let x: Arc<IdrisValue> = panic!(\"" ++ msg ++ "\"); x }"

export
genExprNoArgs : RustExpr -> String
genExprNoArgs expr =
  let (code, _) = State.runState (genExpr expr) empty
  in code

export
genDecl : RustDecl -> String
genDecl (MkFun name args scope) = do
    let (code, usedIds) = State.runState (genExpr scope) empty
    let newClones = freshClones args usedIds
    let newScope = genClones newClones code
    "fn " ++ genRustUN name ++ "(" ++ showSep ", " (map showArg args) ++ ") -> Arc<IdrisValue> { " ++ newScope ++ " }\n"
  where
    showArg : RustMN -> String
    showArg arg = genRustMN arg ++ ": Arc<IdrisValue>"
