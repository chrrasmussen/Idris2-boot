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

freshClones : List RustMN -> SortedMap RustMN Nat -> List (String, String)
freshClones keepIds usedIds = do
  (n, count) <- toList usedIds
  guard $ (n `elem` keepIds) && (count >= 1)
  index <- [0..(count `minus` 1)]
  pure (genRustMN n, genRustMNIndex n (Just index))

cloneRefs : List String -> List (String, String)
cloneRefs [] = []
cloneRefs (x :: xs) = (x, x) :: cloneRefs xs

genClones : List (String, String) -> String -> String
genClones xs scope = "{ " ++ showSep "; " (map genLet xs ++ [scope]) ++ " }"
  where
    genLet : (String, String) -> String
    genLet (from, to) = "let " ++ to ++  " = " ++ (wrapClone from)

deleteArgs : List RustMN -> SortedMap RustMN Nat -> SortedMap RustMN Nat
deleteArgs [] usedIds = usedIds
deleteArgs (x :: xs) usedIds = deleteArgs xs (SortedMap.delete x usedIds)

mutual
  genConAlt : RustConAlt -> State (SortedMap RustMN Nat) (String, List String)
  genConAlt (MkConAlt tag args scope) = do
    (innerScope, scopeRefs) <- genExpr scope
    let assignments = map genAssignment (zip [0..(length args `minus` 1)] args)
    usedIds <- get
    let newClones = freshClones args usedIds
    let newScope = genClones newClones innerScope
    put (deleteArgs args usedIds)
    let introducedVarNames = map genRustMN args
    let subRefs = filter (\argRef => not (argRef `elem` introducedVarNames)) scopeRefs
    pure $ ("DataCon { tag: " ++ show tag ++ ", ref args } => { " ++ showSep "; " (assignments ++ [newScope]) ++ " }", subRefs)
  where
    genAssignment : (Nat, RustMN) -> String
    genAssignment (index, name) =
      "let " ++ genRustMN name ++ " = Arc::clone(&args[" ++ show index ++ "])"

  genConstAlt : RustConstAlt -> State (SortedMap RustMN Nat) (String, List String)
  genConstAlt (MkConstAlt constant scope) = do
    (innerScope, scopeRefs) <- genExpr scope
    pure $ (genConstant constant ++ " => { " ++ innerScope ++ " }", scopeRefs)

  genStrConstAlt : RustConstAlt -> State (SortedMap RustMN Nat) (String, List String)
  genStrConstAlt (MkConstAlt (CStr str) scope) = do
    (innerScope, scopeRefs) <- genExpr scope
    pure $ (genStringLiteral str ++ " => { " ++ innerScope ++ " }", scopeRefs)
  genStrConstAlt (MkConstAlt _ _) =
    pure $ ("_ => panic!(\"Expected CStr in ConstAlt\")", [])

  genAltDef : Maybe RustExpr -> State (SortedMap RustMN Nat) (List String, List String)
  genAltDef Nothing = pure $ ([], [])
  genAltDef (Just def) = do
    (outExpr, exprRefs) <- genExpr def
    pure $ (["_ => " ++ outExpr], exprRefs)

  genExpr : RustExpr -> State (SortedMap RustMN Nat) (String, List String)
  genExpr (RPrimVal val) = pure $ ("Arc::new(" ++ genConstant val ++ ")", [])
  genExpr (RRefUN n) = pure $ (genRustUN n, [])
  genExpr (RRefMN n) = do
    usedIds <- get
    let Just nextIndex = SortedMap.lookup n usedIds
      | Nothing => do
        put $ insert n 0 usedIds
        let refString = genRustMN n
        pure $ (refString, [refString])
    put $ insert n (nextIndex + 1) usedIds
    let refString = genRustMNIndex n (Just nextIndex)
    pure $ (refString, [refString])
  genExpr (RLet n val scope) = do
    (outValue, valueRefs) <- genExpr val
    (outScope, scopeRefs) <- genExpr scope
    usedIds <- get
    let newClones = freshClones [n] usedIds
    let newScope = genClones newClones outScope
    put (deleteArgs [n] usedIds)
    let introducedVarName = genRustMN n
    let subRefs = valueRefs ++ filter (/= introducedVarName) scopeRefs
    pure $ (wrapLet (genRustMN n) outValue newScope, subRefs)
  genExpr (RLam n scope) = do
    (innerScope, scopeRefs) <- genExpr scope
    usedIds <- get
    let introducedVarName = genRustMN n
    let subRefs = filter (/= introducedVarName) scopeRefs
    let repeatClones = cloneRefs subRefs
    let newClones = freshClones [n] usedIds
    let newScope = genClones (repeatClones ++ newClones) innerScope
    put (deleteArgs [n] usedIds)
    pure $ ("Arc::new(Lambda(Box::new(move |" ++ genRustMN n ++ ": Arc<IdrisValue>| { " ++ newScope ++ " })))", subRefs)
  genExpr (RApp expr args) = do
    argsResult <- traverse genExpr args
    (outExpr, exprRefs) <- genExpr expr
    let calledExpr = case expr of
      RRefUN (UN fnName) => fnName
      _ => "(" ++ outExpr ++ ".unwrap_lambda())"
    let outArgs = map fst argsResult
    let subRefs = concat (map snd argsResult) ++ exprRefs
    pure $ (calledExpr ++ "(" ++ showSep ", " outArgs ++ ")", subRefs)
  genExpr (RCon tag args) = do
    argsResult <- traverse genExpr args
    let outArgs = map fst argsResult
    let subRefs = concat (map snd argsResult)
    pure $ ("Arc::new(DataCon { tag: " ++ show tag ++ ", args: vec![" ++ showSep ", " outArgs ++ "]})", subRefs)
  genExpr (RBinOp ty fnName val1 val2) = do
    (outVal1, val1Refs) <- genExpr val1
    (outVal2, val2Refs) <- genExpr val2
    let callUnwrapFn = ".unwrap_" ++ unwrapName ty ++ "()"
    let subRefs = val1Refs ++ val2Refs
    pure $ ("Arc::new(" ++ dataConstructor ty ++ "(" ++ outVal1 ++ callUnwrapFn ++ " " ++ fnName ++ " " ++ outVal2 ++ callUnwrapFn ++ "))", subRefs)
  genExpr (RBoolBinOp ty fnName val1 val2) = do
    (outVal1, val1Refs) <- genExpr val1
    (outVal2, val2Refs) <- genExpr val2
    let callUnwrapFn = ".unwrap_" ++ unwrapName ty ++ "()"
    let subRefs = val1Refs ++ val2Refs
    pure $ ("Arc::new(Int((" ++ outVal1 ++ callUnwrapFn ++ " " ++ fnName ++ " " ++ outVal2 ++ callUnwrapFn ++ ") as i64))", subRefs)
  genExpr (RConCase expr alts def) = do
    (outExpr, exprRefs) <- genExpr expr
    altsResult <- traverse genConAlt alts
    let outAlts = map fst altsResult
    (outDef, defRefs) <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    let subRefs = exprRefs ++ concat (map snd altsResult) ++ defRefs
    pure $ ("match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }", subRefs)
  genExpr (RConstCase expr alts def) = do
    (outExpr, exprRefs) <- genExpr expr
    altsResult <- traverse genConstAlt alts
    let outAlts = map fst altsResult
    (outDef, defRefs) <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    let subRefs = exprRefs ++ concat (map snd altsResult) ++ defRefs
    pure $ ("match *" ++ outExpr ++ " { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }", subRefs)
  genExpr (RStrConstCase expr alts def) = do
    (outExpr, exprRefs) <- genExpr expr
    altsResult <- traverse genStrConstAlt alts
    let outAlts = map fst altsResult
    (outDef, defRefs) <- genAltDef def
    let catchAllCase = ["ref x => panic!(\"No matches for: {:?}\", x)"]
    let subRefs = exprRefs ++ concat (map snd altsResult) ++ defRefs
    pure $ ("match " ++ outExpr ++ ".unwrap_str().as_ref() { " ++ showSep ", " (outAlts ++ outDef ++ catchAllCase) ++ " }", subRefs)
  genExpr (RDelay scope) = do
    (innerScope, scopeRefs) <- genExpr scope
    let newScope = genClones (cloneRefs scopeRefs) innerScope
    pure $ ("Arc::new(Delay(Box::new(move || { " ++ newScope ++ " })))", scopeRefs)
  genExpr (RForce scope) = do
    (innerScope, scopeRefs) <- genExpr scope
    pure $ ("(" ++ innerScope ++ ".unwrap_delay())()", scopeRefs)
  genExpr RErased = pure $ ("Arc::new(Erased)", [])
  genExpr (RCrash msg) = pure $ ("{ let x: Arc<IdrisValue> = panic!(\"" ++ msg ++ "\"); x }", [])

export
genExprNoArgs : RustExpr -> String
genExprNoArgs expr =
  let ((code, refs), _) = State.runState (genExpr expr) empty
  in code

export
genDecl : RustDecl -> String
genDecl (MkFun name args scope) = do
    let ((code, refs), usedIds) = State.runState (genExpr scope) empty
    let newClones = freshClones args usedIds
    let newScope = genClones newClones code
    "fn " ++ genRustUN name ++ "(" ++ showSep ", " (map showArg args) ++ ") -> Arc<IdrisValue> { " ++ newScope ++ " }\n"
  where
    showArg : RustMN -> String
    showArg arg = genRustMN arg ++ ": Arc<IdrisValue>"
