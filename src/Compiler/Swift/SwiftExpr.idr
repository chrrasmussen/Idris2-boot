module Compiler.Swift.SwiftExpr


%default covering


public export
data SwiftUN = UN String

public export
data SwiftMN = MN Nat

Eq SwiftMN where
  (MN x) == (MN y) = x == y

Ord SwiftMN where
  compare (MN x) (MN y) = compare x y

public export
data SwiftConstant : Type where
  CInt : Int -> SwiftConstant
  CInteger : Integer -> SwiftConstant
  CDouble : Double -> SwiftConstant
  CChar : Char -> SwiftConstant
  CStr : String -> SwiftConstant
  CWorld : SwiftConstant

public export
data SwiftType = TInt | TInteger | TDouble | TChar | TStr

mutual
  public export
  data SwiftExpr : Type where
    SPrimVal : SwiftConstant -> SwiftExpr
    SRefUN : SwiftUN -> SwiftExpr
    SRefMN : SwiftMN -> SwiftExpr
    SLet : SwiftMN -> SwiftExpr -> SwiftExpr -> SwiftExpr
    SLam : SwiftMN -> SwiftExpr -> SwiftExpr
    SApp : SwiftExpr -> List SwiftExpr -> SwiftExpr
    SCon : Int -> List SwiftExpr -> SwiftExpr
    SBinOp : SwiftType -> String -> SwiftExpr -> SwiftExpr -> SwiftExpr
    SBoolBinOp : SwiftType -> String -> SwiftExpr -> SwiftExpr -> SwiftExpr
    SConCase : SwiftExpr -> List SwiftConAlt -> Maybe SwiftExpr -> SwiftExpr
    SConstCase : SwiftExpr -> List SwiftConstAlt -> Maybe SwiftExpr -> SwiftExpr
    SDelay : SwiftExpr -> SwiftExpr
    SForce : SwiftExpr -> SwiftExpr
    SFork : SwiftExpr -> SwiftExpr
    SErased : SwiftExpr
    SCrash : String -> SwiftExpr

  public export
  data SwiftConAlt : Type where
    MkConAlt : (tag : Int) -> (args : List SwiftMN) -> SwiftExpr -> SwiftConAlt

  public export
  data SwiftConstAlt : Type where
    MkConstAlt : SwiftConstant -> SwiftExpr -> SwiftConstAlt

public export
data SwiftDecl : Type where
  MkFun : SwiftUN -> List SwiftMN -> SwiftExpr -> SwiftDecl



-- STRING HELPER FUNCTIONS

showSep : String -> List String -> String
showSep sep [] = ""
showSep sep [x] = x
showSep sep (x :: xs) = x ++ sep ++ showSep sep xs


-- SWIFT EXPR TO STRING

unwrapName : SwiftType -> String
unwrapName TInt = "unwrapInt"
unwrapName TInteger = "unwrapInteger"
unwrapName TDouble = "unwrapDouble"
unwrapName TChar = "unwrapChar"
unwrapName TStr = "unwrapString"

dataConstructor : SwiftType -> String
dataConstructor TInt = "IdrisValue.int"
dataConstructor TInteger = "IdrisValue.integer"
dataConstructor TDouble = "IdrisValue.double"
dataConstructor TChar = "IdrisValue.char"
dataConstructor TStr = "IdrisValue.string"

genSwiftUN : SwiftUN -> String
genSwiftUN (UN x) = x

genSwiftMN : SwiftMN -> String
genSwiftMN (MN x) = "v" ++ show x

genSwiftMNIndex : SwiftMN -> Maybe Nat -> String
genSwiftMNIndex mn Nothing = genSwiftMN mn
genSwiftMNIndex mn (Just index) = genSwiftMN mn ++ "_" ++ show index

genStringLiteral : String -> String
genStringLiteral str = "\"\"\"\n" ++ str ++ "\n\"\"\""

genConstant : SwiftConstant -> String
genConstant (CInt x) = "IdrisValue.int(" ++ show x ++ ")"
genConstant (CInteger x) = "IdrisValue.int(" ++ show x ++ ")"
--genConstant (CInteger x) = "IdrisValue.integer(BigInt(\"" ++ show x ++ "\")!)" -- TODO: `Integer` solution
genConstant (CDouble x) = "IdrisValue.double(" ++ show x ++ ")"
genConstant (CChar x) = "IdrisValue.char(\"\\u{" ++ toHex x ++ "}\")"
  where
    toHex : Char -> String
    toHex c = substr 2 6 (b32ToHexString (fromInteger (cast (ord c))))
genConstant (CStr x) = "IdrisValue.string(" ++ genStringLiteral x ++ ")"
genConstant CWorld = "IdrisValue.world"

mutual
  genConAlt : SwiftConAlt -> String
  genConAlt (MkConAlt tag args scope) = do
     let outScope = genExpr scope
     let assignments = map genAssignment (zip [0..(length args `minus` 1)] args)
     "case .dataCon(tag: " ++ show tag ++ ", args: let args): do { " ++ showSep "; " (assignments ++ ["return " ++ outScope]) ++ " }"
  where
    genAssignment : (Nat, SwiftMN) -> String
    genAssignment (index, name) =
      "let " ++ genSwiftMN name ++ " = args[" ++ show index ++ "]"

  genConstAlt : SwiftConstAlt -> String
  genConstAlt (MkConstAlt constant scope) = do
    let outScope = genExpr scope
    "case " ++ genConstant constant ++ ": return " ++ outScope

  genAltDef : Maybe SwiftExpr -> List String
  genAltDef Nothing = []
  genAltDef (Just def) = do
    let outExpr = genExpr def
    ["case _: return " ++ outExpr]

  unwrapCall : SwiftType -> String -> String
  unwrapCall ty expr = expr ++ "." ++ unwrapName ty ++ "()"

  export
  genExpr : SwiftExpr -> String
  genExpr (SPrimVal val) = genConstant val
  genExpr (SRefUN n) = genSwiftUN n
  genExpr (SRefMN n) = genSwiftMN n
  genExpr (SLet n val scope) = do
    let outValue = genExpr val
    let outScope = genExpr scope
    "{ (" ++ genSwiftMN n ++ ": IdrisValue) -> IdrisValue in " ++  outScope ++ " }(" ++ outValue ++ ")"
  genExpr (SLam n scope) = do
    let outScope = genExpr scope
    "IdrisValue.lambda({ " ++ genSwiftMN n ++ " in " ++ outScope ++ " })"
  genExpr (SApp expr args) = do
    let outArgs = map genExpr args
    let outExpr = genExpr expr
    let calledExpr = case expr of
      SRefUN (UN fnName) => fnName
      _ => "(" ++ outExpr ++ ".unwrapLambda())"
    calledExpr ++ "(" ++ showSep ", " outArgs ++ ")"
  genExpr (SCon tag args) = do
    let outArgs = map genExpr args
    "IdrisValue.dataCon(tag: " ++ show tag ++ ", args: [" ++ showSep ", " outArgs ++ "])"
  genExpr (SBinOp ty fnName val1 val2) = do
    let outVal1 = genExpr val1
    let outVal2 = genExpr val2
    dataConstructor ty ++ "(" ++ unwrapCall ty outVal1 ++ " " ++ fnName ++ " " ++ unwrapCall ty outVal2 ++ ")"
  genExpr (SBoolBinOp ty fnName val1 val2) = do
    let outVal1 = genExpr val1
    let outVal2 = genExpr val2
    "IdrisValue.int(idrisRTS_boolToInt(" ++ unwrapCall ty outVal1 ++ " " ++ fnName ++ " " ++ unwrapCall ty outVal2 ++ "))"
  genExpr (SConCase expr alts def) = do
    let outExpr = genExpr expr
    let outAlts = map genConAlt alts
    let outDef = genAltDef def
    let catchAllCase = ["case let x: fatalError(\"No matches for: \\(x)\")"]
    "{ () -> IdrisValue in switch (" ++ outExpr ++ ") { " ++ showSep "; " (outAlts ++ outDef ++ catchAllCase) ++ " } }()"
  genExpr (SConstCase expr alts def) = do
    let outExpr = genExpr expr
    let outAlts = map genConstAlt alts
    let outDef = genAltDef def
    let catchAllCase = ["case let x: fatalError(\"No matches for: \\(x)\")"]
    "{ () -> IdrisValue in switch (" ++ outExpr ++ ") { " ++ showSep "; " (outAlts ++ outDef ++ catchAllCase) ++ " } }()"
  genExpr (SDelay scope) = do
    let outScope = genExpr scope
    "IdrisValue.delay({ " ++ outScope ++ " })"
  genExpr (SForce scope) = do
    let outScope = genExpr scope
    "(" ++ outScope ++ ".unwrapDelay())()"
  genExpr (SFork scope) = do
    -- TODO: Implement
    let outScope = genExpr scope
    "{ " ++ outScope ++ " }()"
  genExpr SErased = "IdrisValue.erased"
  genExpr (SCrash msg) = "fatalError(\"" ++ msg ++ "\")"

export
genDecl : SwiftDecl -> String
genDecl (MkFun name args scope) = do
    let code = genExpr scope
    "func " ++ genSwiftUN name ++ "(" ++ showSep ", " (map showArg args) ++ ") -> IdrisValue { return " ++ code ++ " }\n"
  where
    showArg : SwiftMN -> String
    showArg arg = "_ " ++ genSwiftMN arg ++ ": IdrisValue"
