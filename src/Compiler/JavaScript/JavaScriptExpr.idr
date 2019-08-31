module Compiler.JavaScript.JavaScriptExpr


%default covering


public export
data JavaScriptUN = UN String

public export
data JavaScriptMN = MN Nat

Eq JavaScriptMN where
  (MN x) == (MN y) = x == y

Ord JavaScriptMN where
  compare (MN x) (MN y) = compare x y

public export
data JSConstant : Type where
  CInt : Int -> JSConstant
  CInteger : Integer -> JSConstant
  CDouble : Double -> JSConstant
  CChar : Char -> JSConstant
  CStr : String -> JSConstant
  CWorld : JSConstant

mutual
  public export
  data JSExpr : Type where
    JSPrimVal : JSConstant -> JSExpr
    JSRefUN : JavaScriptUN -> JSExpr
    JSRefMN : JavaScriptMN -> JSExpr
    JSLet : JavaScriptMN -> JSExpr -> JSExpr -> JSExpr
    JSLam : JavaScriptMN -> JSExpr -> JSExpr
    JSApp : JSExpr -> List JSExpr -> JSExpr
    JSCon : Int -> List JSExpr -> JSExpr
    JSBinOp : String -> JSExpr -> JSExpr -> JSExpr
    JSBoolBinOp : String -> JSExpr -> JSExpr -> JSExpr
    JSConCase : JSExpr -> List JavaScriptConAlt -> Maybe JSExpr -> JSExpr
    JSConstCase : JSExpr -> List JavaScriptConstAlt -> Maybe JSExpr -> JSExpr
    JSDelay : JSExpr -> JSExpr
    JSForce : JSExpr -> JSExpr
    JSFork : JSExpr -> JSExpr
    JSErased : JSExpr
    JSCrash : String -> JSExpr

  public export
  data JavaScriptConAlt : Type where
    MkConAlt : (tag : Int) -> (args : List JavaScriptMN) -> JSExpr -> JavaScriptConAlt

  public export
  data JavaScriptConstAlt : Type where
    MkConstAlt : JSConstant -> JSExpr -> JavaScriptConstAlt

public export
data JavaScriptDecl : Type where
  MkFun : JavaScriptUN -> List JavaScriptMN -> JSExpr -> JavaScriptDecl



-- STRING HELPER FUNCTIONS

showSep : String -> List String -> String
showSep sep [] = ""
showSep sep [x] = x
showSep sep (x :: xs) = x ++ sep ++ showSep sep xs


-- JAVASCRIPT EXPR TO STRING

genJavaScriptUN : JavaScriptUN -> String
genJavaScriptUN (UN x) = x

genJavaScriptMN : JavaScriptMN -> String
genJavaScriptMN (MN x) = "v" ++ show x

genConstant : JSConstant -> String
genConstant (CInt x) = show x
genConstant (CInteger x) = show x -- TODO: Fix BigInt
genConstant (CDouble x) = show x
genConstant (CChar x) = "'\\u" ++ toHex x ++ "'"
  where
    toHex : Char -> String
    toHex c = substr 2 6 (b32ToHexString (fromInteger (cast (ord c))))
genConstant (CStr x) = "`" ++ x ++ "`"
genConstant CWorld = "{}" -- TODO: Change this?

mutual
  genConAlt : JavaScriptConAlt -> String
  genConAlt (MkConAlt tag args scope) = do
     let outScope = genExpr scope
     let assignments = map genAssignment (zip [0..(length args `minus` 1)] args)
     "case " ++ show tag ++ ": " ++ showSep "; " (assignments ++ ["return " ++ outScope])
  where
    genAssignment : (Nat, JavaScriptMN) -> String
    genAssignment (index, name) =
      "var " ++ genJavaScriptMN name ++ " = data.args[" ++ show index ++ "]"

  genConstAlt : JavaScriptConstAlt -> String
  genConstAlt (MkConstAlt constant scope) = do
    let outScope = genExpr scope
    "case " ++ genConstant constant ++ ": return " ++ outScope

  genAltDef : Maybe JSExpr -> List String
  genAltDef Nothing = []
  genAltDef (Just def) = do
    let outExpr = genExpr def
    ["default: return " ++ outExpr]

  export
  genExpr : JSExpr -> String
  genExpr (JSPrimVal val) = genConstant val
  genExpr (JSRefUN n) = genJavaScriptUN n
  genExpr (JSRefMN n) = genJavaScriptMN n
  genExpr (JSLet n val scope) = do
    let outValue = genExpr val
    let outScope = genExpr scope
    "(function (" ++ genJavaScriptMN n ++ ") { return " ++  outScope ++ " })(" ++ outValue ++ ")"
  genExpr (JSLam n scope) = do
    let outScope = genExpr scope
    "(function (" ++ genJavaScriptMN n ++ ") { return " ++ outScope ++ " })"
  genExpr (JSApp expr args) = do
    let outArgs = map genExpr args
    let outExpr = genExpr expr
    outExpr ++ "(" ++ showSep ", " outArgs ++ ")"
  genExpr (JSCon tag args) = do
    let outArgs = map genExpr args
    "{ tag: " ++ show tag ++ ", args: [" ++ showSep ", " outArgs ++ "] }"
  genExpr (JSBinOp op val1 val2) = do
    let outVal1 = genExpr val1
    let outVal2 = genExpr val2
    "(" ++ outVal1 ++ " " ++ op ++ " " ++ outVal2 ++ ")"
  genExpr (JSBoolBinOp op val1 val2) = do
    let outVal1 = genExpr val1
    let outVal2 = genExpr val2
    "idrisRTS_boolToInt(" ++ outVal1 ++ " " ++ op ++ " " ++ outVal2 ++ ")"
  genExpr (JSConCase expr alts def) = do
    let outExpr = genExpr expr
    let outAlts = map genConAlt alts
    let outDef = genAltDef def
    "(function () { var data = " ++ outExpr ++ "; switch (data.tag) { " ++ showSep "; " (outAlts ++ outDef) ++ " } })()"
  genExpr (JSConstCase expr alts def) = do
    let outExpr = genExpr expr
    let outAlts = map genConstAlt alts
    let outDef = genAltDef def
    "(function () { switch (" ++ outExpr ++ ") { " ++ showSep "; " (outAlts ++ outDef) ++ " } })()"
  genExpr (JSDelay scope) = do
    let outScope = genExpr scope
    "(function () { return " ++ outScope ++ " })"
  genExpr (JSForce scope) = do
    let outScope = genExpr scope
    outScope ++ "()"
  genExpr (JSFork scope) = do
    -- TODO: Implement correctly
    let outScope = genExpr scope
    "setTimeout(function () { return " ++ outScope ++ " }, 0)"
  genExpr JSErased = "{}" -- TODO: Change?
  genExpr (JSCrash msg) = "throw('" ++ msg ++ "')"
  genExpr _ = "throw('Missing expr')"

export
genDecl : JavaScriptDecl -> String
genDecl (MkFun name args scope) = do
    let code = genExpr scope
    "function " ++ genJavaScriptUN name ++ "(" ++ showSep ", " (map genJavaScriptMN args) ++ ") { return " ++ code ++ "; }\n"
