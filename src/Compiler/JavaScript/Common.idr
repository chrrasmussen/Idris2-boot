module Compiler.JavaScript.Common

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Data.List
import Data.Vect

import Compiler.JavaScript.JavaScriptExpr

%default covering

jsString : String -> String
jsString s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c || c == '_'
                  then cast c
                  else "C_" ++ show (cast {to=Int} c)

jsName : Name -> String
jsName (NS ns n) = "ns_" ++ showSep "_" ns ++ "_" ++ jsName n
jsName (UN n) = jsString n
jsName (MN n i) = jsString n ++ "_" ++ show i
jsName (PV n d) = "pat__" ++ jsName n
jsName (DN _ n) = jsName n
jsName (Nested i n) = "n__" ++ show i ++ "_" ++ jsName n
jsName (CaseBlock x y) = "case__" ++ show x ++ "_" ++ show y
jsName (WithBlock x y) = "with__" ++ show x ++ "_" ++ show y
jsName (Resolved i) = "fn__" ++ show i

-- local variable names as JavaScript names - we need to invent new names for the locals
-- because there might be shadows in the original expression which can't be resolved
-- by the same scoping rules. (e.g. something that computes \x, x => x + x where the
-- names are the same but refer to different bindings in the scope)
public export
data SVars : List Name -> Type where
     Nil : SVars []
     (::) : (svar : Name) -> SVars ns -> SVars (n :: ns)

extendSVars : (xs : List Name) -> SVars ns -> SVars (xs ++ ns)
extendSVars {ns} xs vs = extSVars' (cast (length ns)) xs vs
  where
    extSVars' : Int -> (xs : List Name) -> SVars ns -> SVars (xs ++ ns)
    extSVars' i [] vs = vs
    extSVars' i (x :: xs) vs = MN "v" i :: extSVars' (i + 1) xs vs

initSVars : (xs : List Name) -> SVars xs
initSVars xs = rewrite sym (appendNilRightNeutral xs) in extendSVars xs []

lookupSVar : {idx : Nat} -> .(IsVar n idx xs) -> SVars xs -> Name
lookupSVar First (n :: ns) = n
lookupSVar (Later p) (n :: ns) = lookupSVar p ns

||| Extended primitives for the JavaScript backend, outside the standard set of primFn
public export
data ExtPrim = CCall | PutStr | GetStr
             | Fork
             | FileOpen | FileClose | FileReadLine | FileWriteLine | FileEOF
             | NewIORef | ReadIORef | WriteIORef
             | Stdin | Stdout | Stderr
             | VoidElim | Unknown Name

export
Show ExtPrim where
  show CCall = "CCall"
  show PutStr = "PutStr"
  show GetStr = "GetStr"
  show Fork = "Fork"
  show FileOpen = "FileOpen"
  show FileClose = "FileClose"
  show FileReadLine = "FileReadLine"
  show FileWriteLine = "FileWriteLine"
  show FileEOF = "FileEOF"
  show NewIORef = "NewIORef"
  show ReadIORef = "ReadIORef"
  show WriteIORef = "WriteIORef"
  show Stdin = "Stdin"
  show Stdout = "Stdout"
  show Stderr = "Stderr"
  show VoidElim = "VoidElim"
  show (Unknown n) = "Unknown " ++ show n

||| Match on a user given name to get the JavaScript primitive
toPrim : Name -> ExtPrim
toPrim pn@(NS _ n)
    = cond [(n == UN "prim__cCall", CCall),
            (n == UN "prim__putStr", PutStr),
            (n == UN "prim__getStr", GetStr),
            (n == UN "prim__fork", Fork),
            (n == UN "prim__open", FileOpen),
            (n == UN "prim__close", FileClose),
            (n == UN "prim__readLine", FileReadLine),
            (n == UN "prim__writeLine", FileWriteLine),
            (n == UN "prim__eof", FileEOF),
            (n == UN "prim__newIORef", NewIORef),
            (n == UN "prim__readIORef", ReadIORef),
            (n == UN "prim__writeIORef", WriteIORef),
            (n == UN "prim__stdin", Stdin),
            (n == UN "prim__stdout", Stdout),
            (n == UN "prim__stderr", Stderr),
            (n == UN "void", VoidElim)
            ]
           (Unknown pn)
toPrim pn = Unknown pn

jsWorld : JSExpr -> JSExpr
jsWorld res = JSCon 0 [JSErased, res, JSPrimVal CWorld] -- MkIORes

jsConstant : Constant -> JSConstant
jsConstant (I x) = CInt x
jsConstant (BI x) = CInteger x
jsConstant (Str x) = CStr x
jsConstant (Ch x) = CChar x
jsConstant (Db x) = CDouble x
jsConstant WorldVal = CWorld
jsConstant IntType = CInt (-1)
jsConstant IntegerType = CInt (-1)
jsConstant StringType = CInt (-1)
jsConstant CharType = CInt (-1)
jsConstant DoubleType = CInt (-1)
jsConstant WorldType = CInt (-1)

jsOp : PrimFn arity -> Vect arity JSExpr -> JSExpr
jsOp (Add IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intAdd")) [x, y, JSPrimVal (CInt 31)]
jsOp (Sub IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intSub")) [x, y, JSPrimVal (CInt 31)]
jsOp (Mul IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intMul")) [x, y, JSPrimVal (CInt 31)]
jsOp (Div IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intDiv")) [x, y, JSPrimVal (CInt 31)]
jsOp (Mod IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intMod")) [x, y]
jsOp (Neg IntType) [x] = JSApp (JSRefUN (UN "idrisRTS_intNeg")) [x]
jsOp (ShiftL IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intShl")) [x, y]
jsOp (ShiftR IntType) [x, y] = JSApp (JSRefUN (UN "idrisRTS_intShr")) [x, y]
jsOp (Add ty) [x, y] = JSBinOp "+" x y
jsOp (Sub ty) [x, y] = JSBinOp "-" x y
jsOp (Mul ty) [x, y] = JSBinOp "*" x y
jsOp (Div ty) [x, y] = JSBinOp "/" x y
jsOp (Mod ty) [x, y] = JSBinOp "%" x y
jsOp (Neg ty) [x] = JSApp (JSRefUN (UN "-")) [x]
jsOp (ShiftL ty) [x, y] = JSBinOp "<<" x y
jsOp (ShiftR ty) [x, y] = JSBinOp ">>" x y

jsOp (LT ty) [x, y] = JSBoolBinOp "<" x y
jsOp (LTE ty) [x, y] = JSBoolBinOp "<=" x y
jsOp (EQ ty) [x, y] = JSBoolBinOp "==" x y
jsOp (GTE ty) [x, y] = JSBoolBinOp ">=" x y
jsOp (GT ty) [x, y] = JSBoolBinOp ">" x y

jsOp StrLength [x] = JSApp (JSRefUN (UN "idrisRTS_stringLength")) [x]
jsOp StrHead [x] = JSApp (JSRefUN (UN "idrisRTS_stringHead")) [x]
jsOp StrTail [x] = JSApp (JSRefUN (UN "idrisRTS_stringTail")) [x]
jsOp StrIndex [x, i] = JSApp (JSRefUN (UN "idrisRTS_stringIndex")) [x, i]
jsOp StrCons [x, y] = JSApp (JSRefUN (UN "idrisRTS_stringCons")) [x, y]
jsOp StrAppend [x, y] = JSApp (JSRefUN (UN "idrisRTS_stringAppend")) [x, y]
jsOp StrReverse [x] = JSApp (JSRefUN (UN "idrisRTS_stringReverse")) [x]
jsOp StrSubstr [x, y, z] = JSApp (JSRefUN (UN "idrisRTS_stringSubstr")) [x, y, z]

jsOp (Cast IntegerType IntType) [x] = x -- TODO: JSApp (JSRefUN (UN "idrisRTS_integerToInt")) [x]
jsOp (Cast IntegerType DoubleType) [x] = JSApp (JSRefUN (UN "idrisRTS_intToDouble")) [x] -- TODO: `integerTo`
jsOp (Cast IntegerType StringType) [x] = JSApp (JSRefUN (UN "idrisRTS_intToString")) [x] -- TODO: `integerTo`

jsOp (Cast IntType IntegerType) [x] = x -- TODO: JSApp (JSRefUN (UN "idrisRTS_intToInteger")) [x]
jsOp (Cast IntType DoubleType) [x] = JSApp (JSRefUN (UN "idrisRTS_intToDouble")) [x]
jsOp (Cast IntType CharType) [x] = JSApp (JSRefUN (UN "idrisRTS_intToChar")) [x]
jsOp (Cast IntType StringType) [x] = JSApp (JSRefUN (UN "idrisRTS_intToString")) [x]

jsOp (Cast DoubleType IntegerType) [x] = JSApp (JSRefUN (UN "idrisRTS_doubleToInt")) [x] -- TODO: `toInteger`
jsOp (Cast DoubleType IntType) [x] = JSApp (JSRefUN (UN "idrisRTS_doubleToInt")) [x]
jsOp (Cast DoubleType StringType) [x] = JSApp (JSRefUN (UN "idrisRTS_doubleToString")) [x]

jsOp (Cast CharType IntegerType) [x] = JSApp (JSRefUN (UN "idrisRTS_charToInt")) [x] -- TODO: `toInteger`
jsOp (Cast CharType IntType) [x] = JSApp (JSRefUN (UN "idrisRTS_charToInt")) [x]
jsOp (Cast CharType StringType) [x] = JSApp (JSRefUN (UN "idrisRTS_charToString")) [x]

jsOp (Cast StringType IntegerType) [x] = JSApp (JSRefUN (UN "idrisRTS_stringToInt")) [x] -- TODO: `toInteger`
jsOp (Cast StringType IntType) [x] = JSApp (JSRefUN (UN "idrisRTS_stringToInt")) [x]
jsOp (Cast StringType DoubleType) [x] = JSApp (JSRefUN (UN "idrisRTS_stringToDouble")) [x]

jsOp (Cast from to) [x] = JSCrash ("Invalid cast " ++ show from ++ "->" ++ show to)

jsOp BelieveMe [_, _, x] = x

jsOp op _ = JSCrash ("Unknown operator " ++ show op) -- TODO: Remove this

mutual
  -- oops, no traverse for Vect in Core
  jsArgs : Int -> SVars vars -> Vect n (CExp vars) -> Core (Vect n JSExpr)
  jsArgs i vs [] = pure []
  jsArgs i vs (arg :: args) = pure $ !(jsExp i vs arg) :: !(jsArgs i vs args)

  expectMN : Name -> Core Int
  expectMN (MN n index) = pure index
  expectMN name = throw (InternalError ("Unexpected variable name " ++ show name))

  export
  jsExp : Int -> SVars vars -> CExp vars -> Core JSExpr
  jsExp i vs (CLocal fc el) = do
    index <- expectMN (lookupSVar el vs)
    pure $ JSRefMN (MN (toNat index))
  jsExp i vs (CRef fc n) = pure $ JSRefUN (UN (jsName n))
  jsExp i vs (CLam fc x sc) = do
    let vs' = extendSVars [x] vs
    sc' <- jsExp i vs' sc
    index <- expectMN (lookupSVar First vs')
    pure $ JSLam (MN (toNat index)) sc'
  jsExp i vs (CLet fc x val sc) = do
    let vs' = extendSVars [x] vs
    val' <- jsExp i vs val
    sc' <- jsExp i vs' sc
    index <- expectMN (lookupSVar First vs')
    pure $ JSLet (MN (toNat index)) val' sc'
  jsExp i vs (CApp fc x args) =
    pure $ JSApp !(jsExp i vs x) !(traverse (jsExp i vs) args)
  jsExp i vs (CCon fc x tag args) =
    pure $ JSCon tag!(traverse (jsExp i vs) args)
  jsExp i vs (COp fc op args) =
    pure $ jsOp op !(jsArgs i vs args)
  jsExp i vs (CExtPrim fc p args) =
    jsExtPrim i vs (toPrim p) args
  jsExp i vs (CForce fc t) =
    pure $ JSForce !(jsExp i vs t)
  jsExp i vs (CDelay fc t) =
    pure $ JSDelay !(jsExp i vs t)
  jsExp i vs (CConCase fc expr alts def) = do
    outExpr <- jsExp i vs expr
    outAlts <- traverse (toJavaScriptConAlt i vs) alts
    outDef <- maybe (pure Nothing) (\d => pure (Just !(jsExp i vs d))) def
    pure $ JSConCase outExpr outAlts outDef
  where
    getNames : List Name -> SVars vars -> List Name
    getNames [] _ = []
    getNames _ [] = []
    getNames (n :: ns) (v :: vs) = v :: getNames ns vs
    toJavaScriptConAlt : Int -> SVars vars -> CConAlt vars -> Core JavaScriptConAlt
    toJavaScriptConAlt i vs (MkConAlt name tag args scope) = do
      let vs' = extendSVars args vs
      let argNames = getNames args vs'
      indexes <- traverse expectMN argNames
      let names = map (MN . toNat) indexes
      outScope <- jsExp i vs' scope
      pure $ MkConAlt tag names outScope
  jsExp i vs (CConstCase fc expr alts def) = do
    outExpr <- jsExp i vs expr
    outAlts <- traverse (toJavaScriptConstAlt i vs) alts
    outDef <- maybe (pure Nothing) (\d => pure (Just !(jsExp i vs d))) def
    pure $ JSConstCase outExpr outAlts outDef
  where
    toJavaScriptConstAlt : Int -> SVars vars -> CConstAlt vars -> Core JavaScriptConstAlt
    toJavaScriptConstAlt i vs (MkConstAlt constant scope) = do
      outScope <- jsExp i vs scope
      pure $ MkConstAlt (jsConstant constant) outScope
  jsExp i vs (CPrimVal fc c) =
    pure $ JSPrimVal (jsConstant c)
  jsExp i vs (CErased fc) =
    pure $ JSErased
  jsExp i vs (CCrash fc msg) =
    pure $ JSCrash msg

  jsExtPrim : Int -> SVars vars -> ExtPrim -> List (CExp vars) -> Core JSExpr
  jsExtPrim i vs PutStr [arg, world] = pure $ jsWorld $ JSApp (JSRefUN (UN "idrisRTS_putStr")) [!(jsExp i vs arg)]
  jsExtPrim i vs GetStr [world] = pure $ jsWorld $ JSApp (JSRefUN (UN "idrisRTS_getStr")) []
  jsExtPrim i vs Fork [action, world] = do
    let runAction = CApp EmptyFC (CRef EmptyFC (NS ["PrimIO"] (UN "unsafePerformIO"))) [CErased EmptyFC, action]
    pure $ jsWorld $ JSFork !(jsExp i vs runAction)
  jsExtPrim i vs prim args = throw (InternalError ("Badly formed external primitive " ++ show prim ++ " " ++ show args))

jsArgList : SVars ns -> List JavaScriptMN
jsArgList [] = []
jsArgList ((MN n i) :: xs) = MN (toNat i) :: jsArgList xs
jsArgList (_ :: xs) = jsArgList xs

jsDef : {auto c : Ref Ctxt Defs} -> Name -> CDef -> Core String
jsDef n (MkFun args exp) =
  let vs = initSVars args in
  pure $ genDecl $ MkFun (UN (jsName !(getFullName n))) (jsArgList vs) !(jsExp 0 vs exp)
jsDef n (MkError exp) =
  pure "" -- TODO: Do I need this?
jsDef n (MkCon t a) =
  pure "" -- Nothing to compile here

-- Convert the name to JavaScript code
-- (There may be no code generated, for example if it's a constructor)
export
getJavaScript : {auto c : Ref Ctxt Defs} -> Defs -> Name -> Core String
getJavaScript defs n = do
  Just d <- lookupCtxtExact n (gamma defs)
    | throw (InternalError ("Compiling undefined name " ++ show n))
  let Just d = compexpr d
    | throw (InternalError ("No compiled definition for " ++ show n))
  jsDef n d
