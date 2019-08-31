module Compiler.Swift.Common

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Data.List
import Data.Vect

import Compiler.Swift.SwiftExpr

%default covering

swiftString : String -> String
swiftString s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c || c == '_'
                  then cast c
                  else "C_" ++ show (cast {to=Int} c)

swiftName : Name -> String
swiftName (NS ns n) = "ns_" ++ showSep "_" ns ++ "_" ++ swiftName n
swiftName (UN n) = swiftString n
swiftName (MN n i) = swiftString n ++ "_" ++ show i
swiftName (PV n d) = "pat__" ++ swiftName n
swiftName (DN _ n) = swiftName n
swiftName (Nested i n) = "n__" ++ show i ++ "_" ++ swiftName n
swiftName (CaseBlock x y) = "case__" ++ show x ++ "_" ++ show y
swiftName (WithBlock x y) = "with__" ++ show x ++ "_" ++ show y
swiftName (Resolved i) = "fn__" ++ show i

-- local variable names as Swift names - we need to invent new names for the locals
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

||| Extended primitives for the Swift backend, outside the standard set of primFn
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

||| Match on a user given name to get the Swift primitive
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

swiftWorld : SwiftExpr -> SwiftExpr
swiftWorld res = SCon 0 [SErased, res, SPrimVal CWorld] -- MkIORes

swiftConstant : Constant -> SwiftConstant
swiftConstant (I x) = CInt x
swiftConstant (BI x) = CInteger x
swiftConstant (Str x) = CStr x
swiftConstant (Ch x) = CChar x
swiftConstant (Db x) = CDouble x
swiftConstant WorldVal = CWorld
swiftConstant IntType = CInt (-1)
swiftConstant IntegerType = CInt (-1)
swiftConstant StringType = CInt (-1)
swiftConstant CharType = CInt (-1)
swiftConstant DoubleType = CInt (-1)
swiftConstant WorldType = CInt (-1)

constantToSwiftType : Constant -> Maybe SwiftType
constantToSwiftType IntType = Just TInt
constantToSwiftType IntegerType = Just TInt -- TODO: Should be `TInteger`
constantToSwiftType StringType = Just TStr
constantToSwiftType CharType = Just TChar
constantToSwiftType DoubleType = Just TDouble
constantToSwiftType _ = Nothing

binOp : Constant -> String -> SwiftExpr -> SwiftExpr -> SwiftExpr
binOp constant fnName x y =
  let Just ty = constantToSwiftType constant
    | SCrash "Unknown type for binary operator"
  in SBinOp ty fnName x y

boolBinOp : Constant -> String -> SwiftExpr -> SwiftExpr -> SwiftExpr
boolBinOp constant fnName x y =
  let Just ty = constantToSwiftType constant
    | SCrash "Unknown type for binary operator"
  in SBoolBinOp ty fnName x y

swiftOp : PrimFn arity -> Vect arity SwiftExpr -> SwiftExpr
swiftOp (Add IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intAdd")) [x, y]
swiftOp (Sub IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intSub")) [x, y]
swiftOp (Mul IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intMul")) [x, y]
swiftOp (Div IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intDiv")) [x, y]
swiftOp (Mod IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intMod")) [x, y]
swiftOp (Neg IntType) [x] = SApp (SRefUN (UN "idrisRTS_intNeg")) [x]
swiftOp (ShiftL IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intShl")) [x, y]
swiftOp (ShiftR IntType) [x, y] = SApp (SRefUN (UN "idrisRTS_intShr")) [x, y]
swiftOp (Add ty) [x, y] = binOp ty "+" x y
swiftOp (Sub ty) [x, y] = binOp ty "-" x y
swiftOp (Mul ty) [x, y] = binOp ty "*" x y
swiftOp (Div ty) [x, y] = binOp ty "/" x y
swiftOp (Mod ty) [x, y] = binOp ty "%" x y
swiftOp (Neg ty) [x] = SApp (SRefUN (UN "-")) [x]
swiftOp (ShiftL ty) [x, y] = binOp ty "<<" x y
swiftOp (ShiftR ty) [x, y] = binOp ty ">>" x y

swiftOp (LT ty) [x, y] = boolBinOp ty "<" x y
swiftOp (LTE ty) [x, y] = boolBinOp ty "<=" x y
swiftOp (EQ ty) [x, y] = boolBinOp ty "==" x y
swiftOp (GTE ty) [x, y] = boolBinOp ty ">=" x y
swiftOp (GT ty) [x, y] = boolBinOp ty ">" x y

swiftOp StrLength [x] = SApp (SRefUN (UN "idrisRTS_stringLength")) [x]
swiftOp StrHead [x] = SApp (SRefUN (UN "idrisRTS_stringHead")) [x]
swiftOp StrTail [x] = SApp (SRefUN (UN "idrisRTS_stringTail")) [x]
swiftOp StrIndex [x, i] = SApp (SRefUN (UN "idrisRTS_stringIndex")) [x, i]
swiftOp StrCons [x, y] = SApp (SRefUN (UN "idrisRTS_stringCons")) [x, y]
swiftOp StrAppend [x, y] = SApp (SRefUN (UN "idrisRTS_stringAppend")) [x, y]
swiftOp StrReverse [x] = SApp (SRefUN (UN "idrisRTS_stringReverse")) [x]
swiftOp StrSubstr [x, y, z] = SApp (SRefUN (UN "idrisRTS_stringSubstr")) [x, y, z]

swiftOp (Cast IntegerType IntType) [x] = x -- TODO: SApp (SRefUN (UN "idrisRTS_integerToInt")) [x]
swiftOp (Cast IntegerType DoubleType) [x] = SApp (SRefUN (UN "idrisRTS_intToDouble")) [x] -- TODO: `integerTo`
swiftOp (Cast IntegerType StringType) [x] = SApp (SRefUN (UN "idrisRTS_intToString")) [x] -- TODO: `integerTo`

swiftOp (Cast IntType IntegerType) [x] = x -- TODO: SApp (SRefUN (UN "idrisRTS_intToInteger")) [x]
swiftOp (Cast IntType DoubleType) [x] = SApp (SRefUN (UN "idrisRTS_intToDouble")) [x]
swiftOp (Cast IntType CharType) [x] = SApp (SRefUN (UN "idrisRTS_intToChar")) [x]
swiftOp (Cast IntType StringType) [x] = SApp (SRefUN (UN "idrisRTS_intToString")) [x]

swiftOp (Cast DoubleType IntegerType) [x] = SApp (SRefUN (UN "idrisRTS_doubleToInt")) [x] -- TODO: `toInteger`
swiftOp (Cast DoubleType IntType) [x] = SApp (SRefUN (UN "idrisRTS_doubleToInt")) [x]
swiftOp (Cast DoubleType StringType) [x] = SApp (SRefUN (UN "idrisRTS_doubleToString")) [x]

swiftOp (Cast CharType IntegerType) [x] = SApp (SRefUN (UN "idrisRTS_charToInt")) [x] -- TODO: `toInteger`
swiftOp (Cast CharType IntType) [x] = SApp (SRefUN (UN "idrisRTS_charToInt")) [x]
swiftOp (Cast CharType StringType) [x] = SApp (SRefUN (UN "idrisRTS_charToString")) [x]

swiftOp (Cast StringType IntegerType) [x] = SApp (SRefUN (UN "idrisRTS_stringToInt")) [x] -- TODO: `toInteger`
swiftOp (Cast StringType IntType) [x] = SApp (SRefUN (UN "idrisRTS_stringToInt")) [x]
swiftOp (Cast StringType DoubleType) [x] = SApp (SRefUN (UN "idrisRTS_stringToDouble")) [x]

swiftOp (Cast from to) [x] = SCrash ("Invalid cast " ++ show from ++ "->" ++ show to)

swiftOp BelieveMe [_, _, x] = x

swiftOp op _ = SCrash ("Unknown operator " ++ show op) -- TODO: Remove this

mutual
  -- oops, no traverse for Vect in Core
  swiftArgs : Int -> SVars vars -> Vect n (CExp vars) -> Core (Vect n SwiftExpr)
  swiftArgs i vs [] = pure []
  swiftArgs i vs (arg :: args) = pure $ !(swiftExp i vs arg) :: !(swiftArgs i vs args)

  expectMN : Name -> Core Int
  expectMN (MN n index) = pure index
  expectMN name = throw (InternalError ("Unexpected variable name " ++ show name))

  export
  swiftExp : Int -> SVars vars -> CExp vars -> Core SwiftExpr
  swiftExp i vs (CLocal fc el) = do
    index <- expectMN (lookupSVar el vs)
    pure $ SRefMN (MN (toNat index))
  swiftExp i vs (CRef fc n) = pure $ SRefUN (UN (swiftName n))
  swiftExp i vs (CLam fc x sc) = do
    let vs' = extendSVars [x] vs
    sc' <- swiftExp i vs' sc
    index <- expectMN (lookupSVar First vs')
    pure $ SLam (MN (toNat index)) sc'
  swiftExp i vs (CLet fc x val sc) = do
    let vs' = extendSVars [x] vs
    val' <- swiftExp i vs val
    sc' <- swiftExp i vs' sc
    index <- expectMN (lookupSVar First vs')
    pure $ SLet (MN (toNat index)) val' sc'
  swiftExp i vs (CApp fc x args) =
    pure $ SApp !(swiftExp i vs x) !(traverse (swiftExp i vs) args)
  swiftExp i vs (CCon fc x tag args) =
    pure $ SCon tag!(traverse (swiftExp i vs) args)
  swiftExp i vs (COp fc op args) =
    pure $ swiftOp op !(swiftArgs i vs args)
  swiftExp i vs (CExtPrim fc p args) =
    swiftExtPrim i vs (toPrim p) args
  swiftExp i vs (CForce fc t) =
    pure $ SForce !(swiftExp i vs t)
  swiftExp i vs (CDelay fc t) =
    pure $ SDelay !(swiftExp i vs t)
  swiftExp i vs (CConCase fc expr alts def) = do
    outExpr <- swiftExp i vs expr
    outAlts <- traverse (toSwiftConAlt i vs) alts
    outDef <- maybe (pure Nothing) (\d => pure (Just !(swiftExp i vs d))) def
    pure $ SConCase outExpr outAlts outDef
  where
    getNames : List Name -> SVars vars -> List Name
    getNames [] _ = []
    getNames _ [] = []
    getNames (n :: ns) (v :: vs) = v :: getNames ns vs
    toSwiftConAlt : Int -> SVars vars -> CConAlt vars -> Core SwiftConAlt
    toSwiftConAlt i vs (MkConAlt name tag args scope) = do
      let vs' = extendSVars args vs
      let argNames = getNames args vs'
      indexes <- traverse expectMN argNames
      let names = map (MN . toNat) indexes
      outScope <- swiftExp i vs' scope
      pure $ MkConAlt tag names outScope
  swiftExp i vs (CConstCase fc expr alts def) = do
    outExpr <- swiftExp i vs expr
    outAlts <- traverse (toSwiftConstAlt i vs) alts
    outDef <- maybe (pure Nothing) (\d => pure (Just !(swiftExp i vs d))) def
    pure $ SConstCase outExpr outAlts outDef
  where
    toSwiftConstAlt : Int -> SVars vars -> CConstAlt vars -> Core SwiftConstAlt
    toSwiftConstAlt i vs (MkConstAlt constant scope) = do
      outScope <- swiftExp i vs scope
      pure $ MkConstAlt (swiftConstant constant) outScope
  swiftExp i vs (CPrimVal fc c) =
    pure $ SPrimVal (swiftConstant c)
  swiftExp i vs (CErased fc) =
    pure $ SErased
  swiftExp i vs (CCrash fc msg) =
    pure $ SCrash msg

  swiftExtPrim : Int -> SVars vars -> ExtPrim -> List (CExp vars) -> Core SwiftExpr
  swiftExtPrim i vs PutStr [arg, world] = pure $ swiftWorld $ SApp (SRefUN (UN "idrisRTS_putStr")) [!(swiftExp i vs arg)]
  swiftExtPrim i vs GetStr [world] = pure $ swiftWorld $ SApp (SRefUN (UN "idrisRTS_getStr")) []
  swiftExtPrim i vs Fork [action, world] = do
    let runAction = CApp EmptyFC (CRef EmptyFC (NS ["PrimIO"] (UN "unsafePerformIO"))) [CErased EmptyFC, action]
    pure $ swiftWorld $ SFork !(swiftExp i vs runAction)
  swiftExtPrim i vs prim args = throw (InternalError ("Badly formed external primitive " ++ show prim ++ " " ++ show args))

swiftArgList : SVars ns -> List SwiftMN
swiftArgList [] = []
swiftArgList ((MN n i) :: xs) = MN (toNat i) :: swiftArgList xs
swiftArgList (_ :: xs) = swiftArgList xs

swiftDef : {auto c : Ref Ctxt Defs} -> Name -> CDef -> Core String
swiftDef n (MkFun args exp) =
  let vs = initSVars args in
  pure $ genDecl $ MkFun (UN (swiftName !(getFullName n))) (swiftArgList vs) !(swiftExp 0 vs exp)
swiftDef n (MkError exp) =
  pure "" -- TODO: Do I need this?
swiftDef n (MkCon t a) =
  pure "" -- Nothing to compile here

-- Convert the name to Swift code
-- (There may be no code generated, for example if it's a constructor)
export
getSwift : {auto c : Ref Ctxt Defs} -> Defs -> Name -> Core String
getSwift defs n = do
  Just d <- lookupCtxtExact n (gamma defs)
    | throw (InternalError ("Compiling undefined name " ++ show n))
  let Just d = compexpr d
    | throw (InternalError ("No compiled definition for " ++ show n))
  swiftDef n d
