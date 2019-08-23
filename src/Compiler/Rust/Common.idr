module Compiler.Rust.Common

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Data.List
import Data.Vect

import Compiler.Rust.RustExpr

%default covering

rustString : String -> String
rustString s = concatMap okchar (unpack s)
  where
    okchar : Char -> String
    okchar c = if isAlphaNum c || c == '_'
                  then cast c
                  else "C_" ++ show (cast {to=Int} c)

rustName : Name -> String
rustName (NS ns n) = "ns_" ++ showSep "_" ns ++ "_" ++ rustName n
rustName (UN n) = rustString n
rustName (MN n i) = rustString n ++ "_" ++ show i
rustName (PV n d) = "pat__" ++ rustName n
rustName (DN _ n) = rustName n
rustName (Nested i n) = "n__" ++ show i ++ "_" ++ rustName n
rustName (CaseBlock x y) = "case__" ++ show x ++ "_" ++ show y
rustName (WithBlock x y) = "with__" ++ show x ++ "_" ++ show y
rustName (Resolved i) = "fn__" ++ show i

-- local variable names as Rust names - we need to invent new names for the locals
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

||| Extended primitives for the Rust backend, outside the standard set of primFn
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

||| Match on a user given name to get the Rust primitive
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

rustWorld : RustExpr -> RustExpr
rustWorld res = RCon 0 [RErased, res, RPrimVal CWorld] -- MkIORes

rustConstant : Constant -> RustConstant
rustConstant (I x) = CInt x
rustConstant (BI x) = CInteger x
rustConstant (Str x) = CStr x
rustConstant (Ch x) = CChar x
rustConstant (Db x) = CDouble x
rustConstant WorldVal = CWorld
rustConstant IntType = CInt (-1)
rustConstant IntegerType = CInt (-1)
rustConstant StringType = CInt (-1)
rustConstant CharType = CInt (-1)
rustConstant DoubleType = CInt (-1)
rustConstant WorldType = CInt (-1)

constantToRustType : Constant -> Maybe RustType
constantToRustType IntType = Just TInt
constantToRustType IntegerType = Just TInt -- TODO: Should be `TInteger`
constantToRustType StringType = Just TStr
constantToRustType CharType = Just TChar
constantToRustType DoubleType = Just TDouble
constantToRustType _ = Nothing

binOp : Constant -> String -> RustExpr -> RustExpr -> RustExpr
binOp constant fnName x y =
  let Just ty = constantToRustType constant
    | RCrash "Unknown type for binary operator"
  in RBinOp ty fnName x y

boolBinOp : Constant -> String -> RustExpr -> RustExpr -> RustExpr
boolBinOp constant fnName x y =
  let Just ty = constantToRustType constant
    | RCrash "Unknown type for binary operator"
  in RBoolBinOp ty fnName x y

rustOp : PrimFn arity -> Vect arity RustExpr -> RustExpr
rustOp (Add IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_add")) [x, y]
rustOp (Sub IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_sub")) [x, y]
rustOp (Mul IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_mul")) [x, y]
rustOp (Div IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_div")) [x, y]
rustOp (Mod IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_mod")) [x, y]
rustOp (Neg IntType) [x] = RApp (RRefUN (UN "idris_rts_int_neg")) [x]
rustOp (ShiftL IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_shl")) [x, y]
rustOp (ShiftR IntType) [x, y] = RApp (RRefUN (UN "idris_rts_int_shr")) [x, y]
rustOp (Add ty) [x, y] = binOp ty "+" x y
rustOp (Sub ty) [x, y] = binOp ty "-" x y
rustOp (Mul ty) [x, y] = binOp ty "*" x y
rustOp (Div ty) [x, y] = binOp ty "/" x y
rustOp (Mod ty) [x, y] = binOp ty "%" x y
rustOp (Neg ty) [x] = RApp (RRefUN (UN "-")) [x]
rustOp (ShiftL ty) [x, y] = binOp ty "<<" x y
rustOp (ShiftR ty) [x, y] = binOp ty ">>" x y
rustOp (LT ty) [x, y] = boolBinOp ty "<" x y
rustOp (LTE ty) [x, y] = boolBinOp ty "<=" x y
rustOp (EQ ty) [x, y] = boolBinOp ty "==" x y
rustOp (GTE ty) [x, y] = boolBinOp ty ">=" x y
rustOp (GT ty) [x, y] = boolBinOp ty ">" x y
rustOp StrLength [x] = RApp (RRefUN (UN "idris_rts_str_length")) [x]
rustOp StrHead [x] = RApp (RRefUN (UN "idris_rts_str_head")) [x]
rustOp StrTail [x] = RApp (RRefUN (UN "idris_rts_str_tail")) [x]
rustOp StrIndex [x, i] = RApp (RRefUN (UN "idris_rts_str_index")) [x, i]
rustOp StrCons [x, y] = RApp (RRefUN (UN "idris_rts_str_cons")) [x, y]
rustOp StrAppend [x, y] = RApp (RRefUN (UN "idris_rts_str_append")) [x, y]
rustOp StrReverse [x] = RApp (RRefUN (UN "idris_rts_str_reverse")) [x]
rustOp StrSubstr [x, y, z] = RApp (RRefUN (UN "idris_rts_str_substr")) [x, y, z]

rustOp (Cast IntegerType IntType) [x] = x -- TODO: RApp (RRefUN (UN "idris_rts_integer_to_int")) [x]
rustOp (Cast IntegerType DoubleType) [x] = RApp (RRefUN (UN "idris_rts_int_to_double")) [x] -- TODO: `integer_to`
rustOp (Cast IntegerType StringType) [x] = RApp (RRefUN (UN "idris_rts_int_to_str")) [x] -- TODO: `integer_to`

rustOp (Cast IntType IntegerType) [x] = x -- TODO: RApp (RRefUN (UN "idris_rts_int_to_int")) [x]
rustOp (Cast IntType DoubleType) [x] = RApp (RRefUN (UN "idris_rts_int_to_double")) [x]
rustOp (Cast IntType CharType) [x] = RApp (RRefUN (UN "idris_rts_int_to_char")) [x]
rustOp (Cast IntType StringType) [x] = RApp (RRefUN (UN "idris_rts_int_to_str")) [x]

rustOp (Cast DoubleType IntegerType) [x] = RApp (RRefUN (UN "idris_rts_double_to_int")) [x] -- TODO: `to_integer`
rustOp (Cast DoubleType IntType) [x] = RApp (RRefUN (UN "idris_rts_double_to_int")) [x]
rustOp (Cast DoubleType StringType) [x] = RApp (RRefUN (UN "idris_rts_double_to_str")) [x]

rustOp (Cast CharType IntegerType) [x] = RApp (RRefUN (UN "idris_rts_char_to_int")) [x] -- TODO: `to_integer`
rustOp (Cast CharType IntType) [x] = RApp (RRefUN (UN "idris_rts_char_to_int")) [x]
rustOp (Cast CharType StringType) [x] = RApp (RRefUN (UN "idris_rts_char_to_str")) [x]

rustOp (Cast StringType IntegerType) [x] = RApp (RRefUN (UN "idris_rts_str_to_int")) [x] -- TODO: `to_integer`
rustOp (Cast StringType IntType) [x] = RApp (RRefUN (UN "idris_rts_str_to_int")) [x]
rustOp (Cast StringType DoubleType) [x] = RApp (RRefUN (UN "idris_rts_str_to_double")) [x]

rustOp (Cast from to) [x] = RCrash ("Invalid cast " ++ show from ++ "->" ++ show to)

rustOp BelieveMe [_, _, x] = x

rustOp op _ = RCrash ("Unknown operator " ++ show op) -- TODO: Remove this

mutual
  -- oops, no traverse for Vect in Core
  rustArgs : Int -> SVars vars -> Vect n (CExp vars) -> Core (Vect n RustExpr)
  rustArgs i vs [] = pure []
  rustArgs i vs (arg :: args) = pure $ !(rustExp i vs arg) :: !(rustArgs i vs args)

  expectMN : Name -> Core Int
  expectMN (MN n index) = pure index
  expectMN name = throw (InternalError ("Unexpected variable name " ++ show name))

  export
  rustExp : Int -> SVars vars -> CExp vars -> Core RustExpr
  rustExp i vs (CLocal fc el) = do
    index <- expectMN (lookupSVar el vs)
    pure $ RRefMN (MN (toNat index))
  rustExp i vs (CRef fc n) = pure $ RRefUN (UN (rustName n))
  rustExp i vs (CLam fc x sc) = do
    let vs' = extendSVars [x] vs
    sc' <- rustExp i vs' sc
    index <- expectMN (lookupSVar First vs')
    pure $ RLam (MN (toNat index)) sc'
  rustExp i vs (CLet fc x val sc) = do
    let vs' = extendSVars [x] vs
    val' <- rustExp i vs val
    sc' <- rustExp i vs' sc
    index <- expectMN (lookupSVar First vs')
    pure $ RLet (MN (toNat index)) val' sc'
  rustExp i vs (CApp fc x args) =
    pure $ RApp !(rustExp i vs x) !(traverse (rustExp i vs) args)
  rustExp i vs (CCon fc x tag args) =
    pure $ RCon tag!(traverse (rustExp i vs) args)
  rustExp i vs (COp fc op args) =
    pure $ rustOp op !(rustArgs i vs args)
  rustExp i vs (CExtPrim fc p args) =
    rustExtPrim i vs (toPrim p) args
  rustExp i vs (CForce fc t) =
    pure $ RForce !(rustExp i vs t)
  rustExp i vs (CDelay fc t) =
    pure $ RDelay !(rustExp i vs t)
  rustExp i vs (CConCase fc expr alts def) = do
    outExpr <- rustExp i vs expr
    outAlts <- traverse (toRustConAlt i vs) alts
    outDef <- maybe (pure Nothing) (\d => pure (Just !(rustExp i vs d))) def
    pure $ RConCase outExpr outAlts outDef
  where
    getNames : List Name -> SVars vars -> List Name
    getNames [] _ = []
    getNames _ [] = []
    getNames (n :: ns) (v :: vs) = v :: getNames ns vs
    toRustConAlt : Int -> SVars vars -> CConAlt vars -> Core RustConAlt
    toRustConAlt i vs (MkConAlt name tag args scope) = do
      let vs' = extendSVars args vs
      let argNames = getNames args vs'
      indexes <- traverse expectMN argNames
      let names = map (MN . toNat) indexes
      outScope <- rustExp i vs' scope
      pure $ MkConAlt tag names outScope
  rustExp i vs (CConstCase fc expr alts def) = do
    outExpr <- rustExp i vs expr
    outAlts <- traverse (toRustConstAlt i vs) alts
    outDef <- maybe (pure Nothing) (\d => pure (Just !(rustExp i vs d))) def
    case head' outAlts of
      Just (MkConstAlt (CStr _) _) => pure $ RStrConstCase outExpr outAlts outDef
      _ => pure $ RConstCase outExpr outAlts outDef
  where
    toRustConstAlt : Int -> SVars vars -> CConstAlt vars -> Core RustConstAlt
    toRustConstAlt i vs (MkConstAlt constant scope) = do
      outScope <- rustExp i vs scope
      pure $ MkConstAlt (rustConstant constant) outScope
  rustExp i vs (CPrimVal fc c) =
    pure $ RPrimVal (rustConstant c)
  rustExp i vs (CErased fc) =
    pure $ RErased
  rustExp i vs (CCrash fc msg) =
    pure $ RCrash msg

  rustExtPrim : Int -> SVars vars -> ExtPrim -> List (CExp vars) -> Core RustExpr
  rustExtPrim i vs PutStr [arg, world] = pure $ rustWorld $ RApp (RRefUN (UN "idris_rts_put_str")) [!(rustExp i vs arg)]
  rustExtPrim i vs GetStr [world] = pure $ rustWorld $ RApp (RRefUN (UN "idris_rts_get_str")) []
  rustExtPrim i vs Fork [action, world] = do
    let runAction = CApp EmptyFC (CRef EmptyFC (NS ["PrimIO"] (UN "unsafePerformIO"))) [CErased EmptyFC, action]
    pure $ rustWorld $ RFork !(rustExp i vs runAction)
  rustExtPrim i vs prim args = throw (InternalError ("Badly formed external primitive " ++ show prim ++ " " ++ show args))

rustArgList : SVars ns -> List RustMN
rustArgList [] = []
rustArgList ((MN n i) :: xs) = MN (toNat i) :: rustArgList xs
rustArgList (_ :: xs) = rustArgList xs

rustDef : {auto c : Ref Ctxt Defs} -> Name -> CDef -> Core String
rustDef n (MkFun args exp) =
  let vs = initSVars args in
  pure $ genDecl $ MkFun (UN (rustName !(getFullName n))) (rustArgList vs) !(rustExp 0 vs exp)
rustDef n (MkError exp) =
  pure "" -- TODO: Do I need this?
rustDef n (MkCon t a) =
  pure "" -- Nothing to compile here

-- Convert the name to Rust code
-- (There may be no code generated, for example if it's a constructor)
export
getRust : {auto c : Ref Ctxt Defs} -> Defs -> Name -> Core String
getRust defs n = do
  Just d <- lookupCtxtExact n (gamma defs)
    | throw (InternalError ("Compiling undefined name " ++ show n))
  let Just d = compexpr d
    | throw (InternalError ("No compiled definition for " ++ show n))
  rustDef n d
