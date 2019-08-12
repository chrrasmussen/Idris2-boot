module Compiler.Rust.Rust

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.Rust.Common
import Compiler.Rust.RustExpr

import Core.Context
import Core.Directory
import Core.Name
import Core.TT

import Data.NameMap
import Data.Vect
import System
import System.Info

%default covering

findRustc : IO String
findRustc = pure "rustc"

header : String
header = ""

quoted : String -> String
quoted path = "'" ++ path ++ "'"

compileToRust : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core ()
compileToRust c tm outfile
    = do (ns, tags) <- findUsedNames tm
         defs <- get Ctxt
         compdefs <- traverse (getRust defs) ns
         let code = concat compdefs
         mainExpr <- rustExp 0 [] !(compileExp tags tm)
         let main = genExprNoArgs mainExpr
         support <- readDataFile "rust/support.rs"
         let scm = header ++ support ++ code ++ "fn main() { " ++ main ++ " }\n"
         Right () <- coreLift $ writeFile outfile scm
            | Left err => throw (FileErr outfile err)
         coreLift $ chmod outfile 0o755
         pure ()

compileExpr : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c tm outfile
    = do tmpDir <- coreLift $ tmpName
         let outSrc = tmpDir ++ "/main.rs"
         coreLift $ system ("mkdir -p " ++ quoted tmpDir)
         compileToRust c tm outSrc
         rustc <- coreLift findRustc
         ok <- coreLift $ system (rustc ++ " -o " ++ outfile ++ " " ++ outSrc)
         if ok == 0
            then pure (Just outfile)
            else pure Nothing

executeExpr : Ref Ctxt Defs -> ClosedTerm -> Core ()
executeExpr c tm
    = do tmpDir <- coreLift $ tmpName
         let outSrc = tmpDir ++ "/main.rs"
         let outExec = tmpDir ++ "/main"
         coreLift $ system ("mkdir -p " ++ quoted tmpDir)
         compileToRust c tm outSrc
         rustc <- coreLift findRustc
         coreLift $ system (rustc ++ " -o " ++ outExec ++ " " ++ outSrc)
         coreLift $ system outExec
         pure ()

export
codegenRust : Codegen
codegenRust = MkCG compileExpr executeExpr

