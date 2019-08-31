module Compiler.Swift.Swift

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.Swift.Common
import Compiler.Swift.SwiftExpr

import Core.Context
import Core.Directory
import Core.Name
import Core.TT

import Data.NameMap
import Data.Vect
import System
import System.Info

%default covering

findSwift : IO String
findSwift = pure "swift"

findSwiftc : IO String
findSwiftc = pure "swiftc"

header : String
header = ""

quoted : String -> String
quoted path = "'" ++ path ++ "'"

compileToSwift : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core ()
compileToSwift c tm outfile
    = do (ns, tags) <- findUsedNames tm
         defs <- get Ctxt
         compdefs <- traverse (getSwift defs) ns
         let code = concat compdefs
         mainExpr <- swiftExp 0 [] !(compileExp tags tm)
         let main = genExpr mainExpr
         support <- readDataFile "swift/Support.swift"
         let scm = header ++ support ++ code ++ "\n" ++ main ++ "\n"
         Right () <- coreLift $ writeFile outfile scm
            | Left err => throw (FileErr outfile err)
         coreLift $ chmod outfile 0o755
         pure ()

compileExpr : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c tm outfile
    = do tmpDir <- coreLift $ tmpName
         let outSrc = tmpDir ++ "/Main.swift"
         coreLift $ system ("mkdir -p " ++ quoted tmpDir)
         compileToSwift c tm outSrc
         swiftc <- coreLift findSwiftc
         ok <- coreLift $ system (swiftc ++ " -suppress-warnings -O -o " ++ outfile ++ " " ++ outSrc)
         if ok == 0
            then pure (Just outfile)
            else pure Nothing

executeExpr : Ref Ctxt Defs -> ClosedTerm -> Core ()
executeExpr c tm
    = do tmpDir <- coreLift $ tmpName
         let outSrc = tmpDir ++ "/Main.swift"
         coreLift $ system ("mkdir -p " ++ quoted tmpDir)
         compileToSwift c tm outSrc
         swift <- coreLift findSwift
         coreLift $ system (swift ++ " -suppress-warnings " ++ outSrc)
         pure ()

export
codegenSwift : Codegen
codegenSwift = MkCG compileExpr executeExpr
