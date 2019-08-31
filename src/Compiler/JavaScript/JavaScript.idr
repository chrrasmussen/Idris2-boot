module Compiler.JavaScript.JavaScript

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.JavaScript.Common
import Compiler.JavaScript.JavaScriptExpr

import Core.Context
import Core.Directory
import Core.Name
import Core.TT

import Data.NameMap
import Data.Vect
import System
import System.Info

%default covering

findNodeJS : IO String
findNodeJS = pure "node"

header : String
header = ""

quoted : String -> String
quoted path = "'" ++ path ++ "'"

compileToJavaScript : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core ()
compileToJavaScript c tm outfile
    = do (ns, tags) <- findUsedNames tm
         defs <- get Ctxt
         compdefs <- traverse (getJavaScript defs) ns
         let code = concat compdefs
         mainExpr <- jsExp 0 [] !(compileExp tags tm)
         let main = genExpr mainExpr
         support <- readDataFile "javascript/support.js"
         let scm = header ++ support ++ code ++ "\n" ++ main ++ "\n"
         Right () <- coreLift $ writeFile outfile scm
            | Left err => throw (FileErr outfile err)
         coreLift $ chmod outfile 0o755
         pure ()

compileExpr : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c tm outfile
    = do let outSrc = outfile ++ ".js"
         compileToJavaScript c tm outSrc
         pure (Just outfile)
         -- TODO: Compile using Google Closure Compiler?
         --tmpDir <- coreLift $ tmpName
         --let outSrc = tmpDir ++ "/main.js"
         --coreLift $ system ("mkdir -p " ++ quoted tmpDir)
         --compileToJavaScript c tm outSrc
         --nodeJS <- coreLift findNodeJS
         --ok <- coreLift $ system (nodeJS ++ " -o " ++ outfile ++ " " ++ outSrc)
         --if ok == 0
         --   then pure (Just outfile)
         --   else pure Nothing

executeExpr : Ref Ctxt Defs -> ClosedTerm -> Core ()
executeExpr c tm
    = do tmpDir <- coreLift $ tmpName
         let outSrc = tmpDir ++ "/main.js"
         coreLift $ system ("mkdir -p " ++ quoted tmpDir)
         compileToJavaScript c tm outSrc
         nodeJS <- coreLift findNodeJS
         coreLift $ system (nodeJS ++ " " ++ outSrc)
         pure ()

export
codegenJavaScript : Codegen
codegenJavaScript = MkCG compileExpr executeExpr
