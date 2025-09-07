module Main where

import Control.Monad.Result
import Language.Reality.Backend.ANF.Converter qualified as ANF
import Language.Reality.Backend.Closure.Converter qualified as CC
import Language.Reality.Backend.Codegen qualified as CG
import Language.Reality.Backend.Specialization.Resolver qualified as SR
import Language.Reality.Frontend.Import.Resolver qualified as IR
import Language.Reality.Frontend.Module.Resolver qualified as MR
import Language.Reality.Frontend.Parser hiding (parseError)
import Language.Reality.Frontend.Parser.Toplevel qualified as T
import Language.Reality.Frontend.Typechecker.Checker qualified as TC
import System.Directory
import System.FilePath

main :: IO ()
main = do
    cwd <- (</> "examples") <$> getCurrentDirectory
    let file = "examples/structures.rl"
    fileContent :: Text <- decodeUtf8 <$> readFileBS file

    result <- parseRealityFile file fileContent T.parseProgram

    case result of
        Right ast -> do
            let pipeline =
                    IR.runImportResolver cwd
                        |> MR.runModuleResolver
                        |> TC.runTypechecker
                        |> SR.runSpecializationResolver
                        |> CC.convertProgram
                        |> ANF.convertToANF
                        |> CG.codegenProgram

            pipelineResult <- runExceptT $ pipeline ast

            let includes = ["<stdint.h>", "<stdbool.h>", "<pthread.h>"]

            handle pipelineResult $ \cstr -> do
                let finalCstr = unlines (map ("#include " <>) includes) <> "\n\n" <> cstr

                writeFileText (file -<.> "c") finalCstr
        Left err -> do
            parseError err file (Just fileContent)
