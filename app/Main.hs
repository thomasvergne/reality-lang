module Main where

import Control.Color
import Control.Monad.Result
import Language.Reality.Backend.Closure.Converter qualified as CC
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

            pipelineResult <- runExceptT $ pipeline ast

            handle pipelineResult $ \tlir -> do
                mapM_ printText tlir
        Left err -> do
            parseError err file (Just fileContent)
