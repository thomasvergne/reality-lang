module Main where

import Control.Color
import Control.Monad.Result
import Language.Reality.Frontend.Import.Resolver qualified as IR
import Language.Reality.Frontend.Module.Resolver qualified as MR
import Language.Reality.Frontend.Parser hiding (parseError)
import Language.Reality.Frontend.Parser.Toplevel qualified as T
import System.Directory
import System.FilePath

main :: IO ()
main = do
    cwd <- (</> "examples") <$> getCurrentDirectory
    let file = "examples/main.rl"
    fileContent :: Text <- decodeUtf8 <$> readFileBS file

    result <- parseRealityFile file fileContent T.parseProgram

    case result of
        Right ast -> do
            irResult <- runExceptT $ IR.runImportResolver cwd ast

            handle irResult $ \ir -> do
                mrResult <- runExceptT $ MR.runModuleResolver ir

                handle mrResult $ \mr -> do
                    mapM_ printText mr
        Left err -> do
            parseError err file (Just fileContent)
