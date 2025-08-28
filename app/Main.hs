module Main where

import Control.Color
import Control.Monad.Result
import Language.Reality.Frontend.Parser hiding (parseError)
import Language.Reality.Frontend.Parser.Toplevel qualified as T

main :: IO ()
main = do
    let file = "examples/structures.rl"
    fileContent :: Text <- decodeUtf8 <$> readFileBS file

    result <- parseRealityFile file fileContent T.parseProgram

    case result of
        Right ast -> do
            mapM_ printText ast
        Left err -> do
            parseError err file (Just fileContent)
