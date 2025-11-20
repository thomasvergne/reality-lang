module Main where

import Control.Monad.Result
import Language.Reality.Backend.ANF.Converter qualified as ANF
import Language.Reality.Backend.Closure.Converter qualified as CC
import Language.Reality.Backend.Codegen qualified as CG
import Language.Reality.Backend.Specialization.Resolver qualified as SR
import Language.Reality.Frontend.Import.Resolver qualified as IR
import Language.Reality.Frontend.Module.Resolver qualified as MR
import Language.Reality.Frontend.Parser hiding (parseError, Parser)
import Language.Reality.Frontend.Parser.Toplevel qualified as T
import Language.Reality.Frontend.Typechecker.Checker qualified as TC
import System.Directory
import System.FilePath

import Options.Applicative qualified as App
import qualified Data.Text as Text

data Options = MkOptions 
    { optInputFile :: FilePath
    , optPathAliases :: [(Text, FilePath)]
    }

options :: App.Parser Options
options = MkOptions
    <$> App.strArgument
        ( App.metavar "FILE"
       <> App.help "Reality source file to compile" )
    <*> App.many (App.option (App.eitherReader parseAlias)
        ( App.long "path-alias"
       <> App.short 'p'
       <> App.metavar "ALIAS=PATH"
       <> App.help "Path alias for import resolution" ))
  where
    parseAlias :: String -> Either String (Text, FilePath)
    parseAlias s =
        case break (== '=') s of
            (alias, '=':path) -> Right (Text.pack alias, path)
            _ -> Left "Alias must be in the format ALIAS=PATH"

main :: IO ()
main = buildOutput =<< App.execParser opts
  where
    opts = App.info (App.helper <*> options)
      ( App.fullDesc
     <> App.progDesc "Compile a Reality source FILE to C"
     <> App.header "hello - a test for optparse-applicative" )


buildOutput :: Options -> IO ()
buildOutput (MkOptions inputFile pathAliases) = do
    let file = inputFile
        cwd  = takeDirectory file

    fileContent :: Text <- decodeUtf8 <$> readFileBS file

    result <- parseRealityFile file fileContent T.parseProgram

    let pathAliases' = fromList pathAliases
    pathAliases'' <- mapM makeAbsolute pathAliases'

    case result of
        Right ast -> do
            let pipeline =
                    IR.runImportResolver cwd pathAliases''
                        |> MR.runModuleResolver
                        |> TC.runTypechecker
                        |> SR.runSpecializationResolver
                        |> CC.convertProgram
                        |> ANF.convertToANF
                        |> CG.codegenProgram

            pipelineResult <- runExceptT $ pipeline ast

            let includes = ["<stdint.h>", "<stdbool.h>", "<pthread.h>", "<gc.h>"]
            let defines = ["#define GC_THREADS"]

            handle pipelineResult $ \cstr -> do
                let finalCstr = 
                        unlines defines 
                        <> unlines (map ("#include " <>) includes) 
                        <> "\n\n" 
                        <> cstr

                writeFileText (file -<.> "c") finalCstr
        Left err -> do
            parseError err file (Just fileContent)
