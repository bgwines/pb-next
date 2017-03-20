{-# LANGUAGE InstanceSigs #-}

import System.Environment
import System.Exit

import Control.Monad
import Control.Monad.Trans.Either

import Data.Either.Combinators (isLeft, fromLeft)

import Options.Applicative

import Data.Default (def)

import Data.Text (pack, unpack)

import PbNext.PbAnalyser

-- | Schema for arguments to exposed commands
data Command = Next String FilePath deriving (Show)

parseNext :: Parser Command
parseNext = Next <$> argument str (metavar "MESSAGE-NAME-OR-ENUM-NAME") <*>
    argument str (metavar "FILE-OR-DIRECTORY")

parseCommand :: Parser Command
parseCommand = subparser $ command "next" (parseNext `withInfo` nextHelp)
    where
        nextHelp :: String
        nextHelp = "Generate a valid PB field value for the specified message"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Command -> IO ()
run (Next message path) = do
    result <- runEitherT (getNext (pack message) path)
    case result of
        Left msg -> putStrLn $ msg
        Right next -> print next

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "CLI for PB file static analysis")
