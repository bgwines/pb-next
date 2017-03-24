{-# LANGUAGE InstanceSigs #-}

import System.Environment
import System.Exit

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Options.Applicative

import Data.Default (def)
import Data.Text (pack, unpack)
import Data.Either.Combinators (isLeft, fromLeft, mapLeft)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO (readFile)

import PbNext.Proto
import PbNext.ProtoParser
import PbNext.Analyzer

import qualified Text.Parsec as Parsec

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
run (Next messageName filePath) = do
    result <- runEitherT $ proto >>= getNext (pack messageName)
    case result of
        Left msg -> putStrLn $ msg
        Right next -> print next
    where
        proto :: EitherT String IO Proto
        proto = do
            file <- liftIO $ Data.Text.IO.readFile filePath
            EitherT . return . mapLeft show $ Parsec.parse protoParser "" file

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "CLI for PB file static analysis")
