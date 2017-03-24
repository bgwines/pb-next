{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module PbNext.PbAnalyser
( getNext
) where

import Data.List
import Data.Maybe
import Data.Either.Combinators

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO (readFile)

import qualified Text.Parsec as Parsec

import PbNext.Proto
import PbNext.ProtoParser

parse :: FilePath -> EitherT Parsec.ParseError IO Proto
parse fileName = do
    file <- liftIO $ Data.Text.IO.readFile fileName
    EitherT . return $ Parsec.parse protoParser "" file

getNext :: Text -> FilePath -> EitherT String IO Integer
getNext messageName fileName = do
    proto <- mapLeftEitherT show $ parse fileName
    let matchingMessages = concatMap (foldMap (getMatchingProtoObjs messageName)) proto
    case matchingMessages of
        [] -> fail $ "No proto-objs found with name " ++ unpack messageName
        (message:[]) -> right $ getNext' message
        _ -> fail $ "Multiple proto-objs found with name " ++ unpack messageName

getNext' :: PbNode -> Integer
getNext' (Message _ fields) = getNext'' fields messageFieldValue
getNext' (Enum _ fields) = getNext'' fields enumFieldValue

getNext'' fields getValue
    = maybe (succ . maximum $ fieldNums) fst
    . find (uncurry (/=))
    $ zip [1..] fieldNums
    where
        fieldNums :: [Integer]
        fieldNums = sort . map getValue $ fields

-- TODO: something fancy like fmap . fmap?
mapLeftEitherT :: Monad m => (l -> l') -> EitherT l m r -> EitherT l' m r
mapLeftEitherT getMatchingProtoObjs
    = EitherT
    . fmap (mapLeft getMatchingProtoObjs)
    . runEitherT

-- for debugging
getName :: PbNode -> [Text]
getName e@(Enum name _) = [name]
getName m@(Message name _) = [name]

getMatchingProtoObjs :: Text -> PbNode -> [PbNode]
getMatchingProtoObjs query e@(Enum name _)
    | name == query = [e]
    | otherwise = []
getMatchingProtoObjs query m@(Message name _)
    | name == query = [m]
    | otherwise = []
