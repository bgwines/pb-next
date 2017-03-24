{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module PbNext.Analyzer
( getNext
) where

import Data.List
import Data.Maybe

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Text (Text, pack, unpack)

import qualified Text.Parsec as Parsec

import PbNext.Proto
import PbNext.ProtoParser

getNext :: Text -> Proto -> EitherT String IO Integer
getNext messageName proto = do
    let matchingMessages = concatMap (foldMap (getMatchingProtoObjs messageName)) proto
    case matchingMessages of
        [] -> fail $ "No proto-objs found with name " ++ unpack messageName
        (message:[]) -> right $ getNext' message
        _ -> fail $ "Multiple proto-objs found with name " ++ unpack messageName
    where
        getNext' :: PbNode -> Integer
        getNext' (Message _ fields) = getNextValue $ map messageFieldValue fields
        getNext' (Enum _ fields) = getNextValue $ map enumFieldValue fields

getNextValue :: [Integer] -> Integer
getNextValue values
    = maybe (succ . maximum $ values) fst
    . find (uncurry (/=))
    $ zip [1..] $ sort values

getMatchingProtoObjs :: Text -> PbNode -> [PbNode]
getMatchingProtoObjs query e@(Enum name _)
    | name == query = [e]
    | otherwise = []
getMatchingProtoObjs query m@(Message name _)
    | name == query = [m]
    | otherwise = []
