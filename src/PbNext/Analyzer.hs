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
        (message:[]) -> right $ getNextValue . getFieldValues $ message
        _ -> fail $ "Multiple proto-objs found with name " ++ unpack messageName

getFieldValues :: PbNode -> [Integer]
getFieldValues (Message _ fields) = map messageFieldValue fields
getFieldValues (Enum _ fields) = map enumFieldValue fields

getNextValue :: [Integer] -> Integer
getNextValue values
    = maybe (succ . maximum $ 0 : values') fst
    . find (uncurry (/=))
    $ zip [1..] $ sort values'
    where
        -- Don't suggest 0, but don't get tripped up by
        -- its presence / absence
        values' :: [Integer]
        values' = filter ((/=) 0) values

getName e@(Enum name _) = [name]
getName m@(Message name _) = [name]

getMatchingProtoObjs :: Text -> PbNode -> [PbNode]
getMatchingProtoObjs query e@(Enum name _)
    | name == query = [e]
    | otherwise = []
getMatchingProtoObjs query m@(Message name _)
    | name == query = [m]
    | otherwise = []
