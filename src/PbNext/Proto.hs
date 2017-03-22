{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE DisambiguateRecordFields #-}

module PbNext.Proto
    ( Proto
    , PbNode(..)
    , EnumField(..)
    , MessageField(..)
    , FieldQualifier(..)
    ) where

import Prelude hiding (Enum)
import qualified Data.Text as T

import PbNext.Tree

type Proto = Forest PbNode

data PbNode
    = Enum T.Text [EnumField]
    | Message T.Text [MessageField]
    deriving (Show, Eq)

data EnumField = EnumField
    { enumFieldName :: T.Text
    , enumFieldValue :: Integer
    } deriving (Show, Eq)

data MessageField = MessageField
    { fieldQualifier :: FieldQualifier
    , fieldType :: T.Text
    , messageFieldName :: T.Text
    , messageFieldValue :: Integer
    } deriving (Show, Eq)

data FieldQualifier = Optional | Required | Repeated deriving (Show, Eq)
