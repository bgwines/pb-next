{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Proto
    ( Tree(..)
    , Forest(..)
    , Proto(..)
    , ProtoObj(..)
    , Enum(..)
    , EnumField(..)
    , Message(..)
    , MessageField(..)
    , FieldQualifier(..)
    ) where

import Prelude hiding (Enum)
import qualified Data.Text as T

import Tree

type Forest l i = [Tree l i]

type Proto = Forest Enum Message
type ProtoObj = Tree Enum Message

data Enum = Enum T.Text [EnumField] deriving (Show, Eq)
data EnumField = EnumField T.Text Integer deriving (Show, Eq)

data Message = Message T.Text [MessageField] deriving (Show, Eq)
data MessageField = MessageField
    { fieldQualifier :: FieldQualifier
    , fieldType :: T.Text
    , name :: T.Text
    , value :: Integer
    } deriving (Show, Eq)

data FieldQualifier = Optional | Required | Repeated deriving (Show, Eq)
