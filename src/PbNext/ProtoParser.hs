{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PbNext.ProtoParser
    ( protoParser
    , protoObjParser
    , enumFieldParser
    , messageFieldParser
    , fieldQualifierParser
    ) where

import PbNext.Tree
import PbNext.Proto

import Prelude hiding (maybe, Enum)

import Control.Monad
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Text (Text, pack, unpack)
import Data.Maybe (catMaybes)
import Data.Either (lefts, rights)

import Text.Parsec hiding (token)
import Text.Parsec.Char
import Text.Parsec.Text (Parser)

protoParser :: Parser Proto
protoParser = parseNWithFallback protoObjParser line

protoObjParser :: Parser ProtoObj
protoObjParser = do
    dataType <- token
    case dataType of
            "message" -> (uncurry Internal) <$> messageParser
            "enum" -> Leaf <$> enumParser
            _ -> fail "Failed to parse message fields"

messageParser :: Parser ([ProtoObj], Message)
messageParser = do
    name <- token
    elems <- brackets (parseNWithFallback messageFieldParser comment)
    let nonRecFields = rights elems
    let recFields = lefts elems
    return $ (recFields, Message name nonRecFields)

enumParser :: Parser Enum
enumParser
    =   Enum
    <$> token
    <*> brackets (parseNWithFallback enumFieldParser comment)

enumFieldParser :: Parser EnumField
enumFieldParser = do
    maybe deprecated
    fieldName <- token
    char '='
    whitespace
    value <- positiveNatural
    line
    return $ EnumField fieldName value

messageFieldParser :: Parser (Either ProtoObj MessageField)
messageFieldParser = do
    maybe deprecated
    fieldQualifierOrProtoObjDecl <- token
    case fieldQualifierOrProtoObjDecl of
        "message" -> (Left . uncurry Internal) <$> messageParser
        "enum" -> (Left . Leaf) <$> enumParser
        fieldQualifier -> Right <$> (nonRecMessageFieldParser' $ textToFieldQualifier fieldQualifier)
    where
        nonRecMessageFieldParser' :: FieldQualifier -> Parser MessageField
        nonRecMessageFieldParser' fieldQualifier = do
            fieldType <- token
            fieldName <- token
            char '='
            whitespace
            value <- positiveNatural
            line
            return $ MessageField fieldQualifier fieldType fieldName value

fieldQualifierParser :: Parser FieldQualifier
fieldQualifierParser = textToFieldQualifier <$> token

textToFieldQualifier :: Text -> FieldQualifier
textToFieldQualifier "optional" = Optional
textToFieldQualifier "required" = Required
textToFieldQualifier "repeated" = Repeated
textToFieldQualifier text = error $ "Parsing FieldQualifier failed: " ++ unpack text

deprecated :: Parser ()
deprecated
    = void . maybe
    $ (    string "//"
        >> maybe space
        >> string "DEPRECATED"
        >> maybe (choice [char '.', char ':'])
        >> char ' ' )

brackets :: Parser a -> Parser a
brackets = between (string "{" >> endl) (char '}')

positiveNatural :: Parser Integer
positiveNatural = fromIntegral . digitsToInt <$> many1 digit
    where
        digitsToInt :: [Char] -> Int
        digitsToInt = foldl' (\acc ch -> acc * 10 + digitToInt ch) 0

comment :: Parser Text
comment = pack <$> (whitespace >> char '/' >> char '/' >> many1 (noneOf "\n"))

line :: Parser Text
line = pack <$> many1 (noneOf "\n")

maybe :: Parser a -> Parser (Maybe a)
maybe = try . optionMaybe

token :: Parser Text
token = pack <$> (many1 char' <* whitespace)
    where
        char' :: Parser Char
        char' = choice [letter, char '_', char '.', digit]

whitespace :: Parser ()
whitespace = skipMany $ char ' '

endl :: Parser [()]
endl = many1 $ newline >> whitespace

parseNWithFallback :: forall a b. Parser a -> Parser b -> Parser [a]
parseNWithFallback posParser negParser = catMaybes <$> sepEndBy maybeParser (many1 endl)
    where
        maybeParser :: Parser (Maybe a)
        maybeParser = (try $ Just <$> posParser) <|> (const Nothing <$> negParser)
