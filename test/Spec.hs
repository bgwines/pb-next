{-# LANGUAGE OverloadedStrings #-}

import PbNext.Tree
import PbNext.Proto
import qualified PbNext.ProtoParser as PP

import Test.Tasty
import Test.Tasty.HUnit

import qualified Text.Parsec as Parsec

testMessageFieldParsing1 :: Assertion
testMessageFieldParsing1 = do
    let proto = "optional string B = 1;\n"
    let expected = Right $ Right $ MessageField Optional "string" "B" 1
    let actual = Parsec.parse PP.messageFieldParser "" proto
    actual @?= expected

testMessageFieldParsing2 :: Assertion
testMessageFieldParsing2 = do
    let proto = "required int B = 11;\n"
    let expected = Right $ Right $ MessageField Required "int" "B" 11
    let actual = Parsec.parse PP.messageFieldParser "" proto
    actual @?= expected

testMessageFieldParsing3 :: Assertion
testMessageFieldParsing3 = do
    let proto = "optional string B = 1;// here's a comment!\n"
    let expected = Right $ Right $ MessageField Optional "string" "B" 1
    let actual = Parsec.parse PP.messageFieldParser "" proto
    actual @?= expected

testMessageFieldParsing4 :: Assertion
testMessageFieldParsing4 = do
    let proto = "optional string B = 1; [default = 2]"
    let expected = Right $ Right $ MessageField Optional "string" "B" 1
    let actual = Parsec.parse PP.messageFieldParser "" proto
    actual @?= expected

testEnumFieldParsing1 :: Assertion
testEnumFieldParsing1 = do
    let proto = "B = 1;\n"
    let expected = Right $ EnumField "B" 1
    let actual = Parsec.parse PP.enumFieldParser "" proto
    actual @?= expected

testEnumFieldParsing2 :: Assertion
testEnumFieldParsing2 = do
    let proto = "B = 10; // z\n"
    let expected = Right $ EnumField "B" 10
    let actual = Parsec.parse PP.enumFieldParser "" proto
    actual @?= expected

testEnumFieldParsing3 :: Assertion
testEnumFieldParsing3 = do
    let proto = "optional string B = 10;\n"
    let expected = Right $ EnumField "B" 10
    let actual = Parsec.parse PP.enumFieldParser "" proto
    actual @?= expected

testProtoObjParsing1 :: Assertion
testProtoObjParsing1 = do
    let proto = "message A {\noptional string B = 1;\n}\n"
    let expected = Right $ Internal [] $ Message "A" [MessageField Optional "string" "B" 1]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing2 :: Assertion
testProtoObjParsing2 = do
    let proto = "enum A {\nB = 1;\n\n}\n"
    let expected = Right $ Leaf $ Enum "A" [EnumField "B" 1]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing3 :: Assertion
testProtoObjParsing3 = do
    let proto = "message A {\noptional string B = 1;\nrequired string C = 2;\n}\n"
    let expected = Right $ Internal [] $ Message "A" [MessageField Optional "string" "B" 1, MessageField Required "string" "C" 2]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing4 :: Assertion
testProtoObjParsing4 = do
    let proto = "enum A {\nB = 1;\nC = 2;\n}\n"
    let expected = Right $ Leaf $ Enum "A" [EnumField "B" 1, EnumField "C" 2]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing5 :: Assertion
testProtoObjParsing5 = do
    let proto = "message A {\nmessage B {\noptional string C = 1;\n}\n}\n"
    let expected = Right $ Internal [Internal [] $ Message "B" [MessageField Optional "string" "C" 1]] $ Message "A" []
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing6 :: Assertion
testProtoObjParsing6 = do
    let proto = "message A {\nenum B {\nC = 1;\n}\n}\n"
    let expected = Right $ Internal [Leaf $ Enum "B" [EnumField "C" 1]] $ Message "A" []
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing7 :: Assertion
testProtoObjParsing7 = do
    let proto = "message BrokerJobStatus {\nenum State {\nBROKER_INCOMING = 0;\nWORKER_SLOT = 1;\nWORKER_PROCESSING = 2;\nBROKER_OUTGOING = 3;\nWORKER_DIRECT = 4;\nWORKER_PARALLEL = 5;\n}\noptional State state = 1;\noptional int usec = 2;\noptional int broker_index = 3;\noptional string worker_id = 4;\noptional int32 slot = 5;\noptional string likely_worker_id = 6;\n}\n"
    let expected = Right $ Internal [Leaf $ Enum "State" [ EnumField "BROKER_INCOMING" 0 , EnumField "WORKER_SLOT" 1 , EnumField "WORKER_PROCESSING" 2 , EnumField "BROKER_OUTGOING" 3 , EnumField "WORKER_DIRECT" 4 , EnumField "WORKER_PARALLEL" 5 ]] $ Message "BrokerJobStatus" [MessageField Optional "State" "state" 1 , MessageField Optional "int" "usec" 2 , MessageField Optional "int" "broker_index" 3 , MessageField Optional "string" "worker_id" 4 , MessageField Optional "int32" "slot" 5 , MessageField Optional "string" "likely_worker_id" 6 ]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing8 :: Assertion
testProtoObjParsing8 = do
    let proto = "enum A {\nB = 1; // x\nC = 2;\n}\n"
    let expected = Right $ Leaf $ Enum "A" [EnumField "B" 1, EnumField "C" 2]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing10 :: Assertion
testProtoObjParsing10 = do
    let proto = "message WorkerLoop {\n  optional string name = 1;\n}"
    let expected = Right $ Internal [] $ Message "WorkerLoop" [MessageField Optional "string" "name" 1]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing11 :: Assertion
testProtoObjParsing11 = do
    let proto = "message DependentRule {\n    optional string name = 1;\n    }"
    let expected = Right $ Internal [] $ Message "DependentRule" [MessageField Optional "string" "name" 1]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing12 :: Assertion
testProtoObjParsing12 = do
    let proto = "message WorkerLoop {\n  message DependentRule {\n    optional string name = 1;\n  }\n}"
    let expected = Right $ Internal [Internal [] $ Message "DependentRule" [MessageField Optional "string" "name" 1]] $ Message "WorkerLoop" []
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing13 :: Assertion
testProtoObjParsing13 = do
    let proto = "message WorkerLoop {\n  // DEPRECATED optional string name = 1;\n}"
    let expected = Right $ Internal [] $ Message "WorkerLoop" [MessageField Optional "string" "name" 1]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoObjParsing14 :: Assertion
testProtoObjParsing14 = do
    let proto = "message WorkerLoop {\n  // DEPRECATED: optional string name = 1;\n}"
    let expected = Right $ Internal [] $ Message "WorkerLoop" [MessageField Optional "string" "name" 1]
    let actual = Parsec.parse PP.protoTreeParser "" proto
    actual @?= expected

testProtoParsing1 :: Assertion
testProtoParsing1 = do
    let proto = "enum A {\nB = 1;\n}\n\nenum C {\nD = 1;\n}\n"
    let expected = Right [Leaf $ Enum "A" [EnumField "B" 1], Leaf $ Enum "C" [EnumField "D" 1]]
    let actual = Parsec.parse PP.protoParser "" proto
    actual @?= expected

testProtoParsing2 :: Assertion
testProtoParsing2 = do
    let proto = "import public \"workqueue-common.proto\";\nenum A {\nB = 1;\n}\n"
    let expected = Right [Leaf $ Enum "A" [EnumField "B" 1]]
    let actual = Parsec.parse PP.protoParser "" proto
    actual @?= expected

testProtoParsing3 :: Assertion
testProtoParsing3 = do
    let proto = "// Copyright 2013 Quip\n\npackage proto.workqueue;\noption java_package = \"com.quip.proto\";\noption java_outer_classname = \"workqueue\";\n\nimport public \"workqueue-common.proto\";\nenum A {\nB = 1;\n}\n"
    let expected = Right [Leaf $ Enum "A" [EnumField "B" 1]]
    let actual = Parsec.parse PP.protoParser "" proto
    actual @?= expected

testProtoParsing4 :: Assertion
testProtoParsing4 = do
    let proto = "message A {\n  // comment\n}"
    let expected = Right $ [Internal [] $ Message "A" []]
    let actual = Parsec.parse PP.protoParser "" proto
    actual @?= expected

testGetNext1 :: Assertion
testGetNext1 = do
    1 @?= 1

testParser :: TestTree
testParser = testGroup "Parser tests"
    [ testCase "MessageField parsing 1" testMessageFieldParsing1
    , testCase "MessageField parsing 2" testMessageFieldParsing2
    , testCase "MessageField parsing 3" testMessageFieldParsing3
    , testCase "MessageField parsing 4" testMessageFieldParsing4
    , testCase "EnumField parsing 1" testEnumFieldParsing1
    , testCase "EnumField parsing 2" testEnumFieldParsing2
    , testCase "EnumField parsing 3" testEnumFieldParsing3
    , testCase "ProtoObj parsing 1" testProtoObjParsing1
    , testCase "ProtoObj parsing 2" testProtoObjParsing2
    , testCase "ProtoObj parsing 3" testProtoObjParsing3
    , testCase "ProtoObj parsing 4" testProtoObjParsing4
    , testCase "ProtoObj parsing 5" testProtoObjParsing5
    , testCase "ProtoObj parsing 6" testProtoObjParsing6
    , testCase "ProtoObj parsing 7" testProtoObjParsing7
    , testCase "ProtoObj parsing 8" testProtoObjParsing8
    --, testCase "ProtoObj parsing 9" testProtoObjParsing9
    , testCase "ProtoObj parsing 10" testProtoObjParsing10
    , testCase "ProtoObj parsing 11" testProtoObjParsing11
    , testCase "ProtoObj parsing 12" testProtoObjParsing12
    , testCase "ProtoObj parsing 13" testProtoObjParsing13
    , testCase "ProtoObj parsing 14" testProtoObjParsing14
    , testCase "Proto parsing 1" testProtoParsing1
    , testCase "Proto parsing 2" testProtoParsing2
    , testCase "Proto parsing 3" testProtoParsing3
    , testCase "Proto parsing 4" testProtoParsing4
    ]

testAnalyzer :: TestTree
testAnalyzer = testGroup "Analyser tests"
    [ testCase "getNext 1" testGetNext1
    ]

main :: IO ()
main = defaultMain $ testGroup "All tests" [testParser, testAnalyzer]
