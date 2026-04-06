module Main (main) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Text.Megaparsec

import Lib

assertParseSuccess :: String -> JValue -> Assertion
assertParseSuccess input expected =
  case fromJSON input of
    Left err -> assertFailure $ "\n" ++ errorBundlePretty err
    Right result -> assertEqual ("Parsing: " ++ input) expected result

assertParseFailure :: String -> Assertion
assertParseFailure input =
  case fromJSON input of
    Left _ -> return ()
    Right result -> assertFailure $ "Expected parse error. Instead got: " ++ show result

data ParseSuccessCase = ParseSuccessCase String String JValue

data ParseFailureCase = ParseFailureCase String String

ok :: String -> String -> JValue -> ParseSuccessCase
ok = ParseSuccessCase

bad :: String -> String -> ParseFailureCase
bad = ParseFailureCase

toSuccessTest :: ParseSuccessCase -> Test
toSuccessTest (ParseSuccessCase name input expected) =
  testCase name $ assertParseSuccess input expected

toFailureTest :: ParseFailureCase -> Test
toFailureTest (ParseFailureCase name input) =
  testCase name $ assertParseFailure input

correctFormatInput :: String
correctFormatInput =
  "{\"JSON Test Pattern pass3\":{\"The outermost value\":\"must be an object or array.\",\"In this test\":\"It is an object.\"}}"

correctFormatExpected :: JValue
correctFormatExpected =
  JObject
    [
      ( "JSON Test Pattern pass3"
      , JObject
          [ ("The outermost value", JString "must be an object or array.")
          , ("In this test", JString "It is an object.")
          ]
      )
    ]

generalCaseInput :: String
generalCaseInput =
  "[\"JSON Test Pattern pass1\",{\"object with 1 member\":[\"array with 1 element\"]},{},[],-42,true,false,null,{\"integer\":1234567890,\"real\":-9876.54321,\"e\":1.23456789e-13,\"E\":1.23456789e+34,\"\":2.3456789012e+76,\"zero\":0,\"one\":1,\"space\":\" \",\"quote\":\"\\\"\",\"backslash\":\"\\\\\",\"controls\":\"\\b\\f\\n\\r\\t\",\"slash\":\"/ & /\",\"alpha\":\"abcdefghijklmnopqrstuvwyz\",\"ALPHA\":\"ABCDEFGHIJKLMNOPQRSTUVWYZ\",\"digit\":\"0123456789\",\"0123456789\":\"digit\",\"special\":\"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",\"hex\":\"ģ䕧覫췯屴屴\",\"true\":true,\"false\":false,\"null\":null,\"array\":[],\"object\":{},\"address\":\"50 St. James Street\",\"url\":\"http://www.JSON.org/\",\"comment\":\"// /* <!-- --\",\"# -- --> */\":\" \",\" s p a c e d \":[1,2,3,4,5,6,7],\"compact\":[1,2,3,4,5,6,7],\"jsontext\":\"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",\"quotes\":\"&#34; \\\" %22 0x22 034 &#x22;\",\"/\\\\\\\"쫾몾ꮘﳞ볚屴\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\":\"A key can be any string\"},0.5,98.6,99.44,1066,10,1,0.1,1,2,2,\"rosebud\"]"

generalCaseExpected :: JValue
generalCaseExpected =
  JArray
    [ JString "JSON Test Pattern pass1"
    , JObject [("object with 1 member", JArray [JString "array with 1 element"])]
    , JObject []
    , JArray []
    , JInt (-42)
    , JBool True
    , JBool False
    , JNull
    , JObject
        [ ("integer", JInt 1234567890)
        , ("real", JDouble (-9876.54321))
        , ("e", JDouble (1.23456789e-13))
        , ("E", JDouble (1.23456789e+34))
        , ("", JDouble (2.3456789012e+76))
        , ("zero", JInt 0)
        , ("one", JInt 1)
        , ("space", JString " ")
        , ("quote", JString "\"")
        , ("backslash", JString "\\")
        , ("controls", JString "\b\f\n\r\t")
        , ("slash", JString "/ & /")
        , ("alpha", JString "abcdefghijklmnopqrstuvwyz")
        , ("ALPHA", JString "ABCDEFGHIJKLMNOPQRSTUVWYZ")
        , ("digit", JString "0123456789")
        , ("0123456789", JString "digit")
        , ("special", JString "`1~!@#$%^&*()_+-={':[,]}|;.</>?")
        , ("hex", JString "ģ䕧覫췯屴屴")
        , ("true", JBool True)
        , ("false", JBool False)
        , ("null", JNull)
        , ("array", JArray [])
        , ("object", JObject [])
        , ("address", JString "50 St. James Street")
        , ("url", JString "http://www.JSON.org/")
        , ("comment", JString "// /* <!-- --")
        , ("# -- --> */", JString " ")
        , (" s p a c e d ", JArray (map JInt [1, 2, 3, 4, 5, 6, 7]))
        , ("compact", JArray (map JInt [1, 2, 3, 4, 5, 6, 7]))
        , ("jsontext", JString "{\"object with 1 member\":[\"array with 1 element\"]}")
        , ("quotes", JString "&#34; \" %22 0x22 034 &#x22;")
        , ("/\\\"쫾몾ꮘﳞ볚屴\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?", JString "A key can be any string")
        ]
    , JDouble 0.5
    , JDouble 98.6
    , JDouble 99.44
    , JInt 1066
    , JInt 10
    , JInt 1
    , JDouble 0.1
    , JInt 1
    , JInt 2
    , JInt 2
    , JString "rosebud"
    ]

parseSuccessCases :: [ParseSuccessCase]
parseSuccessCases =
  [ ok "Null" "null" JNull
  , ok "True" "true" (JBool True)
  , ok "False" "false" (JBool False)
  , ok "Number" "123.45" (JDouble 123.45)
  , ok "String" "\"hello\"" (JString "hello")
  , ok "Array" "[null,true,1]" (JArray [JNull, JBool True, JInt 1])
  , ok "Object" "{\"a\":null,\"b\":false}" (JObject [("a", JNull), ("b", JBool False)])
  , ok "Negative number" "-21" (JInt (-21))
  , ok "Empty string" "\"\"" (JString "")
  , ok "Correct format" correctFormatInput correctFormatExpected
  , ok "Deep nesting" "[[[[[\"Deep nesting\"]]]]]" (JArray [JArray [JArray [JArray [JArray [JString "Deep nesting"]]]]])
  , ok "General case" generalCaseInput generalCaseExpected
  , ok "Empty array" "[]" (JArray [])
  , ok "Empty object" "{}" (JObject [])
  , ok "Root whitespace" "  \n\t {\"k\" : [1,2,3]} \r\n " (JObject [("k", JArray [JInt 1, JInt 2, JInt 3])])
  , ok "Escaped characters in string" "\"line\\nfeed\\tquote:\\\"\"" (JString "line\nfeed\tquote:\"")
  ]

parseFailureCases :: [ParseFailureCase]
parseFailureCases =
  [ bad "Misspelled null" "nulls"
  , bad "Explicit string" "Explicit string"
  , bad "Unclosed array" "[\"Unclosed array\""
  , bad "Unquoted key" "{unquotedKey:true}"
  , bad "Trailing comma in array" "[\"Trailing comma\",]"
  , bad "Double comma in array" "[\"Double comma\",,]"
  , bad "Missing value in array" "[,\"Missing value\"]"
  , bad "Comma after closed array" "[\"Comma after closing bracket\"],"
  , bad "Extra bracket" "[\"Extra bracket\"]]"
  , bad "Trailing comma in object" "{\"Trailing comma\":true,}"
  , bad "Extra value after object" "{\"key\":\"value\"}\"Extra value after object\""
  , bad "Illegal expression" "{\"Illegal expression\":1+2}"
  , bad "Illegal invocation" "{\"Illegal invocation\":alert()}"
  , bad "Hex number" "{\"Hex number\":0x1A}"
  , bad "Illegal escape" "{\"Illegal escape\":\"\\x\"}"
  , bad "Escape outside string" "[\n]"
  , bad "Incorrect nesting" "[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]"
  , bad "Missing colon" "{\"key\"\"Missing colon\"}"
  , bad "Double colon" "{\"key\"::\"Double colon\"}"
  , bad "Comma instead of colon in object" "{\"key\",\"Comma instead of colon\"}"
  , bad "Colon instead of comma in array" "[\"Colon instead of comma\":false]"
  , bad "Incorrect value" "[\"Incorrect value\",truth]"
  , bad "Single quote" "['single quote']"
  , bad "Incorrect exponent" "[\"Incorrect exponent\",0e]"
  , bad "Incorrect plus in exponent" "[\"Incorrect plus in exponent\",0e+]"
  , bad "Incorrect minus in exponent" "[\"Incorrect minus in exponent\",0e+-1]"
  , bad "Comma instead of closing brace" "{\"key\":\"value\","
  , bad "Mismatch" "[\"Mismatch\"}"
  , bad "Dangling minus" "-"
  , bad "Unterminated string" "\"abc"
  , bad "Missing object value" "{\"k\":}"
  , bad "Missing comma between object pairs" "{\"a\":1 \"b\":2}"
  ]

passedParseTests :: [Test]
passedParseTests = map toSuccessTest parseSuccessCases

failedParseTests :: [Test]
failedParseTests = map toFailureTest parseFailureCases

newtype GeneratedJValue = GeneratedJValue JValue
  deriving (Show, Eq)

instance Arbitrary GeneratedJValue where
  arbitrary = GeneratedJValue <$> sized genJsonValue

genJsonValue :: Int -> Gen JValue
genJsonValue size
  | size <= 0 = genPrimitiveJsonValue
  | otherwise =
      frequency
        [ (4, genPrimitiveJsonValue)
        , (1, JArray <$> resize nextSize (listOf (genJsonValue nextSize)))
        , (1, JObject <$> resize nextSize (listOf ((,) <$> genJsonString <*> genJsonValue nextSize)))
        ]
 where
  nextSize = size `div` 2

genPrimitiveJsonValue :: Gen JValue
genPrimitiveJsonValue =
  oneof
    [ return JNull
    , JBool <$> arbitrary
    , JInt <$> arbitrary
    , JDouble <$> genFiniteDouble
    , JString <$> genJsonString
    ]

genFiniteDouble :: Gen Double
genFiniteDouble = arbitrary `suchThat` (\d -> not (isNaN d || isInfinite d))

genJsonString :: Gen String
genJsonString = listOf $ elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " -_")

propRoundTripJson :: GeneratedJValue -> Property
propRoundTripJson (GeneratedJValue v) =
  let s = toJSON v
   in case fromJSON s of
        Right v' -> v' === v
        Left _ -> property False

quickCheckPropertyCases :: [(String, Property)]
quickCheckPropertyCases =
  [ ("Randomized identity test", property propRoundTripJson)
  ]

qcTests :: [Test]
qcTests = map (uncurry testProperty) quickCheckPropertyCases

tests :: [Test]
tests =
  [ testGroup "Parse Success" passedParseTests
  , testGroup "Parse Failure" failedParseTests
  , testGroup "Identity" qcTests
  ]

main :: IO ()
main = defaultMain tests
