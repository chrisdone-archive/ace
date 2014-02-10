{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Test suite for ACE.

module Main where

import ACE.Combinators
import ACE.Parsers
import ACE.Tokenizer (tokenize)
import ACE.Types.Syntax
import ACE.Types.Tokens
import Control.Monad

import Control.Applicative
import Data.Bifunctor
import Test.HUnit
import Test.Hspec
import Text.Parsec (Stream,ParsecT,parse,try)

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "tokenizer" tokenizer
  describe "parser" parser
  where
    tokenizer =
      do it "empty string" (tokenize "" == Right [])
         it "word" (tokenize "word" == Right [Word (1,0) "word"])
         it "period" (tokenize "." == Right [Period (1,0)])
         it "comma" (tokenize "," == Right [Comma (1,0)])
         it "question mark" (tokenize "?" == Right [QuestionMark (1,0)])
         it "quotation" (tokenize "\"foo\"" == Right [QuotedString (1,0) "foo"])
         it "empty-quotation" (isLeft (tokenize "\"\""))
         it "words" (tokenize "foo bar" == Right [Word (1,0) "foo",Word (1,4) "bar"])
         it "genitive '" (tokenize "foo'" == Right [Word (1,0) "foo",Genitive (1,3) False])
         it "genitive 's" (tokenize "foo's" == Right [Word (1,0) "foo",Genitive (1,3) True])
         it "newline" (tokenize "foo\nbar" == Right [Word (1,0) "foo",Word (2,0) "bar"])
    parser =
      do it "Universal global quantor"
            (parsed universalGlobalQuantor "for every" == Right ForEveryEach)
    isLeft = either (const True) (const False)

parsed p = tokenize >=> bimap show id . parse p ""
