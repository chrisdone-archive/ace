{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Test suite for ACE.

module Main where

import ACE.Combinators
import ACE.Parsers
import ACE.Tokenizer (tokenize)
import ACE.Types.Syntax
import ACE.Types.Tokens

import Control.Applicative
import Control.Monad hiding (ap)
import Control.Monad.Identity hiding (ap)
import Data.Bifunctor
import Data.Text (Text)
import Test.HUnit
import Test.Hspec
import Text.Parsec (Stream,ParsecT,runP,try,Parsec,ParseError)
import Text.Parsec.Prim

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "tokenizer" tokenizer
  describe "parser" parser

-- | Tests for the tokenizer.
tokenizer :: Spec
tokenizer =
  do it "empty string"
        (tokenize "" == Right [])
     it "word"
        (tokenize "word" == Right [Word (1,0) "word"])
     it "period"
        (tokenize "." == Right [Period (1,0)])
     it "comma"
        (tokenize "," == Right [Comma (1,0)])
     it "number"
        (tokenize "55" == Right [Number (1,0) 55])
     it "question mark"
        (tokenize "?" == Right [QuestionMark (1,0)])
     it "quotation"
        (tokenize "\"foo\"" == Right [QuotedString (1,0) "foo"])
     it "empty-quotation"
        (isLeft (tokenize "\"\""))
     it "words"
        (tokenize "foo bar" == Right [Word (1,0) "foo",Word (1,4) "bar"])
     it "genitive '"
        (tokenize "foo'" == Right [Word (1,0) "foo",Genitive (1,3) False])
     it "genitive 's"
        (tokenize "foo's" == Right [Word (1,0) "foo",Genitive (1,3) True])
     it "newline"
        (tokenize "foo\nbar" == Right [Word (1,0) "foo",Word (2,0) "bar"])

-- | Parser tests.
parser :: Spec
parser =
  do simple
     adjective
     ap'
     copulae
     complVs
     genitiveNP
     possessives
     specifiers
     verbs
     adverbs
     verbModifiers
     nouns
     relatives

relatives =
  do it "relativeClause"
        (parsed relativeClause "that <intrans-verb>" ==
         Right (RelativeClauseThat (VPCoordVP
                                      (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                              [])))))
     it "relativeClause"
        (parsed relativeClause "a <noun> <intrans-verb>" ==
         Right (RelativeClauseNP (NPCoordUnmarked
                                    (UnmarkedNPCoord
                                       (NP (SpecifyDeterminer A)
                                           (N' Nothing (N "<noun>") Nothing Nothing Nothing))
                                    Nothing))
                                 (VPCoordVP (VP (V' Nothing
                                                    (ComplVIV (IntransitiveV "<intrans-verb>"))
                                                    [])))))
     it "relativeClause"
        (parsed relativeClause "that a <noun> <intrans-verb>" ==
         Right (RelativeClauseThatNPVP
                  (NPCoordUnmarked
                     (UnmarkedNPCoord
                        (NP (SpecifyDeterminer A)
                            (N' Nothing (N "<noun>") Nothing Nothing Nothing)) Nothing))
                  (VPCoordVP (VP (V' Nothing
                                     (ComplVIV (IntransitiveV "<intrans-verb>"))
                                     [])))))
     it "relativeClause"
        (parsed relativeClause "a <noun> a <noun> <intrans-verb>" ==
         Right (RelativeClauseNPVP
                  (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                  (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                  (VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                     [])))))
     it "relativeClause"
        (parsed relativeClause "<prep> a <noun> a <noun> <intrans-verb>" ==
         Right (RelativeClausePP
                  (PP (Preposition "<prep>")
                      (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))
                  (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                  (VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                     [])))))

nouns =
  do it "genitiveN'"
        (parsed genitiveN' "<noun> <var>" ==
         Right (GenitiveN' Nothing (N "<noun>")
                           (Just (ApposCoord (AppositionVar (Variable "<var>")) Nothing))))
     it "genitiveN'"
        (parsed genitiveN' "<intrans-adj> and <intrans-adj> <noun> <var>" ==
         Right (GenitiveN' (Just (AdjectiveCoord
                                    (IntransitiveAdjective "<intrans-adj>")
                                    (Just (AdjectiveCoord (IntransitiveAdjective "<intrans-adj>")
                                                          Nothing))))
                           (N "<noun>")
                           (Just (ApposCoord (AppositionVar (Variable "<var>")) Nothing))))
     it "npCoord"
        (parsed npCoord "each of some <noun>" ==
         Right (NPCoordDistributed
                  EachOf
                  (UnmarkedNPCoord (NP (SpecifyDeterminer Some)
                                       (N' Nothing (N "<noun>") Nothing Nothing Nothing))
                                   Nothing)))
     it "npCoord"
        (parsed npCoord "some <noun>" ==
         Right (NPCoordUnmarked
                  (UnmarkedNPCoord (NP (SpecifyDeterminer Some)
                                       (N' Nothing (N "<noun>") Nothing Nothing Nothing))
                                   Nothing)))

verbModifiers =
  do it "vModifier"
        (parsed vModifier "<adverb> and <adverb>" ==
         Right (VModifierVC (AdverbCoord (Adverb "<adverb>") (Just (AdverbCoord (Adverb "<adverb>") Nothing)))))
     it "vModifier"
        (parsed vModifier "<prep> a <noun>" ==
         Right (VModifierPP (PP (Preposition "<prep>")
                                (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))))
     it "vModifier"
        (parsed vModifier "<prep> <adverb> and <adverb>" ==
         Right (VModifierAVPP
                  (AdverbialPP (Preposition "<prep>")
                               (AdverbCoord (Adverb "<adverb>")
                                            (Just (AdverbCoord (Adverb "<adverb>") Nothing))))))

adverbs =
  do it "adverbialPP"
        (parsed adverbialPP "<prep> <adverb> and <adverb>" ==
         Right (AdverbialPP (Preposition "<prep>")
                            (AdverbCoord (Adverb "<adverb>")
                                         (Just (AdverbCoord (Adverb "<adverb>") Nothing)))))

verbs =
  do -- An intransitive verb is OK: walks
     it "v'"
        (parsed v' "<intrans-verb>" ==
         Right (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>")) []))
     -- A transitive verb must take a preposition and a noun phrase.
     it "v'"
        (parsed v' "<trans-verb> <prep> a <noun>" ==
         Right (V' Nothing
                   (ComplVTV (TransitiveV "<trans-verb>")
                             (ComplPP (PP (Preposition "<prep>")
                                          (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))))
                   []))
     -- A phrasal transitive verb with adverbs on both sides, e.g.
     -- quickly walks up to a chair hastily
     it "v'"
        (parsed v' "<adverb> <ptrans-verb> <pparticle> <prep> a <noun> <adverb>" ==
         Right (V' (Just (AdverbCoord (Adverb "<adverb>") Nothing))
                   (ComplVPV (PhrasalTransitiveV "<ptrans-verb>")
                             (PhrasalParticle "<pparticle>")
                             (ComplPP (PP (Preposition "<prep>")
                                          (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))))
                   [VModifierVC (AdverbCoord (Adverb "<adverb>") Nothing)]))
     it "vp"
        (parsed vp "<intrans-verb>" ==
         Right (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>")) [])))
     it "vp"
        (parsed vp "is not <intrans-verb>" ==
         Right (VPNeg Is (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>")) [])))
     it "vpCoord"
        (parsed vpCoord "<intrans-verb> and is not <intrans-verb>" ==
         Right (VPCoord' (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>")) []))
                         And
                         (VPCoordVP
                            (VPNeg Is
                                   (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                       [])))))

specifiers =
  do it "genitiveSpecifier"
        (parsed genitiveSpecifier "1" ==
         Right (GenitiveSpecifierN 1))
     it "genitiveSpecifier"
        (parsed genitiveSpecifier "a" ==
         Right (GenitiveSpecifierD A))
     it "genitiveSpecifier"
        (parsed genitiveSpecifier "some" ==
         Right (GenitiveSpecifierD Some))
     it "genitiveSpecifier"
        (parsed genitiveSpecifier "his" ==
         Right (GenitiveSpecifierPPC (PossessivePronounCoord HisHer Nothing)))

possessives =
  do it "possessivePronounCoord"
        (parsed possessivePronounCoord "his and her" ==
         Right (PossessivePronounCoord HisHer (Just (PossessivePronounCoord HisHer Nothing))))
     it "possessivePronounCoord"
        (parsed possessivePronounCoord "its" ==
         Right (PossessivePronounCoord Its Nothing))
     it "possessiveNPCoord"
        (parsed possessiveNPCoord "his and her" ==
         Right (PossessiveNPCoordPronoun (PossessivePronounCoord HisHer (Just (PossessivePronounCoord HisHer Nothing)))))
     it "possessiveNPCoord"
        (parsed possessiveNPCoord "a <noun>'s" ==
         Right (PossessiveNPCoordGen
                  (GenitiveNPCoord (GenitiveSpecifierD A)
                                   (GenitiveN' Nothing (N "<noun>") Nothing)
                                   (GenitiveTailSaxonTail (SaxonGenitiveTail ApostropheS Nothing)))))

genitiveNP =
  do it "genitiveNPCoord"
        (parsed genitiveNPCoord "<proper-name>'s" ==
         Right (GenitiveNPCoordName (ProperName "<proper-name>")
                                    (GenitiveTailSaxonTail (SaxonGenitiveTail ApostropheS Nothing))))
     it "genitiveNPCoord"
        (parsed genitiveNPCoord "some <noun>'s" ==
         Right (GenitiveNPCoord (GenitiveSpecifierD Some)
                                (GenitiveN' Nothing (N "<noun>") Nothing)
                                (GenitiveTailSaxonTail (SaxonGenitiveTail ApostropheS Nothing))))
     it "genitiveNPCoord"
        (parsed genitiveNPCoord "some <noun> and a <noun>'s" ==
         Right (GenitiveNPCoord (GenitiveSpecifierD Some)
                                (GenitiveN' Nothing (N "<noun>") Nothing)
                                (GenitiveTailCoordtail
                                   (GenitiveCoordTail
                                      (GenitiveNPCoord
                                         (GenitiveSpecifierD A)
                                         (GenitiveN' Nothing (N "<noun>") Nothing)
                                         (GenitiveTailSaxonTail
                                            (SaxonGenitiveTail ApostropheS Nothing)))))))

adjective =
  do it "adjectiveCoord"
        (parsed adjectiveCoord "<intrans-adj>" ==
         Right (AdjectiveCoord intransAdj
                               Nothing))
     it "adjectiveCoord"
        (parsed adjectiveCoord "<intrans-adj> and <intrans-adj>" ==
         Right (AdjectiveCoord intransAdj
                               (Just (AdjectiveCoord intransAdj
                                                     Nothing))))
     it "adverbCoord"
        (parsed adverbCoord "<adverb> and <adverb>" ==
         Right (AdverbCoord adverb'
                            (Just (AdverbCoord adverb'
                                               Nothing))))

ap' =
  do it "ap"
          (parsed ap "<intrans-adj>" ==
           Right (APIntrans intransAdj))
     it "ap"
        (parsed ap "<trans-adj> <prep> a <noun>" ==
         Right (APTrans (TransitiveAdjective "<trans-adj>")
                        (PP (Preposition "<prep>")
                            (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))))
     it "apGrad"
        (parsed apGrad "<intrans-adj> than a <noun>" ==
         Right (APgradAPThan (APIntrans intransAdj)
                             (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))))
     it "apCoord"
        (parsed apCoord "<intrans-adj> than a <noun> and <intrans-adj> than a <noun>" ==
         Right (APCoordAnd (APgradAPThan (APIntrans intransAdj)
                                         (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))
                           (APCoord (APgradAPThan (APIntrans intransAdj)
                                                  (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))))))

copulae =
  do it "copulaCompl"
        (parsed copulaCompl "<prep> a <noun>" ==
         Right (CopulaComplPP (PP (Preposition "<prep>")
                                  (NPCoordUnmarked
                                     (UnmarkedNPCoord anoun
                                                      Nothing)))))
     it "copulaCompl"
        (parsed copulaCompl "a <noun> and a <noun>" ==
         Right (CopulaComplNPC (NPCoordUnmarked
                                  (UnmarkedNPCoord anoun
                                                   (Just (UnmarkedNPCoord anoun Nothing))))))
     it "copulaCompl"
        (parsed copulaCompl "<intrans-adj> than a <noun> and a <noun>" ==
         Right (CopulaComplAPC
                  (APCoord
                     (APgradAPThan (APIntrans intransAdj)
                                   (NPCoordUnmarked (UnmarkedNPCoord anoun
                                                                     (Just (UnmarkedNPCoord anoun Nothing))))))))

simple =
  do it "universalGlobalQuantor"
        (parsed universalGlobalQuantor "for every" == Right ForEveryEach)
     it "possessivePronoun"
        (parsed possessivePronoun "his" == Right HisHer)
     it "generalizedQuantor"
        (parsed generalizedQuantor "not more than" == Right NotMoreThan)
     it "distributiveMarker"
        (parsed distributiveMarker "each of" == Right EachOf)
     it "distributiveGlobalQuantor"
        (parsed distributiveGlobalQuantor "for each of" == Right ForEachOf)
     it "existentialGlobalQuestionQuantor"
        (parsed existentialGlobalQuestionQuantor "is there" ==
         Right (ExistentialGlobalQuestionQuantor Is))
     it "existentialGlobalQuantor"
        (parsed existentialGlobalQuantor "there is" ==
         Right (ExistentialGlobalQuantor Is))
     it "numberP"
        (parsed numberP "not more than 5" ==
         Right (NumberP (Just NotMoreThan) 5))
     it "numberP"
        (parsed numberP "5" == Right (NumberP Nothing 5))
     it "determiner"
        (parsed determiner "the" == Right The)
     it "determiner"
        (parsed determiner "not every" == Right NotEveryEach)

complVs =
  do it "complVPI"
        (parsed complV "<pintrans-verb> <pparticle>" ==
         Right (ComplVPI (PhrasalIntransitiveV "<pintrans-verb>")
                         (PhrasalParticle "<pparticle>")))
     it "complVIV"
        (parsed complV "<intrans-verb>" ==
         Right (ComplVIV (IntransitiveV "<intrans-verb>")))
     it "complVTV"
        (parsed complV "<trans-verb> <prep> a <noun>" ==
         Right (ComplVTV (TransitiveV "<trans-verb>")
                         (ComplPP (PP (Preposition "<prep>")
                                      (NPCoordUnmarked
                                         (UnmarkedNPCoord anoun Nothing))))))
     it "complVPV"
        (parsed complV "<ptrans-verb> <pparticle> a <noun>" ==
         Right (ComplVPV (PhrasalTransitiveV "<ptrans-verb>")
                         (PhrasalParticle "<pparticle>")
                         (ComplNP (NPCoordUnmarked
                                     (UnmarkedNPCoord anoun Nothing)))))
     it "complVDisV"
        (parsed complV "<distrans-verb> a <noun> <prep> a <noun>" ==
         Right (ComplVDisV (DistransitiveV "<distrans-verb>")
                           (ComplNP (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))
                           (ComplPP (PP (Preposition "<prep>")
                                        (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))))))
     it "complVPDV"
        (parsed complV "<pdistrans-verb> a <noun> <pparticle> a <noun>" ==
         Right (ComplVPDV (PhrasalDistransitiveV "<pdistrans-verb>")
                          (ComplNP (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))
                          (PhrasalParticle "<pparticle>")
                          (ComplNP (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))))
     it "complCopula"
        (parsed complV "is a <noun>" ==
         Right (ComplVCopula Is
                             (CopulaComplNPC (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing)))))

intransAdj = IntransitiveAdjective "<intrans-adj>"

adverb' = Adverb "<adverb>"

anoun = (NP (SpecifyDeterminer A)
             (N' Nothing
                 (N "<noun>")
                 Nothing
                 Nothing
                 Nothing))

-- | Is that left?
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Get the parsed result after tokenizing.
parsed :: Parsec [Token] (ACEParser [Token] Identity) c -> Text -> Either String c
parsed p = tokenize >=> bimap show id . runP (p <* eof) defaultACEParser "<test>"

-- | Test a parser.
testp :: Show a => Parsec [Token] (ACEParser [Token] Identity) a -> Text -> IO ()
testp p i =
  case parsed p i  of
    Left e -> putStrLn e
    Right p -> print p
