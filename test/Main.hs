{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Test suite for ACE.

module Main where

import ACE.Combinators
import ACE.Parsers
import ACE.Pretty
import ACE.Tokenizer (tokenize)
import ACE.Types.Syntax
import ACE.Types.Tokens
import Data.Text.Lazy.Builder (fromText,toLazyText)
import Data.Text.Lazy (toStrict)

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
  describe "printer" printer

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
     noun
     sentences

sentences =
  do it "existentialTopic"
        (parsed existentialTopic "there is a <noun>" ==
         Right (ExistentialTopic
                  (ExistentialGlobalQuantor Is)
                  (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))))
     it "sentence"
        (parsed sentence "a <noun> <intrans-verb>" ==
         Right (Sentence
                  (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                  (VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                     [])))))
     it "sentence"
        (parsed sentence "a <noun> that <intrans-verb> <intrans-verb>" ==
         Right
           (Sentence
              (NPCoordUnmarked
                 (UnmarkedNPCoord
                    (NP (SpecifyDeterminer A)
                        (N' Nothing
                            (N "<noun>")
                            Nothing
                            Nothing
                            (Just (RelativeClauseCoord
                                     (RelativeClauseThat
                                        (VPCoordVP
                                           (VP
                                              (V' Nothing
                                                  (ComplVIV (IntransitiveV "<intrans-verb>"))
                                                  []))))
                                     Nothing))))
                                  Nothing))
                     (VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                        [])))))
     it "conditionalSentence"
        (parsed conditionalSentence "if a <noun> <intrans-verb> then some <noun> <intrans-verb>" ==
         Right (ConditionalSentence
                  (SentenceCoord
                     (SentenceCoord_1
                        (SentenceCoord_2
                           (SentenceCoord_3
                              (TopicalizedSentenceComposite
                                 (CompositeSentence
                                    (Sentence (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                                              (VPCoordVP
                                                 (VP
                                                    (V' Nothing
                                                        (ComplVIV (IntransitiveV "<intrans-verb>"))
                                                        []))))))
                              Nothing)
                           Nothing)
                        Nothing)
                     Nothing)
                  (SentenceCoord
                     (SentenceCoord_1
                        (SentenceCoord_2
                           (SentenceCoord_3
                              (TopicalizedSentenceComposite
                                 (CompositeSentence
                                    (Sentence
                                       (NPCoordUnmarked
                                          (UnmarkedNPCoord
                                             (NP (SpecifyDeterminer Some)
                                                 (N' Nothing (N "<noun>") Nothing Nothing Nothing)) Nothing))
                                       (VPCoordVP
                                          (VP (V' Nothing
                                                  (ComplVIV (IntransitiveV "<intrans-verb>"))
                                                  []))))))
                              Nothing)
                           Nothing)
                        Nothing)
                     Nothing)))
     it "universalTopic"
        (parsed universalTopic "for all <noun>" ==
         Right (UniversalTopic ForAll (N' Nothing (N "<noun>") Nothing Nothing Nothing)))
     it "negatedSentence"
        (parsed negatedSentence "it is not the case that a <noun> <intrans-verb>" ==
         Right (NegatedSentence
                  (SentenceCoord
                     (SentenceCoord_1
                        (SentenceCoord_2
                           (SentenceCoord_3
                              (TopicalizedSentenceComposite
                                 (CompositeSentence
                                    (Sentence
                                       (NPCoordUnmarked
                                          (UnmarkedNPCoord
                                             (NP (SpecifyDeterminer A)
                                                 (N' Nothing (N "<noun>") Nothing Nothing Nothing))
                                             Nothing))
                                       (VPCoordVP
                                          (VP (V' Nothing
                                                  (ComplVIV (IntransitiveV "<intrans-verb>"))
                                                  []))))))
                              Nothing)
                           Nothing)
                        Nothing)
                     Nothing)))
     it "specification"
        (parsed specification "it is not the case that a <noun> <intrans-verb>." ==
         Right
           (Specification
              (SentenceCoord
                 (SentenceCoord_1
                    (SentenceCoord_2
                       (SentenceCoord_3
                          (TopicalizedSentenceComposite
                             (CompositeSentenceNeg
                                (NegatedSentence
                                   (SentenceCoord
                                      (SentenceCoord_1
                                         (SentenceCoord_2
                                            (SentenceCoord_3
                                               (TopicalizedSentenceComposite
                                                  (CompositeSentence
                                                     (Sentence (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                                                               (VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>")) []))))))
                                               Nothing)
                                            Nothing)
                                         Nothing)
                                      Nothing))))
                          Nothing)
                       Nothing)
                    Nothing)
                 Nothing)
              Nothing))

noun =
  do it "pp"
        (parsed pp "<prep> a <noun>" ==
         Right (PP (Preposition "<prep>")
                   (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))))
     it "n'"
        (parsed (n' False) "<noun>" == Right (N' Nothing (N "<noun>") Nothing Nothing Nothing) )
     it "n'"
        (parsed (n' False) "<intrans-adj> <noun>" ==
         Right (N' (Just (AdjectiveCoord (IntransitiveAdjective "<intrans-adj>") Nothing))
                   (N "<noun>") Nothing Nothing Nothing))
     it "n'"
        (parsed (n' False) "<intrans-adj> <noun> <var>" ==
         Right (N' (Just (AdjectiveCoord (IntransitiveAdjective "<intrans-adj>") Nothing))
                   (N "<noun>")
                   (Just (ApposCoord (AppositionVar (Variable "<var>")) Nothing))
                   Nothing
                   Nothing))
     it "n'"
        (parsed (n' False) "<intrans-adj> <noun> <var> of a <noun> a <noun> <intrans-verb>" ==
         Right (N' (Just (AdjectiveCoord (IntransitiveAdjective "<intrans-adj>") Nothing))
                   (N "<noun>")
                   (Just (ApposCoord (AppositionVar (Variable "<var>")) Nothing))
                   (Just
                     (NPCoordUnmarked
                       (UnmarkedNPCoord
                         (NP (SpecifyDeterminer A)
                             (N' Nothing
                                 (N "<noun>")
                                 Nothing
                                 Nothing
                                 (Just
                                   (RelativeClauseCoord
                                     (RelativeClauseNP
                                       (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                                       (VPCoordVP
                                          (VP
                                             (V' Nothing
                                                 (ComplVIV (IntransitiveV "<intrans-verb>"))
                                                 []))))
                                     Nothing))))
                         Nothing)))
                   Nothing))

relatives =
  do it "relativeClauseCoord"
        (parsed relativeClauseCoord "that <intrans-verb> and a <noun> <intrans-verb>" ==
         Right (RelativeClauseCoord
                  (RelativeClauseThat
                     (VPCoordVP (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                        []))))
                  (Just (And
                        ,RelativeClauseCoord
                          (RelativeClauseNP
                            (NPCoordUnmarked (UnmarkedNPCoord anoun Nothing))
                            (VPCoordVP
                               (VP (V' Nothing (ComplVIV (IntransitiveV "<intrans-verb>"))
                                       []))))
                          Nothing))))
     it "relativeClause"
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
  do it "specifier"
        (parsed specifier "<proper-name>'s" ==
         Right (SpecifyPossessive
                  (PossessiveNPCoordGen
                     (GenitiveNPCoordName
                       (ProperName "<proper-name>")
                       (GenitiveTailSaxonTail
                          (SaxonGenitiveTail ApostropheS
                                             Nothing))))))
     it "specifier"
        (parsed specifier "1" ==
         Right (SpecifyNumberP (NumberP Nothing 1)))
     it "specifier"
        (parsed specifier "a" ==
         Right (SpecifyDeterminer A))
     it "genitiveSpecifier"
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
         Right (GenitiveSpecifierPPC (PossessivePronounCoord His Nothing)))

possessives =
  do it "possessivePronounCoord"
        (parsed possessivePronounCoord "his and her" ==
         Right (PossessivePronounCoord His (Just (PossessivePronounCoord Her Nothing))))
     it "possessivePronounCoord"
        (parsed possessivePronounCoord "its" ==
         Right (PossessivePronounCoord Its Nothing))
     it "possessiveNPCoord"
        (parsed possessiveNPCoord "his and her" ==
         Right (PossessiveNPCoordPronoun (PossessivePronounCoord His (Just (PossessivePronounCoord Her Nothing)))))
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
        (parsed universalGlobalQuantor "for every" == Right ForEvery)
     it "possessivePronoun"
        (parsed possessivePronoun "his" == Right His)
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
        (parsed determiner "not every" == Right NotEvery)

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

printer =
  do isomorphic "existentialTopic" existentialTopic "there is a <noun>"
     isomorphic "sentence" sentence "a <noun> <intrans-verb>"
     isomorphic "sentence" sentence "a <noun> that <intrans-verb> <intrans-verb>"
     isomorphic "conditionalSentence" conditionalSentence "if a <noun> <intrans-verb> then some <noun> <intrans-verb>"
     isomorphic "universalTopic" universalTopic "for all <noun>"
     isomorphic "negatedSentence" negatedSentence "it is not the case that a <noun> <intrans-verb>"
     isomorphic "specification" specification "it is not the case that a <noun> <intrans-verb>."
     isomorphic "pp" pp "<prep> a <noun>"
     isomorphic "(n' False)" (n' False) "<noun>"
     isomorphic "(n' False)" (n' False) "<intrans-adj> <noun>"
     isomorphic "(n' False)" (n' False) "<intrans-adj> <noun> <var>"
     isomorphic "(n' False)" (n' False) "<intrans-adj> <noun> <var> of a <noun> a <noun> <intrans-verb>"
     isomorphic "relativeClauseCoord" relativeClauseCoord "that <intrans-verb> and a <noun> <intrans-verb>"
     isomorphic "relativeClause" relativeClause "that <intrans-verb>"
     isomorphic "relativeClause" relativeClause "a <noun> <intrans-verb>"
     isomorphic "relativeClause" relativeClause "that a <noun> <intrans-verb>"
     isomorphic "relativeClause" relativeClause "a <noun> a <noun> <intrans-verb>"
     isomorphic "relativeClause" relativeClause "<prep> a <noun> a <noun> <intrans-verb>"
     isomorphic "genitiveN'" genitiveN' "<noun> <var>"
     isomorphic "genitiveN'" genitiveN' "<intrans-adj> and <intrans-adj> <noun> <var>"
     isomorphic "npCoord" npCoord "each of some <noun>"
     isomorphic "npCoord" npCoord "some <noun>"
     isomorphic "vModifier" vModifier "<adverb> and <adverb>"
     isomorphic "vModifier" vModifier "<prep> a <noun>"
     isomorphic "vModifier" vModifier "<prep> <adverb> and <adverb>"
     isomorphic "adverbialPP" adverbialPP "<prep> <adverb> and <adverb>"
     isomorphic "v'" v' "<intrans-verb>"
     isomorphic "v'" v' "<trans-verb> <prep> a <noun>"
     isomorphic "v'" v' "<adverb> <ptrans-verb> <pparticle> <prep> a <noun> <adverb>"
     isomorphic "vp" vp "<intrans-verb>"
     isomorphic "vp" vp "is not <intrans-verb>"
     isomorphic "vpCoord" vpCoord "<intrans-verb> and is not <intrans-verb>"
     isomorphic "specifier" specifier "<proper-name>'s"
     isomorphic "specifier" specifier "1"
     isomorphic "specifier" specifier "a"
     isomorphic "genitiveSpecifier" genitiveSpecifier "1"
     isomorphic "genitiveSpecifier" genitiveSpecifier "a"
     isomorphic "genitiveSpecifier" genitiveSpecifier "some"
     isomorphic "genitiveSpecifier" genitiveSpecifier "his"
     isomorphic "possessivePronounCoord" possessivePronounCoord "his and her"
     isomorphic "possessivePronounCoord" possessivePronounCoord "its"
     isomorphic "possessiveNPCoord" possessiveNPCoord "his and her"
     isomorphic "possessiveNPCoord" possessiveNPCoord "a <noun>'s"
     isomorphic "genitiveNPCoord" genitiveNPCoord "<proper-name>'s"
     isomorphic "genitiveNPCoord" genitiveNPCoord "some <noun>'s"
     isomorphic "genitiveNPCoord" genitiveNPCoord "some <noun> and a <noun>'s"
     isomorphic "adjectiveCoord" adjectiveCoord "<intrans-adj>"
     isomorphic "adjectiveCoord" adjectiveCoord "<intrans-adj> and <intrans-adj>"
     isomorphic "adverbCoord" adverbCoord "<adverb> and <adverb>"
     isomorphic "ap" ap "<intrans-adj>"
     isomorphic "ap" ap "<trans-adj> <prep> a <noun>"
     isomorphic "apGrad" apGrad "<intrans-adj> than a <noun>"
     isomorphic "apCoord" apCoord "<intrans-adj> than a <noun> and <intrans-adj> than a <noun>"
     isomorphic "copulaCompl" copulaCompl "<prep> a <noun>"
     isomorphic "copulaCompl" copulaCompl "a <noun> and a <noun>"
     isomorphic "copulaCompl" copulaCompl "<intrans-adj> than a <noun> and a <noun>"
     isomorphic "universalGlobalQuantor" universalGlobalQuantor "for every"
     isomorphic "possessivePronoun" possessivePronoun "his"
     isomorphic "generalizedQuantor" generalizedQuantor "not more than"
     isomorphic "distributiveMarker" distributiveMarker "each of"
     isomorphic "distributiveGlobalQuantor" distributiveGlobalQuantor "for each of"
     isomorphic "existentialGlobalQuestionQuantor" existentialGlobalQuestionQuantor "is there"
     isomorphic "existentialGlobalQuantor" existentialGlobalQuantor "there is"
     isomorphic "numberP" numberP "not more than 5"
     isomorphic "numberP" numberP "5"
     isomorphic "determiner" determiner "the"
     isomorphic "determiner" determiner "not every"
     isomorphic "complV" complV "<pintrans-verb> <pparticle>"
     isomorphic "complV" complV "<intrans-verb>"
     isomorphic "complV" complV "<trans-verb> <prep> a <noun>"
     isomorphic "complV" complV "<ptrans-verb> <pparticle> a <noun>"
     isomorphic "complV" complV "<distrans-verb> a <noun> <prep> a <noun>"
     isomorphic "complV" complV "<pdistrans-verb> a <noun> <pparticle> a <noun>"
     isomorphic "complV" complV "is a <noun>"

  where isomorphic name parser text =
          it name
             ((printed parser text >>= printed parser . toText) == Right (fromText text))
          where toText = toStrict . toLazyText

-- | Is that left?
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Get the parsed result after tokenizing.
parsed :: Parsec [Token] (ACEParser [Token] Identity) c -> Text -> Either String c
parsed p = tokenize >=> bimap show id . runP (p <* eof) defaultACEParser "<test>"

printed p = fmap pretty . parsed p

-- | Test a parser.
testp :: Show a => Parsec [Token] (ACEParser [Token] Identity) a -> Text -> IO ()
testp p i =
  case parsed p i  of
    Left e -> putStrLn e
    Right p -> print p
