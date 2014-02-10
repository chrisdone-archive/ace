{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Parsers for ACE syntax types.

module ACE.Parsers where

import ACE.Types.Syntax

import Control.Applicative
import Text.Parsec hiding (optional,many,(<|>))

-- | Specifications consist of a sentence coordination followed by a
-- period and optionally one ore more subsequent specifications.
specification =
  Specification
    <$> sentenceCoord <* string "."
    <*> optional (try specification)

-- | Sentences can be coordinated by @and@ and @or@. @And@ refers to the
-- logical conjunction, while @or@ de-notes the logical disjunction. The
-- logical conjunction has a higher precedence than the disjunction.
sentenceCoord =
  SentenceCoord
    <$> sentenceCoord_1
    <*> optional (try (string " or " *> sentenceCoord))

sentenceCoord_1 =
  SentenceCoord_1
    <$> sentenceCoord_2
    <*> optional (try (string ", and " *> sentenceCoord_1))

sentenceCoord_2 =
  SentenceCoord_2
    <$> sentenceCoord_3
    <*> optional (try (string " or " *> sentenceCoord_2))

sentenceCoord_3 =
  SentenceCoord_3
    <$> topicalizedSentence
    <*> optional (try (string " and " *> sentenceCoord_3))

topicalizedSentence =
  (TopicalizedSentenceExistential <$> existentialTopic <*> optional sentenceCoord) <|>
  (TopicalizedSentenceUniversal <$> universalTopic <*> sentenceCoord) <|>
  (TopicalizedSentenceComposite <$> compositeSentence)

universalTopic =
  UniversalTopic <$> universalGlobalQuantor
                 <*> n'

compositeSentence =
  compositeSentenceCond <|>
  compositeSentenceNeg <|>
  compositeSentence
  where compositeSentenceCond =
          CompositeSentenceCond <$> conditionalSentence
        compositeSentenceNeg =
          CompositeSentenceNeg <$> negatedSentence
        compositeSentence =
          CompositeSentence <$> sentence

conditionalSentence =
  ConditionalSentence
    <$> (string "if " *> sentenceCoord)
    <*> (string "then " *> sentenceCoord)

negatedSentence =
  NegatedSentence
    <$> (string "it is not the case that " *>
         sentenceCoord)

sentence =
  Sentence
    <$> (npCoord <* string " ")
    <*> vpCoord

existentialTopic =
  ExistentialTopic <$> existentialGlobalQuantor
                   <*> npCoord

npCoord =
  NPCoordDistributed
    <$> distributiveMarker
    <*> unmarkedNPCoord

unmarkedNPCoord =
  UnmarkedNPCoord
    <$> np
    <*> unmarkedNPCoord

np =
  NP <$> specifier
     <*> n'

n' =
  N' <$> optional adjectiveCoord
     <*> n
     <*> optional apposCoord
     <*> optional pp
     <*> optional relativeClauseCoord

n =
  N <$> string "<noun>"

pp =
  PP <$> preposition
     <*> npCoord

preposition =
  Preposition <$> string "<preposition>"

apposCoord =
  ApposCoord
    <$> apposition
    <*> optional (string "and " *> apposCoord)

apposition =
  string " " *>
  (try (AppositionVar <$> variable) <|>
   (AppositionQuote <$> quotedString))

quotedString =
  QuotedString <$> string "<quoted-string>"

-- | Some variable.
variable =
  Variable <$> string "<variable>"

-- | A relative clause coordination.
relativeClauseCoord =
  RelativeClauseCoord
    <$> relativeClause
    <*> optional ((,) <$> coord
                      <*> relativeClauseCoord)

possessiveNPCoord =
  PossessiveNPCoordGen <$> genitiveNPCoord

properName =
  ProperName <$> string "<proper-name>"

genitiveNPCoord =
  try specifier <|> name
  where specifier =
          GenitiveNPCoord
            <$> genitiveSpecifier
            <*> genitiveN'
            <*> genitiveTail
        name =
          GenitiveNPCoordName
            <$> properName
            <*> genitiveTail

possessivePronounCoord =
  PossessivePronounCoord
    <$> possessivePronoun
    <*> optional (try (string " and " *> possessivePronounCoord))

genitiveTail =
  try (GenitiveTailSaxonTail <$> saxonGenitiveTail) <|>
  (GenitiveTailCoordtail <$> genitiveCoordTail)

genitiveCoordTail =
  GenitiveCoordTail <$> (string " and " *> genitiveNPCoord)

saxonGenitiveTail =
  SaxonGenitiveTail
    <$> saxonGenitiveMarker
    <*> optional (try ((,) <$> genitiveN'
                           <*> saxonGenitiveTail))

relativeClause =
  RelativeClause <$> vpCoord

-- | Verb phrase coordination.
vpCoord =
  (VPCoord'
     <$> vp
     <*> coord
     <*> vpCoord) <|>
  VPCoordVP <$> vp

genitiveSpecifier =
  try (GenitiveSpecifierD <$> determiner) <|>
  try (GenitiveSpecifierPPC <$> possessivePronounCoord) <|>
  GenitiveSpecifierN <$> number

genitiveN' =
  GenitiveN'
    <$> optional (try (adjectiveCoord <* string " "))
    <*> n
    <*> optional (try apposCoord)

-- | Verb phrase.
vp =
  VP <$> v'

-- | Verb.
v' =
  V' <$> optional adverbCoord
     <*> complV
     <*> many vModifier

adverbCoord =
  AdverbCoord <$> adverb
              <*> optional adverbCoord

complV =
  ComplV <$> intransitiveV

-- | Intransitive verb.
intransitiveV =
  IntransitiveV <$> string "<intransitive-verb>"

-- | Intransitive adjective.
intransitiveAdjective =
  IntransitiveAdjective <$> string "<intransitive-adjective>"

vModifier =
  vModifierVC <|> vModifierPP <|> vModifierAVPP
  where vModifierVC =
          VModifierVC <$> adverbCoord
        vModifierPP =
          VModifierPP <$> pp
        vModifierAVPP =
          VModifierAVPP <$> adverbialPP

adverbialPP =
  AdverbialPP
    <$> preposition
    <*> adverbCoord

-- | An adverb.
adverb =
  Adverb <$> string "<adverb>"

specifier =
  try specifierDeterminer <|>
  try specifierPossessive <|>
  specifierNumber
  where specifierDeterminer =
          SpecifyDeterminer <$> determiner
        specifierPossessive =
          SpecifyPossessive <$> possessiveNPCoord
        specifierNumber =
          SpecifyNumberP <$> numberP

-- | Adjective coordination.
adjectiveCoord =
  AdjectiveCoord
    <$> intransitiveAdjective
    <*> optional (try (string " and " *> adjectiveCoord))

-- | A number phrase.
numberP =
  NumberP
    <$> optional generalizedQuantor
    <*> (number <* string " ")

-- | Some positive integer number.
number =
  (Number . read) <$> many1 digit

existentialGlobalQuantor =
  string "there " *> (ExistentialGlobalQuantor <$> copula)

existentialGlobalQuestionQuantor =
  (ExistentialGlobalQuestionQuantor <$> copula) *> string " there"

-- | Do/does.
aux =
  (string "do" *> pure Do) <|>
  (string "does" *> pure Does)

-- | And/or.
coord =
  (string "and" *> pure And) <|>
  (string "or" *> pure Or)

-- | Is/are.
copula =
  (string "is" *> pure Is) <|>
  (string "are" *> pure Are)

determiner =
  (string "the " *> pure The) <|>
  try (string "an " *> pure An) <|>
  (string "a " *> pure A) <|>
  (string "some " *> pure Some) <|>
  try (string "not " *>
       (try (string "every ") <|> string "each ") *>
       pure NotEveryEach) <|>
  (string "not all " *> pure NotAll) <|>
  (string "no " *> pure No) <|>
  ((try (string "every ") <|> string "each ") *>
   pure EveryEach) <|>
  (string "all " *> pure All) <|>
  (string "which " *> pure Which)

distributiveGlobalQuantor = string "for each of" *> pure ForEachOf

distributiveMarker = string "each of" *> pure EachOf

generalizedQuantor =
  try (string "at most " *> pure AtMost) <|>
  try (string "at least " *> pure AtLeast) <|>
  try (string "more than " *> pure MoreThan) <|>
  try (string "less than " *> pure LessThan) <|>
  try (string "not more than " *> pure NotMoreThan) <|>
  try (string "not less than " *> pure NotLessThan)

possessivePronoun =
  try hisHer <|>
  try its
  where hisHer =
          (try (string "his") <|>
           try (string "her") <|>
           (string "his/her")) *>
          pure HisHer
        its =
          string "its" *> pure Its

saxonGenitiveMarker =
  (string "'s" *> pure ApostropheS) <|>
  (string "'" *> pure Apostrophe)

universalGlobalQuantor =
  string "for " *>
  (string "e" *>
   (string "very" <|> string "ach") *>
   pure ForEveryEach) <|>
  (string "all" *> pure ForAll)
