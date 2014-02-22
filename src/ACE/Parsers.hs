{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Parsers for ACE syntax types.

module ACE.Parsers where

import ACE.Combinators
import ACE.Tokenizer (tokenize)
import ACE.Types.Syntax
import ACE.Types.Tokens
import Data.Default

import Control.Applicative
import Control.Monad hiding (ap)
import Data.Bifunctor
import Data.Text (Text)
import Text.Parsec ()
import Text.Parsec.Prim (Stream,ParsecT,parse,try,getState)

-- | Parser configuration.
data ACEParser s m = ACE
  { aceIntransitiveAdjective :: ParsecT s (ACEParser s m) m Text -- ^ Parser for intransitive adjectives.
  , aceTransitiveAdjective   :: ParsecT s (ACEParser s m) m Text -- ^ Parser for transitive adjectives.
  , aceNoun                  :: ParsecT s (ACEParser s m) m Text -- ^ Parser for nouns.
  , acePreposition           :: ParsecT s (ACEParser s m) m Text -- ^ Parser for prepositions.
  , aceVariable              :: ParsecT s (ACEParser s m) m Text -- ^ Parser for variables.
  , aceProperName            :: ParsecT s (ACEParser s m) m Text -- ^ Parser for proper names.
  , aceAdverb                :: ParsecT s (ACEParser s m) m Text -- ^ Parser for adverbs.
  , aceIntransitiveVerb      :: ParsecT s (ACEParser s m) m Text -- ^ Parser for intransitive verbs.
  , acePhrasalParticle       :: ParsecT s (ACEParser s m) m Text -- ^ Parser for phrasal particles.
  , acePhrasalIntransitiveV  :: ParsecT s (ACEParser s m) m Text -- ^ Parser for phrasal intransitive verbs.
  }

-- | A default ACE parser configuration. Just fills in all the parsers as blanks: @<noun>@, @<prep>@, etc.
defaultACEParser :: Stream s m Token => ACEParser s m
defaultACEParser =
  ACE { aceIntransitiveAdjective = string "<intrans-adj>"
      , aceTransitiveAdjective   = string "<trans-adj>"
      , aceNoun                  = string "<noun>"
      , acePreposition           = string "<prep>"
      , aceVariable              = string "<var>"
      , aceProperName            = string "<proper-name>"
      , aceAdverb                = string "<adverb>"
      , aceIntransitiveVerb      = string "<intrans-verb>"
      , acePhrasalParticle       = string "<pparticle>"
      , acePhrasalIntransitiveV  = string "<pintrans-verb>"
      }

specification =
  Specification
    <$> sentenceCoord <* period
    <*> optional (try specification)

sentenceCoord =
  SentenceCoord
    <$> sentenceCoord_1
    <*> optional (try (string "or" *> sentenceCoord))

sentenceCoord_1 =
  SentenceCoord_1
    <$> sentenceCoord_2
    <*> optional (try (comma *> string "and" *> sentenceCoord_1))

sentenceCoord_2 =
  SentenceCoord_2
    <$> sentenceCoord_3
    <*> optional (try (string "or" *> sentenceCoord_2))

sentenceCoord_3 =
  SentenceCoord_3
    <$> topicalizedSentence
    <*> optional (try (string "and" *> sentenceCoord_3))

topicalizedSentence =
  (TopicalizedSentenceExistential <$> existentialTopic <*> optional (try sentenceCoord)) <|>
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
    <$> (string "if" *> sentenceCoord)
    <*> (string "then" *> sentenceCoord)

negatedSentence =
  NegatedSentence
    <$> (strings ["it","is","not","the","case","that"] *>
         sentenceCoord)

sentence =
  Sentence
    <$> npCoord
    <*> vpCoord

existentialTopic =
  ExistentialTopic <$> existentialGlobalQuantor
                   <*> npCoord

npCoord =
  distributed <|> unmarked
  where distributed =
          NPCoordDistributed <$> distributiveMarker <*> unmarkedNPCoord
        unmarked =
          NPCoordUnmarked <$> unmarkedNPCoord

unmarkedNPCoord =
  UnmarkedNPCoord
    <$> np
    <*> optional (try (string "and" *> unmarkedNPCoord))

np =
  NP <$> specifier
     <*> n'

specifier =
  specifierDeterminer <|>
  specifierPossessive <|>
  specifierNumber
  where specifierDeterminer =
          SpecifyDeterminer <$> determiner
        specifierPossessive =
          SpecifyPossessive <$> possessiveNPCoord
        specifierNumber =
          SpecifyNumberP <$> numberP

n' =
  N' <$> optional (try adjectiveCoord)
     <*> n
     <*> optional (try apposCoord)
     <*> optional (try pp)
     <*> optional (try relativeClauseCoord)

n =
  N <$> join (fmap aceNoun getState)

pp =
  PP <$> preposition
     <*> npCoord

preposition =
  Preposition <$> join (fmap acePreposition getState)

apposCoord =
  ApposCoord
    <$> apposition
    <*> optional (try (string "and" *> apposCoord))

apposition =
  (AppositionVar <$> variable) <|>
  (AppositionQuote <$> quotation)

variable =
  Variable <$> join (fmap aceVariable getState)

quotation =
  Quotation <$> quoted

relativeClauseCoord =
  RelativeClauseCoord
    <$> relativeClause
    <*> optional (try ((,) <$> coord
                           <*> relativeClauseCoord))

properName =
  ProperName <$> join (fmap aceProperName getState)

possessivePronounCoord =
  PossessivePronounCoord
    <$> possessivePronoun
    <*> optional (try (string "and" *> possessivePronounCoord))

genitiveTail =
  (GenitiveTailSaxonTail <$> saxonGenitiveTail) <|>
  (GenitiveTailCoordtail <$> genitiveCoordTail)

genitiveCoordTail =
  GenitiveCoordTail <$> (string "and" *> genitiveNPCoord)

saxonGenitiveTail =
  SaxonGenitiveTail
    <$> saxonGenitiveMarker
    <*> optional (try ((,) <$> genitiveN'
                           <*> saxonGenitiveTail))

saxonGenitiveMarker =
  fmap (\s -> if s then ApostropheS else Apostrophe)
       genitive

relativeClause =
  RelativeClause <$> vpCoord

vpCoord =
  do vp <- vp
     (try (VPCoord'
             <$> pure vp
             <*> coord
             <*> vpCoord) <|>
      (VPCoordVP
         <$> pure vp))

genitiveSpecifier =
  (GenitiveSpecifierD <$> determiner) <|>
  (GenitiveSpecifierPPC <$> possessivePronounCoord) <|>
  (GenitiveSpecifierN <$> number)

genitiveN' =
  GenitiveN'
    <$> optional (try adjectiveCoord)
    <*> n
    <*> optional (try apposCoord)

vp =
  VP <$> v'

v' =
  V' <$> optional (try adverbCoord)
     <*> complV
     <*> many vModifier

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

possessiveNPCoord =
  PossessiveNPCoordGen <$> genitiveNPCoord

genitiveNPCoord =
  specifier <|> name
  where specifier =
          GenitiveNPCoord
            <$> genitiveSpecifier
            <*> genitiveN'
            <*> genitiveTail
        name =
          GenitiveNPCoordName
            <$> properName
            <*> genitiveTail

complV =
  complVIV <|>
  complVPI -- <|>
  -- complVTV <|>
  -- complVPV <|>
  -- complVPV' <|>
  -- complVPV' <|>
  -- complVDisV <|>
  -- complVPDV <|>
  -- complVCopula

complVIV =
  ComplVIV <$> intransitiveV

complVPI =
  ComplVPI <$> phrasalIntransitiveV <*> phrasalParticle

phrasalParticle =
  PhrasalParticle <$> join (fmap acePhrasalParticle getState)

phrasalIntransitiveV =
  PhrasalIntransitiveV <$> join (fmap acePhrasalIntransitiveV getState)

--  | Either a graded adjective coordination (\"better than a duck and
--  faster than a mouse\"), or a noun phrase coordination (\"a goose
--  and an ocelot\"), or a prepositional phrase (\"to a bucket or a
--  kettle\").
copulaCompl =
  copulaComplAPC <|>
  copulaComplNPC <|>
  copulaComplPP

  where copulaComplAPC = CopulaComplAPC <$> apCoord
        copulaComplNPC = CopulaComplNPC <$> npCoord
        copulaComplPP  = CopulaComplPP  <$> pp

-- | A coordination of a graded adjective: \"better than a potato and
-- nicer than some bacon\"
apCoord = apCoordAnd <|> apCoord'
  where apCoordAnd = APCoordAnd <$> try (apGrad <* string "and") <*> apCoord
        apCoord' = APCoord <$> apGrad

-- | A graded adjective. Either comparative adjective phrase (\"better
-- than a potato\"), or a simple adjective phrase (see 'ap').
apGrad = apGradThan <|> apGrad'
  where apGradThan = APgradAPThan <$> try (ap <* string "than") <*> npCoord
        apGrad' = APgradAP <$> ap

-- | An adjective phrase. Transitive (fond of Mary, interested in an
-- account) or intransitive (correct, green, valid).
ap =
  (APTrans <$> transitiveAdjective <*> pp) <|>
  (APIntrans <$> intransitiveAdjective)

-- | Some intransitive verb.
intransitiveV =
  IntransitiveV <$> join (fmap aceIntransitiveVerb getState)

-- | Adverb coordination: quickly and hastily and manually
adverbCoord =
  AdverbCoord <$> adverb
              <*> optional (try (string "and" *> adverbCoord))

-- | Adverb: quickly
adverb =
  Adverb <$> join (fmap aceAdverb getState)

-- | Adjective coordination: correct and green
adjectiveCoord =
  AdjectiveCoord
    <$> intransitiveAdjective
    <*> optional (try (string "and" *> adjectiveCoord))

-- | Intransitive adjective: correct, green, valid
--
-- The actual parser for this is provided as
-- 'aceIntransitiveAdjective' in the parser configuration. You can
-- configure this.
intransitiveAdjective =
  IntransitiveAdjective <$> join (fmap aceIntransitiveAdjective getState)

-- | Transitive adjective: correct, green, valid
transitiveAdjective =
  TransitiveAdjective <$> join (fmap aceTransitiveAdjective getState)

-- | A determiner: the, an, not every, etc.
determiner =
  (string "the" *> pure The) <|>
  (string "an" *> pure An) <|>
  (string "a" *> pure A) <|>
  (string "some" *> pure Some) <|>
  (strings ["not","every"] *> pure NotEveryEach) <|>
  (strings ["not","each"] *> pure NotEveryEach) <|>
  (strings ["not","all"] *> pure NotAll) <|>
  (string "no" *> pure No) <|>
  (string "every" *> pure EveryEach) <|>
  (string "each" *> pure EveryEach) <|>
  (string "all" *> pure All) <|>
  (string "which" *> pure Which)

-- | A number phrase: more than 5
numberP =
  NumberP
    <$> optional (try generalizedQuantor)
    <*> number

-- | There is/are.
existentialGlobalQuantor =
  string "there" *>
  (ExistentialGlobalQuantor <$> copula)

-- | Is/are there?
existentialGlobalQuestionQuantor =
  (ExistentialGlobalQuestionQuantor <$> copula) <*
  string "there"

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

-- | A distributive global quantor: for each of
distributiveGlobalQuantor =
  strings ["for","each","of"] *> pure ForEachOf

-- | A distributive marker: each of
distributiveMarker =
  strings ["each","of"] *> pure EachOf

-- | A generalized quantor: at most, at least, etc.
generalizedQuantor =
  (strings ["at","most"] *> pure AtMost) <|>
  (strings ["at","least"] *> pure AtLeast) <|>
  (strings ["more","than"] *> pure MoreThan) <|>
  (strings ["less","than"] *> pure LessThan) <|>
  (strings ["not","more","than"] *> pure NotMoreThan) <|>
  (strings ["not","less","than"] *> pure NotLessThan)

-- | A possessive pronoun: his, her, his/her.
possessivePronoun =
  hisHer <|> its
  where hisHer =
          (string "his" <|> string "her" <|> string "his/her") *>
          pure HisHer
        its = string "its" *> pure Its

-- | A universal global quantor: for every/for each, for all.
universalGlobalQuantor =
  string "for" *> (everyEach <|> forAll)
  where everyEach = (string "every" <|> string "each") *> pure ForEveryEach
        forAll = string "all" *> pure ForAll
