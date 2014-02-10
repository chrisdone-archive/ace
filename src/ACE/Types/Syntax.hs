{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- | Types for the syntax tree.

module ACE.Types.Syntax where

import ACE.Pretty

import Data.Semigroup
import Data.Text (Text)
import Prelude hiding (String)

-- | Specifications consist of a sentence coordination followed by a
-- period and optionally one ore more subsequent specifications.
data Specification =
  Specification !SentenceCoord !(Maybe Specification)
  deriving (Eq)

-- | Sentences can be coordinated by @and@ and @or@. @And@ refers to the
-- logical conjunction, while @or@ de-notes the logical disjunction. The
-- logical conjunction has a higher precedence than the disjunction.
--
-- Both connectors are right-associative. The expression
--
-- A or B and C or D
--
-- is therefore ordered like
--
-- A ∨ ((B ∧ C) ∨ D)
--
-- To enable more combinations, we have introduced comma-and and
-- comma-or. These expressions reverse the order of precedence. To
-- achieve the order
--
-- A ∨ (B ∧ (C ∨ D))
--
-- we can write
--
-- A, or B, and C or D
--
-- A sentence coordination in general consists of a sentence
-- coordination of a lower level (thus ensuring right-associativity)
-- optionally followed by the respective connector and a sentence
-- coordination of the same level.
--
data SentenceCoord =
  SentenceCoord !SentenceCoord_1 !(Maybe SentenceCoord)
  deriving (Eq)

data SentenceCoord_1 =
  SentenceCoord_1 !SentenceCoord_2 !(Maybe SentenceCoord_1)
  deriving (Eq)

data SentenceCoord_2 =
  SentenceCoord_2 !SentenceCoord_3 !(Maybe SentenceCoord_2)
  deriving (Eq)

data SentenceCoord_3 =
  SentenceCoord_3 !TopicalizedSentence !(Maybe SentenceCoord_3)
  deriving (Eq)

-- | Singular/plural.
data Plurality
  = Singular
  | Plural
  deriving (Eq)

-- | A topicalized sentence can start with an existential topic or a
-- universal topic. It needs, however, not be topicalized at all but
-- can just be an ordinary composite sentence.
data TopicalizedSentence
  = TopicalizedSentenceExistential !ExistentialTopic !(Maybe SentenceCoord) -- ^ Example: \"There is a card such that the code of the card is valid.\"
  | TopicalizedSentenceUniversal !UniversalTopic !SentenceCoord -- ^ Example: \"For every code there is a card such that the code belongs to it.\"
  | TopicalizedSentenceComposite !CompositeSentence -- ^ Example: \"Homer is a man.\"
  deriving (Eq)

data UniversalTopic =
  UniversalTopic !UniversalGlobalQuantor !N'
  deriving (Eq)

data CompositeSentence
  = CompositeSentenceCond !ConditionalSentence
  | CompositeSentenceNeg !NegatedSentence
  | CompositeSentence !Sentence
  deriving (Eq)

data ConditionalSentence =
  ConditionalSentence !SentenceCoord
                      !SentenceCoord
  deriving (Eq)

data NegatedSentence =
  NegatedSentence !SentenceCoord
  deriving (Eq)

data Sentence =
  Sentence !NPCoord !VPCoord
  deriving (Eq)

data ExistentialTopic =
  ExistentialTopic !ExistentialGlobalQuantor
                   !NPCoord
  deriving (Eq)

data NPCoord =
  NPCoordDistributed !DistributiveMarker !UnmarkedNPCoord
  deriving (Eq)

data UnmarkedNPCoord =
  UnmarkedNPCoord !NP !UnmarkedNPCoord
  deriving (Eq)

-- | Modified noun.
data N' =
  N' !(Maybe AdjectiveCoord)
     !N
     !(Maybe ApposCoord)
     !(Maybe PP)
     !(Maybe RelativeClauseCoord)
  deriving (Eq)

-- | Noun-phrase.
data NP =
  NP !Specifier !N'
  deriving (Eq)

-- | A noun.
data N =
  N !Text
  deriving (Eq)

data PP =
  PP !Preposition !NPCoord
  deriving (Eq)

data Preposition =
  Preposition !Text
  deriving (Eq)

data ApposCoord =
  ApposCoord !Apposition !(Maybe ApposCoord)
  deriving (Eq)

data Apposition
  = AppositionVar !Variable
  | AppositionQuote !Quotation
  deriving (Eq)

data Quotation =
  Quotation !Text
  deriving (Eq)

data Variable =
  Variable !Text
  deriving (Eq)

data RelativeClauseCoord =
  RelativeClauseCoord !RelativeClause !(Maybe (Coord,RelativeClauseCoord))
  deriving (Eq)

data PossessiveNPCoord
  = PossessiveNPCoordGen !GenitiveNPCoord
  | PossessiveNPCoordPronoun !PossessivePronounCoord
  deriving (Eq)

data GenitiveNPCoord
  = GenitiveNPCoord !GenitiveSpecifier !GenitiveN' !GenitiveTail
  | GenitiveNPCoordName !ProperName !GenitiveTail
  deriving (Eq)

data ProperName =
  ProperName !Text
  deriving (Eq)

data PossessivePronounCoord =
  PossessivePronounCoord !PossessivePronoun
                         !(Maybe PossessivePronounCoord)
  deriving (Eq)

data GenitiveTail
  = GenitiveTailSaxonTail !SaxonGenitiveTail
  | GenitiveTailCoordtail !GenitiveCoordTail
  deriving (Eq)

data GenitiveCoordTail =
  GenitiveCoordTail !GenitiveNPCoord
  deriving (Eq)

data SaxonGenitiveTail =
  SaxonGenitiveTail !SaxonGenitiveMarker !(Maybe (GenitiveN',SaxonGenitiveTail))
  deriving (Eq)

data RelativeClause =
  RelativeClause !VPCoord
  deriving (Eq)

data VPCoord
  = VPCoord' !VP !Coord !VPCoord
  | VPCoordVP !VP
  deriving (Eq)

data GenitiveSpecifier
  = GenitiveSpecifierD !Determiner
  | GenitiveSpecifierPPC !PossessivePronounCoord
  | GenitiveSpecifierN !Integer
  deriving (Eq)

data GenitiveN' =
  GenitiveN' !(Maybe AdjectiveCoord)
             !N
             !(Maybe ApposCoord)
  deriving (Eq)

data VP =
  VP !V'
  deriving (Eq)

data V' =
  V' !(Maybe AdverbCoord) !ComplV ![VModifier] -- What is *?
  deriving (Eq)

data AdverbCoord =
  AdverbCoord !Adverb !(Maybe AdverbCoord)
  deriving (Eq)

data ComplV =
  ComplV !IntransitiveV
  deriving (Eq)

data IntransitiveV =
  IntransitiveV !Text
  deriving (Eq)

data IntransitiveAdjective =
  IntransitiveAdjective !Text
  deriving (Eq)

data VModifier
  = VModifierVC !AdverbCoord
  | VModifierPP !PP
  | VModifierAVPP !AdverbialPP
  deriving (Eq)

data AdverbialPP =
  AdverbialPP !Preposition !AdverbCoord
  deriving (Eq)

data Adverb =
  Adverb !Text
  deriving (Eq)

data Specifier
  = SpecifyDeterminer !Determiner
  | SpecifyPossessive !PossessiveNPCoord
  | SpecifyNumberP !NumberP
  deriving (Eq)

data AdjectiveCoord =
  AdjectiveCoord !IntransitiveAdjective
                 !(Maybe AdjectiveCoord)
  deriving (Eq)

data NumberP =
  NumberP !(Maybe GeneralizedQuantor) !Integer
  deriving (Eq)

data ExistentialGlobalQuantor =
  ExistentialGlobalQuantor !Copula
  deriving (Eq)

data ExistentialGlobalQuestionQuantor =
  ExistentialGlobalQuestionQuantor !Copula
  deriving (Eq)

instance Pretty ExistentialGlobalQuestionQuantor where
  pretty s x =
    pretty s x <> " there"

data Aux
  = Do -- ^ \"do\"
  | Does -- ^ \"does\"
  deriving (Eq)

instance Pretty Aux where
  pretty s Do = "do"
  pretty s Does = "does"

data Coord
  = And -- ^ \"and\"
  | Or -- ^ \"or\"
  deriving (Eq)

instance Pretty Coord where
  pretty s And = "and"
  pretty s Or = "or"

data Copula
  = Is -- ^ \"is\"
  | Are -- ^ \"are\"
  deriving (Eq)

instance Pretty Copula where
  pretty s Is = "is"
  pretty s Are = "are"

data Determiner
  = The -- ^ \"the\"
  | A -- ^ \"a\"
  | An -- ^ \"an\"
  | Some -- ^ \"some\"
  | No -- ^ \"no\"
  | EveryEach -- ^ \"every\" / \"each\"
  | All -- ^ \"all\"
  | NotEveryEach -- ^ \"not every\" / \"not each\"
  | NotAll -- ^ \"not all\"
  | Which -- ^ \"which\"
  deriving (Eq)

data DistributiveGlobalQuantor =
  ForEachOf -- ^ \"for each of\"
  deriving (Eq)

data DistributiveMarker =
  EachOf -- ^ \"each of\"
  deriving (Eq)

data GeneralizedQuantor
  = AtMost -- ^ \"at most\"
  | AtLeast -- ^ \"at least\"
  | MoreThan -- ^ \"more than\"
  | LessThan -- ^ \"less than\"
  | NotMoreThan -- ^ \"not more than\"
  | NotLessThan -- ^ \"not less than\"
  deriving (Eq)

data PossessivePronoun
  = HisHer -- ^ \"his\" / \"her\" / \"his/her\"
  | Its -- ^ \"its\"
  | Their -- ^ \"their\"
  | HisHerOwn -- ^ \"his own\" / \"her own\" / \"his/her own\"
  | ItsOwn -- ^ \"its own\"
  | TheirOwn -- ^ \"their own\"
  | Whose -- ^ \"whose\"
  deriving (Eq)

data Pronoun
  = It -- ^ \"it\"
  | HeShe -- ^ \"he\" / \"she\"
  | Himher -- ^ \"him\" / \"her\"
  | They -- ^ \"they\"
  | Them -- ^ \"them\"
  | Itself -- ^ \"itself\"
  | HimHerSelf -- ^ \"himself\" / \"herself\"
  | Themselves -- ^ \"themselves\"
  | SomeoneSomebody -- ^ \"someone\" / \"somebody\"
  | Something -- ^ \"something\"
  | NoOneNobody -- ^ \"no one\" / \"nobody\"
  | NoThing -- ^ \"nothing\"
  | EveryoneEverybody -- ^ \"everyone\" / \"everybody\"
  | Everything -- ^ \"everything\"
  | NotEveryoneEverybody -- ^ \"not everyone\" / \"not everybody\"
  | NotEverything -- ^ \"not everything\"
  | WhatWho -- ^ \"what\" / \"who\"
  | Whom -- ^ \"whom\"
  | WhichWho -- ^ \"which\" / \"who\"
  deriving (Eq)

-- | The Saxon genitive used for possession.
data SaxonGenitiveMarker
  = Apostrophe -- ^ \"'\"
  | ApostropheS -- ^ \"'s\"
  deriving (Eq)

data UniversalGlobalQuantor
  = ForEveryEach -- ^ \"for every\" / \"for each\"
  | ForAll -- ^ \"for all\"
  deriving (Eq)
