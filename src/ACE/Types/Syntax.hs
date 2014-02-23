{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- | Types for the syntax tree.

module ACE.Types.Syntax where

import ACE.Pretty

import Data.Monoid
import Data.Text (Text)
import Prelude hiding (String)

-- | Specifications consist of a sentence coordination followed by a
-- period and optionally one ore more subsequent specifications.
data Specification =
  Specification !SentenceCoord !(Maybe Specification)
  deriving (Show,Eq)

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
  deriving (Show,Eq)

data SentenceCoord_1 =
  SentenceCoord_1 !SentenceCoord_2 !(Maybe SentenceCoord_1)
  deriving (Show,Eq)

data SentenceCoord_2 =
  SentenceCoord_2 !SentenceCoord_3 !(Maybe SentenceCoord_2)
  deriving (Show,Eq)

data SentenceCoord_3 =
  SentenceCoord_3 !TopicalizedSentence !(Maybe SentenceCoord_3)
  deriving (Show,Eq)

-- | Singular/plural.
data Plurality
  = Singular
  | Plural
  deriving (Show,Eq)

-- | A topicalized sentence can start with an existential topic or a
-- universal topic. It needs, however, not be topicalized at all but
-- can just be an ordinary composite sentence.
data TopicalizedSentence
  = TopicalizedSentenceExistential !ExistentialTopic !(Maybe SentenceCoord) -- ^ Example: \"There is a card such that the code of the card is valid.\"
  | TopicalizedSentenceUniversal !UniversalTopic !SentenceCoord -- ^ Example: \"For every code there is a card such that the code belongs to it.\"
  | TopicalizedSentenceComposite !CompositeSentence -- ^ Example: \"Homer is a man.\"
  deriving (Show,Eq)

data UniversalTopic =
  UniversalTopic !UniversalGlobalQuantor !N'
  deriving (Show,Eq)

data CompositeSentence
  = CompositeSentenceCond !ConditionalSentence
  | CompositeSentenceNeg !NegatedSentence
  | CompositeSentence !Sentence
  deriving (Show,Eq)

data ConditionalSentence =
  ConditionalSentence !SentenceCoord
                      !SentenceCoord
  deriving (Show,Eq)

data NegatedSentence =
  NegatedSentence !SentenceCoord
  deriving (Show,Eq)

data Sentence =
  Sentence !NPCoord !VPCoord
  deriving (Show,Eq)

data ExistentialTopic =
  ExistentialTopic !ExistentialGlobalQuantor
                   !NPCoord
  deriving (Show,Eq)

data NPCoord
  = NPCoordDistributed !DistributiveMarker !UnmarkedNPCoord
  | NPCoordUnmarked !UnmarkedNPCoord
  deriving (Show,Eq)

data UnmarkedNPCoord =
  UnmarkedNPCoord !NP !(Maybe UnmarkedNPCoord)
  deriving (Show,Eq)

-- | Modified noun.
data N' =
  N' !(Maybe AdjectiveCoord)
     !N
     !(Maybe ApposCoord)
     !(Maybe NPCoord)
     !(Maybe RelativeClauseCoord)
  deriving (Show,Eq)

-- | Noun-phrase.
data NP =
  NP !Specifier !N'
  deriving (Show,Eq)

-- | A noun.
data N =
  N !Text
  deriving (Show,Eq)

data PP =
  PP !Preposition !NPCoord
  deriving (Show,Eq)

data Preposition =
  Preposition !Text
  deriving (Show,Eq)

data ApposCoord =
  ApposCoord !Apposition !(Maybe ApposCoord)
  deriving (Show,Eq)

data Apposition
  = AppositionVar !Variable
  | AppositionQuote !Quotation
  deriving (Show,Eq)

data Quotation =
  Quotation !Text
  deriving (Show,Eq)

data Variable =
  Variable !Text
  deriving (Show,Eq)

data RelativeClauseCoord =
  RelativeClauseCoord !RelativeClause !(Maybe (Coord,RelativeClauseCoord))
  deriving (Show,Eq)

data PossessiveNPCoord
  = PossessiveNPCoordGen !GenitiveNPCoord
  | PossessiveNPCoordPronoun !PossessivePronounCoord
  deriving (Show,Eq)

data GenitiveNPCoord
  = GenitiveNPCoord !GenitiveSpecifier !GenitiveN' !GenitiveTail
  | GenitiveNPCoordName !ProperName !GenitiveTail
  deriving (Show,Eq)

data ProperName =
  ProperName !Text
  deriving (Show,Eq)

data PossessivePronounCoord =
  PossessivePronounCoord !PossessivePronoun
                         !(Maybe PossessivePronounCoord)
  deriving (Show,Eq)

data GenitiveTail
  = GenitiveTailSaxonTail !SaxonGenitiveTail
  | GenitiveTailCoordtail !GenitiveCoordTail
  deriving (Show,Eq)

data GenitiveCoordTail =
  GenitiveCoordTail !GenitiveNPCoord
  deriving (Show,Eq)

data SaxonGenitiveTail =
  SaxonGenitiveTail !SaxonGenitiveMarker !(Maybe (GenitiveN',SaxonGenitiveTail))
  deriving (Show,Eq)

data RelativeClause
  = RelativeClauseThat !VPCoord
  | RelativeClauseNP !NPCoord !VPCoord
  | RelativeClauseThatNPVP !NPCoord !VPCoord
  | RelativeClauseNPVP !NPCoord !NPCoord !VPCoord
  | RelativeClausePP !PP !NPCoord !VPCoord
  deriving (Show,Eq)

data VPCoord
  = VPCoord' !VP !Coord !VPCoord
  | VPCoordVP !VP
  deriving (Show,Eq)

data GenitiveSpecifier
  = GenitiveSpecifierD !Determiner
  | GenitiveSpecifierPPC !PossessivePronounCoord
  | GenitiveSpecifierN !Integer
  deriving (Show,Eq)

data GenitiveN' =
  GenitiveN' !(Maybe AdjectiveCoord)
             !N
             !(Maybe ApposCoord)
  deriving (Show,Eq)

data VP
  = VP !V'
  | VPNeg !Copula !V'
  deriving (Show,Eq)

data V' =
  V' !(Maybe AdverbCoord) !ComplV ![VModifier] -- What is *?
  deriving (Show,Eq)

data AdverbCoord =
  AdverbCoord !Adverb !(Maybe AdverbCoord)
  deriving (Show,Eq)

data ComplV
  = ComplVIV !IntransitiveV
  | ComplVPI !PhrasalIntransitiveV !PhrasalParticle
  | ComplVTV !TransitiveV !Compl
  | ComplVPV !PhrasalTransitiveV !PhrasalParticle !Compl
  | ComplVPV' !PhrasalTransitiveV !Compl !PhrasalParticle
  | ComplVDisV !DistransitiveV !Compl !Compl
  | ComplVPDV !PhrasalDistransitiveV !Compl !PhrasalParticle !Compl
  | ComplVCopula !Copula !CopulaCompl
  deriving (Show,Eq)

data PhrasalTransitiveV =
  PhrasalTransitiveV !Text
  deriving (Show,Eq)

data PhrasalDistransitiveV =
  PhrasalDistransitiveV !Text
  deriving (Show,Eq)

data CopulaCompl
  = CopulaComplAPC !APCoord
  | CopulaComplNPC !NPCoord
  | CopulaComplPP  !PP
  deriving (Show,Eq)

data APCoord
  = APCoordAnd !APgrad !APCoord
  | APCoord !APgrad
  deriving (Show,Eq)

data APgrad
  = APgradAPThan !AP !NPCoord
  | APgradAP !AP
  deriving (Show,Eq)

data AP
  = APIntrans !IntransitiveAdjective
  | APTrans !TransitiveAdjective !PP
  deriving (Show,Eq)

data TransitiveAdjective =
  TransitiveAdjective !Text
  deriving (Show,Eq)

data Compl
  = ComplNP !NPCoord
  | ComplPP !PP
  deriving (Show,Eq)

data PhrasalIntransitiveV =
  PhrasalIntransitiveV !Text
  deriving (Show,Eq)

data PhrasalParticle =
  PhrasalParticle !Text
  deriving (Show,Eq)

data IntransitiveV =
  IntransitiveV !Text
  deriving (Show,Eq)

data TransitiveV =
  TransitiveV !Text
  deriving (Show,Eq)

data DistransitiveV =
  DistransitiveV !Text
  deriving (Show,Eq)

data IntransitiveAdjective =
  IntransitiveAdjective !Text
  deriving (Show,Eq)

data VModifier
  = VModifierVC !AdverbCoord
  | VModifierPP !PP
  | VModifierAVPP !AdverbialPP
  deriving (Show,Eq)

data AdverbialPP =
  AdverbialPP !Preposition !AdverbCoord
  deriving (Show,Eq)

data Adverb =
  Adverb !Text
  deriving (Show,Eq)

data Specifier
  = SpecifyDeterminer !Determiner
  | SpecifyPossessive !PossessiveNPCoord
  | SpecifyNumberP !NumberP
  deriving (Show,Eq)

data AdjectiveCoord =
  AdjectiveCoord !IntransitiveAdjective
                 !(Maybe AdjectiveCoord)
  deriving (Show,Eq)

data NumberP =
  NumberP !(Maybe GeneralizedQuantor) !Integer
  deriving (Show,Eq)

data ExistentialGlobalQuantor =
  ExistentialGlobalQuantor !Copula
  deriving (Show,Eq)

data ExistentialGlobalQuestionQuantor =
  ExistentialGlobalQuestionQuantor !Copula
  deriving (Show,Eq)

instance Pretty ExistentialGlobalQuestionQuantor where
  pretty s x =
    pretty s x <> " there"

data Aux
  = Do -- ^ \"do\"
  | Does -- ^ \"does\"
  deriving (Show,Eq)

instance Pretty Aux where
  pretty _ Do = "do"
  pretty _ Does = "does"

data Coord
  = And -- ^ \"and\"
  | Or -- ^ \"or\"
  deriving (Show,Eq)

instance Pretty Coord where
  pretty _ And = "and"
  pretty _ Or = "or"

data Copula
  = Is -- ^ \"is\"
  | Are -- ^ \"are\"
  deriving (Show,Eq)

instance Pretty Copula where
  pretty _ Is = "is"
  pretty _ Are = "are"

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
  deriving (Show,Eq)

data DistributiveGlobalQuantor =
  ForEachOf -- ^ \"for each of\"
  deriving (Show,Eq)

data DistributiveMarker =
  EachOf -- ^ \"each of\"
  deriving (Show,Eq)

data GeneralizedQuantor
  = AtMost -- ^ \"at most\"
  | AtLeast -- ^ \"at least\"
  | MoreThan -- ^ \"more than\"
  | LessThan -- ^ \"less than\"
  | NotMoreThan -- ^ \"not more than\"
  | NotLessThan -- ^ \"not less than\"
  deriving (Show,Eq)

data PossessivePronoun
  = HisHer -- ^ \"his\" / \"her\" / \"his/her\"
  | Its -- ^ \"its\"
  | Their -- ^ \"their\"
  | HisHerOwn -- ^ \"his own\" / \"her own\" / \"his/her own\"
  | ItsOwn -- ^ \"its own\"
  | TheirOwn -- ^ \"their own\"
  | Whose -- ^ \"whose\"
  deriving (Show,Eq)

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
  deriving (Show,Eq)

-- | The Saxon genitive used for possession.
data SaxonGenitiveMarker
  = Apostrophe -- ^ \"'\"
  | ApostropheS -- ^ \"'s\"
  deriving (Show,Eq)

data UniversalGlobalQuantor
  = ForEveryEach -- ^ \"for every\" / \"for each\"
  | ForAll -- ^ \"for all\"
  deriving (Show,Eq)
