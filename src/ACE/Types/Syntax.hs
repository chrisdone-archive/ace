-- | Types for the syntax tree.

module ACE.Types.Syntax where

import ACE.Pretty

import Data.Semigroup

-- | Specifications consist of a sentence coordination followed by a
-- period and optionally one ore more subsequent specifications.
data Specification =
  Specification SentenceCoord (Maybe Specification)
  deriving (Show)

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
  SentenceCoord SentenceCoord_1 (Maybe SentenceCoord)
  deriving (Show)

data SentenceCoord_1 =
  SentenceCoord_1 SentenceCoord_2 (Maybe SentenceCoord_1)
  deriving (Show)

data SentenceCoord_2 =
  SentenceCoord_2 SentenceCoord_3 (Maybe SentenceCoord_2)
  deriving (Show)

data SentenceCoord_3 =
  SentenceCoord_3 TopicalizedSentence (Maybe SentenceCoord_3)
  deriving (Show)

-- | Singular/plural.
data Plurality
  = Singular
  | Plural
  deriving (Show)

-- | A topicalized sentence can start with an existential topic or a
-- universal topic. It needs, however, not be topicalized at all but
-- can just be an ordinary composite sentence.
data TopicalizedSentence
  = TopicalizedSentenceExistential ExistentialTopic (Maybe SentenceCoord) -- ^ Example: \"There is a card such that the code of the card is valid.\"
  | TopicalizedSentenceUniversal UniversalTopic SentenceCoord -- ^ Example: \"For every code there is a card such that the code belongs to it.\"
  | TopicalizedSentenceComposite CompositeSentence -- ^ Example: \"Homer is a man.\"
  deriving (Show)

data UniversalTopic =
  UniversalTopic UniversalGlobalQuantor N'
  deriving (Show)

data CompositeSentence
  = CompositeSentenceCond ConditionalSentence
  | CompositeSentenceNeg NegatedSentence
  | CompositeSentence Sentence
  deriving (Show)

data ConditionalSentence =
  ConditionalSentence SentenceCoord
                      SentenceCoord
  deriving (Show)

data NegatedSentence =
  NegatedSentence SentenceCoord
  deriving (Show)

data Sentence =
  Sentence NPCoord VPCoord
  deriving (Show)

data ExistentialTopic =
  ExistentialTopic ExistentialGlobalQuantor
                   NPCoord
  deriving (Show)

data NPCoord =
  NPCoordDistributed DistributiveMarker UnmarkedNPCoord
  deriving (Show)

data UnmarkedNPCoord =
  UnmarkedNPCoord NP UnmarkedNPCoord
  deriving (Show)

-- | Modified noun.
data N' =
  N' (Maybe AdjectiveCoord)
     N
     (Maybe ApposCoord)
     (Maybe PP)
     (Maybe RelativeClauseCoord)
  deriving (Show)

-- | Noun-phrase.
data NP =
  NP Specifier N'
  deriving (Show)

-- | A noun.
data N =
  N String
  deriving (Show)

data PP =
  PP Preposition NPCoord
  deriving (Show)

data Preposition =
  Preposition String
  deriving (Show)

data ApposCoord =
  ApposCoord Apposition (Maybe ApposCoord)
  deriving (Show)

data Apposition
  = AppositionVar Variable
  | AppositionQuote Quotation
  deriving (Show)

data Quotation =
  Quotation String
  deriving (Show)

data Variable =
  Variable String
  deriving (Show)

data RelativeClauseCoord =
  RelativeClauseCoord RelativeClause (Maybe (Coord,RelativeClauseCoord))
  deriving (Show)

data PossessiveNPCoord
  = PossessiveNPCoordGen GenitiveNPCoord
  | PossessiveNPCoordPronoun PossessivePronounCoord
  deriving (Show)

data GenitiveNPCoord
  = GenitiveNPCoord GenitiveSpecifier GenitiveN' GenitiveTail
  | GenitiveNPCoordName ProperName GenitiveTail
  deriving (Show)

data ProperName =
  ProperName String
  deriving (Show)

data PossessivePronounCoord =
  PossessivePronounCoord PossessivePronoun
                         (Maybe PossessivePronounCoord)
  deriving (Show)

data GenitiveTail
  = GenitiveTailSaxonTail SaxonGenitiveTail
  | GenitiveTailCoordtail GenitiveCoordTail
  deriving (Show)

data GenitiveCoordTail =
  GenitiveCoordTail GenitiveNPCoord
  deriving (Show)

data SaxonGenitiveTail =
  SaxonGenitiveTail SaxonGenitiveMarker (Maybe (GenitiveN',SaxonGenitiveTail))
  deriving (Show)

data RelativeClause =
  RelativeClause VPCoord
  deriving (Show)

data VPCoord
  = VPCoord' VP Coord VPCoord
  | VPCoordVP VP
  deriving (Show)

data GenitiveSpecifier
  = GenitiveSpecifierD Determiner
  | GenitiveSpecifierPPC PossessivePronounCoord
  | GenitiveSpecifierN Number
  deriving (Show)

data GenitiveN' =
  GenitiveN' (Maybe AdjectiveCoord)
             N
             (Maybe ApposCoord)
  deriving (Show)

data VP =
  VP V'
  deriving (Show)

data V' =
  V' (Maybe AdverbCoord) ComplV [VModifier] -- What is *?
  deriving (Show)

data AdverbCoord =
  AdverbCoord Adverb (Maybe AdverbCoord)
  deriving (Show)

data ComplV =
  ComplV IntransitiveV
  deriving (Show)

data IntransitiveV =
  IntransitiveV String
  deriving (Show)

data IntransitiveAdjective =
  IntransitiveAdjective String
  deriving (Show)

data VModifier
  = VModifierVC AdverbCoord
  | VModifierPP PP
  | VModifierAVPP AdverbialPP
  deriving (Show)

data AdverbialPP =
  AdverbialPP Preposition AdverbCoord
  deriving (Show)

data Adverb =
  Adverb String
  deriving (Show)

data Specifier
  = SpecifyDeterminer Determiner
  | SpecifyPossessive PossessiveNPCoord
  | SpecifyNumberP NumberP
  deriving (Show)

data AdjectiveCoord =
  AdjectiveCoord IntransitiveAdjective
                 (Maybe AdjectiveCoord)
  deriving (Show)

data NumberP =
  NumberP (Maybe GeneralizedQuantor) Number
  deriving (Show)

data Number =
  Number Integer
  deriving (Show)

data ExistentialGlobalQuantor =
  ExistentialGlobalQuantor Copula
  deriving (Show)

data ExistentialGlobalQuestionQuantor =
  ExistentialGlobalQuestionQuantor Copula
  deriving (Show)

instance Pretty ExistentialGlobalQuestionQuantor where
  pretty s x =
    pretty s x <> " there"

data Aux
  = Do -- ^ \"do\"
  | Does -- ^ \"does\"
  deriving (Show)

instance Pretty Aux where
  pretty s Do = "do"
  pretty s Does = "does"

data Coord
  = And -- ^ \"and\"
  | Or -- ^ \"or\"
  deriving (Show)

instance Pretty Coord where
  pretty s And = "and"
  pretty s Or = "or"

data Copula
  = Is -- ^ \"is\"
  | Are -- ^ \"are\"
  deriving (Show)

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
  deriving (Show)

data DistributiveGlobalQuantor =
  ForEachOf -- ^ \"for each of\"
  deriving (Show)

data DistributiveMarker =
  EachOf -- ^ \"each of\"
  deriving (Show)

data GeneralizedQuantor
  = AtMost -- ^ \"at most\"
  | AtLeast -- ^ \"at least\"
  | MoreThan -- ^ \"more than\"
  | LessThan -- ^ \"less than\"
  | NotMoreThan -- ^ \"not more than\"
  | NotLessThan -- ^ \"not less than\"
  deriving (Show)

data PossessivePronoun
  = HisHer -- ^ \"his\" / \"her\" / \"his/her\"
  | Its -- ^ \"its\"
  | Their -- ^ \"their\"
  | HisHerOwn -- ^ \"his own\" / \"her own\" / \"his/her own\"
  | ItsOwn -- ^ \"its own\"
  | TheirOwn -- ^ \"their own\"
  | Whose -- ^ \"whose\"
  deriving (Show)

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
  deriving (Show)

data SaxonGenitiveMarker
  = Apostrophe -- ^ \"'\"
  | ApostropheS -- ^ \"'s\"
  deriving (Show)

data UniversalGlobalQuantor
  = ForEveryEach -- ^ \"for every\" / \"for each\"
  | ForAll -- ^ \"for all\"
  deriving (Show)
