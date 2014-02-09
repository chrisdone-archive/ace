{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Attempto Controlled English parser and printer.

module ACE where

import Control.Applicative
import Data.Default
import Data.Semigroup ((<>))
import Text.Parsec hiding (optional,many,(<|>))

-- | Specifications consist of a sentence coordination followed by a
-- period and optionally one ore more subsequent specifications.
data Specification =
  Specification SentenceCoord (Maybe Specification)
  deriving (Show)

instance Pretty Specification where
  pretty s (Specification coord ms) =
    parens s (parens s (pretty s coord) <> "." <>
              parens s (pretty s ms))

-- | Specifications consist of a sentence coordination followed by a
-- period and optionally one ore more subsequent specifications.
specification =
  Specification
    <$> sentenceCoord <* string "."
    <*> optional (try specification)

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

instance Pretty SentenceCoord where
  pretty s (SentenceCoord c1 mc) =
    parens s (pretty s c1) <> maybe "" ((", or " <>) . parens s . pretty s) mc

-- | Sentences can be coordinated by @and@ and @or@. @And@ refers to the
-- logical conjunction, while @or@ de-notes the logical disjunction. The
-- logical conjunction has a higher precedence than the disjunction.
sentenceCoord =
  SentenceCoord
    <$> sentenceCoord_1
    <*> optional (try (string " or " *> sentenceCoord))

data SentenceCoord_1 =
  SentenceCoord_1 SentenceCoord_2 (Maybe SentenceCoord_1)
  deriving (Show)

instance Pretty SentenceCoord_1 where
  pretty s (SentenceCoord_1 c2 mc) =
    parens s (pretty s c2) <> maybe "" ((", and " <>) . parens s . pretty s) mc

sentenceCoord_1 =
  SentenceCoord_1
    <$> sentenceCoord_2
    <*> optional (try (string ", and " *> sentenceCoord_1))

data SentenceCoord_2 =
  SentenceCoord_2 SentenceCoord_3 (Maybe SentenceCoord_2)
  deriving (Show)

instance Pretty SentenceCoord_2 where
  pretty s (SentenceCoord_2 c3 mc) =
    parens s (pretty s c3) <> maybe "" ((" or " <>) . parens s . pretty s) mc

sentenceCoord_2 =
  SentenceCoord_2
    <$> sentenceCoord_3
    <*> optional (try (string " or " *> sentenceCoord_2))

data SentenceCoord_3 =
  SentenceCoord_3 TopicalizedSentence (Maybe SentenceCoord_3)
  deriving (Show)

instance Pretty SentenceCoord_3 where
  pretty s (SentenceCoord_3 ts mc) =
    parens s (pretty s ts) <> maybe "" ((" and " <>) . parens s . pretty s) mc

sentenceCoord_3 =
  SentenceCoord_3
    <$> topicalizedSentence
    <*> optional (try (string " and " *> sentenceCoord_3))

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

instance Pretty TopicalizedSentence where
  pretty s (TopicalizedSentenceExistential topic _) =
    pretty s topic

topicalizedSentence =
  (TopicalizedSentenceExistential <$> existentialTopic <*> optional sentenceCoord) <|>
  (TopicalizedSentenceUniversal <$> universalTopic <*> sentenceCoord) <|>
  (TopicalizedSentenceComposite <$> compositeSentence)

data UniversalTopic =
  UniversalTopic {-TODO-}
  deriving (Show)

universalTopic = undefined

data CompositeSentence =
  CompositeSentence {-TODO-}
  deriving (Show)

compositeSentence = undefined

data ExistentialTopic =
  ExistentialTopic ExistentialGlobalQuantor
                   NPCoord
  deriving (Show)

instance Pretty ExistentialTopic where
  pretty s (ExistentialTopic q x) = pretty s q

existentialTopic =
  ExistentialTopic <$> existentialGlobalQuantor
                   <*> npCoord

npCoord = undefined

data NPCoord
  = NPcoordDistributed DistributiveMarker UnmarkedNPCoord
  deriving (Show)

data UnmarkedNPCoord
  = UnmarkedNPCoord NP UnmarkedNPCoord
  deriving (Show)

-- | Noun-phrase.
data NP = NP Specifier N'
  deriving (Show)

-- | Modified noun.
data N' = N' (Maybe AdjectiveCoord) N (Maybe ApposCoord) (Maybe PP) (Maybe RelativeClauseCoord)
  deriving (Show)

-- | A noun.
data N = N String
  deriving (Show)

-- | Prepositional phrase.
data PP = PP Preposition NPCoord
  deriving (Show)

-- | A preposition.
data Preposition = Preposition String
  deriving (Show)

data ApposCoord = ApposCoord Apposition (Maybe ApposCoord)
  deriving (Show)

data Apposition
  = AppositionVar Variable
  | AppositionQuote QuotedString
  deriving (Show)

data QuotedString
  = QuotedString String
  deriving (Show)

data Variable
  = Variable String
  deriving (Show)

data RelativeClauseCoord =
  RelativeClauseCoord RelativeClause (Maybe (Coord,RelativeClauseCoord))
  deriving (Show)

data PossessiveNPCoord =
  PossessiveNPCoord GenitiveNPCoord SaxonGenitiveMarker
  deriving (Show)

data RelativeClause =
  RelativeClause VPCoord
  deriving (Show)

-- | Verb phrase coordination.
data VPCoord =
  VPCoord VP Coord (Maybe VPCoord) -- TODO: Check that this Maybe
                                   -- guess is correct.
  deriving (Show)

data GenitiveNPCoord =
  GenitiveNPCoord GenitiveNP (Maybe GenitiveNPCoord)
  deriving (Show)

data GenitiveNP =
  GenitiveNP Specifier GenitiveN'
  deriving (Show)

data GenitiveN' =
  GenitiveN' (Maybe AdjectiveCoord) N (Maybe ApposCoord)
  deriving (Show)

-- | Verb phrase.
data VP =
  VP V'
  deriving (Show)

-- | Verb.
data V' =
  V' (Maybe AdverbCoord) ComplV [VModifier] -- What is *?
  deriving (Show)

data AdverbCoord =
  AdverbCoord Adverb (Maybe AdverbCoord)
  deriving (Show)

data ComplV =
  ComplV IntransitiveV
  deriving (Show)

-- | Intransitive verb.
data IntransitiveV =
  IntransitiveV String
  deriving (Show)

data VModifier
  = VModifierVC AdverbCoord
  | VModifierPP PP
  | VModifierAVPP AdverbialPP
  deriving (Show)

data AdverbialPP =
  AdverbialPP Preposition AdverbCoord
  deriving (Show)

-- | An adverb.
data Adverb =
  Adverb String
  deriving (Show)

data Specifier
  = SpecifyDeterminer Determiner
  | SpecifyPossessive PossessiveNPCoord
  | SpecifyNumberP NumberP
  deriving (Show)

-- | Adjective coordination.
data AdjectiveCoord = AdjectiveCoord
  deriving (Show)

-- | A number phrase.
data NumberP =
  NumberP (Maybe GeneralizedQuantor) Number
  deriving (Show)

-- | Some integer number.
data Number =
  Number Integer
  deriving (Show)

-- | There is / there are.
data ExistentialGlobalQuantor = ExistentialGlobalQuantor Copula
  deriving (Show)

instance Pretty ExistentialGlobalQuantor where
  pretty s x =
    "there " <> pretty s x

existentialGlobalQuantor =
  string "there " *> (ExistentialGlobalQuantor <$> copula)

-- | Is there / are there?
data ExistentialGlobalQuestionQuantor = ExistentialGlobalQuestionQuantor Copula
  deriving (Show)

instance Pretty ExistentialGlobalQuestionQuantor where
  pretty s x =
    pretty s x <> " there"

existentialGlobalQuestionQuantor =
  (ExistentialGlobalQuestionQuantor <$> copula) *> string " there"

-- | Do/does.
data Aux
  = Do -- ^ \"do\"
  | Does -- ^ \"does\"
  deriving (Show)

aux =
  (string "do" *> pure Do) <|>
  (string "does" *> pure Does)

instance Pretty Aux where
  pretty s Do = "do"
  pretty s Does = "does"

-- | And/or.
data Coord
  = And -- ^ \"and\"
  | Or -- ^ \"or\"
  deriving (Show)

coord =
  (string "and" *> pure And) <|>
  (string "or" *> pure Or)

instance Pretty Coord where
  pretty s And = "and"
  pretty s Or = "or"

-- | Is/are.
data Copula
  = Is -- ^ \"is\"
  | Are -- ^ \"are\"
  deriving (Show)

copula =
  (string "is" *> pure Is) <|>
  (string "are" *> pure Are)

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

determiner =
  (string "the" *> pure The) <|>
  try (string "an" *> pure An) <|>
  (string "a" *> pure A) <|>
  (string "some" *> pure Some) <|>
  try (string "not " *> (try (string "every") <|> string "each") *> pure NotEveryEach) <|>
  (string "not all" *> pure NotAll) <|>
  (string "no" *> pure No) <|>
  ((try (string "every") <|> string "each") *> pure EveryEach) <|>
  (string "all" *> pure All) <|>
  (string "which" *> pure Which)

data DistributiveGlobalQuantor =
  ForEachOf -- ^ \"for each of\"
  deriving (Show)

distributiveGlobalQuantor = string "for each of" *> pure ForEachOf

data DistributiveMarker =
  EachOf -- ^ \"each of\"
  deriving (Show)

distributiveMarker = string "each of" *> pure EachOf

data GeneralizedQuantor
  = AtMost -- ^ \"at most\"
  | AtLeast -- ^ \"at least\"
  | MoreThan -- ^ \"more than\"
  | LessThan -- ^ \"less than\"
  | NotMoreThan -- ^ \"not more than\"
  | NotLessThan -- ^ \"not less than\"
  deriving (Show)

generalizedQuantor =
  try (string "at most" *> pure AtMost) <|>
  try (string "at least" *> pure AtLeast) <|>
  try (string "more than" *> pure MoreThan) <|>
  try (string "less than" *> pure LessThan) <|>
  try (string "not more than" *> pure NotMoreThan) <|>
  try (string "not less than" *> pure NotLessThan)

data PossessivePronoun
  = HisHer -- ^ \"his\" / \"her\"
  | Its -- ^ \"its\"
  | Their -- ^ \"their\"
  | HisHerOwn -- ^ \"his own\" / \"her own\"
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
  | Nothing -- ^ \"nothing\"
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

class Pretty p where
  pretty :: PrettySettings -> p -> String

data PrettySettings = PrettySettings
  { prettyShowPrecedence :: Bool }

instance Default PrettySettings where
  def = PrettySettings { prettyShowPrecedence = True }

-- | Human-readable pretty printing settings.
human = def { prettyShowPrecedence = False }

instance Pretty a => Pretty (Maybe a) where
  pretty s = maybe "" (pretty s)

-- | Add parentheses for debugging purposes.
parens :: PrettySettings -> String -> String
parens PrettySettings{prettyShowPrecedence=True} s = "{" <> s <> "}"
parens _ s = s
