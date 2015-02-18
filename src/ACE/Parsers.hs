{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-missing-signatures #-}

-- | Parsers for ACE syntax types.

module ACE.Parsers where

import ACE.Combinators
import ACE.Types.Syntax
import ACE.Types.Tokens

import Control.Applicative
import Control.Applicative.QQ.Idiom
import Control.Monad hiding (ap)
import Data.Text (Text)
import GHC.Tuple
import Text.Parsec ()
import Text.Parsec.Prim (Stream,ParsecT,try,getState)

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
  , acePhrasalTransitiveV    :: ParsecT s (ACEParser s m) m Text -- ^ Parser for phrasal transitive verbs.
  , acePhrasalDistransitiveV :: ParsecT s (ACEParser s m) m Text -- ^ Parser for phrasal distransitive verbs.
  , aceTransitiveVerb        :: ParsecT s (ACEParser s m) m Text -- ^ Parser for transitive verbs.
  , aceDistransitiveVerb     :: ParsecT s (ACEParser s m) m Text -- ^ Parser for distransitive verbs.
  , acePhrasalParticle       :: ParsecT s (ACEParser s m) m Text -- ^ Parser for phrasal particles.
  , acePhrasalIntransitiveV  :: ParsecT s (ACEParser s m) m Text -- ^ Parser for phrasal intransitive verbs.
  }

-- | A default ACE parser configuration. Just fills in all the parsers as blanks: @\<noun\>@, @\<prep\>@, etc.
defaultACEParser :: Stream s m Token => ACEParser s m
defaultACEParser =
  ACE { aceIntransitiveAdjective   = string "<intrans-adj>"
      , aceTransitiveAdjective     = string "<trans-adj>"
      , aceNoun                    = string "<noun>"
      , acePreposition             = string "<prep>"
      , aceVariable                = string "<var>"
      , aceProperName              = string "<proper-name>"
      , aceAdverb                  = string "<adverb>"
      , aceIntransitiveVerb        = string "<intrans-verb>"
      , aceDistransitiveVerb       = string "<distrans-verb>"
      , acePhrasalParticle         = string "<pparticle>"
      , acePhrasalIntransitiveV    = string "<pintrans-verb>"
      , acePhrasalDistransitiveV   = string "<pdistrans-verb>"
      , aceTransitiveVerb          = string "<trans-verb>"
      , acePhrasalTransitiveV      = string "<ptrans-verb>"
      }

-- | Some specification. A 'sentenceCoord' followed by a 'period', and
-- optionally another 'specification'.
specification =
  [i|Specification (sentenceCoord <* period)
                   (optional (try specification))|]

-- | Coordinated sentence, by: or
sentenceCoord =
  [i|SentenceCoord
       sentenceCoord_1
       (optional (try (string "or" *>
                       sentenceCoord)))|]

-- | Coordinated sentence, by: and
sentenceCoord_1 =
  [i|SentenceCoord_1
        sentenceCoord_2
       (optional (try (comma *> string "and" *> sentenceCoord_1)))|]

-- | Coordinated sentence, by: or
sentenceCoord_2 =
  [i|SentenceCoord_2
        sentenceCoord_3
       (optional (try (string "or" *> sentenceCoord_2)))|]

-- | Coordinated sentence, by: and
sentenceCoord_3 =
  [i|SentenceCoord_3
        topicalizedSentence
       (optional (try (string "and" *> sentenceCoord_3)))|]

-- | A topicalized sentence.
topicalizedSentence =
  [i|TopicalizedSentenceExistential existentialTopic (optional (try sentenceCoord))|] <|>
  [i|TopicalizedSentenceUniversal universalTopic sentenceCoord|] <|>
  [i|TopicalizedSentenceComposite compositeSentence|]

-- | A universally quantified topic.
universalTopic =
  [i|UniversalTopic universalGlobalQuantor
                    (n' False)|]

-- | A composite sentence: 'conditionalSentence', 'negatedSentence' or 'sentence'.
compositeSentence =
  compositeSentenceCond <|>
  compositeSentenceNeg <|>
  compositeSentence'
  where compositeSentenceCond =
          [i|CompositeSentenceCond conditionalSentence|]
        compositeSentenceNeg =
          [i|CompositeSentenceNeg negatedSentence|]
        compositeSentence' =
          [i|CompositeSentence sentence|]

-- | A negated sentence: it is not the case that 'sentenceCoord'
negatedSentence =
  [i|NegatedSentence
       (strings ["it","is","not","the","case","that"] *>
        sentenceCoord)|]

-- | A condition if 'sentenceCoord' then 'sentenceCoord'.
conditionalSentence =
  [i|ConditionalSentence
       (string "if" *> sentenceCoord)
       (string "then" *> sentenceCoord)|]

-- | Sentence: 'npCoord' 'vpCoord': a cat meows
sentence = [i|Sentence npCoord vpCoord|]

-- | Existential topic, a 'existentialGlobalQuantor' and a 'npCoord': there is a chair
existentialTopic =
  [i|ExistentialTopic existentialGlobalQuantor npCoord|]

-- | A noun specifier: \"a\", \"some\", \"1\", \"<proper-name>'s\".
specifier = specifierDeterminer <|> specifierPossessive <|> specifierNumber
  where specifierDeterminer =
          [i|SpecifyDeterminer determiner|]
        specifierPossessive =
          [i|SpecifyPossessive possessiveNPCoord|]
        specifierNumber =
          [i|SpecifyNumberP numberP|]

-- | A preposition. Configured by 'acePreposition'.
preposition =
  [i|Preposition (join (fmap acePreposition getState)) |]

-- | A genitive tail: dave's and a dog's
genitiveTail =
  [i|GenitiveTailSaxonTail saxonGenitiveTail|] <|>
  [i|GenitiveTailCoordtail genitiveCoordTail|]

-- | A genitive coordination tail: dave's and a dog's
genitiveCoordTail =
  [i|GenitiveCoordTail (try (string "and" *> genitiveNPCoord))|]

-- | Genitive tail.
saxonGenitiveTail =
  [i|SaxonGenitiveTail saxonGenitiveMarker
                       (optional (try next))|]
  where next = [i|(,) genitiveN' saxonGenitiveTail|]

-- | Apposition: either a 'variable' or a 'quotation'.
apposition =
  [i|AppositionVar variable|] <|>
  [i|AppositionQuote quotation|]

-- | A apposition coordination: X and Y.
apposCoord =
  [i|ApposCoord
       apposition
       (optional (try (string "and" *>
                       apposCoord)))|]

-- | A prepositional noun phrase coordination.
pp = [i|PP preposition npCoord'|]

-- | A 'relativeClause' coordination: person that walks and cake a
-- person made.
relativeClauseCoord =
  [i|RelativeClauseCoord relativeClause
                         (optional (try next))|]
  where next = [i|(,) coord relativeClauseCoord|]

-- | A noun surrounded by optional 'adjectiveCoord', a noun word 'n',
-- an optional 'apposCoord', an optional 'ofPP', an optional
-- 'relativeClauseCoord'.
n' b =
  [i|N' (optional (try adjectiveCoord))
        n
        (optional (try apposCoord))
        (optional (try ofPP))
        (if b
            then pure Nothing
            else optional (try relativeClauseCoord))|]

-- | Unmarked noun phrase coordination: some thing and a thing.
unmarkedNPCoord b =
  [i|UnmarkedNPCoord
       (np b)
       (optional (try (string "and" *>
                       unmarkedNPCoord b)))|]

-- | A noun phrase: a thing, some stuff, the thing.
np b =
  [i|NP specifier (n' b)|] <|>
  [i|NPPro pronoun|] <|>
  [i|NPProper properName|] <|>
  [i|NPVar variable|]

-- | A coordinated noun phrase. See 'npCoordX'.
npCoord = npCoordX False

-- | A coordinated noun phrase. Inside a relative clause. See 'npCoordX'.
npCoord' = npCoordX True

-- | Relative clause: person that walks, cake a person made, cake that a person made, etc.
relativeClause =
  try [i|RelativeClauseThat (string "that" *> vpCoord)|] <|>
  try [i|RelativeClauseNP npCoord' vpCoord|] <|>
  [i|RelativeClauseThatNPVP  (string "that" *> npCoord') vpCoord|] <|>
  try [i|RelativeClauseNPVP npCoord' npCoord' vpCoord|] <|>
  [i|RelativeClausePP pp npCoord' vpCoord|]

-- | An "of" prepositional phrase: of the bank
ofPP =
  string "of" *> npCoord

-- | A coordinated noun phrase: each of some customers, some customers
npCoordX b =
  distributed <|> unmarked
  where distributed =
          [i|NPCoordDistributed distributiveMarker (unmarkedNPCoord b)|]
        unmarked =
          [i|NPCoordUnmarked (unmarkedNPCoord b)|]

-- | A variable. Customized by 'aceVariable'.
variable =
  [i|Variable (join (fmap aceVariable getState))|]

-- | A proper name. Customized by 'aceProperName'.
properName =
  [i|ProperName (join (fmap aceProperName getState))|]

-- | Some quotation: \"foo bar\"
quotation =
  [i|Quotation quoted|]

-- | A noun. Customized by 'aceNoun'.
n =
  [i|N (join (fmap aceNoun getState))|]

-- | A verb phrase coordination is either a 'vp' followed by a 'coord'
-- and more 'vpCoord', or just a 'vp': walks, walks and runs, bad and
-- is not valid
vpCoord =
  do vp' <- vp
     (try [i|VPCoord' (pure vp') coord vpCoord|] <|>
      [i|VPCoordVP (pure vp')|])

-- | A verb phrase. Can be normal 'v'' or a 'copula' followed by
-- \"not\" then 'v'': walks, is not valid, etc.
vp =
  try [i|VP v'|] <|>
  [i|VPNeg (copula <* string "not") v'|]

-- | A genitive noun: dog, red cat, person 1, movie \"We Need to Talk
-- About Kevin\".
genitiveN' =
  [i|GenitiveN' (optional (try adjectiveCoord))
                n
                (optional (try apposCoord))|]

-- | A verb modifier: quickly and loudly, to a house, from now and forever
vModifier = vModifierVC <|> try vModifierPP <|> vModifierAVPP
  where vModifierVC =
          [i|VModifierVC adverbCoord|]
        vModifierPP = [i|VModifierPP pp|]
        vModifierAVPP =
          [i|VModifierAVPP adverbialPP|]

-- | Adverbial prepositional phrase: until here, by then, until now
-- and then
adverbialPP =
  [i|AdverbialPP preposition adverbCoord|]

-- | A verb. Consists of an optional 'adverbCoord', a complemented
-- verb ('complV'), and one or more verb modifiers.
--
-- TODO: I'm not actually sure whether it should be zero-to-1 or
-- zero-to-many. The paper isn't clear what VModifier* means.
v' =
  [i|V' (optional (try adverbCoord))
        complV
        (many (try vModifier))|]

-- | Genitive specifier: a, 1, some, his
genitiveSpecifier =
  [i|GenitiveSpecifierD determiner|] <|>
  [i|GenitiveSpecifierPPC possessivePronounCoord|] <|>
  [i|GenitiveSpecifierN number|]

-- | Either a 'genitiveNPCoord', or a 'possessivePronounCoord'.
possessiveNPCoord =
  try [i|PossessiveNPCoordGen genitiveNPCoord|] <|>
  [i|PossessiveNPCoordPronoun possessivePronounCoord|]

-- | A \' or \'s saxon genitive.
saxonGenitiveMarker =
  fmap (\s -> if s then ApostropheS else Apostrophe)
       genitive

-- | Possessive pronoun coordination: his and her
possessivePronounCoord =
  [i|PossessivePronounCoord
       possessivePronoun
       (optional (try (string "and" *>
                       possessivePronounCoord)))|]

-- | A genitive noun phrase coordination: dave's, a dog's, a man and a dog's
genitiveNPCoord = specifier' <|> name
  where specifier' =
          [i|GenitiveNPCoord genitiveSpecifier genitiveN' genitiveTail|]
        name =
          [i|GenitiveNPCoordName properName genitiveTail|]

-- | A complemented verb. One of 'complVCopula', 'complVPDV',
-- 'complVDisV', 'complVPV', 'complVPV'', 'complVTV'.
complV =
  complVIV <|>
  complVPI <|>
  complVTV <|>
  complVPV <|>
  complVPV' <|>
  complVDisV <|>
  complVPDV <|>
  complVCopula

-- | A complemented copula: is valid
complVCopula =
  [i|ComplVCopula copula copulaCompl|]

-- | A distransitive phrasal verb: puts an error down to a customer
complVPDV =
  [i|ComplVPDV phrasalDistransitiveV compl phrasalParticle compl|]

-- | A distransitive complemented verb: gives a card to a customer
complVDisV =
  [i|ComplVDisV distransitiveV compl compl|]

-- | A complemented phrasal transitive verb: gives away a code
complVPV =
  [i|ComplVPV phrasalTransitiveV phrasalParticle compl|]

-- | A complemented phrasal transitive verb, flipped: gives a code away
complVPV' =
  [i|ComplVPV' phrasalTransitiveV compl phrasalParticle|]

-- | Complemented transitive verb: inserts a card
complVTV =
  [i|ComplVTV transitiveV compl|]

-- | A phrasal distransitive verb: puts an error down to a customer
phrasalDistransitiveV =
  [i|PhrasalDistransitiveV (join (fmap acePhrasalDistransitiveV getState))|]

-- | A phrasal transitive verb: give away a thing
phrasalTransitiveV =
  [i|PhrasalTransitiveV (join (fmap acePhrasalTransitiveV getState))|]

-- | Complemented non-copula verb, e.g. Mary sees him.
compl =
  try [i|ComplNP npCoord|] <|>
  [i|ComplPP pp|]

-- | An intransitive verb. Takes no complement. E.g. walks.
complVIV =
  [i|ComplVIV intransitiveV|]

-- | A phrasal intransitive verb with a complement, in this case a
-- particle: gets in, sits up.
complVPI =
  [i|ComplVPI phrasalIntransitiveV phrasalParticle|]

-- | A phrasal intransitive verb: gives, sits (e.g. gives up, sits
-- down). This is customized by 'acePhrasalIntransitiveV'.
phrasalIntransitiveV =
  [i|PhrasalIntransitiveV (join (fmap acePhrasalIntransitiveV getState))|]

-- | A phrasal verb particle, e.g. in, up, out (get in, get up, get
-- out). This is customized via 'acePhrasalParticle'.
phrasalParticle =
  [i|PhrasalParticle (join (fmap acePhrasalParticle getState))|]

-- | Either a graded adjective coordination (\"better than a duck and
-- faster than a mouse\"), or a noun phrase coordination (\"a goose
-- and an ocelot\"), or a prepositional phrase (\"to a bucket or a
-- kettle\").
copulaCompl = copulaComplAPC <|> copulaComplNPC <|> copulaComplPP
  where copulaComplAPC =
          [i|CopulaComplAPC apCoord|]
        copulaComplNPC =
          [i|CopulaComplNPC npCoord|]
        copulaComplPP = [i|CopulaComplPP pp|]

-- | A coordination of a graded adjective: \"better than a potato and
-- nicer than some bacon\"
apCoord = apCoordAnd <|> apCoord'
  where apCoordAnd = [i|APCoordAnd (try (apGrad <* string "and")) apCoord|]
        apCoord' = [i|APCoord apGrad|]

-- | A graded adjective. Either comparative adjective phrase (\"better
-- than a potato\"), or a simple adjective phrase (see 'ap').
apGrad = apGradThan <|> apGrad'
  where apGradThan =
          [i|APgradAPThan
               (try (ap <*
                     string "than"))
               npCoord|]
        apGrad' = [i|APgradAP ap|]

-- | An adjective phrase. Transitive (fond of Mary, interested in an
-- account) or intransitive (correct, green, valid).
ap =
  [i|APTrans transitiveAdjective pp|] <|>
  [i|APIntrans intransitiveAdjective|]

-- | Some intransitive verb: walks
intransitiveV =
  [i|IntransitiveV (join (fmap aceIntransitiveVerb getState))|]

-- | Some transitive verb: inserts
transitiveV =
  [i|TransitiveV (join (fmap aceTransitiveVerb getState))|]

-- | Some distransitive verb: inserts
distransitiveV =
  [i|DistransitiveV (join (fmap aceDistransitiveVerb getState))|]

-- | Adverb coordination: quickly and hastily and manually
adverbCoord =
  [i|AdverbCoord
       adverb
       (optional (try (string "and" *> adverbCoord)))|]

-- | Adverb: quickly
adverb =
  [i|Adverb (join (fmap aceAdverb getState))|]

-- | Adjective coordination: correct and green
adjectiveCoord =
  [i|AdjectiveCoord
       intransitiveAdjective
       (optional (try (string "and" *> adjectiveCoord)))|]

-- | Intransitive adjective: correct, green, valid
--
-- The actual parser for this is provided as
-- 'aceIntransitiveAdjective' in the parser configuration. You can
-- configure this.
intransitiveAdjective =
  [i|IntransitiveAdjective (join (fmap aceIntransitiveAdjective getState))|]

-- | Transitive adjective: correct, green, valid
transitiveAdjective =
  [i|TransitiveAdjective (join (fmap aceTransitiveAdjective getState))|]

-- | A determiner: the, an, not every, etc.
determiner =
  (string "the" *> pure The) <|>
  (string "an" *> pure An) <|>
  (string "a" *> pure A) <|>
  (string "some" *> pure Some) <|>
  (strings ["not","every"] *> pure NotEvery) <|>
  (strings ["not","each"] *> pure NotEach) <|>
  (strings ["not","all"] *> pure NotAll) <|>
  (string "no" *> pure No) <|>
  (string "every" *> pure EveryEach) <|>
  (string "each" *> pure EveryEach) <|>
  (string "all" *> pure All) <|>
  (string "which" *> pure Which)

-- | A number phrase: more than 5
numberP =
  [i|NumberP (optional (try generalizedQuantor)) number|]

-- | There is/are.
existentialGlobalQuantor =
  string "there" *>
  [i|ExistentialGlobalQuantor copula|]

-- | Is/are there?
existentialGlobalQuestionQuantor =
  [i|ExistentialGlobalQuestionQuantor copula|] <*
  string "there"

-- | Do/does.
aux =
  (string "do" *> pure Do) <|>
  (string "does" *> pure Does)

-- | Pronouns.
pronoun =
  (string "it" *> pure It) <|>
  (string "he" *> pure He) <|>
  (string "she" *> pure She) <|>
  (string "he/she" *> pure HeShe) <|>
  (string "him" *> pure Him) <|>
  (string "her" *> pure HerP) <|>
  (string "him/her" *> pure HimHer) <|>
  (string "they" *> pure They) <|>
  (string "them" *> pure Them) <|>
  (string "itself" *> pure Itself) <|>
  (string "himself" *> pure Himself) <|>
  (string "herself" *> pure Herself) <|>
  (string "himself/herself" *> pure HimselfHerself) <|>
  (string "themselves" *> pure Themselves) <|>
  (string "someone" *> pure Someone) <|>
  (string "somebody" *> pure Somebody) <|>
  (string "something" *> pure Something) <|>
  (string "no one" *> pure NoOne) <|>
  (string "nobody" *> pure Nobody) <|>
  (string "nothing" *> pure NoThing) <|>
  (string "everyone" *> pure Everyone) <|>
  (string "everybody" *> pure Everybody) <|>
  (string "everything" *> pure Everything) <|>
  (string "not everyone" *> pure NotEveryone) <|>
  (string "not everybody" *> pure NotEverybody) <|>
  (string "not everything" *> pure NotEverything) <|>
  (string "what" *> pure What) <|>
  (string "who" *> pure Who) <|>
  (string "whom" *> pure Whom) <|>
  (string "which" *> pure WhichP)

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
  his <|> her <|> hisHer <|> its
  where his = string "his" *> pure His
        her = string "her" *> pure Her
        hisHer =
          (string "his" <|> string "her" <|> string "his/her") *>
          pure HisHer
        its = string "its" *> pure Its

-- | A universal global quantor: for every/for each, for all.
universalGlobalQuantor =
  string "for" *> (everyEach <|> forAll)
  where everyEach = ((string "every" *> pure ForEvery) <|>
                     (string "each" *> pure ForEach))
        forAll = string "all" *> pure ForAll
