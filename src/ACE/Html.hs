{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-orphans #-}

-- | Instances for pretty printing as HTML.

module ACE.Html () where

import ACE.Types.Pretty
import ACE.Types.Syntax
import Data.Monoid hiding (All)
import Data.Text (pack)
import Data.Text.Lazy.Builder
import Text.Blaze
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

-- | Maybe pretty print if anything.
mtoMarkup :: ToMarkup a => Markup -> Maybe a -> Markup
mtoMarkup x = maybe "" ((x <>) . toMarkup)

-- | Maybe pretty print if anything. Other side.
toMarkupm :: ToMarkup a => Markup -> Maybe a -> Markup
toMarkupm x = maybe "" ((<> x) . toMarkup)

-- | Maybe pretty print if anything. Other side.
toMarkupmf :: ToMarkup a => Markup -> (Markup -> Markup) -> Maybe a -> Markup
toMarkupmf x f = maybe "" (f . (<> x) . toMarkup)

wrap cls inner = H.span ! A.title cls ! A.class_ ("ace-" <> cls) $ inner

instance ToMarkup Specification where
  toMarkup (Specification c mspec) =
    wrap "specification"
         (toMarkup c <> wrap "period" "." <> mtoMarkup " " mspec)

instance ToMarkup SentenceCoord where
  toMarkup (SentenceCoord c mcoord) =
    wrap "sentence-or" (toMarkup c <> mtoMarkup (wrap "sentence-op" " or ") mcoord)

instance ToMarkup SentenceCoord_1 where
  toMarkup (SentenceCoord_1 c mcoord) =
    wrap "sentence-and" (toMarkup c <> mtoMarkup (wrap "sentence-op" " and ") mcoord)

instance ToMarkup SentenceCoord_2 where
  toMarkup (SentenceCoord_2 c mcoord) =
    wrap "sentence-or" (toMarkup c <> mtoMarkup (wrap "sentence-op" " or ") mcoord)

instance ToMarkup SentenceCoord_3 where
  toMarkup (SentenceCoord_3 c mcoord) =
    wrap "sentence-and" (toMarkup c <> mtoMarkup (wrap "sentence-op" " and ") mcoord)

instance ToMarkup TopicalizedSentence where
  toMarkup t =
    case t of
      TopicalizedSentenceExistential t' c -> toMarkup t' <> mtoMarkup " " c
      TopicalizedSentenceUniversal u s -> toMarkup u <> " " <> toMarkup s
      TopicalizedSentenceComposite c -> toMarkup c

instance ToMarkup UniversalTopic where
  toMarkup (UniversalTopic q n) =
    wrap "universal-topic"
         (wrap "universal-quantor" (toMarkup q) <> " " <> toMarkup n)

instance ToMarkup CompositeSentence where
  toMarkup c =
    case c of
      CompositeSentenceCond s -> toMarkup s
      CompositeSentenceNeg s -> toMarkup s
      CompositeSentence s -> toMarkup s

instance ToMarkup ConditionalSentence where
  toMarkup (ConditionalSentence x y) =
    wrap "conditional"
         (wrap "if-if" "if " <> toMarkup x <> wrap "if-then" " then " <> toMarkup y)

instance ToMarkup NegatedSentence where
  toMarkup (NegatedSentence s) =
    wrap "negated-sentence"
         ("it is not the case that " <> toMarkup s)

instance ToMarkup Sentence where
  toMarkup (Sentence n v) =
    wrap "sentence" (toMarkup n <> " " <> toMarkup v)

instance ToMarkup ExistentialTopic where
  toMarkup (ExistentialTopic g np) =
    wrap "existential"
         (wrap "existential-quantor" (toMarkup g) <> " " <> toMarkup np)

instance ToMarkup NPCoord where
  toMarkup n =
    wrap "npcoord"
         (case n of
            NPCoordDistributed d u -> toMarkup d <> " " <> toMarkup u
            NPCoordUnmarked u -> toMarkup u)

instance ToMarkup UnmarkedNPCoord where
  toMarkup (UnmarkedNPCoord np mu) =
    toMarkup np <> mtoMarkup (wrap "unmarked-npcoord-and" " and ") mu

instance ToMarkup N' where
  toMarkup (N' mad n mappos mnp mrel) =
    wrap "n_"
         (toMarkupm " " mad <>
          toMarkup n <>
          mtoMarkup " " mappos <>
          mtoMarkup (wrap "of-pp" " of ") mnp <>
          mtoMarkup " " mrel)

instance ToMarkup NP where
  toMarkup (NP s n') =
    wrap "np"
         (toMarkup s <> " " <> toMarkup n')

instance ToMarkup N where
  toMarkup (N t) =
    wrap "n"
         (toMarkup t)

instance ToMarkup PP where
  toMarkup (PP p np) =
    wrap "pp"
         (toMarkup p <> " " <> toMarkup np)

instance ToMarkup Preposition where
  toMarkup (Preposition t) =
    wrap "preposition" (toMarkup t)

instance ToMarkup ApposCoord where
  toMarkup (ApposCoord a ma) =
    wrap "appos-coord" (toMarkup a <> mtoMarkup " " ma)

instance ToMarkup Apposition where
  toMarkup a =
    wrap "apposition"
         (case a of
            AppositionVar v -> toMarkup v
            AppositionQuote q -> toMarkup q)

instance ToMarkup Quotation where
  toMarkup (Quotation q) =
    wrap "quotation"
         ("\"" <> toMarkup q <> "\"")

instance ToMarkup Variable where
  toMarkup (Variable t) =
    wrap "variable" (toMarkup t)

instance ToMarkup RelativeClauseCoord where
  toMarkup (RelativeClauseCoord r me) =
    wrap "relative-clause-coord"
         (toMarkup r <>
          case me of
            Nothing -> ""
            Just (c,r') -> " " <> toMarkup c <> " " <> toMarkup r')

instance ToMarkup PossessiveNPCoord where
  toMarkup p =
    wrap "possessive-np-coord"
         (case p of
           PossessiveNPCoordGen g -> toMarkup g
           PossessiveNPCoordPronoun p' -> toMarkup p')

instance ToMarkup GenitiveNPCoord where
  toMarkup g =
    case g of
      GenitiveNPCoord s n t -> toMarkup s <> " " <> toMarkup n <> toMarkup t
      GenitiveNPCoordName n t -> toMarkup n <> toMarkup t

instance ToMarkup ProperName where
  toMarkup (ProperName t) =
    wrap "proper-name" (toMarkup t)

instance ToMarkup PossessivePronounCoord where
  toMarkup (PossessivePronounCoord p mp) =
    wrap "possessive-pronoun-coord"
         (toMarkup p <> mtoMarkup (wrap "possessive-pronoun-and" " and ") mp)

instance ToMarkup GenitiveTail where
  toMarkup g =
    wrap "genitive-tail"
         (case g of
            GenitiveTailSaxonTail t -> toMarkup t
            GenitiveTailCoordtail t -> toMarkup t)

instance ToMarkup GenitiveCoordTail where
  toMarkup (GenitiveCoordTail t) =
    (wrap "genitive-and" " and " <> toMarkup t)

instance ToMarkup SaxonGenitiveTail where
  toMarkup (SaxonGenitiveTail m mg) =
    wrap "genitive-tail"
         (toMarkup m <>
          case mg of
            Nothing -> ""
            Just (c,r) -> toMarkup c <> " " <> toMarkup r)

instance ToMarkup RelativeClause where
  toMarkup r =
    wrap "relative-clause"
         (case r of
            RelativeClauseThat v       -> wrap "relative-clause-that" "that " <> toMarkup v
            RelativeClauseNP a b       -> toMarkup a <> " " <> toMarkup b
            RelativeClauseThatNPVP a b -> wrap "relative-clause-that" "that " <> toMarkup a <> " " <> toMarkup b
            RelativeClauseNPVP a b c   -> toMarkup a <> " " <> toMarkup b <> " " <> toMarkup c
            RelativeClausePP p n v     -> toMarkup p <> " " <> toMarkup n <> " " <> toMarkup v)

instance ToMarkup VPCoord where
  toMarkup v =
    wrap "vp-coord"
         (case v of
            VPCoord' vp coord vpcoord -> toMarkup vp <> " " <> toMarkup coord <> " " <> toMarkup vpcoord
            VPCoordVP vp -> toMarkup vp)

instance ToMarkup GenitiveSpecifier where
  toMarkup g =
    wrap "genitive-specifier"
         (case g of
            GenitiveSpecifierD d -> toMarkup d
            GenitiveSpecifierPPC p -> toMarkup p
            GenitiveSpecifierN i -> toMarkup (pack (show i)))

instance ToMarkup GenitiveN' where
  toMarkup (GenitiveN' ma n mac) =
    wrap "genitive-n_"
         (toMarkupm " " ma <> toMarkup n <> mtoMarkup " " mac)

instance ToMarkup VP where
  toMarkup v =
    wrap "vp"
         (case v of
            VP v' -> toMarkup v'
            VPNeg cop v' -> toMarkup cop <> wrap "vp-not" " not " <> toMarkup v')

instance ToMarkup V' where
  toMarkup (V' madverb compl mo) =
    wrap "v_"
         (toMarkupm " " madverb <>
          toMarkup compl <>
          mconcat (map ((" " <>) . toMarkup) mo))

instance ToMarkup AdverbCoord where
  toMarkup (AdverbCoord ad mad) =
    wrap "adverb-coord"
         (toMarkup ad <> mtoMarkup (wrap "adverb-and" " and ") mad)

instance ToMarkup ComplV where
  toMarkup c =
    wrap "compl-v"
         (case c of
            ComplVIV i -> toMarkup i
            ComplVPI pi' pp -> toMarkup pi' <> " " <> toMarkup pp
            ComplVTV tv compl -> toMarkup tv <> " " <> toMarkup compl
            ComplVPV pt pp compl -> toMarkup pt <> " " <> toMarkup pp <> " " <> toMarkup compl
            ComplVPV' pt compl pp -> toMarkup pt <> " " <> toMarkup compl <> " " <> toMarkup pp
            ComplVDisV dis compl compl' -> toMarkup dis <> " " <> toMarkup compl <> " " <> toMarkup compl'
            ComplVPDV pd compl pp compl' -> toMarkup pd <> " " <> toMarkup compl <> " " <> toMarkup pp <> " " <> toMarkup compl'
            ComplVCopula cop copcomp -> toMarkup cop <> " " <> toMarkup copcomp)

instance ToMarkup PhrasalTransitiveV where
  toMarkup (PhrasalTransitiveV t) =
    wrap "phrasal-transitive-v"
         (toMarkup t)

instance ToMarkup PhrasalDistransitiveV where
  toMarkup (PhrasalDistransitiveV t) =
    wrap "phrasal-distransitive-v"
         (toMarkup t)

instance ToMarkup CopulaCompl where
  toMarkup c =
    wrap "copula-compl"
         (case c of
            CopulaComplAPC apc -> toMarkup apc
            CopulaComplNPC npc -> toMarkup npc
            CopulaComplPP pp -> toMarkup pp)

instance ToMarkup APCoord where
  toMarkup a =
    case a of
      APCoordAnd x y -> toMarkup x <> " and " <> toMarkup y
      APCoord a' -> toMarkup a'

instance ToMarkup APgrad where
  toMarkup a =
    case a of
      APgradAPThan x y -> toMarkup x <> " than " <> toMarkup y
      APgradAP a' -> toMarkup a'

instance ToMarkup AP where
  toMarkup a =
    case a of
      APIntrans i -> toMarkup i
      APTrans aj pp -> toMarkup aj <> " "  <> toMarkup pp

instance ToMarkup TransitiveAdjective where
  toMarkup (TransitiveAdjective t) = toMarkup t

instance ToMarkup Compl where
  toMarkup c =
    case c of
      ComplNP np -> toMarkup np
      ComplPP pp -> toMarkup pp

instance ToMarkup PhrasalIntransitiveV where
  toMarkup (PhrasalIntransitiveV t) =
    wrap "phrasal-intransitive-v"
         (toMarkup t)

instance ToMarkup PhrasalParticle where
  toMarkup (PhrasalParticle t) =
    wrap "phrasal-particle"
         (toMarkup t)

instance ToMarkup IntransitiveV where
  toMarkup (IntransitiveV v) =
    wrap "intransitive-v"
         (toMarkup v)

instance ToMarkup TransitiveV where
  toMarkup (TransitiveV t) =
    wrap "transitive-v"
         (toMarkup t)

instance ToMarkup DistransitiveV where
  toMarkup (DistransitiveV t) =
    wrap "distransitive-v" (toMarkup t)

instance ToMarkup IntransitiveAdjective where
  toMarkup (IntransitiveAdjective t) =
    wrap "intransitive-adjective"
         (toMarkup t)

instance ToMarkup VModifier where
  toMarkup v =
    wrap "v-modifier" (case v of
                         VModifierVC adv -> toMarkup adv
                         VModifierPP pp -> toMarkup pp
                         VModifierAVPP x -> toMarkup x)

instance ToMarkup AdverbialPP where
  toMarkup (AdverbialPP pp ac) =
    toMarkup pp <> " " <> toMarkup ac

instance ToMarkup Adverb where
  toMarkup (Adverb a) =
    wrap "adverb" (toMarkup a)

instance ToMarkup Specifier where
  toMarkup s =
    wrap "specifier"
         (case s of
            SpecifyDeterminer d -> toMarkup d
            SpecifyPossessive np -> toMarkup np
            SpecifyNumberP n -> toMarkup n)

instance ToMarkup AdjectiveCoord where
  toMarkup (AdjectiveCoord i ma) =
    wrap "adjective-coord" (toMarkup i <> mtoMarkup " and " ma)

instance ToMarkup NumberP where
  toMarkup (NumberP mq i) =
    wrap "number-p"
         (toMarkupm " " mq <> toMarkup (pack (show i)))

instance ToMarkup ExistentialGlobalQuantor where
  toMarkup (ExistentialGlobalQuantor c) =
    wrap "existential-quantor"
         ("there " <> toMarkup c)

instance ToMarkup ExistentialGlobalQuestionQuantor where
  toMarkup (ExistentialGlobalQuestionQuantor c) =
    wrap "existential-question-quantor"
         (toMarkup c <> " there")

instance ToMarkup Aux where
  toMarkup d =
    wrap "aux"
         (case d of
            Do -> "do"
            Does -> "does")

instance ToMarkup Coord where
  toMarkup c =
    wrap "coord"
         (case c of
            And -> "and"
            Or -> "or")

instance ToMarkup Copula where
  toMarkup c =
    wrap "copula"
         (case c of
            Is -> "is"
            Are -> "are")

instance ToMarkup Determiner where
  toMarkup d =
    wrap "determiner"
         (case d of
            The -> "the"
            A -> "a"
            An -> "an"
            Some -> "some"
            No -> "no"
            EveryEach -> "every/each"
            All -> "all"
            NotEvery -> "not every"
            NotEach -> "not each"
            NotAll -> "not all"
            Which -> "which")

instance ToMarkup DistributiveGlobalQuantor where
  toMarkup ForEachOf =
    wrap "distributive-global-quantor" "for each of"

instance ToMarkup DistributiveMarker where
  toMarkup EachOf =
    wrap "distributive-marker" "each of"

instance ToMarkup GeneralizedQuantor where
  toMarkup g =
    wrap "generalized-quantor"
         (case g of
            AtMost -> "at most"
            AtLeast -> "at least"
            MoreThan -> "more than"
            LessThan -> "less than"
            NotMoreThan -> "not more than"
            NotLessThan -> "not less than")

instance ToMarkup PossessivePronoun where
  toMarkup p =
    wrap "possessive-pronoun"
         (case p of
            His -> "his"
            Her -> "her"
            HisHer -> "his/her"
            Its -> "its"
            Their -> "their"
            HisHerOwn -> "his own/her own"
            ItsOwn -> "its own"
            TheirOwn -> "their own"
            Whose -> "whose")

instance ToMarkup Pronoun where
  toMarkup p =
    wrap "pronoun"
         (case p of
            It                   -> "it"
            HeShe                -> "he/she"
            Himher               -> "him/her"
            They                 -> "they"
            Them                 -> "them"
            Itself               -> "itself"
            HimHerSelf           -> "himself/herself"
            Themselves           -> "themselves"
            SomeoneSomebody      -> "someone/somebody"
            Something            -> "something"
            NoOneNobody          -> "no-one/nobody"
            NoThing              -> "nothing"
            EveryoneEverybody    -> "everyone/everybody"
            Everything           -> "everything"
            NotEveryoneEverybody -> "not everyone/not everybody"
            NotEverything        -> "not everything"
            WhatWho              -> "what/who"
            Whom                 -> "whom"
            WhichWho             -> "which/who")

instance ToMarkup SaxonGenitiveMarker where
  toMarkup a =
    wrap "saxon-genitive-marker"
         (case a of
            Apostrophe -> "'"
            ApostropheS -> "'s")

instance ToMarkup UniversalGlobalQuantor where
  toMarkup u =
    wrap "universal-quantor"
         (case u of
            ForEvery -> "for every"
            ForEach -> "for each"
            ForAll -> "for all")
