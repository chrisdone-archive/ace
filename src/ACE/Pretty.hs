{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-orphans #-}

-- | Pretty printing classes.

module ACE.Pretty
  (module ACE.Types.Pretty)
  where

import ACE.Types.Pretty
import ACE.Types.Syntax
import Data.Monoid hiding (All)
import Data.Text (pack)
import Data.Text.Lazy.Builder

-- | Maybe pretty print if anything.
mpretty :: Pretty a => Builder -> Maybe a -> Builder
mpretty x = maybe "" ((x <>) . pretty)

-- | Maybe pretty print if anything. Other side.
prettym :: Pretty a => Builder -> Maybe a -> Builder
prettym x = maybe "" ((<> x) . pretty)

instance Pretty Specification where
  pretty (Specification c mspec) =
    pretty c <> "." <> mpretty " " mspec

instance Pretty SentenceCoord where
  pretty (SentenceCoord c mcoord) =
    pretty c <> mpretty " or " mcoord

instance Pretty SentenceCoord_1 where
  pretty (SentenceCoord_1 c mcoord) =
    pretty c <> mpretty " and " mcoord

instance Pretty SentenceCoord_2 where
  pretty (SentenceCoord_2 c mcoord) =
    pretty c <> mpretty " or " mcoord

instance Pretty SentenceCoord_3 where
  pretty (SentenceCoord_3 c mcoord) =
    pretty c <> mpretty " and " mcoord

instance Pretty TopicalizedSentence where
  pretty t =
    case t of
      TopicalizedSentenceExistential t' c -> pretty t' <> mpretty " " c
      TopicalizedSentenceUniversal u s   -> pretty u <> " " <> pretty s
      TopicalizedSentenceComposite c     -> pretty c

instance Pretty UniversalTopic where
  pretty (UniversalTopic q n) = pretty q <> " " <> pretty n

instance Pretty CompositeSentence where
  pretty c =
    case c of
      CompositeSentenceCond s -> pretty s
      CompositeSentenceNeg s -> pretty s
      CompositeSentence s -> pretty s

instance Pretty ConditionalSentence where
  pretty (ConditionalSentence x y) =
    "if " <> pretty x <> " then " <> pretty y

instance Pretty NegatedSentence where
  pretty (NegatedSentence s) = "it is not the case that " <> pretty s

instance Pretty Sentence where
  pretty (Sentence n v) =
    pretty n <> " " <> pretty v

instance Pretty ExistentialTopic where
  pretty (ExistentialTopic g np) =
    pretty g <> " " <> pretty np

instance Pretty NPCoord where
  pretty n =
    case n of
      NPCoordDistributed d u -> pretty d <> " " <> pretty u
      NPCoordUnmarked u -> pretty u

instance Pretty UnmarkedNPCoord where
  pretty (UnmarkedNPCoord np mu) =
    pretty np <> mpretty " and " mu

instance Pretty N' where
  pretty (N' mad n mappos mnp mrel) =
    prettym " " mad <>
    pretty n <>
    mpretty " " mappos <>
    mpretty " of " mnp <>
    mpretty " " mrel

instance Pretty NP where
  pretty x =
    case x of
      NP s n' -> pretty s <> " " <> pretty n'
      NPPro p -> pretty p
      NPProper pn -> pretty pn
      NPVar v -> pretty v

instance Pretty N where
  pretty (N t) = fromText t

instance Pretty PP where
  pretty (PP p np) = pretty p <> " " <> pretty np

instance Pretty Preposition where
  pretty (Preposition t) = fromText t

instance Pretty ApposCoord where
  pretty (ApposCoord a ma) = pretty a <> mpretty " " ma

instance Pretty Apposition where
  pretty a =
    case a of
      AppositionVar v -> pretty v
      AppositionQuote q -> pretty q

instance Pretty Quotation where
  pretty (Quotation q) =
    "\"" <> fromText q <> "\""

instance Pretty Variable where
  pretty (Variable t) = fromText t

instance Pretty RelativeClauseCoord where
  pretty (RelativeClauseCoord r me) =
    pretty r <>
    case me of
      Nothing -> ""
      Just (c,r') -> " " <> pretty c <> " " <> pretty r'

instance Pretty PossessiveNPCoord where
  pretty p =
    case p of
      PossessiveNPCoordGen g -> pretty g
      PossessiveNPCoordPronoun p' -> pretty p'

instance Pretty GenitiveNPCoord where
  pretty g =
    case g of
      GenitiveNPCoord s n t -> pretty s <> " " <> pretty n <> pretty t
      GenitiveNPCoordName n t -> pretty n <> pretty t

instance Pretty ProperName where
  pretty (ProperName t) = fromText t

instance Pretty PossessivePronounCoord where
  pretty (PossessivePronounCoord p mp) = pretty p <> mpretty " and " mp

instance Pretty GenitiveTail where
  pretty g = case g of
               GenitiveTailSaxonTail t -> pretty t
               GenitiveTailCoordtail t -> pretty t

instance Pretty GenitiveCoordTail where
  pretty (GenitiveCoordTail t) = " and " <> pretty t

instance Pretty SaxonGenitiveTail where
  pretty (SaxonGenitiveTail m mg) =
    pretty m <>
    case mg of
      Nothing -> ""
      Just (c,r) -> pretty c <> " " <> pretty r

instance Pretty RelativeClause where
  pretty r =
    case r of
      RelativeClauseThat v       -> "that " <> pretty v
      RelativeClauseNP a b       -> pretty a <> " " <> pretty b
      RelativeClauseThatNPVP a b -> "that " <> pretty a <> " " <> pretty b
      RelativeClauseNPVP a b c   -> pretty a <> " " <> pretty b <> " " <> pretty c
      RelativeClausePP p n v     -> pretty p <> " " <> pretty n <> " " <> pretty v

instance Pretty VPCoord where
  pretty v =
    case v of
      VPCoord' vp coord vpcoord -> pretty vp <> " " <> pretty coord <> " " <> pretty vpcoord
      VPCoordVP vp -> pretty vp

instance Pretty GenitiveSpecifier where
  pretty g =
    case g of
      GenitiveSpecifierD d -> pretty d
      GenitiveSpecifierPPC p -> pretty p
      GenitiveSpecifierN i -> fromText (pack (show i))

instance Pretty GenitiveN' where
  pretty (GenitiveN' ma n mac) =
    prettym " " ma <> pretty n <> mpretty " " mac

instance Pretty VP where
  pretty v =
    case v of
      VP v' -> pretty v'
      VPNeg cop v' -> pretty cop <> " not " <> pretty v'

instance Pretty V' where
  pretty (V' madverb compl mo) =
    prettym " " madverb <>
    pretty compl <>
    mconcat (map ((" " <>) . pretty) mo)

instance Pretty AdverbCoord where
  pretty (AdverbCoord ad mad) = pretty ad <> mpretty " and " mad

instance Pretty ComplV where
  pretty c =
    case c of
      ComplVIV i -> pretty i
      ComplVPI pi' pp -> pretty pi' <> " " <> pretty pp
      ComplVTV tv compl -> pretty tv <> " " <> pretty compl
      ComplVPV pt pp compl -> pretty pt <> " " <> pretty pp <> " " <> pretty compl
      ComplVPV' pt compl pp -> pretty pt <> " " <> pretty compl <> " " <> pretty pp
      ComplVDisV dis compl compl' -> pretty dis <> " " <> pretty compl <> " " <> pretty compl'
      ComplVPDV pd compl pp compl' -> pretty pd <> " " <> pretty compl <> " " <> pretty pp <> " " <> pretty compl'
      ComplVCopula cop copcomp -> pretty cop <> " " <> pretty copcomp

instance Pretty PhrasalTransitiveV where
  pretty (PhrasalTransitiveV t) = fromText t

instance Pretty PhrasalDistransitiveV where
  pretty (PhrasalDistransitiveV t) = fromText t

instance Pretty CopulaCompl where
  pretty c =
    case c of
      CopulaComplAPC apc -> pretty apc
      CopulaComplNPC npc -> pretty npc
      CopulaComplPP pp -> pretty pp

instance Pretty APCoord where
  pretty a =
    case a of
      APCoordAnd x y -> pretty x <> " and " <> pretty y
      APCoord a' -> pretty a'

instance Pretty APgrad where
  pretty a =
    case a of
      APgradAPThan x y -> pretty x <> " than " <> pretty y
      APgradAP a' -> pretty a'

instance Pretty AP where
  pretty a =
    case a of
      APIntrans i -> pretty i
      APTrans aj pp -> pretty aj <> " "  <> pretty pp

instance Pretty TransitiveAdjective where
  pretty (TransitiveAdjective t) = fromText t

instance Pretty Compl where
  pretty c =
    case c of
      ComplNP np -> pretty np
      ComplPP pp -> pretty pp

instance Pretty PhrasalIntransitiveV where
  pretty (PhrasalIntransitiveV t) = fromText t

instance Pretty PhrasalParticle where
  pretty (PhrasalParticle t) = fromText t

instance Pretty IntransitiveV where
  pretty (IntransitiveV v) = fromText v

instance Pretty TransitiveV where
  pretty (TransitiveV t) = fromText t

instance Pretty DistransitiveV where
  pretty (DistransitiveV t) = fromText t

instance Pretty IntransitiveAdjective where
  pretty (IntransitiveAdjective t) = fromText t

instance Pretty VModifier where
  pretty v =
    case v of
      VModifierVC adv -> pretty adv
      VModifierPP pp -> pretty pp
      VModifierAVPP x -> pretty x

instance Pretty AdverbialPP where
  pretty (AdverbialPP pp ac) =
    pretty pp <> " " <> pretty ac

instance Pretty Adverb where
  pretty (Adverb a) = fromText a

instance Pretty Specifier where
  pretty s =
    case s of
      SpecifyDeterminer d -> pretty d
      SpecifyPossessive np -> pretty np
      SpecifyNumberP n -> pretty n

instance Pretty AdjectiveCoord where
  pretty (AdjectiveCoord i ma) =
    pretty i <> mpretty " and " ma

instance Pretty NumberP where
  pretty (NumberP mq i) =
    prettym " " mq <> fromText (pack (show i))

instance Pretty ExistentialGlobalQuantor where
  pretty (ExistentialGlobalQuantor c) = "there " <> pretty c

instance Pretty ExistentialGlobalQuestionQuantor where
  pretty (ExistentialGlobalQuestionQuantor c) = pretty c <> " there"

instance Pretty Aux where
  pretty d =
    case d of
      Do -> "do"
      Does -> "does"

instance Pretty Coord where
  pretty c =
    case c of
      And -> "and"
      Or -> "or"

instance Pretty Copula where
  pretty c =
    case c of
      Is -> "is"
      Are -> "are"

instance Pretty Determiner where
  pretty d =
    case d of
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
      Which -> "which"

instance Pretty DistributiveGlobalQuantor where
  pretty ForEachOf = "for each of"

instance Pretty DistributiveMarker where
  pretty EachOf = "each of"

instance Pretty GeneralizedQuantor where
  pretty g =
    case g of
      AtMost -> "at most"
      AtLeast -> "at least"
      MoreThan -> "more than"
      LessThan -> "less than"
      NotMoreThan -> "not more than"
      NotLessThan -> "not less than"

instance Pretty PossessivePronoun where
  pretty p =
    case p of
      His -> "his"
      Her -> "her"
      HisHer -> "his/her"
      Its -> "its"
      Their -> "their"
      HisHerOwn -> "his own/her own"
      ItsOwn -> "its own"
      TheirOwn -> "their own"
      Whose -> "whose"

instance Pretty Pronoun where
  pretty p =
    case p of
      It             -> "it"
      He             -> "he"
      She            -> "she"
      HeShe          -> "he/she"
      Him            -> "him"
      HerP           -> "her"
      HimHer         -> "him/her"
      They           -> "they"
      Them           -> "them"
      Itself         -> "itself"
      Himself        -> "himself"
      Herself        -> "herself"
      HimselfHerself -> "himself/herself"
      Themselves     -> "themselves"
      Someone        -> "someone"
      Somebody       -> "somebody"
      Something      -> "something"
      NoOne          -> "no one"
      Nobody         -> "nobody"
      NoThing        -> "nothing"
      Everyone       -> "everyone"
      Everybody      -> "everybody"
      Everything     -> "everything"
      NotEveryone    -> "not everyone"
      NotEverybody   -> "not everybody"
      NotEverything  -> "not everything"
      What           -> "what"
      Who            -> "who"
      Whom           -> "whom"
      WhichP         -> "which"

instance Pretty SaxonGenitiveMarker where
  pretty a =
    case a of
      Apostrophe -> "'"
      ApostropheS -> "'s"

instance Pretty UniversalGlobalQuantor where
  pretty u =
    case u of
      ForEvery -> "for every"
      ForEach -> "for each"
      ForAll -> "for all"
