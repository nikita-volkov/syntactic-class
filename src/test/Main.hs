module Main (main) where

import SyntacticClass (Syntactic)
import qualified SyntacticClass
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main = hspec do
  describe "InIso8601" do
    describe "UTCTime" do
      itMeetsSyntacticLaws (Proxy @(SyntacticClass.InIso8601 UTCTime))

itMeetsSyntacticLaws ::
  ( Syntactic value,
    Eq value,
    Show value,
    Arbitrary value
  ) =>
  Proxy value ->
  Spec
itMeetsSyntacticLaws valueProxy =
  SyntacticClass.syntacticProperties valueProxy
    & traverse_ (uncurry prop)
