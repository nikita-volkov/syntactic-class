module Main (main) where

import qualified Data.Text.Lazy as LazyText
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
  describe "UUID" do
    itMeetsSyntacticLaws (Proxy @UUID)
  describe "Text" do
    itMeetsSyntacticLaws (Proxy @Text)
  describe "LazyText" do
    itMeetsSyntacticLaws (Proxy @LazyText.Text)

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
