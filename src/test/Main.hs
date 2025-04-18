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
    describe "Day" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InIso8601 Day))
    describe "UTCTime" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InIso8601 UTCTime))
  describe "InFixedBinary" do
    describe "Int" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Int))
    describe "Int8" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Int8))
    describe "Int16" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Int16))
    describe "Int32" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Int32))
    describe "Int64" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Int64))
    describe "Word" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Word))
    describe "Word8" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Word8))
    describe "Word16" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Word16))
    describe "Word32" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Word32))
    describe "Word64" do itMeetsSyntacticLaws (Proxy @(SyntacticClass.InFixedBinary Word64))
  describe "Int" do itMeetsSyntacticLaws (Proxy @Int)
  describe "Int8" do itMeetsSyntacticLaws (Proxy @Int8)
  describe "Int16" do itMeetsSyntacticLaws (Proxy @Int16)
  describe "Int32" do itMeetsSyntacticLaws (Proxy @Int32)
  describe "Int64" do itMeetsSyntacticLaws (Proxy @Int64)
  describe "Word" do itMeetsSyntacticLaws (Proxy @Word)
  describe "Word8" do itMeetsSyntacticLaws (Proxy @Word8)
  describe "Word16" do itMeetsSyntacticLaws (Proxy @Word16)
  describe "Word32" do itMeetsSyntacticLaws (Proxy @Word32)
  describe "Word64" do itMeetsSyntacticLaws (Proxy @Word64)
  describe "UUID" do itMeetsSyntacticLaws (Proxy @UUID)
  describe "Text" do itMeetsSyntacticLaws (Proxy @Text)
  describe "LazyText" do itMeetsSyntacticLaws (Proxy @LazyText.Text)
  describe "String" do itMeetsSyntacticLaws (Proxy @String)

itMeetsSyntacticLaws ::
  ( Syntactic value,
    Eq value,
    Show value,
    Arbitrary value
  ) =>
  Proxy value ->
  Spec
itMeetsSyntacticLaws valueProxy =
  SyntacticClass.properties valueProxy
    & traverse_ (uncurry prop)
