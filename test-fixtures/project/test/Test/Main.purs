module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do

  Spec.describe "metropolis" do
    Spec.it "is a bird" do
      true `shouldEqual` false
    Spec.it "is a plane" do
      pure unit
    Spec.it "is superman" do
      delay $ Milliseconds 2000.0
      pure unit

  Spec.describe "gotham city" do
    Spec.it "is a dark night" do
      1 `shouldEqual` 2
    Spec.it "is time for revenge" do
      pure unit

