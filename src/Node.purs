module Test.Spec.Runner.Node where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Process (exit')
import Test.Spec (Spec)
import Test.Spec.Result (Result)
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner as Spec
import Test.Spec.Runner.Node.Config as Cfg
import Test.Spec.Runner.Node.Persist as Persist
import Test.Spec.Summary (successful)
import Test.Spec.Tree (Tree)

runSpecAndExitProcess :: Array Reporter -> Spec Unit -> Effect Unit
runSpecAndExitProcess reporters spec = do
  config <- Cfg.fromCommandLine
  runSpecAndExitProcess' config reporters spec

runSpecAndExitProcess' :: ∀ c. Cfg.TestRunConfig' c -> Array Reporter -> Spec Unit -> Effect Unit
runSpecAndExitProcess' config reporters spec = launchAff_ do
  res <- runSpecAndGetResults config reporters spec
  liftEffect $ exit' $ if successful res then 0 else 1

runSpecAndGetResults :: ∀ c. Cfg.TestRunConfig' c -> Array Reporter -> Spec Unit -> Aff (Array (Tree String Void Result))
runSpecAndGetResults config reporters spec = do
  specCfg <- Cfg.toSpecConfig config <#> _ { exit = false }
  results <- un Identity $ Spec.runSpecT specCfg reporters spec
  Persist.persistResults results
  pure results
