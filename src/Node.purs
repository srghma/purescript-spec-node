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

-- | Runs the given spec, using configuration derived from CLI options (if any),
-- | and exits the process with an exit indicating success or failure.
runSpecAndExitProcess :: Array Reporter -> Spec Unit -> Effect Unit
runSpecAndExitProcess =
  runSpecAndExitProcess' { defaultConfig: Cfg.defaultConfig, parseCLIOptions: true }

-- | Runs the given spec and exits the process with an exit code indicating
-- | success or failure.
-- |
-- | The `parseCLIOptions` parameter determines whether the `defaultConfig`
-- | should be used as is or CLI options (if any provided) should be applied on
-- | top of it.
runSpecAndExitProcess' :: ∀ c.
  { defaultConfig :: Cfg.TestRunConfig' c
  , parseCLIOptions :: Boolean
  }
  -> Array Reporter
  -> Spec Unit
  -> Effect Unit
runSpecAndExitProcess' args reporters spec = launchAff_ do
  config <-
    if args.parseCLIOptions then
      Cfg.fromCommandLine' args.defaultConfig Cfg.commandLineOptionParsers
    else
      pure args.defaultConfig
  res <- runSpecAndGetResults config reporters spec
  liftEffect $ exit' $ if successful res then 0 else 1

runSpecAndGetResults :: ∀ c. Cfg.TestRunConfig' c -> Array Reporter -> Spec Unit -> Aff (Array (Tree String Void Result))
runSpecAndGetResults config reporters spec = do
  specCfg <- Cfg.toSpecConfig config <#> _ { exit = false }
  results <- un Identity $ Spec.evalSpecT specCfg reporters spec
  Persist.persistResults results
  pure results
