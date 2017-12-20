module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Either (Either(Right))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Parse (parseContract)

-- lockWithPublicKey :: Contract
-- lockWithPublicKey = let
--     params = (fromFoldable [ Parameter "pubKey" PublicKey, Parameter "val" Value])
--     clauseParams = fromFoldable [Parameter "sig" Signature]
--     verify = Verify $ Call "checkTxSig" (fromFoldable [Variable "pubKey", Variable "sig"])
--     unlockStatement = Unlock $ Variable "val"
--     spendClause = Clause "spend" clauseParams (fromFoldable [verify, unlockStatement])
--     statements = (fromFoldable [spendClause])
--   in
--     Contract "LockWithPublicKey" params statements

lockWithPublicKey :: String
lockWithPublicKey = "contract LockWithPublicKey(pubKey: PublicKey, val: Value) {\n\
      \  clause spend(sig: Signature) {\n\
      \    verify checkTxSig(pubKey, sig)\n\
      \    unlock val\n\
      \  }\n\
      \}"

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "parseContract" $ do
    it "parses LockWithPublicKey" $
      map show (parseContract lockWithPublicKey) `shouldEqual` Right lockWithPublicKey
