module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.List (fromFoldable)
import Data.Either (Either(Right))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Parse (parseContract, Contract(Contract), Clause(Clause), Parameter(Parameter), Type(PublicKey, Value, Signature), Statement(Verify, Unlock), 
              Expr(Call, Variable))

lockWithPublicKey :: Contract
lockWithPublicKey = let
    params = (fromFoldable [ Parameter "pubKey" PublicKey, Parameter "val" Value])
    clauseParams = fromFoldable [Parameter "sig" Signature]
    verify = Verify $ Call "checkTxSig" (fromFoldable [Variable "pubKey", Variable "sig"])
    unlockStatement = Unlock $ Variable "val"
    spendClause = Clause "spend" clauseParams (fromFoldable [verify, unlockStatement])
    statements = (fromFoldable [spendClause])
  in
    Contract "LockWithPublicKey" params statements

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "parseContract" $ do
    it "parses LockWithPublicKey" $
      parseContract "contract LockWithPublicKey(pubKey: PublicKey, val: Value) {\n\
      \  clause spend(sig: Signature) { \n\
      \    verify checkTxSig(pubKey, sig) \n\
      \    unlock val\n\
      \  } \n\
      \}" `shouldEqual` Right lockWithPublicKey
