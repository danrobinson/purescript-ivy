module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Parse (parseExpression)


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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let con = parseExpression "1"
  log (show con)
