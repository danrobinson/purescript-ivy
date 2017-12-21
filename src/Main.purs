module Main where

import Prelude (Unit, ($), show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Parse (parseContract)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ parseContract """contract LockWithMultisig(pubKey1: PublicKey, pubKey2: PublicKey, pubKey3: PublicKey, val: Value) {
  clause spend(sig1: Signature, sig2: Signature) {
    verify checkMultiSig([pubKey1, pubKey2, pubKey3], [sig1, sig2])
    unlock val
  }
}"""

