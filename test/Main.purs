module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
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
lockWithPublicKey = """contract LockWithPublicKey(pubKey: PublicKey, val: Value) {
  clause spend(sig: Signature) {
    verify checkTxSig(pubKey, sig)
    unlock val
  }
}"""

lockWithMultiSig :: String
lockWithMultiSig = """contract LockWithMultisig(pubKey1: PublicKey, pubKey2: PublicKey, pubKey3: PublicKey, val: Value) {
  clause spend(sig1: Signature, sig2: Signature) {
    verify checkMultiSig([pubKey1, pubKey2, pubKey3], [sig1, sig2])
    unlock val
  }
}"""

lockWithPublicKeyHash :: String
lockWithPublicKeyHash = """contract LockWithPublicKeyHash(pubKeyHash: Sha256(PublicKey), val: Value) {
  clause spend(pubKey: PublicKey, sig: Signature) {
    verify sha256(pubKey) == pubKeyHash
    verify checkSig(pubKey, sig)
    unlock val
  }
}"""

revealPreimage :: String
revealPreimage = """contract RevealPreimage(hash: Sha256(Bytes), val: Value) {
  clause reveal(string: Bytes) {
    verify sha256(string) == hash
    unlock val
  }
}"""

revealCollision :: String
revealCollision = """contract RevealCollision(val: Value) {
  clause reveal(string1: Bytes, string2: Bytes) {
    verify string1 != string2
    verify sha1(string1) == sha1(string2)
    unlock val
  }
}"""

lockUntil :: String
lockUntil = """contract LockUntil(publicKey: PublicKey, time: Time, val: Value) {
  clause spend(sig: Signature) {
    verify checkSig(publicKey, sig)
    verify after(time)
    unlock val
  }
}"""

lockDelay :: String
lockDelay = """contract LockDelay(publicKey: PublicKey, delay: Duration, val: Value) {
  clause spend(sig: Signature) {
    verify checkSig(publicKey, sig)
    verify older(delay)
    unlock val
  }
}"""

transferWithTimeout :: String
transferWithTimeout = """contract TransferWithTimeout(sender: PublicKey, recipient: PublicKey, timeout: Time, val: Value) {
  clause transfer(senderSig: Signature, recipientSig: Signature) {
    verify checkSig(sender, senderSig)
    verify checkSig(recipient, recipientSig)
    unlock val
  }
  clause timeout(senderSig: Signature) {
    verify checkSig(sender, senderSig)
    verify after(timeout)
    unlock val
  }
}"""

escrowWithDelay :: String
escrowWithDelay = """contract EscrowWithDelay(sender: PublicKey, recipient: PublicKey, escrow: PublicKey, delay: Duration, val: Value) {
  clause transfer(sig1: Signature, sig2: Signature) {
    verify checkMultiSig([sender, recipient, escrow], [sig1, sig2])
    unlock val
  }
  clause timeout(sig: Signature) {
    verify checkSig(sender, sig)
    verify older(delay)
    unlock val
  }
}"""

vaultSpend :: String
vaultSpend = """contract VaultSpend(hotKey: PublicKey, coldKey: PublicKey, delay: Duration, val: Value) {
  clause cancel(sig: Signature) {
    verify checkSig(coldKey, sig)
    unlock val
  }
  clause complete(sig: Signature) {
    verify older(delay)
    verify checkSig(hotKey, sig)
    unlock val
  }
}"""

revealFixedPoint :: String
revealFixedPoint = """contract RevealFixedPoint(val: Value) {
  clause reveal(hash: Bytes) {
    verify bytes(sha256(hash)) == hash
    unlock val
  }
}"""


testContract :: String -> Aff (RunnerEffects ()) Unit
testContract c = map show (parseContract c) `shouldEqual` Right c

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "parseContract" $ do
    it "parses LockWithPublicKey" $
      testContract lockWithPublicKey
    it "parses LockWithMultiSig" $
      testContract lockWithMultiSig
    it "parses LockWithPublicKeyHash" $
      testContract lockWithPublicKeyHash
    it "parses RevealPreimage" $
      testContract revealPreimage
    it "parses RevealCollision" $
      testContract revealCollision
    it "parses LockUntil" $
      testContract lockUntil
    it "parses LockDelay" $
      testContract lockDelay
    it "parses TransferWithTimeout" $
      testContract transferWithTimeout
    it "parses EscrowWithDelay" $
      testContract escrowWithDelay
    it "parses VaultSpend" $
      testContract vaultSpend
    it "parses RevealFixedPoint" $
      testContract revealFixedPoint
    
