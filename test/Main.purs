module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Data.Either (Either(Left, Right))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Parse (parseContract)
import Compile (compile)

lockWithPublicKey :: String
lockWithPublicKey = """contract LockWithPublicKey(pubKey: PublicKey, val: Value) {
  clause spend(sig: Signature) {
    verify checkSig(pubKey, sig)
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

revealFixedPointWithComment :: String
revealFixedPointWithComment = """contract RevealFixedPoint(val: Value) {
  clause reveal(hash: Bytes) {
    verify bytes(sha256(hash)) == hash
    unlock val // test that comments work
  }
}"""


testParseContract :: String -> Aff (RunnerEffects ()) Unit
testParseContract source =
  case parseContract source of
    Left parseError -> fail (show parseError)
    Right contract -> pure unit

testCompileContract :: String -> Aff (RunnerEffects ()) Unit
testCompileContract source =
  case compile source of
    Left compileError -> fail (show compileError)
    Right contract -> pure unit

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "parseContract" $ do
    it "parses LockWithPublicKey" $
      testParseContract lockWithPublicKey
    it "parses LockWithMultiSig" $
      testParseContract lockWithMultiSig
    it "parses LockWithPublicKeyHash" $
      testParseContract lockWithPublicKeyHash
    it "parses RevealPreimage" $
      testParseContract revealPreimage
    it "parses RevealCollision" $
      testParseContract revealCollision
    it "parses LockUntil" $
      testParseContract lockUntil
    it "parses LockDelay" $
      testParseContract lockDelay
    it "parses TransferWithTimeout" $
      testParseContract transferWithTimeout
    it "parses EscrowWithDelay" $
      testParseContract escrowWithDelay
    it "parses VaultSpend" $
      testParseContract vaultSpend
    it "parses RevealFixedPoint" $
      testParseContract revealFixedPoint
    it "ignores comments" $
      map show (parseContract revealFixedPointWithComment) `shouldEqual` Right revealFixedPoint
  describe "compile" $ do
    it "compiles LockWithPublicKey" $
      testCompileContract lockWithPublicKey
    it "compiles LockWithMultiSig" $
      testCompileContract lockWithMultiSig
    it "compiles LockWithPublicKeyHash" $
      testCompileContract lockWithPublicKeyHash
    it "compiles RevealPreimage" $
      testCompileContract revealPreimage
    it "compiles RevealCollision" $
      testCompileContract revealCollision
    it "compiles LockUntil" $
      testCompileContract lockUntil
    it "compiles LockDelay" $
      testCompileContract lockDelay
    it "compiles TransferWithTimeout" $
      testCompileContract transferWithTimeout
    it "compiles EscrowWithDelay" $
      testCompileContract escrowWithDelay
    it "compiles VaultSpend" $
      testCompileContract vaultSpend
    it "compiles RevealFixedPoint" $
      testCompileContract revealFixedPoint
