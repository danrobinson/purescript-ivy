module Functions (functions, TypeSignature(TypeSignature)) where

import AST (HashFunction(Ripemd160, Sha256, Sha1), 
            Type(Boolean, Duration, Time, Bytes, HashType, Signature, ListLiteral, PublicKey))
import Data.List (List, fromFoldable)
import Data.StrMap as Map
import Data.Tuple (Tuple(Tuple))
import Prelude (class Eq)

data TypeSignature =
  TypeSignature (List Type) Type

derive instance eqTypeSignature :: Eq TypeSignature

functions :: Map.StrMap TypeSignature
functions = Map.fromFoldable [
    Tuple "checkSig" (TypeSignature (fromFoldable [PublicKey, Signature]) Boolean),
    Tuple "checkMultiSig" (TypeSignature (fromFoldable [ListLiteral PublicKey, ListLiteral Signature]) Boolean),
    Tuple "sha1" (TypeSignature (fromFoldable [Bytes]) (HashType { hashFunction: Sha1, inputType: Bytes})),
    Tuple "sha256" (TypeSignature (fromFoldable [Bytes]) (HashType { hashFunction: Sha256, inputType: Bytes})),
    Tuple "ripemd160" (TypeSignature (fromFoldable [Bytes]) (HashType { hashFunction: Ripemd160, inputType: Bytes})),
    Tuple "after" (TypeSignature (fromFoldable [Time]) Boolean),
    Tuple "older" (TypeSignature (fromFoldable [Duration]) Boolean)
  ]


