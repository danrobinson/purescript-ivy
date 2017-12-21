module AST where

import Data.Functor (map)
import Data.Foldable (intercalate, fold)
import Data.List(List)
import Prelude(class Show, class Eq, show, (<>))

data Expr
    = BinaryOp { op :: String, left :: Expr, right :: Expr }
    | Variable String
    | Call { name :: String, args :: (List Expr) }
    | ArgumentList (List Expr)

instance showExpr :: Show Expr where
    show (BinaryOp { op: op, left: left, right: right}) = show left <> " " <> op <> " " <> show right
    show (Variable name) = name
    show (Call { name: name, args: args }) = name <> "(" <> intercalate ", " (map show args) <> ")"
    show (ArgumentList args) = "[" <> intercalate ", " (map show args) <> "]"

data Statement
    = Verify Expr
    | Unlock Expr

instance showStatement :: Show Statement where
    show (Verify exp) = "    verify " <> show exp <> "\n"
    show (Unlock exp) = "    unlock " <> show exp <> "\n"

data Parameter
    = Parameter { name :: String, typeName :: Type }

instance showParameter :: Show Parameter where
    show (Parameter { name: paramName, typeName: typeName}) =
        paramName <> ": " <> show typeName

data HashFunction = Sha256 | Sha1 | Ripemd160

instance showHashfunction :: Show HashFunction where
  show Sha256 = "Sha256"
  show Sha1 = "Sha1"
  show Ripemd160 = "Ripemd160"

data Type
    = Void
    | Boolean
    | Bytes
    | Signature
    | PublicKey
    | Value
    | Time
    | Duration
    | ListLiteral Type
    | HashType { hashFunction :: HashFunction, inputType :: Type }

instance showType :: Show Type where
    show (HashType { hashFunction: hashFunction, inputType: inputType }) = show hashFunction <> "(" <> show inputType <> ")"
    show Boolean = "Boolean"
    show Bytes = "Bytes"
    show Signature = "Signature"
    show PublicKey = "PublicKey"
    show Time = "Time"
    show Duration = "Duration"
    show Value = "Value"
    show Void = "Void"
    show (ListLiteral t) = "[" <> show t <> "]"

data Contract
  = Contract { name :: String, params :: (List Parameter), clauses :: (List Clause) }

instance showContract :: Show Contract where
    show (Contract { name: name, params: params, clauses: clauses }) =
        let clausesString = (map show clauses) in
        "contract " <> name <> "(" <> intercalate ", " (map show params) <> ") {\n" <>
        fold clausesString <> "}"

data Clause
  = Clause String (List Parameter) (List Statement)

instance showClause :: Show Clause where
  show (Clause clauseName params statements) =
      let statementsString = (map show statements) in
      "  clause " <> clauseName <> "(" <> intercalate ", " (map show params) <> ") {\n" <>
      (fold statementsString) <> "  }\n"

derive instance eqParameter :: Eq Parameter
derive instance eqContract :: Eq Contract
derive instance eqClause :: Eq Clause
derive instance eqHashFunction :: Eq HashFunction
derive instance eqType :: Eq Type
derive instance eqExpr :: Eq Expr
derive instance eqStatement :: Eq Statement

