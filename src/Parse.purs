module Parse (parseContract, Contract(Contract), Clause(Clause), Parameter(Parameter), Type(PublicKey, Value, Signature), 
              Statement(Unlock, Verify), Expr(Call, Variable), parseExpression) where

import Data.Identity

import Control.Alt ((<|>))
import Control.Monad (pure, (>>=))
import Control.Plus (empty)
import Data.Functor (map)
import Data.Either (Either(Left, Right))
import Data.List (List, many)
import Data.String (toCharArray)
import Data.Tuple (Tuple(Tuple))
import Data.Unit (Unit)
import Data.Map (fromFoldable, Map, lookup)
import Prelude (class Eq, class Show, discard, bind, (<<<), (<>), ($), show)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (try, choice, (<?>))
import Text.Parsing.Parser.Expr (Assoc(AssocLeft), Operator(Infix), OperatorTable, buildExprParser)
import Text.Parsing.Parser.String (char, oneOf, string)
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(LanguageDef), letter, alphaNum, TokenParser, makeTokenParser)
import Data.Foldable (intercalate, fold)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Lazy (fix)

data Expr
    = NaturalLiteral Int
    | BinaryOp String Expr Expr
    | Variable String
    | Call String (List Expr)
    | ArgumentList (List Expr)

instance showExpr :: Show Expr where
    show (NaturalLiteral int) = show int
    show (BinaryOp op left right)    = show left <> " " <> op <> " " <> show right
    show (Variable name) = name
    show (Call name exps) = name <> "(" <> intercalate ", " (map show exps) <> ")"
    show (ArgumentList exps) = "[" <> intercalate ", " (map show exps) <> "]"

data Statement
    = Verify Expr
    | Unlock Expr

instance showStatement :: Show Statement where
    show (Verify exp) = "    verify " <> show exp <> "\n"
    show (Unlock exp) = "    unlock " <> show exp <> "\n"

data Parameter
    = Parameter String Type

derive instance eqParameter :: Eq Parameter

instance showParameter :: Show Parameter where
    show (Parameter paramName typeName) =
        paramName <> ": " <> show typeName

data Type
    = Void
    | Boolean
    | Bytes
    | Signature
    | PublicKey
    | Value
    | Time
    | Duration
    | HashType String Type

instance showType :: Show Type where
    show (HashType hashFunction inputType) = hashFunction <> "(" <> show inputType <> ")"
    show Boolean = "Boolean"
    show Bytes = "Bytes"
    show Signature = "Signature"
    show PublicKey = "PublicKey"
    show Time = "Time"
    show Duration = "Duration"
    show Value = "Value"
    show Void = "Void"

data Contract
  = Contract String (List Parameter) (List Clause)

instance showContract :: Show Contract where
    show (Contract contractName params clauses) =
        let clausesString = (map show clauses) in
        "contract " <> contractName <> "(" <> intercalate ", " (map show params) <> ") {\n" <>
        fold clausesString <> "}"

data Clause
  = Clause String (List Parameter) (List Statement)

instance showClause :: Show Clause where
  show (Clause clauseName params statements) =
      let statementsString = (map show statements) in
      "  clause " <> clauseName <> "(" <> intercalate ", " (map show params) <> ") {\n" <>
      (fold statementsString) <> "  }\n"

derive instance eqContract :: Eq Contract
derive instance eqClause :: Eq Clause
derive instance eqType :: Eq Type
derive instance eqExpr :: Eq Expr
derive instance eqStatement :: Eq Statement

langDef :: LanguageDef
langDef = LanguageDef {
    commentStart: "/*"
  , commentEnd: "*/"
  , commentLine: "//"
  , nestedComments: true
  , identStart: letter
  , identLetter: alphaNum <|> char '_'
  , opStart: oneOf (toCharArray ":!#$%&*+./<=>?@\\^|-~")
  , opLetter: oneOf (toCharArray ":!#$%&*+./<=>?@\\^|-~")
  , reservedNames: ["contract", "clause", "lock", "unlock", "verify", "with"]
  , reservedOpNames: []
  , caseSensitive: true
  }

lexer :: TokenParser
lexer = makeTokenParser langDef

parens :: forall a. Parser String a -> Parser String a
parens = lexer.parens

brackets :: forall a. Parser String a -> Parser String a
brackets = lexer.brackets

braces :: forall a. Parser String a -> Parser String a
braces = lexer.braces

reserved :: String -> Parser String Unit
reserved = lexer.reserved

semiSep :: forall a. Parser String a -> Parser String (List a)
semiSep = lexer.semiSep

reservedOp :: String -> Parser String Unit
reservedOp = lexer.reservedOp

commaSep :: forall a. Parser String a -> Parser String (List a)
commaSep = lexer.commaSep

identifier :: Parser String String
identifier = lexer.identifier

variable :: Parser String Expr
variable = map Variable identifier

call :: Parser String Expr -> Parser String Expr
call exprParser = do
    funcName <- identifier
    args <- parens (commaSep exprParser)
    pure (Call funcName args)

infixOp :: String -> Operator Identity String Expr
infixOp s = Infix (reservedOp s >>= \_ -> pure (BinaryOp s)) AssocLeft

table :: OperatorTable Identity String Expr
table = [
    map infixOp ["==", "!="]
  ]

expTerm :: Parser String Expr
expTerm = fix \p ->  -- unfortunately needed to get around circular definition
    parens p
    <|> try (call p)
    <|> map ArgumentList (brackets (commaSep p))
    <|> variable
    <?> "term"

expr :: Parser String Expr
expr = buildExprParser table expTerm

prims :: Array (Tuple String Type)
prims = [Tuple "Bytes" Bytes,
    Tuple "Time" Time,
    Tuple "Duration" Duration,
    Tuple "Boolean" Boolean,
    Tuple "Signature" Signature,
    Tuple "PublicKey" PublicKey,
    Tuple "Value" Value
    ]

primitives :: Map String Type
primitives = fromFoldable prims

primitive :: Parser String Type
primitive = do
        typeName <- identifier
        let primitiveType = lookup typeName primitives
        case primitiveType of
            Nothing   -> empty
            Just prim -> pure prim

hashType :: Parser String Type -> Parser String Type
hashType ivyTypeParser = do
    hashFunction <- choice $ map (try <<< string) ["Sha1", "Sha256", "Ripemd160"]
    inputType <- parens ivyTypeParser
    pure (HashType hashFunction inputType)

ivyType :: Parser String Type
ivyType = fix \p -> -- unfortunately needed to get around circular reference
          try primitive
      <|> hashType p
      <?> "type"

statement :: Parser String Statement
statement =
        assertion
    <|> unlock
    <?> "statement"

assertion :: Parser String Statement
assertion = do
    reserved "verify"
    e <- expr
    pure (Verify e)

unlock :: Parser String Statement
unlock = do
    reserved "unlock"
    e <- expr
    pure (Unlock e)

clause :: Parser String Clause
clause = do
    reserved "clause"
    name <- identifier
    params <- parens parameters
    statements <- braces (many statement)
    pure $ Clause name params statements

parameter :: Parser String Parameter
parameter = do
    paramName <- identifier
    reserved ":"
    paramType <- ivyType
    pure (Parameter paramName paramType)

parameters :: Parser String (List Parameter)
parameters = commaSep parameter

contract :: Parser String Contract
contract = do
    reserved "contract"
    name <- identifier
    params <- parens parameters
    clauses <- braces (many clause)
    pure $ Contract name params clauses

parseContract :: String -> Either ParseError Contract
parseContract str = runParser str contract

parseClause :: String -> Either ParseError Clause
parseClause str = runParser str clause

parseExpression :: String -> Either ParseError Expr
parseExpression str = runParser str expr

parsePrimitive :: String -> Either ParseError Type
parsePrimitive str = runParser str primitive

testContract :: String -> forall e. Eff (console :: CONSOLE | e) Unit
testContract contractString = do
    let result = parseContract contractString
    case result of
        (Right contract) -> log (show contract)
        (Left err)       -> log (show err)


