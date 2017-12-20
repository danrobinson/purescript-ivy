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
import Data.Unit (Unit, unit)
import Data.Map (fromFoldable, Map, lookup)
import Data.Integral (fromIntegral)
import Prelude (class Eq, class Show, discard, bind, (<<<), (<>), ($), show)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (try, choice, (<?>))
import Text.Parsing.Parser.Expr (Assoc(AssocLeft), Operator(Prefix, Infix), OperatorTable, buildExprParser)
import Text.Parsing.Parser.String (char, oneOf, string)
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(LanguageDef), letter, alphaNum, TokenParser, makeTokenParser)
import Data.Foldable (intercalate, fold)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data Expr
    = NaturalLiteral Int
    | UnaryOp String Expr
    | BinaryOp String Expr Expr
    | Variable String
    | Call String (List Expr)

instance showExpr :: Show Expr where
    show (NaturalLiteral int) = show int
    show (UnaryOp op (NaturalLiteral nat))     = op <> show nat
    show (UnaryOp op exp)     = op <> "(" <> show exp <> ")"
    show (BinaryOp op left right)    = "(" <> show left <> " " <> op <> " " <> show right <> ")"
    show (Variable name) = name
    show (Call name exps) = name <> "(" <> intercalate ", " (map show exps) <> ")"

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
    | String
    | Integer
    | Signature
    | PublicKey
    | Value
    | Program
    | HashType String Type

instance showType :: Show Type where
    show (HashType hashFunction inputType) = hashFunction <> "(" <> show inputType <> ")"
    show Boolean = "Boolean"
    show String = "String"
    show Signature = "Signature"
    show PublicKey = "PublicKey"
    show Integer = "Integer"
    show Value = "Value"
    show Program = "Program"
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

natural :: Parser String Expr
natural = map (NaturalLiteral <<< fromIntegral) lexer.natural

variable :: Parser String Expr
variable = map Variable identifier

call :: Parser String Expr
call = do
    funcName <- identifier
    args <- parens arguments
    pure (Call funcName args)

prefixOp :: String -> Operator Identity String Expr
prefixOp s = Prefix (reservedOp s >>= \_ -> pure (UnaryOp s))

infixOp :: String -> Operator Identity String Expr
infixOp s = Infix (reservedOp s >>= \_ -> pure (BinaryOp s)) AssocLeft

table :: OperatorTable Identity String Expr
table = [
    map prefixOp ["-", "!"],
    map infixOp ["+", "-"],
    map infixOp ["==", "!=", ">=", "<=", ">", "<"]
  ]

expTerm :: Parser String Expr
expTerm = ((\_ -> parens expr) unit)
    <|> try ((\_ -> call) unit)
    <|> variable
    <|> natural
    <?> "term"

expr :: Parser String Expr
expr = buildExprParser table ((\_ -> expTerm) unit)

arguments :: Parser String (List Expr)
arguments = commaSep ((\_ -> expr) unit)

prims :: Array (Tuple String Type)
prims = [Tuple "String" String,
    Tuple "Integer" Integer,
    Tuple "Boolean" Boolean,
    Tuple "Signature" Signature,
    Tuple "PublicKey" PublicKey,
    Tuple "Value" Value,
    Tuple "Program" Program
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

hashType :: Parser String Type
hashType = do
    hashFunction <- choice $ map (try <<< string) ["Sha3", "Sha256"]
    inputType <- parens ivyType
    pure (HashType hashFunction inputType)

ivyType :: Parser String Type
ivyType = try primitive
      <|> ((\_ -> hashType) unit)
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
parseExpression str = runParser str expTerm

parsePrimitive :: String -> Either ParseError Type
parsePrimitive str = runParser str primitive

testContract :: String -> forall e. Eff (console :: CONSOLE | e) Unit
testContract contractString = do
    let result = parseContract contractString
    case result of
        (Right contract) -> log (show contract)
        (Left err)       -> log (show err)


