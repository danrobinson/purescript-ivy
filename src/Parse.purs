module Parse (parseContract, parseExpression) where

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
import Prelude (discard, bind, ($), show)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.Expr (Assoc(AssocLeft), Operator(Infix), OperatorTable, buildExprParser)
import Text.Parsing.Parser.String (char, oneOf, string)
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(LanguageDef), letter, alphaNum, TokenParser, makeTokenParser)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Lazy (fix)
import Control.Applicative((*>))
import AST(Contract(Contract), Clause(Clause), Parameter(Parameter), 
           Type(PublicKey, Value, Signature, HashType, Bytes, Time, Duration, Boolean), 
           Statement(Unlock, Verify), Expr(Call, Variable, BinaryOp, ArgumentList), 
           HashFunction(Sha256, Sha1, Ripemd160))

hashFunc :: Parser String HashFunction
hashFunc = (try $ string "Sha256") *> pure Sha256
       <|> (try $ string "Sha1") *> pure Sha1
       <|> (try $ string "Ripemd160") *> pure Ripemd160

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
  , reservedNames: ["contract", "clause", "unlock", "verify"]
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
    pure (Call { name: funcName, args: args})

infixOp :: String -> Operator Identity String Expr
infixOp s = Infix (reservedOp s >>= \_ -> pure (\l r -> BinaryOp { op: s, left: l, right: r})) AssocLeft

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
    hashFunction <- hashFunc
    inputType <- parens ivyTypeParser
    pure (HashType {hashFunction: hashFunction, inputType})

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
    name <- identifier
    reserved ":"
    typeName <- ivyType
    pure (Parameter { name: name, typeName: typeName})

parameters :: Parser String (List Parameter)
parameters = commaSep parameter

contract :: Parser String Contract
contract = do
    reserved "contract"
    name <- identifier
    params <- parens parameters
    clauses <- braces (many clause)
    pure $ Contract { name: name, params: params, clauses: clauses }

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

