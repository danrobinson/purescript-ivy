module Typecheck
  ( Check, TypeError(ParseFailure, Mismatch, AlreadyDefined, NotInScope), 
    typecheckContract, runTypecheck, Env, Name ) where

import Functions

import AST (Clause(..), Contract(..), Expr(..), Parameter(..), Statement(..), Type(..))
import Control.Monad (bind, pure, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, ask, local, runReaderT)
import Data.Either (Either)
import Data.Functor (map)
import Data.List (List, length, foldl, zipWithA)
import Data.List.NonEmpty (fromList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.NonEmpty (foldl1)
import Data.StrMap (StrMap, lookup, empty, insert) as M
import Data.Traversable (traverse_)
import Prelude (($), (==), (/=), (<>), (<<<), unit, discard, class Eq, class Show, show, Unit)


type Name = String

type Env = M.StrMap Type

type Check = (ExceptT TypeError (Reader Env))

lookupVar :: Name -> Check Type
lookupVar name = do
  env <- ask
  case M.lookup name env of
    Just t  -> pure t
    Nothing -> throwError $ NotInScope name

checkUnused :: Parameter -> Check Unit
checkUnused (Parameter { name: name  }) = do
  env <- ask
  case M.lookup name env of
    Just e  -> throwError $ AlreadyDefined name
    Nothing -> pure unit

match :: Expr -> Type -> Check Type
match e t2 = do
  t1 <- typecheckExpression e
  matchTypes t1 t2

match_ :: Expr -> Type -> Check Type
match_ e t = do
  _ <- match e t
  pure Void

matchTypes :: Type -> Type -> Check Type
matchTypes t1 t2 = if t1 == t2 then pure t1 else throwError $ Mismatch t1 t2

matchCheckTypes :: Check Type -> Check Type -> Check Type
matchCheckTypes ct1 ct2 = do
  t1 <- ct1
  t2 <- ct2
  matchTypes t1 t2

unify :: TypeSignature -> List Expr -> Check Type
unify (TypeSignature inputTypes outputType) exprs = do
  when (length inputTypes /= length exprs) (throwError $ WrongNumber (length inputTypes) (length exprs))
  _ <- zipWithA match exprs inputTypes
  pure outputType

unifyInstruction :: String -> List Expr -> M.StrMap TypeSignature -> Check Type
unifyInstruction opName es opMap =
  let op = M.lookup opName opMap in
    case op of
      Nothing      -> throwError $ NotInScope opName
      Just typeSig -> unify typeSig es

typecheckExpression :: Expr -> Check Type
typecheckExpression e = case e of
  (Variable name) -> lookupVar name
  (BinaryOp { op: opName, left: e1, right: e2}) -> do
    t1 <- typecheckExpression e1
    t2 <- typecheckExpression e2
    when (t1 /= t2) $ throwError $ CompareDifferent t1 t2
    when (t1 == Boolean) $ throwError $ CompareBoolean
    pure Boolean
  (ArgumentList exps) -> 
    let expCheckTypes = map typecheckExpression exps in
      case fromList expCheckTypes of
        Nothing -> throwError $ NoEmptyLists
        Just es -> do
          t <- foldl1 matchCheckTypes (unwrap es)
          pure $ ListLiteral t
  (Call { name: "bytes", args: args }) -> do 
    let numArgs = length args
    when (numArgs /= 1) $ throwError $ WrongNumber numArgs 1
    pure Bytes
  (Call { name: funcName, args: args }) -> do      
    let func = M.lookup funcName functions
    case func of
      Nothing      -> throwError $ NotInScope funcName
      Just typeSig -> unify typeSig args

typecheckStatement :: Statement -> Check Type
typecheckStatement st = case st of
  Verify exp    -> match_ exp Boolean
  Unlock val    -> match_ val Value

addParameter ::  Env -> Parameter -> Env
addParameter env (Parameter { name: n, typeName: t }) = M.insert n t env

composeParameters :: List Parameter -> Env -> Env
composeParameters ps env =
  foldl addParameter env ps

scoped :: forall t. List Parameter -> Check t -> Check t
scoped ps = local (composeParameters ps)

typecheckClause :: Clause -> Check Type
typecheckClause (Clause _ parameters statements)
  = do
    traverse_ checkUnused parameters
    scoped parameters (traverse_ typecheckStatement statements)
    pure Void

contractCheck :: Contract -> Check Type
contractCheck (Contract { params: params, clauses: clauses })
  = do
    scoped params (traverse_ typecheckClause clauses)
    pure Void

--typecheckExpression _                     = return Integer

data TypeError
  = Mismatch Type Type
  | WrongNumber Int Int
  | NotInScope Name
  | ParseFailure String
  | AlreadyDefined String
  | NoEmptyLists
  | CompareDifferent Type Type
  | CompareBoolean

derive instance eqTypeError :: Eq TypeError

instance showTypeError :: Show TypeError where
  show (Mismatch t1 t2) = "Mismatch: got " <> show t1 <> ", expected " <> show t2
  show (WrongNumber n1 n2) = "Wrong number of arguments: got " <> show n1 <> ", expected " <> show n2
  show (NotInScope name) = "Variable '" <> name <> "' is not in scope"
  show (ParseFailure s) = "Parsing failed: " <> s
  show (AlreadyDefined n) = "Variable '" <> n <> "' is already defined"
  show NoEmptyLists = "Empty lists are not allowed"
  show (CompareDifferent t1 t2) = "Cannot compare types " <> show t1 <> " and " <> show t2
  show CompareBoolean = "Comparing two Booleans is not allowed"

runTypecheck :: forall t. Check t -> Either TypeError t
runTypecheck c = unwrap $ runReaderT (runExceptT c) M.empty

typecheckContract :: Contract -> Either TypeError Type
typecheckContract = runTypecheck <<< contractCheck