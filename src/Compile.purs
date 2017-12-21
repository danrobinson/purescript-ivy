module Compile where

import Parse (parseContract)
import Text.Parsing.Parser (ParseError)
import Typecheck (typecheckContract, TypeError)
import Control.Monad.Except (Except, throwError, runExcept)
import Data.Unit (Unit)
import Data.Either(Either(Left, Right))
import Prelude (($), (<>), (<<<), pure, unit, class Show, show)

data CompileError =
    ParseError ParseError
  | TypeError TypeError

instance showCompileError :: Show CompileError where
  show (ParseError parseError) = "Parse error: " <> show parseError
  show (TypeError typeError) = "Type error: " <> show typeError

type Compiled = (Except CompileError Unit)

compileContract :: String -> Compiled
compileContract source = do
  case parseContract source of
    Left parseError -> throwError $ ParseError parseError
    Right parsed -> case typecheckContract parsed of
      Left typeError -> throwError $ TypeError typeError
      Right compiled -> pure unit

compile :: String -> Either CompileError Unit
compile = runExcept <<< compileContract