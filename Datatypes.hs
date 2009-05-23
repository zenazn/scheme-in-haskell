module Datatypes ( LispVal(..), LispError(..), ThrowsError, IOThrowsError, Env, nullEnv, throwError, catchError, trapError, extractValue, runErrorT, liftThrows, liftIO ) where
import Control.Monad.Error
import Text.ParserCombinators.Parsec(ParseError)
import Data.IORef
import IO (Handle)

-- Scheme Datatypes

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal -- Apparently a valid datatype in Scheme
             | Number Integer
             | Float Float
             | Rational Rational
             | String String
             | Bool Bool
             | Char Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), 
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
instance Show LispError where show = showError
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

-- Environment

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-- Display LispVals
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char x) = '#':'\\':x:[]
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr


-- Helper Functions
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


-- Utility functions (exported)
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val