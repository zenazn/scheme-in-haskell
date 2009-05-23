module Eval (eval, bindVars, apply, load) where 
import Datatypes
import Monad (liftM)
import Data.IORef
import Parser (parseSchemeList)

eval :: Env -> LispVal -> IOThrowsError LispVal

-- Primitives
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val

-- State-related Evaluators

eval env (Atom id) = getVar env id
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = 
    eval env form >>= defineVar env var

--If Clause

eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
-- Functions 

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

-- IO
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)

-- Catch-all for user-defined functions
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

-- Bad forms
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--State-related workhorses

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return. maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable: " var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable: " var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)



-- Helpers for Functions
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of 
                                  Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                  Nothing -> return env
apply (IOFunc func) args = func args

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

-- IO Helpers
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . parseSchemeList