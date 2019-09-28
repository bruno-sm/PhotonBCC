module TypeCheck 
    ( typeCheck 
    ) where

import Data.Map as M
import Data.List as L
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Maybe
import Debug.Trace
import Ast


data Type = TNat
          | TInt
          | TReal
          | TString
          | TColor
          | TSceneId { sceneParamTypes::[Type] }
          | TScene { sceneId::String, sceneParamTypes::[Type] }
          deriving (Show, Eq)


subtype :: Type -> Type -> Bool
subtype TNat TInt = True
subtype TNat TReal = True
subtype TInt TReal = True
subtype a b | a == b = True
subtype _ _ = False


data Context = Context
  { sceneTypes :: M.Map String Type
  , paramTypes :: M.Map String Type
  , localVariableTypes :: M.Map Integer Type }
  deriving (Show)


initialContext :: Context
initialContext =
  let stMap = fromList [("rectangle", TSceneId [TReal, TReal, TReal, TReal, TColor])] in
  Context {sceneTypes=stMap, paramTypes=M.empty, localVariableTypes=M.empty}


type Infer a = ExceptT String (State Context) a 


runInfer :: Infer Context -> Context -> Maybe String
runInfer m c =
  case evalState (runExceptT m) c of
    Left err -> Just err
    Right c -> Nothing


typeCheck :: Ast -> Maybe String
typeCheck (Ast _ definitions) =
  runInfer (typeCheckDefinitions definitions) initialContext


typeCheckDefinitions :: [Definition] -> Infer Context
typeCheckDefinitions [] = lift get >>= \ctx -> return ctx
typeCheckDefinitions (def:tl) = do
  typeCheckRules $ defRules def
  ctx <- lift get
  let defType = definitionType (defParameters def) (paramTypes ctx) in do
    lift $ put (ctx{ sceneTypes=M.insert (defName def) defType (sceneTypes ctx)
                   , paramTypes=M.empty
                   , localVariableTypes=M.empty})
    typeCheckDefinitions tl
  where
    definitionType parameters ptMap =
      let usedParameters = L.filter (\p -> member p ptMap) parameters in
      let parameterTypes = L.map (\p -> fromJust $ M.lookup p ptMap) usedParameters in do
      TSceneId parameterTypes


typeCheckRules :: [Rule] -> Infer () 
typeCheckRules [] = return () 
typeCheckRules ((Scene _
                       (LocalVariable _ varId)
                       (SceneIdGlobalVariable _ sceneId)
                       args)
                :tl) = do
  ctx <- lift get
  sceneIdType <- getSceneIdType sceneId
  let spt = sceneParamTypes sceneIdType in
    if length spt == length args then do
      checkArgTypes args spt 
      initLocalVariable varId $ TScene sceneId spt 
      typeCheckRules tl
    else
      throwE $ "The scene " ++ sceneId ++ " needs " ++ (show $ length spt)
               ++ " arguments, but is being applied to " ++ (show $ length args)
  where
    checkArgTypes :: [GenArgument] -> [Type] -> Infer ()
    checkArgTypes [] _ = lift $ state $ \s -> ((), s)

    checkArgTypes ((GenArgumentParameter _ name):argsTl) (t:typesTl) = do
      setParameterType name t
      checkArgTypes argsTl typesTl 

    checkArgTypes ((GenArgumentGlobalVariable _ sceneId):argsTl) (t:typesTl) = do
      checkGlobalVariableType sceneId t
      checkArgTypes argsTl typesTl

    checkArgTypes ((GenArgumentLocalVariable _ varId):argsTl) (t:typesTl) = do
      checkLocalVariableType varId t
      checkArgTypes argsTl typesTl

    checkArgTypes ((GenArgumentNatural _ _):_) (t:_) | not $ TNat `subtype` t =
      throwE $ "The argument needs to be of type " ++ (show t)
               ++ "but is a natural number"

    checkArgTypes ((GenArgumentInteger _ _):_) (t:_) | not $ TInt `subtype` t =
      throwE $ "The argument needs to be of type " ++ (show t)
               ++ "but is an integer number"

    checkArgTypes ((GenArgumentReal _ _):_) (t:_) | t /= TReal =
      throwE $ "The argument needs to be of type " ++ (show t)
               ++ "but is a real number"

    checkArgTypes ((GenArgumentString _ _):_) (t:_) | t /= TString =
      throwE $ "The argument needs to be of type " ++ (show t)
               ++ "but is a string literal"

    checkArgTypes ((GenArgumentColor _ _):_) (t:_) | t /= TColor =
      throwE $ "The argument needs to be of type " ++ (show t)
               ++ "but is a color"

    checkArgTypes (a:argsTl) (_:typesTl) = checkArgTypes argsTl typesTl


typeCheckRules ((Paint _ (SceneArgumentLocalVariable _ varId)):tl) = do
  ctx <- lift get
  parameterType <- getLocalVariableType varId 
  case parameterType of
    TScene _ _ -> typeCheckRules tl 
    t -> throwE $ "Variable %" ++ (show varId)
                  ++ " was believed to be of type " ++ (show t)
                  ++ " but it also needs to be of type Scene"

typeCheckRules ((Clear _ (SceneArgumentLocalVariable _ varId)):tl) = do
  ctx <- lift get
  varType <- getLocalVariableType varId 
  case varType of
    TScene _ _ -> typeCheckRules tl 
    t -> throwE $ "Variable %" ++ (show varId)
                  ++ " was believed to be of type " ++ (show t)
                  ++ " but it also needs to be of type Scene"

typeCheckRules ((Param _ (LocalVariable _ resVarId)
                       (SceneArgumentLocalVariable _ sceneVarId)
                       (NaturalLiteral _ i)):tl) = do
  ctx <- lift get
  sceneVarType <- getLocalVariableType sceneVarId
  case sceneVarType of
    TScene _ pt ->
      if (fromIntegral i) >= length pt then
        throwE $ "There is not parameter number " ++ (show i)
                 ++ " in type " ++ (show sceneVarType)
      else
        let iParameterType = pt !! (fromIntegral i) in
        let ltMap = localVariableTypes ctx in
          case M.lookup resVarId ltMap of
            Nothing -> do
              lift $ put (ctx{localVariableTypes = M.insert resVarId iParameterType ltMap}) 
              typeCheckRules tl
            Just t | t == iParameterType -> typeCheckRules tl 
            Just t -> throwE $ "Variable %" ++ (show sceneVarId)
                               ++ " was believed to be of type " ++ (show t)
                               ++ " but it also needs to be of type "
                               ++ (show iParameterType)
    t -> throwE $ "Variable %" ++ (show sceneVarId)
                  ++ " was believed to be of type " ++ (show t)
                  ++ " but it also needs to be of type Scene"

typeCheckRules ((Wait _ (NumericParameter _ name)):tl) = do
  setParameterType name TReal
  typeCheckRules tl

typeCheckRules ((Wait _ (NumericLocalVariable _ varId)):tl) = do
  varType <- checkLocalVariableType varId TReal
  typeCheckRules tl

typeCheckRules ((Wait _ (NumericLiteral _ _)):tl) = typeCheckRules tl 


getSceneIdType :: String -> Infer Type
getSceneIdType sceneId = do
  ctx <- lift get
  let stMap = sceneTypes ctx in
    case M.lookup sceneId stMap of
      Nothing -> throwE $ "Scene " ++ sceneId ++ " not defined"
      Just (TSceneId pt) -> return $ TSceneId pt
      Just t -> throwE $ "Scene " ++ sceneId
                         ++ " should have type TSceneId, but it has type " ++ (show t)

getLocalVariableType :: Integer -> Infer Type
getLocalVariableType varId = do
  ctx <- lift get
  let ltMap = localVariableTypes ctx in
    case M.lookup varId ltMap of
      Nothing -> throwE $ "Variable %" ++ (show varId) ++ " not defined"
      Just t -> return t 

setParameterType :: String -> Type -> Infer ()
setParameterType name t = do
  ctx <- lift get
  let ptMap = paramTypes ctx in
    case M.lookup name ptMap of
      Nothing -> lift $ put ctx{paramTypes = M.insert name t ptMap}
      Just oldT | oldT == t -> lift $ state $ \s -> ((), s)
      Just oldT -> throwE $ "Parameter " ++ name
                            ++ " was believed to be of type " ++ (show oldT)
                            ++ " but it also needs to be of type " ++ (show t)

initLocalVariable :: Integer -> Type -> Infer ()
initLocalVariable varId t = do
  ctx <- lift get
  let ltMap = localVariableTypes ctx in
    case M.lookup varId ltMap of
      Nothing -> lift $ put (ctx{localVariableTypes = M.insert varId t ltMap})
      Just oldT | oldT == t -> lift $ state $ \s -> ((), s)
      Just oldT -> throwE $ "Variable %" ++ (show varId)
                            ++ " was already initialized with type " ++ (show oldT)
                            ++ " so can't be redefined with type " ++ (show t)


checkGlobalVariableType :: String -> Type -> Infer ()
checkGlobalVariableType sceneId t = do
  sceneIdType <- getSceneIdType sceneId
  if t == sceneIdType then lift $ state $ \s -> ((), s) 
  else throwE $ "The argument needs to be of type " ++ (show t)
                ++ " ,but actually is of type " ++ (show sceneIdType)


checkLocalVariableType :: Integer -> Type -> Infer ()
checkLocalVariableType varId t = do
  varType <- getLocalVariableType varId
  if varType `subtype` t then lift $ state $ \s -> ((), s)
  else
    throwE $ "Variable %" ++ (show varId)
                          ++ " was believed to be of type " ++ (show varType)
                          ++ " but it also needs to be of type " ++ (show t)
