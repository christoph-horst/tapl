-- TODO: implement a parser
--import Text.Trifecta
--import Control.Applicative
import Control.Monad (guard, unless)

data Ty = TyBool
        | TyFun Ty Ty
        deriving (Eq, Show)

data Term = TmVar Int Int
            | TmAbs String Ty Term
            | TmApp Term Term
            | TmTrue
            | TmFalse
            | TmIf Term Term Term
            deriving (Eq, Show)

data Binding = NameBind | VarBind Ty deriving (Eq, Show)

type Context = [(String, Binding)]

isVal :: Context -> Term -> Bool
isVal _ (TmAbs _ _ _) = True
isVal _ TmTrue = True
isVal _ TmFalse = True
isVal _ _ = False

-- small-step evaluation
eval1 :: Context -> Term -> Maybe Term
-- E-IfTrue
eval1 _ (TmIf TmTrue t2 t3) = pure t2
-- E-IfFalse
eval1 _ (TmIf TmFalse t2 t3) = pure t3
-- E-If
eval1 ctx (TmIf t1 t2 t3) = do
  t1' <- eval1 ctx t1
  pure $ TmIf t1' t2 t3
-- E-AppAbs
eval1 ctx (TmApp (TmAbs x _ t12) v2)
  | isVal ctx v2 = Just $ termSubstTop v2 t12
-- E-App2, E-App1
eval1 ctx (TmApp t1 t2)
  | isVal ctx t1 =   
      TmApp t1 <$> eval1 ctx t2
  | otherwise =
      (\t1' -> TmApp t1' t2) <$> eval1 ctx t1
eval1 ctx _ = Nothing

-- multi-step evaluation
eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Just t' -> eval ctx t'
  Nothing -> t

-- the "shift t up by d (above cutoff c, which is 0 initially)" operation
termShift :: Int -> Term -> Term           
termShift d t = shift 0 t
  where shift c (TmVar x n) =
          if x >= c
          then TmVar (x+d) (n+d)
          else TmVar x (n+d)
        shift c (TmAbs x ty t1) = TmAbs x ty (shift (c+1) t1)
        shift c (TmApp t1 t2) = TmApp (shift c t1) (shift c t2)
        shift c TmTrue = TmTrue
        shift c TmFalse = TmFalse
        shift c (TmIf t1 t2 t3) = TmIf (shift c t1) (shift c t2) (shift c t3)

-- substitute the term s for the variable with index j in term t
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = go 0 t
  where go c v@(TmVar x n) =
          if x == j + c
          then termShift c s
          else v
        go c (TmAbs x ty t1) = TmAbs x ty (go (c+1) t1)
        go c (TmApp t1 t2) = TmApp (go c t1) (go c t2)
        go c TmTrue = TmTrue
        go c TmFalse = TmFalse
        go c (TmIf t1 t2 t3) = TmIf (go c t1) (go c t2) (go c t3)

termSubstTop :: Term -> Term -> Term        
termSubstTop s t =
  termShift (-1) $ termSubst 0 (termShift 1 s) t

-- type checking
typeFromContext :: Context -> Int -> Maybe Ty
typeFromContext ctx k = do
  guard (k < length ctx)
  case snd (ctx !! k) of
   VarBind ty -> Just ty
   NameBind -> Nothing

typeOf :: Context -> Term -> Either String Ty
-- T-True
typeOf _ TmTrue = pure TyBool
-- T-False
typeOf _ TmFalse = pure TyBool
-- T-If
typeOf ctx (TmIf t1 t2 t3) = do
  ty1 <- typeOf ctx t1
  unless (ty1 == TyBool) (Left "condition must be of type Bool")
  ty2 <- typeOf ctx t2
  ty3 <- typeOf ctx t3
  unless (ty2 == ty3) (Left "arms of conditional have different types")
  pure ty2
-- T-Var
typeOf ctx (TmVar k n) = note "variable not found in context" $ typeFromContext ctx k
-- T-Abs
typeOf ctx (TmAbs x ty1 t2) = do
  let ctx' = (x, VarBind ty1) : ctx
  ty2 <- typeOf ctx' t2
  pure $ TyFun ty1 ty2
-- T-App
typeOf ctx (TmApp t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case ty1 of
   TyFun ty11 ty12
     | ty11 == ty2 -> pure $ ty12
     | otherwise -> Left "parameter type mismatch"
   _ -> Left "function type expected"

note :: String -> Maybe a -> Either String a
note s = maybe (Left s) Right

-------------

evalTermAndPrint :: Term -> IO ()
evalTermAndPrint t = do
  print t
  let ctx = []
  case typeOf ctx t of
    Right ty -> do
      putStrLn $ "Type: " ++ show ty
      putStrLn $ "Result: " ++ show (eval ctx t)
    Left e -> putStrLn $ "Type error: " ++ e

main = do
  evalTermAndPrint $ TmApp (TmAbs "x" TyBool (TmVar 0 1)) TmTrue

  evalTermAndPrint $ TmAbs "x" TyBool (TmVar 0 1)

  evalTermAndPrint $ TmApp
    (TmAbs "x" (TyFun TyBool TyBool) (TmIf (TmApp (TmVar 0 1) TmFalse) TmTrue TmFalse))
    (TmAbs "x" TyBool (TmIf (TmVar 0 1) TmFalse TmTrue))
