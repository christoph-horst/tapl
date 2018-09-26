import Text.Trifecta
import Control.Applicative
import Control.Monad (unless, guard)

data Ty = TyBool
        | TyFun Ty Ty
          deriving (Eq, Show)

data Term = TmVar Int Int
          | TmLet String Term Term
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          deriving (Eq, Show)

data Binding = NameBind
             | VarBind Ty
             deriving (Eq, Show)

type Context = [(String, Binding)]

{-data Command = Eval Term
             | Bind String Binding -}

               
----

isVal :: Context -> Term -> Bool
isVal _ TmTrue = True
isVal _ TmFalse = True
isVal _ (TmAbs _ _ _) = True
isVal _ _ = False

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
  | isVal ctx v2 = pure $ termSubstTop v2 t12
-- E-App2, E-App1
eval1 ctx (TmApp t1 t2)
  | isVal ctx t1 =   
      TmApp t1 <$> eval1 ctx t2
  | otherwise =
      (\t1' -> TmApp t1' t2) <$> eval1 ctx t1
-- E-Let1, E-Let
eval1 ctx (TmLet x t1 t2)
  | isVal ctx t1 = pure $ termSubstTop t1 t2
  | otherwise = do
      t1' <- eval1 ctx t1
      pure $ TmLet x t1' t2
eval1 ctx _ = Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Just t' -> eval ctx t'
  Nothing -> t

termShift :: Int -> Term -> Term
termShift d = termMap (\c x n -> if x >= c
                                 then TmVar (x+d) (n+d)
                                 else TmVar x (n+d))
              0

termSubst :: Int -> Term -> Term -> Term
termSubst j s = termMap (\c x n -> if x == j + c
                                   then termShift c s
                                   else TmVar x n)
                0
                  
termMap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
termMap onVar c t = go c t
  where go c (TmVar x n) = onVar c x n
        go c (TmLet x t1 t2) = TmLet x (go c t1) (go (c+1) t2)
        go c (TmAbs x ty1 t2) = TmAbs x ty1 (go (c+1) t2)
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
-- T-Let
typeOf ctx (TmLet x t1 t2) = do
  ty1 <- typeOf ctx t1
  let ctx' = (x, VarBind ty1):ctx
  typeOf ctx' t2


note :: String -> Maybe a -> Either String a
note s = maybe (Left s) Right

main :: IO ()
main = do
  let t = TmLet "x" TmTrue (TmVar 0 1)
  print t
  print $ eval [] t
  print $ typeOf [] t
  
  let t = TmLet "x" (TmAbs "y" TyBool (TmVar 0 1)) (TmApp (TmVar 0 1) TmFalse)
  print t
  print $ eval [] t
  print $ typeOf [] t

  let t = TmLet "f" (TmAbs "x" TyBool (TmVar 0 1)) (TmVar 0 1)
  print t
  print $ eval [] t
  print $ typeOf [] t

  let t2 = TmApp t TmFalse
  print t
  print $ eval [] t2
  print $ typeOf [] t2
  
