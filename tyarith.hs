import Text.Trifecta
import Control.Applicative
import Control.Monad (unless)

-- Types type
data Ty = TyBool | TyNat deriving (Eq, Show)

-- Term type
data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Show, Eq)

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

-- small-step evaluation
eval1 :: Term -> Maybe Term
-- E-IfTrue
eval1 (TmIf TmTrue t2 t3) = pure t2
-- E-IfFalse
eval1 (TmIf TmFalse t2 t3) = pure t3
-- E-If
eval1 (TmIf t1 t2 t3) = do
  t1' <- eval1 t1
  pure $ TmIf t1' t2 t3
-- E-Succ
eval1 (TmSucc t1) = do
  t1' <- eval1 t1
  pure $ TmSucc t1'
-- E-PredZero
eval1 (TmPred TmZero) = pure TmZero
-- E-PredSucc
eval1 (TmPred (TmSucc nv1))
  | isNumericVal nv1 = pure nv1
-- E-Pred
eval1 (TmPred t1) = do
  t1' <- eval1 t1
  pure $ TmPred t1'
-- E-IszeroZero
eval1 (TmIsZero TmZero) = pure TmTrue
-- E-IszeroSucc
eval1 (TmIsZero (TmSucc nv1))
  | isNumericVal nv1 = pure TmFalse
-- E-Iszero
eval1 (TmIsZero t1) = do
  t1' <- eval1 t1
  pure $ TmIsZero t1'
-- No rule applies.
eval1 _ = Nothing

-- repeated small-step evaluation
eval :: Term -> Term
eval t = case eval1 t of
  Nothing -> t
  Just t' -> eval t'
  
-- Typing
typeOf :: Term -> Either String Ty
-- T-True
typeOf TmTrue = pure TyBool
-- T-False
typeOf TmFalse = pure TyBool
-- T-If
typeOf (TmIf t1 t2 t3) = do
  ty1 <- typeOf t1
  unless (ty1 == TyBool) (Left "condition must be of type Bool")
  ty2 <- typeOf t2
  ty3 <- typeOf t3
  unless (ty2 == ty3) (Left "arms of conditional have different types")
  pure ty2
-- T-Zero
typeOf TmZero = pure TyNat
-- T-Succ
typeOf (TmSucc t1) = do
  ty1 <- typeOf t1
  unless (ty1 == TyNat) (Left "succ must be applied to a Nat")
  pure TyNat
-- T-Pred
typeOf (TmPred t1) = do
  ty1 <- typeOf t1
  unless (ty1 == TyNat) (Left "pred must be applied to a Nat")
  pure TyNat
-- T-IsZero
typeOf (TmIsZero t1) = do
  ty1 <- typeOf t1
  unless (ty1 == TyNat) (Left "iszero must be applied to a Nat")
  pure TyBool

-- Parser for terms
termP :: Parser Term
termP = do
  trueP
  <|> falseP
  <|> zeroP
  <|> succP
  <|> predP
  <|> isZeroP
  <|> ifP
  <|> token (char '(') *> termP <* token (char ')')
  
trueP = symbol "true" *> pure TmTrue
falseP = symbol "false" *> pure TmFalse
zeroP = symbol "0" *> pure TmZero
succP = symbol "succ" *> (TmSucc <$> termP)
predP = symbol "pred" *> (TmPred <$> termP)
isZeroP = symbol "iszero" *> (TmIsZero <$> termP)
ifP = do
  symbol "if"
  t1 <- termP
  symbol "then"
  t2 <- termP
  symbol "else"
  t3 <- termP
  pure (TmIf t1 t2 t3)

             
---------------------------------------------------
evalAndPrint :: String -> IO ()
evalAndPrint s = do
  let r = parseString termP mempty s
  case r of
   Success t -> do
     print t
     case typeOf t of
      Right ty -> do
        putStrLn $ "Type: " ++ show ty
        putStrLn $ "Result: " ++ show (eval t)
      Left e -> putStrLn $ "Type error: " ++ e
   _ -> putStrLn "Parse error." 

main = do
  evalAndPrint "if iszero (pred 0) then 0 else succ 0"

  -- gets stuck
  evalAndPrint "if 0 then true else false"
  
  evalAndPrint "pred (pred (succ (if false then 0 else (succ (succ 0)))))"
