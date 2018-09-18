import Text.Trifecta
import Control.Applicative

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

-- one-step small-step evaluation
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
  
-- big-step evaluation
bigEvalStep :: Term -> Maybe Term
-- B-Value
bigEvalStep v | isVal v = pure v
-- B-IfTrue, B-IfFalse
bigEvalStep (TmIf t1 t2 t3) = do
  v1 <- bigEvalStep t1
  case v1 of
   TmTrue -> bigEvalStep t2
   TmFalse -> bigEvalStep t3
   _ -> Nothing
-- B-Succ
bigEvalStep (TmSucc t1) = do
  nv1 <- bigEvalStep t1
  pure $ TmSucc nv1
-- B-PredZero, B-PredSucc
bigEvalStep (TmPred t1) = do
  t1' <- bigEvalStep t1
  case t1' of
   TmZero -> Just TmZero
   TmSucc nv1 -> Just nv1
   _ -> Nothing
-- B-IszeroZero, B-IszeroSucc
bigEvalStep (TmIsZero t1) = do
  t1' <- bigEvalStep t1
  case t1' of
   TmZero -> Just TmTrue
   TmSucc nv1 -> Just TmFalse
   _ -> Nothing
-- No rule applies
bigEvalStep _ = Nothing

bigEval :: Term -> Term
bigEval t = case bigEvalStep t of
  Just v
    | isVal v -> v
    | otherwise -> bigEval v
  Nothing -> t

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
evalAndPrint :: (Term -> Term) -> String -> IO ()
evalAndPrint ev s = do
  let r = parseString termP mempty s
  print r
  case r of
   Success t -> print (ev t)
   _ -> print "Error." 

evalTest = evalAndPrint eval

main = do
  print . bigEval $ TmIf (TmIsZero (TmPred TmZero)) TmZero (TmSucc TmZero)
  print . eval $ TmIf (TmIsZero (TmPred TmZero)) TmZero (TmSucc TmZero)

  evalAndPrint eval "if iszero (pred 0) then 0 else succ 0"
  evalAndPrint bigEval "if iszero (pred 0) then 0 else succ 0"

  -- gets stuck
  evalTest "if 0 then true else false"
  evalAndPrint bigEval "if 0 then true else false"
  
  evalTest "pred (pred (succ (if false then 0 else (succ (succ 0)))))"
