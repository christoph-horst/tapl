import Data.Maybe
import Data.List (foldl1', elemIndex)
import Text.Trifecta
import Control.Applicative
import Control.Monad (guard)

-- Term type (lambda terms)
data Term = TmVar Int Int  -- de Bruijn index, total context length
          | TmAbs String Term -- with name hint
          | TmApp Term Term
          deriving (Show, Eq)

data Binding = NameBind deriving (Show, Eq)
type Context = [(String, Binding)]

printTerm :: Context -> Term -> String
printTerm ctx (TmAbs x t1) =
  let (ctx', x') = freshName ctx x
  in "(lambda " ++ x' ++ ". " ++ printTerm ctx' t1 ++ ")"
printTerm ctx (TmApp t1 t2) =
  "(" ++ printTerm ctx t1 ++ " " ++ printTerm ctx t2 ++ ")"
printTerm ctx (TmVar x n) =
  if length ctx == n
  then fst (ctx !! x)
  else "[bad context]"

-- pick an unused name based on the given hint
freshName :: Context -> String -> (Context, String)
freshName ctx hint =
  let n = head (filter notInCtx rawCandidates)
  in ((n, NameBind):ctx, n)
     where rawCandidates = (hint ++) <$> iterate ('\'' :) ""
           notInCtx n = not $ any (\(s, _) -> s == n) ctx

-- the "shift t up by d (above cutoff c, which is 0 initially)" operation
termShift :: Int -> Term -> Term           
termShift d t = shift 0 t
  where shift c (TmVar x n) =
          if x >= c
          then TmVar (x+d) (n+d)
          else TmVar x (n+d)
        shift c (TmAbs x t1) = TmAbs x (shift (c+1) t1)
        shift c (TmApp t1 t2) = TmApp (shift c t1) (shift c t2)

-- substitute the term s for the variable with index j in term t
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = go 0 t
  where go c v@(TmVar x n) =
          if x == j + c
          then termShift c s
          else v
        go c (TmAbs x t1) = TmAbs x (go (c+1) t1)
        go c (TmApp t1 t2) = TmApp (go c t1) (go c t2)

termSubstTop :: Term -> Term -> Term        
termSubstTop s t =
  termShift (-1) $ termSubst 0 (termShift 1 s) t

-- evaluation
isVal ctx (TmAbs _ _) = True
isVal ctx _ = False

-- single-step evaluation (call-by-value)
eval1 :: Context -> Term -> Maybe Term
-- E-AppAbs
eval1 ctx (TmApp (TmAbs x t12) v2)
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

-- big-step evaluation
bigEval1 :: Context -> Term -> Maybe Term
bigEval1 ctx t@(TmAbs _ _) = Just t
bigEval1 ctx (TmApp t1 t2) = do
  t1' <- bigEval1 ctx t1
  case t1' of
   TmAbs x t12 -> do
     t2' <- bigEval1 ctx t2
     guard (isVal ctx t2')
     v <- bigEval1 ctx (termSubstTop t2' t12)
     guard (isVal ctx v)
     pure v
   _ -> Nothing
bigEval1 ctx _ = Nothing


bigEval :: Context -> Term -> Term
bigEval ctx t = case bigEval1 ctx t of
  Just v
    | isVal ctx v -> v
    | otherwise -> bigEval ctx v
  Nothing -> t
  
-- parser
-- named terms ("lambda" terms)
data LTerm = LVar String
           | LAbs String LTerm
           | LApp LTerm LTerm
           deriving (Show, Eq)

term :: Parser LTerm
term = foldl1' LApp <$> some simpleTerm

simpleTerm :: Parser LTerm
simpleTerm = abstraction
             <|> variable
             <|> symbol "(" *> term <* symbol ")"
             
identifier :: Parser String
identifier = whiteSpace >> token ((:) <$> letter <*> many alphaNum)

variable :: Parser LTerm
variable = LVar <$> identifier

abstraction :: Parser LTerm
abstraction = LAbs <$> (symbol "lambda" *> identifier <* symbol ".") <*> term

removeNames :: Context -> LTerm -> Term
removeNames ctx (LVar var) = case elemIndex (var, NameBind) ctx of
  Nothing -> error $ "Unknown variable " ++ var
  Just idx -> TmVar idx (length ctx)
removeNames ctx (LAbs var t) = TmAbs var (removeNames ((var, NameBind):ctx) t)
removeNames ctx (LApp t1 t2) = TmApp (removeNames ctx t1) (removeNames ctx t2)


-- examples
x = TmVar 0 0
idL = TmAbs "x" (TmVar 0 0)
expr = TmApp idL x
y = TmApp (TmAbs "x" (TmAbs "y" (TmVar 1 2))) (TmAbs "z" (TmVar 0 1))

testEval str = case parseString term mempty str of
                Success t -> do
                  let nt = removeNames [] t
                  let small = eval [] nt
                  let big = bigEval [] nt
                  
                  print small
                  print big
                  print (printTerm [] big)
                _ -> print "Error"


main = do
  --testEval "(lambda x. lambda y. x) (lambda z. z)"
  testEval "(lambda m. lambda n. lambda s. lambda z. m s (n s z)) (lambda s. lambda z. s z) (lambda s. lambda z. s (s (s z)))"
    
