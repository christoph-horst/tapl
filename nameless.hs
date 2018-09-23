import Data.Tuple (swap)
import Data.List (foldl1', elemIndex)
import Text.Trifecta
import Control.Applicative


data Term = TmVar String
          | TmAbs String Term
          | TmApp Term Term
          deriving (Show, Eq)

-- Parser for lambda terms
term :: Parser Term
term = foldl1' TmApp <$> some simpleTerm

simpleTerm :: Parser Term
simpleTerm = abstraction
             <|> variable
             <|> symbol "(" *> term <* symbol ")"
             
identifier :: Parser String
identifier = whiteSpace >> token ((:) <$> letter <*> many alphaNum)

variable :: Parser Term
variable = TmVar <$> identifier

abstraction :: Parser Term
abstraction = TmAbs <$> (symbol "lambda" *> identifier <* symbol ".") <*> term

-- nameless terms
data NTerm = NVar Int
           | NAbs NTerm
           | NApp NTerm NTerm
           deriving (Show, Eq)
                      
type Variable = String
type Context = [Variable]

removeNames :: Context -> Term -> NTerm
removeNames ctx (TmVar var) = case elemIndex var ctx of
  Nothing -> error $ "Unknown variable " ++ var
  Just idx -> NVar idx
removeNames ctx (TmAbs var t) = NAbs (removeNames (var:ctx) t)
removeNames ctx (TmApp t1 t2) = NApp (removeNames ctx t1) (removeNames ctx t2)

restoreNames :: Context -> NTerm -> Term
restoreNames ctx (NVar n) = TmVar (ctx !! n)
restoreNames ctx (NAbs t) = TmAbs x (restoreNames (x:ctx) t)
  where x = autoVar ctx
restoreNames ctx (NApp t1 t2) = TmApp (restoreNames ctx t1) (restoreNames ctx t2)

autoVar :: Context -> Variable
autoVar ctx = "$" ++ show (length ctx + 1)

printNTerm (NVar n) = show n
printNTerm (NAbs t) = "(\\." ++ printNTerm t ++ ")"
printNTerm (NApp t1 t2) = printNTerm t1 ++ " " ++ printNTerm t2

