
module Parsing where

import Exp
import Lab2
import Control.Applicative (some, many, (<|>))
import Data.Char (isAlpha, isAlphaNum)
--import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Language
 --   ( haskellStyle, LanguageDef )
--import Text.ParserCombinators.Parsec.Token


parseFirst :: Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_): _ -> Just a

var :: Parser Var
var = Var <$> identifier p1 p2 
  where
    p1 = satisfy isAlpha 
    p2 = satisfy isAlphaNum

-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})

varExp :: Parser ComplexExp
varExp = CX <$> var

-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))

lambdaExp :: Parser ComplexExp
lambdaExp =
    do b <- reserved "\\"
       v <- var
       s <- reserved "->"
       e <- expr
       return (CLam v e)
-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))

parenExp :: Parser ComplexExp
parenExp = parens expr
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))

basicExp :: Parser ComplexExp
basicExp = parenExp <|> lambdaExp <|> varExp
-- >>> parseFirst basicExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]) // ex gresit

appExpr :: Parser ComplexExp
appExpr = 
    do 
        e1 <- basicExp
        e2 <- basicExp
        rest <- many basicExp
        return (foldl CApp (CApp e1 e2) rest )

expr :: Parser ComplexExp
expr = basicExp <|> appExpr 
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))

exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))