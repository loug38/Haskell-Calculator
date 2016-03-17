---------------------
--ligeorge Azpeitia
--Program: hw5.hs
--Authors: George Azpeitia and Lou Goerge
--On this homework, we worked together for 5.5 hours,
--Lou worked independently for 0 hour,
--and George worked independently for 0 hours.
----------------------

import Data.Char
import Control.Monad
import Control.Applicative (Applicative,pure,(<*>))

-- Parser Framework

data Parser a = Parser (String -> [(a,String)])

run :: Parser a -> String -> [(a, String)]
run (Parser f) s = f s

satP :: (Char -> Bool) -> Parser Char
satP pred = Parser (\cs -> case cs of
                            []    -> []
                            c:cs' -> if pred c then [(c,cs')] else [])

digit = satP isDigit

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  pa >>= fpb = Parser (\cs -> do (a,cs') <- run pa cs
                                 run (fpb a) cs')

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\cs -> run p2 cs ++ run p1 cs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = return [] <|> oneOrMore p

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p
                 xs <- zeroOrMore p
                 return (x:xs)

first :: Parser a -> Parser a
first p = Parser (\cs -> case run p cs of
                          [] -> []
                          (r:rs) -> [r])

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p opp = do x <- p
                 tryMore x
  where tryMore x = first (return x <|> more x)
        more x = do op <- opp
                    y <- p
                    tryMore (op x y)

-- Calculator
{-
intP :: Parser Int
intP = do digits <- first (oneOrMore digit)
          return (read digits)

addOp :: Parser (Int -> Int -> Int)
addOp = do satP (== '+') ; return (+)
    <|> do satP (== '-') ; return (-)

mulOp :: Parser (Int -> Int -> Int)
mulOp = do satP (== '*') ; return (*)
    <|> do satP (== '/') ; return div

calc :: Parser Int
calc = let mulExpr = chain intP mulOp
       in  chain mulExpr addOp
-}

-- Doubles
doubleP :: Parser Double
doubleP = do first (do  digits <- first (oneOrMore digit)
                        return (read (digits ++ ".0"))
                 <|> do digits <- first (oneOrMore digit)
                        satP (=='.')
                        secondDigits <- first(oneOrMore digit)
                        return (read (digits ++ "." ++ secondDigits)))

addOp :: Parser (Double -> Double -> Double)
addOp = do satP (== '+') ; return (+)
     <|> do satP (== '-') ; return (-)

mulOp :: Parser (Double -> Double -> Double)
mulOp = do satP (== '*') ; return (*)
     <|> do satP (== '/') ; return (/)
 
calc :: Parser Double
calc = let mulExpr = chain doubleP mulOp
       in  chain mulExpr addOp

-- Exponentiation
expO :: Parser (Double -> Double -> Double)
expO = do satP (== '^') ; return (**)

-- Parenthesis
paren :: Parser (Double)
paren = do satP (== '(')
           value <- calc3
           satP (== ')')
           return value

calc2 :: Parser Double
calc2 = let mulExpr = chain doubleP expO
            expExpr = chain mulExpr mulOp
        in chain expExpr addOp

calc3 :: Parser Double
calc3 = do  let mulExpr = chain (doubleP <|> paren) expO
                expExpr = chain mulExpr mulOp
            value <- chain expExpr addOp
            return value





