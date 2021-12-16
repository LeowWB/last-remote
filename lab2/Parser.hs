module Parser where

import Text.Parsec
import Lexer
import AST

type Parser = Parsec [(SourcePos, Token)] ()

tokenP :: (Token -> Maybe a) -> Parser a
tokenP test = token show fst (test . snd)

symbol :: String -> Parser ()
symbol c = tokenP (\t -> case t of
  TSym s -> if s == c then Just () else Nothing
  _ -> Nothing)

---------------------------------------------------------------------------------------------------------------------------------------
stringP :: Parser String
stringP = do
    name <- tokenP (\t -> case t of
                        (TName s) -> Just (Atom s)
                        _ -> Nothing)
    case name of -- parser consumes name which can be of atom or functor
      (Atom a) -> return a

-- functor(term1,term2,term3)
functorP :: Parser (String, [Term]) --this gives u back a string (functor name) and a list of terms (functor args)
functorP = stringP ~~ ((symbol "(") *> ((sepBy termP (symbol ",")) <* (symbol ")")))
---------------------------------------------------------------------------------------------------------------------------------------

termP :: Parser Term
termP = do
    name <- tokenP (\t -> case t of
                        (TName s) -> Just (Atom s)
                        (TVar s) -> Just (Var s)
                        _ -> Nothing)
    case name of -- parser consumes name which can be of atom or functor
      (Atom a) -> (fmap (Func a) . between (symbol "(") (symbol ")")
                 . flip sepBy1 (symbol ",") $ termP) <|> return name
      _ -> return name

{- parse a relation or cut in body of clause -}
relP :: Parser Rel
relP = (symbol "!" *> return Cut) --if it's a ! then obv it's a cut oper8r
       <|> relHeadP --else just use the same rules as relHeadP

{- parse a relation in head of clause -}
relHeadP :: Parser Rel
relHeadP = fmap (uncurry Rel) functorP --the relation in the head of a clause is basically just a functor

---------------------------------------------------------------------------------------------------------------------------------------
--rule has form head:-body. or i guess it could just be head.

(~~) ph1 ph2 = do
  tup1 <- ph1
  tup2 <- ph2
  return (tup1, tup2)

ruleBodyP :: Parser (Rel, [[Rel]])
ruleBodyP = relP ~~ (  ( option [[]] ((symbol ":-") *> (sepBy (many1 relP) (symbol ",")))   ) <* (symbol "."))

ruleP :: Parser Rule
ruleP = fmap (uncurry Rule) ruleBodyP
---------------------------------------------------------------------------------------------------------------------------------------

programP :: Parser Program
programP = fmap Program $ many ruleP --a program is made of many rules. so we just take (many ruleP) and apply it throughout the program

parseProgram :: String -> Either ParseError Program
parseProgram source = do
  tokens  <- parse (tokensL   <* eof) "" source --to get the tokens, i parse "source" according to the rules defined by "tokensL"
  parse (programP <* eof) "" tokens --then the output is the result of parsing the tokens according to the rules of "programP"

parseRel :: String -> Either ParseError Rel
parseRel source = do
  tokens  <- parse (tokensL   <* eof) "" source --to get the tokens, i parse source according to the rules defined by tokensL
  parse (relHeadP <* (symbol ".") <* eof) "" tokens --then the output is the result of parsing tokens according to the rules of relHeadP.
  --the extra (symbol ".") above is because i'm expecting a "." at the end of the line. so it's like string.split by "."
