module Example where

import Control.Applicative
import Parser
import SExpr

sexpr :: Parser a -> Parser (S a)
sexpr atom = do
  whitespace
  char '('
  whitespace
  subs <- sepBy whitespace (fmap Atom atom <|> sexpr atom)
  whitespace
  char ')'
  return (S subs)

atom :: Parser D
atom =
  let sym1 = alpha <|> inClass "!@#$%^&*-+_=~|\\:;'<>/?.," in
  let sym = sym1 <|> digit in
  choice
    [ DI <$> integer `notFollowedBy` sym1
    , DNull <$ string "nil" `notFollowedBy` sym
    , DS <$> liftA2 (:) sym1 (many sym)
    ]

parser :: Parser (S D)
parser = do
  whitespace
  exp <- fmap Atom atom <|> sexpr atom
  whitespace
  endOfInput
  return exp

parseSExp :: String -> Either Error (S D)
parseSExp = fmap fst . parse parser
