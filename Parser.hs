{-# LANGUAGE LambdaCase #-}
module Parser where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Monoid
import Data.Char

type Error = String
newtype Parser a = Parser (String -> Either Error (a, String))

parse :: Parser a -> String -> Either Error (a, String)
parse (Parser p) s = p s

always :: a -> Parser a
always x = Parser $ \s -> Right (x, s)

apply :: Parser (a -> b) -> Parser a -> Parser b
apply (Parser pf) (Parser px) = Parser $ \s -> case pf s of
  Left e -> Left e
  Right (f, s') -> case px s' of
    Left e -> Left e
    Right (x, s'') -> Right (f x, s'')

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser px) f = Parser $ \s -> case px s of
  Left e -> Left e
  Right (x, s') -> let Parser py = f x in py s'

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left e -> Left e
    Right (x, s') -> Right (f x, s')

instance Applicative Parser where
  pure = always
  (<*>) = apply

instance Monad Parser where
  return = always
  (>>=) = bind
  fail msg = Parser $ \_ -> Left msg

instance Alternative Parser where
  empty = Parser $ \_ -> Left "parse failure"
  Parser p1 <|> Parser p2 = Parser $ \s -> case p1 s of
    Left e -> p2 s
    answer -> answer

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
  mappend p1 p2 = liftA2 mappend p1 p2

char :: Char -> Parser Char
char c = Parser $ \s -> case s of
  "" -> Left ("expected " ++ show c ++ " not end of input")
  (c':cs) | c == c'   -> Right (c, cs)
          | otherwise -> Left ("expected " ++ show c ++ " not " ++ show c')

string :: String -> Parser String
string s = mapM char s

anyChar :: Parser Char
anyChar = Parser $ \case
  ""   -> Left "expected any char not end of input"
  c:cs -> Right (c, cs)

endOfInput :: Parser ()
endOfInput = Parser $ \case
  "" -> Right ((), "")
  s  -> Left ("expected end of input not " ++ show (take 10 s))

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Left "choice: no parsers given"
choice [p] = p
choice (p:ps) = p <|> choice ps

option :: Parser a -> Parser (Maybe a)
option (Parser p) = Parser $ \s -> case p s of
  Left e -> Right (Nothing, s)
  Right (x, s') -> Right (Just x, s')

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = go where
  go = do
    optional item >>= \case
      Nothing -> return []
      Just x -> optional sep >>= \case
        Nothing -> return [x]
        Just _ -> do
          xs <- go
          return (x:xs)

whitespace :: Parser ()
whitespace = many (char ' ') >> return ()

satisfy :: (Char -> Bool) -> Parser Char
satisfy check = Parser $ \case
  "" -> Left "expected char not end of input"
  (c:cs) -> if check c
    then Right (c, cs)
    else Left (show c ++ " failed to satisfy check")

inClass :: [Char] -> Parser Char
inClass chars = satisfy (`elem` chars)

takeWhile :: (Char -> Bool) -> Parser [Char]
takeWhile f = optional (satisfy f) >>= \case
  Nothing -> return []
  Just c -> do
    cs <- takeWhile f
    return (c:cs)

takeWhile1 f = takeWhile f >>= \case
  [] -> fail "takeWhile1 failed"
  xs -> return xs

notIn :: [Char] -> Parser Char
notIn chars = satisfy (\c -> not (elem c chars))

integer :: Parser Integer
integer = go where
  go = optional (char '-') >>= \case
    Nothing -> go' id
    Just _  -> go' negate
  go' sign = sign . read <$> takeWhile1 isDigit

many1 :: Parser a -> Parser [a]
many1 p = many p >>= \case
  [] -> fail "many1 failed"
  xs -> return xs

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = alpha <|> digit

digit :: Parser Char
digit = satisfy isDigit

getInput :: Parser String
getInput = Parser $ \s -> Right (s, s)

hypothetically :: Parser a -> Parser (Maybe a)
hypothetically p = do
  result <- parse p <$> getInput
  case result of
    Left _ -> return Nothing
    Right (x,_) -> return (Just x)

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy p fol = do
  x <- p
  hypothetically fol >>= \case
    Nothing -> return x
    Just _ -> fail "unexpectedly followed by"

