# Parser Combinators

Demonstration of parsing S expressions with parser combinators. For a detailed
explanation of what's going on see README-detailed.

```
import Parser

-- S expressions over atoms of type a
data S a = Atom a | S [S a]
data D = DI Integer | DS String | DNull -- dummy dynamic type

-- a parser for S expressions which uses a parser for atoms
sexpr :: Parser a -> Parser (S a)
sexpr atom = do
  whitespace
  char '('
  whitespace
  subs <- sepBy whitespace (fmap Atom atom <|> sexpr atom)
  whitespace
  char ')'
  return (S subs)

-- a parser for the dynamic type D
atom :: Parser D
atom =
  let sym1 = alpha <|> inClass "!@#$%^&*-+_=~|\\:;'<>/?.," in
  let sym = sym1 <|> digit in
  choice
    [ DI <$> (integer `notFollowedBy` sym1)
    , DNull <$ keyword "nil" sym
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
```

```
> parseSExp "()"
Right ()

> parseSExp "abc"
Right abc

> parseSExp "  (x   (    ) z    (w    9999    nil))   "
Right (x () z (w 9999 nil))

> parseSExp "  (x   (    ) z  )  (w    9999    nil))   "
Left "unexpected ')' expecting '('"
```

