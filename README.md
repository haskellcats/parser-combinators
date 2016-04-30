# Parser Combinators

Parsing is sometimes considered a difficult programming task. But there
is a functional programming technique which makes parsing surprisingly
simple. This method, known simply as parser combinators or combinator parsers,
relies on two things:

- simple, perhaps trivial, functions to parse characters in an input stream
- a library of ways to combine simple and not-so-simple parsers to form more
complex parsers.

I will develop an incredibly simplistic set of parsers to demonstrate the
technique, and use it to parse S expressions. 

## Parsers

There is a classic Dr. Seuss-inspired image which explains the basic idea of
how a functional parser should work. I will simplify it even further to the
following type.

```
type Parser a = String -> Either Error (a, String)
```

A parser is a function that takes an input stream and returns either an
error message or the successfully parsed value along with the remaining input.
The type is parameterized by the type of value that we are trying to extract
from the input stream. In what follows the type Error will be a synonym for
String but in real life you can expand it to include line numbers and other
information.

We can immediately define some trivial parsers.

```
failure :: Error -> Parser a
failure e s = Left e

success :: a -> Parser a
success x s = Right (x, s)
```

The failure parser ignores the input and unconditionally bails out. The success
parser is pre-equipped to succeed by returning some value. Also success does not
consume any of the input stream. These might seem pointless but they will come
in handy later.

To demonstrate a parser that actually does something, the next simplest parser
is one that does a Bool test on the next char. If the test passes then that
char is returned as the parsed value.

```
satisfy :: (Char -> Error) -> (Char -> Bool) -> Parser Char
satisfy msg test ""     = Left "unexpected end of input"
satisfy msg test (c:cs) = if test c then Right (c,cs) else Left (msg c)

inClass :: [Char] -> Parser Char
inClass chars = satisfy msg (`elem` chars) where
  msg = ("unexpected "++) . (++" expecting one of "++ chars) . show

> inClass "!@#$" "#??"
Right ('#', "??")

> inClass "!@#$" "???"
Left "unexpected '?' expecting one of !@#$"
```

Great, now we're parsing! Let's create some more primitives before moving on.
I'll omit the code because it's trivial and available in the repo.

```
eof        :: Parser ()              -- succeeds if no more input, otherwise fails
char       :: Char -> Parser Char    -- succeeds only on particular next char
notInClass :: [Char] -> Parser Char  -- negation of inClass
anyChar    :: Parser Char            -- only fail if no more input
space      :: Parser ()              -- succeeds only on whitespace
```

But we want to a) use the remaining input somehow to parse more data b) output
data structures more interesting than just Char and c) construct new parsers
using existing parsers to implement new parsing strategies. How?

## Functor

First we will make it more convenient to output something other than a Char.
One obvious way is to convert the output char to something else with a function.

```
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p s = case p s of
  Left e -> Left e
  Right (x, s') = Right (f x, s')

> mapParser digitToInt (satisfy isDigit) "4321"
Right (4, "321")
```

Now you can modify the output of a parser without disturbing the failure
behavior. This kind of mapping is very handy and there is a standard typeclass
for it.

```
instance Functor Parser where
  fmap = mapParser

f <$> p = fmap f p -- standard operator version of fmap
```

Functor has such well-behaved laws (not enforced by the language) that the
code for any suitable Functor type's fmap can be automatically derived by GHC.
Actually a lot of parsing support I will mention is obtained for free by
implementing the appropriate typeclasses. More on that later.

## Sequencing Parsers

That was pretty easy, so now to figure out how to parse two things in a row and
combine them.

```
fuse :: (a -> a -> a) -> Parser a -> Parser a -> Parser a
fuse f p1 p2 s = case p1 s of
  Left e -> Left e
  Right (x, s') -> case p2 s' of
    Left e -> Left e
    Right (y, s'') -> Right (f x y, s'')

> fuse (\x -> [x,x]) (char 'A') (char 'B') "ABCD"
Right ("AB", "CD")

> fuse (\x -> [x,x]) (char 'A') (char 'B') "Apple: bogus data"
Left "unexpected 'p' expecting 'B'"
```

Alright that successfully bailed out after succesfully reading an 'A' and then
encountering the wrong stuff. Immediately we can see some problems with the 
utility of fuse. The combining function is limited to outputting values of the
same type
