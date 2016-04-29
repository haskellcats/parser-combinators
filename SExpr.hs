module SExpr where

import Data.List

data S a = Atom a | S [S a]
data D = DI Integer | DS String | DNull

instance Show D where
  show (DI i) = show i
  show (DS s) = s
  show DNull = "nil"

instance Show a => Show (S a) where
  show (Atom x) = show x
  show (S xs) = "(" ++ concat (intersperse " " (map show xs)) ++ ")"
