module Path where

import Data.Foldable
import Data.List
import Data.String
import Data.Tuple
import Prelude

data PathElement =
    TL
  | TR
  | BR
  | BL
  | H Number
  | V Number

infixr 6 BL as └
infixr 6 BR as ┘
infixr 6 TR as ┐
infixr 6 TL as ┌ 

type Path = Array PathElement

r :: String
r = "15"

nr :: String
nr = "-" <> r

z :: String
z = "0"

pathToString :: Tuple Number Number -> Path -> String
pathToString (Tuple x y) p = "M " <> show x <> " " <> show y <> go true (fromFoldable p)
 where
   go st (Cons e es) = " " <> v <> go st' es
     where
       Tuple v st' = pe st e
   go _ Nil = ""

   -- true = horizontal, false = vertical
   pe :: Boolean -> PathElement -> Tuple String Boolean
   pe false TL    = Tuple ("q " <> joinWith " " [ z, nr, r, nr ]) true
   pe true  TL    = Tuple ("q " <> joinWith " " [ nr, z, nr, r ]) false
   pe false TR    = Tuple ("q " <> joinWith " " [ z, nr, nr, nr ]) true
   pe true  TR    = Tuple ("q " <> joinWith " " [ r, z, r, r ]) false
   pe false BR    = Tuple ("q " <> joinWith " " [ z, r, nr, r ]) true
   pe true  BR    = Tuple ("q " <> joinWith " " [ r, z, r, nr ]) false
   pe false BL    = Tuple ("q " <> joinWith " " [ z, r, r, r ]) true
   pe true  BL    = Tuple ("q " <> joinWith " " [ nr, z, nr, nr ]) false
   pe _ (H h)     = Tuple ("h " <> show h) true
   pe _ (V v)     = Tuple ("v " <> show v) false
