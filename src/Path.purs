module Path (Path, PathElement(..), pathToString) where

import Data.Foldable
import Data.List
import Data.String
import Data.Tuple
import Prelude

data PathElement =
    TL Number Number
  | TR Number Number
  | BR Number Number
  | BL Number Number
  | H Number
  | V Number

type Path = Array PathElement

z :: String
z = "0"

p :: Number -> String
p = show

n :: Number -> String
n = (\x -> "-" <> show x)

pathToString :: Tuple Number Number -> Path -> String
pathToString (Tuple x y) path = "M " <> show x <> " " <> show y <> go true (fromFoldable path)
 where
   go st (Cons e es) = " " <> v <> go st' es
     where
       Tuple v st' = pe st e
   go _ Nil = ""

   -- true = horizontal, false = vertical
   pe :: Boolean -> PathElement -> Tuple String Boolean
   pe false (TL hr vr) = Tuple ("q " <> joinWith " " [ z, n vr, p hr, n vr ]) true
   pe true  (TL hr vr) = Tuple ("q " <> joinWith " " [ n hr, z, n hr, p vr]) false
   pe false (TR hr vr) = Tuple ("q " <> joinWith " " [ z, n vr, n hr, n vr ]) true
   pe true  (TR hr vr) = Tuple ("q " <> joinWith " " [ p hr, z, p hr, p vr ]) false
   pe false (BR hr vr) = Tuple ("q " <> joinWith " " [ z, p vr, n hr, p vr ]) true
   pe true  (BR hr vr) = Tuple ("q " <> joinWith " " [ p hr, z, p hr, n vr ]) false
   pe false (BL hr vr) = Tuple ("q " <> joinWith " " [ z, p vr, p hr, p vr ]) true
   pe true  (BL hr vr) = Tuple ("q " <> joinWith " " [ n hr, z, n hr, n vr ]) false
   pe _ (H h)          = Tuple ("h " <> show h) true
   pe _ (V v)          = Tuple ("v " <> show v) false
