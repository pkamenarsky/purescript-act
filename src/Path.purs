module Path where

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

radius :: String
radius = "15"

pathToString :: Path -> String
pathToString = joinWith " " <<< map pe
 where
   pe TL    = "q 0 -" <> radius <> " " <> radius <> " -" <> radius
   pe TR    = "q " <> radius <> " 0 " <> radius <> " " <> radius
   pe BR    = "q 0 " <> radius <> " -" <> radius <> " " <> radius
   pe BL    = "q -" <> radius <> " 0 -" <> radius <> " " <> radius
   pe (H h) = "h " <> show h
   pe (V v) = "v " <> show v
