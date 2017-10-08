module Layer where

import Component

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude hiding (div)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

infixr 6 Tuple as ×
infixr 6 type Tuple as ×

type Rect  = Number × Number × Number × Number

data Layer b a = Layer Rect b (Layer b a)
               | Append (∀ c. Layer b c) (Layer b a)
               | Empty a 

mapLayerRect :: ∀ b a. (Rect -> Rect) -> Layer b a -> Layer b a
mapLayerRect f (Empty a) = Empty a
mapLayerRect f (Layer rect b children) = Layer (f rect) b (mapLayerRect f children)
mapLayerRect f (Append x y) = Append (mapLayerRect f x) (mapLayerRect f y)

type LayerCmp eff st a = Layer (Array (Component eff st) -> Component eff st) a

renderLayer :: ∀ eff st. LayerCmp eff st Unit -> Array (Component eff st)
renderLayer (Empty _) = []
renderLayer (Append x y) = renderLayer x <> renderLayer y
renderLayer (Layer rect b children) = [ b (renderLayer children) ]

instance functorLayer :: Functor (Layer b) where
  map f (Layer rect b children) = Layer rect b (map f children)
  map f (Append x y) = Append x (map f y)
  map f (Empty a) = Empty (f a)

instance applyLayer :: Apply (Layer b) where
  apply = undefined

instance applicativeLayer :: Applicative (Layer b) where
  pure = Empty

layerValue :: ∀ b a. Layer b a -> a
layerValue = undefined

instance bindLayer :: Bind (Layer b) where
  bind m f = Append (unsafeCoerce m) (f (layerValue m))

mvrect :: Rect -> Rect -> Rect
mvrect = undefined

layer :: ∀ b a. Rect -> b -> Layer b a -> Layer b a
layer rect b children = Layer rect b (mapLayerRect (mvrect rect) children)

rect :: ∀ eff st a. LayerCmp eff st a -> Maybe Rect
rect (Layer rect _ _) = Just rect
rect _ = Nothing

intersect :: ∀ eff st a. Rect -> LayerCmp eff st a -> Maybe (LayerCmp eff st a)
intersect = undefined

testM :: ∀ eff st. LayerCmp eff st (LayerCmp eff st Unit × LayerCmp eff st Unit)
testM = Layer (0.0 × 0.0 × 0.0 × 0.0) (div []) $ do
  a <- Layer (0.0 × 0.0 × 0.0 × 0.0) (div []) undefined
  b <- Layer (0.0 × 0.0 × 0.0 × 0.0) (div []) undefined
  pure (a × b)