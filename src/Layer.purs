module Layer where

import Component

import Data.Tuple (Tuple(..))
import Prelude hiding (div)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

infixr 6 Tuple as ×
infixr 6 type Tuple as ×

type Rect  = Number × Number × Number × Number

data Layer a = Layer Rect a (Array (Layer a))

layer :: forall a. Rect -> a -> Array (Layer a) -> Layer a
layer = Layer

data LayerM b a = LayerM Rect b (LayerM b a) | Empty a | Append (forall c. LayerM b c) (LayerM b a)

mapLayerMRect :: forall b a. (Rect -> Rect) -> LayerM b a -> LayerM b a
mapLayerMRect f (Empty a) = Empty a
mapLayerMRect f (LayerM rect b children) = LayerM (f rect) b (mapLayerMRect f children)
mapLayerMRect f (Append x y) = Append (mapLayerMRect f x) (mapLayerMRect f y)

instance functorLayer :: Functor (LayerM b) where
  map = undefined

instance applyLayer :: Apply (LayerM b) where
  apply = undefined

instance applicativeLayer :: Applicative (LayerM b) where
  pure = Empty

layerValue :: forall b a. LayerM b a -> a
layerValue = undefined

instance bindLayer :: Bind (LayerM b) where
  bind m f = Append (unsafeCoerce m) (f (layerValue m))

type LayerCmpM eff st a = LayerM (Array (Component eff st) -> Component eff st) a

data LayerRef

mvrect :: Rect -> Rect -> Rect
mvrect = undefined

layerM :: forall b a. Rect -> b -> LayerM b a -> LayerM b a
layerM rect b children = LayerM rect b (mapLayerMRect (mvrect rect) children)

rect :: forall eff st a. LayerCmpM eff st a -> LayerCmpM eff st Rect
rect _ = undefined

testM :: forall eff st. LayerCmpM eff st (LayerRef × LayerRef)
testM = layerM (0.0 × 0.0 × 0.0 × 0.0) (div []) $ do
  a <- layerM (0.0 × 0.0 × 0.0 × 0.0) (div []) undefined
  b <- layerM (0.0 × 0.0 × 0.0 × 0.0) (div []) undefined
  pure (a × b)

--------------------------------------------------------------------------------

type LayerCmp eff st = Layer (Array (Component eff st) -> Component eff st)

test :: forall eff st. LayerCmp eff st
test = layer (0.0 × 0.0 × 0.0 × 0.0) (div [])
  [ layer (0.0 × 0.0 × 0.0 × 0.0) (const $ text "bla") []
  , layer (0.0 × 0.0 × 0.0 × 0.0) (div []) []
  ]

cmpFromLayerCmp :: forall eff st. LayerCmp eff st -> Component eff st
cmpFromLayerCmp (Layer _ a children) = a (map cmpFromLayerCmp children)