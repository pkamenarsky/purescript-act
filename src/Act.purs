module Act where

import Control.Alternative
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free
import Data.Argonaut.Core
import Data.Array
import Data.Either
import Data.Functor
import Data.Exists
import Data.Generic
import Data.Traversable
import Data.Lens
import Data.Maybe
import Data.Lens.Index
import Data.Tuple
import Type.Proxy
import Unsafe.Coerce
import Match
import Undefined
import Component
import Trace
import DOM as D
import DOM.Node.Types as D
import Data.Array as A
import Data.Int as I
import Data.List as L
import Data.Map as M
import React as R
import React.DOM as R
import React.DOM.Props as P
import React.DOM.SVG as SVG
import ReactDOM as RD
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import React (transformState)
import Prelude hiding (div)

--------------------------------------------------------------------------------

data DragState =
  DragConn
    { start :: Vec
    , end   :: Vec
    }
  | DragHOC
    { hoc   :: RType
    , label :: Label
    , pos   :: Vec
    }

type AppState =
  { debug     :: String
  , dragState :: Maybe DragState
  , rtype     :: RType
  , substs    :: L.List Substitution
  }

_substs :: Lens' AppState (L.List Substitution)
_substs = lens (_.substs) (\st s -> st { substs = s })

_const :: forall st a. a -> Lens' st a
_const a = lens (const a) (\st s -> st)

emptyAppState :: AppState
emptyAppState =
  { debug     : "Debug: "
  , dragState : Nothing
  , rtype     : componentType
  , substs    : L.Nil
  }

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui')
  where ui' = R.createFactory (R.createClass (mkSpec emptyAppState ui)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

ui :: forall eff. Component eff AppState
ui = state \st -> div
 []
 [ svg [ shapeRendering "geometricPrecision", width "2000px", height "600px" ]
   $ [ snapValue $ typeComponent st M.empty (200.5 × 100.5 × 1000.0 × 400.0) _substs type2
     ]
  <> case st.dragState of
       -- Just (DragConn ds) -> [ line ds.start ds.end ]
       Just (DragHOC { hoc, label, pos: (px × py) }) -> [ snapValue $ typeComponent st M.empty ((px + 0.5) × (py + 0.5) × 200.0 × 100.0) (_const L.Nil) hoc ]
       _ -> []
 , state \st -> code [] [ text $ show st.substs ]
 , state \st -> code [] [ text st.debug ]
 ]

--------------------------------------------------------------------------------

arrayRange :: forall a. Array a -> Array Int
arrayRange a = 0 .. (length a - 1)

indexedRange :: forall a. Array a -> Array (Int × a)
indexedRange a = zip (arrayRange a) a

type Vec  = Number × Number
type Size = Number × Number
type Rect = Number × Number × Number × Number

px :: Number -> String
px x = show x <> "px"

line :: forall eff st. Vec -> Vec -> Component eff st
line (sx × sy) (ex × ey) = path [ strokeWidth (px 3.0), stroke "#d90e59", d ("M" <> show sx <> " " <> show sy <> " L" <> show ex <> " " <> show ey)] []

label :: forall eff st. Vec -> String -> String -> Component eff st
label (vx × vy) align str = svgtext [ textAnchor align, fontFamily "Helvetica Neue", fontWeight "700", fontSize "14px", fill "#d90e59", x (px vx), y (px vy) ] [ text str ]

type Label = String

intersect :: Rect -> Rect -> Boolean
intersect (rx × ry × rw × rh) (sx × sy × sw × sh)
  | sx + sw < rx = false
  | sy + sh < ry = false
  | sx >= rx + rw = false
  | sy >= ry + rh = false
  | otherwise = true

inside :: Vec -> Rect -> Boolean
inside (vx × vy) (rx × ry × rw × rh)
  | vx < rx = false
  | vy < ry = false
  | vx >= rx + rw = false
  | vy >= ry + rh = false
  | otherwise = true

inradius :: Number -> Vec -> Vec -> Boolean
inradius r (ox × oy) (vx × vy) = dx * dx + dy * dy <= r * r
  where
    dx = vx - ox
    dy = vy - oy

firstJustL :: forall a b. L.List a -> (a -> Maybe b) -> Maybe b
firstJustL as f = go as
  where
    go (L.Cons a as)
      | Just a' <- f a = Just a'
      | otherwise      = go as
    go L.Nil = Nothing

firstJust :: forall a b. Array a -> (a -> Maybe b) -> Maybe b
firstJust as f = go (L.fromFoldable as)
  where
    go (L.Cons a as)
      | Just a' <- f a = Just a'
      | otherwise      = go as
    go L.Nil = Nothing

uirect :: forall eff st. Rect -> Component eff st
uirect (bx × by × bw × bh)
  | bw > 0.0 && bh > 0.0 = rect [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", fill "transparent" ] []
  | otherwise            = g [] []

uirectDashed :: forall eff st. Rect -> Component eff st
uirectDashed (bx × by × bw × bh)
  | bw > 0.0 && bh > 0.0 = rect [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", strokeDashArray "5, 5", fill "transparent" ] []
  | otherwise            = g [] []

data UILabel = UILabelLeft String | UILabelRight String

uicircle :: forall eff st. Vec -> UILabel -> Component eff st
uicircle (x' × y') label' = g [] $
  [ circle [ cx (px (x' + offset label')), cy (px y'), r (px 5.0), fill "transparent", stroke "#d90e59", strokeWidth (px 3.0) ] []
  , uilabel label'
  ]
  where
    offset (UILabelLeft _) = -60.0
    offset (UILabelRight _) = 60.0

    uilabel (UILabelLeft str)  = label (x' + 20.0 × y' + 4.0) "end" str
    uilabel (UILabelRight str) = label (x' - 20.0 × y' + 4.0) "start" str

tn :: Int -> Number
tn = I.toNumber

gap :: Number
gap = 30.0

subdivide :: forall eff st a. Rect -> Array a -> (Rect -> a -> Component eff st) -> Component eff st
subdivide (bx × by × bw × bh) as f = g [] $
  flip map (indexedRange as) \(i × a) -> f (cx (tn i) × (by + gap) × cw × (bh - 2.0 * gap)) a
  where
     count = A.length as
     cx i  = bx + (i + 1.0) * gap + i * cw
     cw    = (bw - (gap * (tn count + 1.0))) / tn count
     cy    = by + gap

subdivide' :: forall eff st a b c. Rect -> (Rect -> Rect) -> Array a -> (Rect -> Int -> a -> SnapM c b (Component eff st)) -> SnapM c b (Array (Component eff st))
subdivide' (bx × by × bw × bh) snapf as f = flip traverse (indexedRange as) \(i × a) -> f (cx (tn i) × (by + gap) × cw × (bh - 2.0 * gap)) i a
   where
      count     = A.length as
      cx i      = bx + (i + 1.0) * gap + i * cw
      cw        = (bw - (gap * (tn count + 1.0))) / tn count
      cy        = by + gap

shrink :: Rect -> Rect -> Rect
shrink (sl × st × sr × sb) (rx × ry × rw × rh) = ((rx + sl) × (ry + st) × (rw - (sl + sr)) × (rh - (st + sb)))

meToV :: R.MouseEvent -> Vec
meToV { pageX, pageY } = pageX × pageY

--------------------------------------------------------------------------------

isHOC :: RType -> Boolean
isHOC (RFun _ (RConst (Const "Component"))) = true
isHOC _ = false

zipMaybe :: forall a b. L.List a -> L.List b -> L.List (Maybe a × b)
zipMaybe (L.Cons a as) (L.Cons b bs) = L.Cons (Just a × b) (zipMaybe as bs)
zipMaybe L.Nil (L.Cons b bs) = L.Cons (Nothing × b) (zipMaybe L.Nil bs)
zipMaybe _ L.Nil = L.Nil

type Context = M.Map Label Vec

-- TODO: Product (Kleisli c (Maybe b)) (Identity a)
data SnapM c b a = SnapM (c -> Maybe b) a

instance applicativeSnapM :: Applicative (SnapM c b) where
  pure a = SnapM (const Nothing) a

instance functorSnapM :: Functor (SnapM c b) where
  map f (SnapM s a) = SnapM s (f a)

instance applySnapM :: Apply (SnapM c b) where
  apply (SnapM s f) (SnapM s' a) = SnapM (\r -> s r <|> s' r) (f a)

instance bindSnapM :: Bind (SnapM c b) where
  bind (SnapM s a) f = SnapM (\r -> s r <|> s' r) a'
    where
      SnapM s' a' = f a

type SnapComponent' t  = SnapM Rect (Label -> RType -> AppState -> AppState) t
type SnapComponent eff = SnapComponent' (Component eff AppState)

snappable :: forall a b c. (c -> Maybe b) -> a -> SnapM c b a
snappable f a = SnapM f a

snappableRect :: forall a b. Rect -> b -> a -> SnapM Rect b a
snappableRect bounds b a = SnapM (\bounds' -> if intersect bounds bounds' then (Just b) else Nothing) a

snap :: forall a b c. SnapM c b a -> c -> Maybe b
snap (SnapM f _) c = f c

snapValue :: forall a b c. SnapM c b a -> a
snapValue (SnapM _ a) = a

--------------------------------------------------------------------------------

typeComponent :: forall eff. AppState -> Context -> Rect -> Lens' AppState (L.List Substitution) -> RType -> SnapComponent eff
typeComponent st ctx r ss t = typeComponent' t ctx r ss t
  where
    typeComponent' :: RType -> Context -> Rect -> Lens' AppState (L.List Substitution) -> RType -> SnapComponent eff
    typeComponent' tt ctx bounds@(bx × by × bw × bh) substs rtype
      | Just (incTypes × chTypes) <- extract rtype = do
          children' <- children
          pure $ g [] $ concat
            [ [ uirect bounds ]
            , inc (A.fromFoldable incTypes)
            , children'
            ]
      where
        childMargin = ((8.0 * gap) × (1.0 * gap) × gap × gap)

        inc :: Array (RArgIndex × Label × RType) -> Array (Component eff AppState)
        inc incoming = flip map (indexedRange incoming) \(i × ai × l × t) -> g
          [ onMouseDrag \e -> case e of
             DragStart e -> modify \st -> st { debug = "START" }
             DragMove  e -> modify \st -> st { debug = "MOVE" }
             DragEnd   e -> modify \st -> st { debug = "END" }
          ]
          [ uicircle (bx - gap × by + (tn i * gap)) (UILabelLeft $ show t) ]

        ext :: Vec -> Array (Label × RType) -> Array (Component eff AppState)
        ext (ox × oy) external = map ext' (indexedRange external)
          where
            ext' (i × l × t@(RFun _ (RConst (Const "Component")))) = g
              [ onMouseDrag \e -> case e of
                  DragStart e -> modify \st -> st { dragState = Just $ DragHOC { hoc: t, label: l, pos: meToV e } }
                  DragMove  e -> modify \st -> st { dragState = Just $ DragHOC { hoc: t, label: l, pos: meToV e } }
                  DragEnd   e -> do
                    modify \st -> st { dragState = Nothing, debug = "" {- show $ map snd $ (fst (snch st)) (e.pageX × e.pageY × 200.0 × 100.0) -} }

                    case snap children (e.pageX × e.pageY × 200.0 × 100.0) of
                      Just f  -> modify (f l t)
                      Nothing -> pure unit
              ]
              [ uicircle (ox + gap × oy + (tn i * gap)) (UILabelRight "HOC") ]
            ext' (i × l × t) = uicircle (ox + gap × oy + (tn i * gap)) (UILabelRight $ show t)

        children :: SnapComponent' (Array (Component eff AppState))
        children = do
          subdivide' bounds (shrink childMargin) (A.fromFoldable $ zipSubsts (st ^. substs) chTypes) child
          where
            zipSubsts :: L.List Substitution -> L.List (RArgIndex × Label × RType) -> L.List (Maybe Substitution × RArgIndex × Label × RType)
            zipSubsts ss (L.Cons ch@(RArgIndex ai × _ × _) chs) = case L.index ss ai of
              Just s  -> L.Cons (Just s × ch) (zipSubsts ss chs)
              Nothing -> L.Cons (Nothing × ch) (zipSubsts ss chs)
            zipSubsts ss L.Nil = L.Nil
    
            child :: Rect -> Int -> Maybe Substitution × RArgIndex × Label × RType -> SnapComponent eff
            child bounds@(ix × iy × _ × _) index (s × RArgIndex ai × l × t@(RFun args _))
              | isHOC t = do
                childCmp' <- childCmp
                snappableRect shrunkBounds insertChild $ g [] $ concat
                  [ [ uirect bounds ]
                  , [ childCmp' ]
                  , ext (ix × (iy + gap)) (A.fromFoldable args)
                  ]
                where
                  childCmp = case s of
                    Just (SApp fs ss) -> case labeltype fs tt of
                      Just t' -> typeComponent' tt ctx shrunkBounds (substs <<< lensAtL ai <<< _SApp' <<< _2) t'
                      Nothing -> pure $ uirectDashed shrunkBounds
                    _ -> pure $ uirectDashed shrunkBounds

                  shrunkBounds = shrink childMargin bounds

                  insertChild l t st = flip (over substs) st \sss -> if L.length sss == 0
                    then (L.Cons (SApp l (repeat (argCount t) Placeholder)) L.Nil)
                    else fromMaybe sss (L.updateAt ai (SApp l (repeat (argCount t) Placeholder)) sss)

                  repeat :: forall a. Int -> a -> L.List a
                  repeat 0 _ = L.Nil
                  repeat n a = L.Cons a (repeat (n - 1) a)
                  
                  argCount :: RType -> Int
                  argCount (RFun args _) = L.length args
                  argCount _ = 0

              | otherwise = pure $ g [] []
            child _ _ _ = pure $ g [] []
      | otherwise = pure $ g [] []
    typeComponent' _ _ _ _ _ = pure $ g [] []
