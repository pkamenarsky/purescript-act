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
import Data.Map
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

-- mkCmp :: RType -> UIComponent
-- mkCmp rtype = UIComponent c1'
--   where
--    UIComponent c1 = layoutUIComponent L.Nil (200.5 × 100.5 × 1000.0 × 400.0) (either undefined id (extractComponents rtype))
--    c2 = case c1.internal A.!! 0 of
--      Just i  -> layoutUIComponent L.Nil i.inner testComponent2
--      Nothing -> undefined
--    c1' = (c1 { internal = fromMaybe undefined (A.modifyAt 0 (\uii -> uii { component = Just c2 }) c1.internal) })

-- layout :: AppState -> AppState
-- layout st = st
--   { component = layoutUIComponent L.Nil (200.5 × 100.5 × 1000.0 × 400.0) (either undefined id (extractComponents st.rtype))
--   }

emptyAppState :: AppState
emptyAppState =
  { debug     : "Debug: "
  , dragState : Nothing
  -- , component : mkCmp componentType
  , rtype     : componentType
  , substs    : L.Cons (SApp "a6" ((SApp "a8" L.Nil) L.: L.Nil)) L.Nil
  }

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui')
  -- where ui = R.createFactory (R.createClass (mkSpec (Tuple Nothing []) list)) unit
  where ui' = R.createFactory (R.createClass (mkSpec emptyAppState ui)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          -- void $ traceAnyM $ show $ from (undefined :: A)
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

ui :: forall eff. Component eff AppState
ui = state \st -> div
 []
 [ svg [ shapeRendering "geometricPrecision", width "2000px", height "600px" ]
   $ [ -- uicomponent L.Nil st.component
       typeComponent (200.5 × 100.5 × 1000.0 × 400.0) _substs type2
     ]
  <> case st.dragState of
       -- Just (DragConn ds) -> [ line ds.start ds.end ]
       Just (DragHOC { hoc, label, pos: (px × py) }) -> [ typeComponent (px × py × 200.0 × 100.0) (_const L.Nil) hoc ]
       _ -> []
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

-- type UIExternal = 
--   { conns   :: Array (Label × Vec × RArgIndex)
--   }
-- 
-- type UIInternal = 
--   { outer     :: Rect
--   , inner     :: Rect
--   , conns     :: Array (Label × Vec × Either RType RComponent × RArgIndex)
--   , component :: Maybe UIComponent
--   }
-- 
-- newtype UIComponent = UIComponent
--   { component :: RComponent
--   , bounds    :: Rect
--   , external  :: UIExternal
--   , internal  :: Array UIInternal
--   , substs    :: L.List Substitution
--   }

-- type ExConnLayout =
--   { rtype :: RType
--   , pos   :: Vec
--   , label :: String
--   }
-- 
-- type InConnLayout =
--   { rtype :: RType
--   , pos   :: Vec
--   , label :: String
--   }
-- 
-- type InCmpLayout =
--   { rcomponent :: RComponent'
--   , bounds     :: Rect
--   }
-- 
-- type InnerLayout =
--   { inner :: Rect
--   , outer :: Rect
--   , child :: Maybe UIComponent'
--   }

-- newtype UIComponent' = UIComponent'
--   { rtype    :: RType
--   , utype    :: L.List (Either ExConnLayout (L.List (Either InConnLayout InCmpLayout) × InnerLayout))
--   }
-- 
-- layoutUIComponent' :: Rect -> RComponent' -> UIComponent'
-- layoutUIComponent' bounds@(bx × by × bw × bh) cmp@(RComponent' { rtype, utype })
--   = UIComponent' { rtype: rtype, utype: map layout utype }
--   where
--     layout :: Either RType (L.List (Either RType RComponent'))
--            -> Either ExConnLayout (L.List (Either InConnLayout InCmpLayout) × InnerLayout)
--     layout t = case t of
--       Left  t -> undefined
--       Right t -> undefined

-- data Path = Done | Go Int Path

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

-- snap :: UIComponent -> Vec -> Maybe (Array RArgIndex)
-- snap (UIComponent uicmp) v
--   -- | inside v uicmp.bounds =
--   | true =
--         firstJust uicmp.internal goI
--     <|> goC (L.fromFoldable uicmp.external.conns)
--   where
--     goI :: UIInternal -> Maybe (Array RArgIndex)
--     goI uiint
--       | inside v uiint.inner
--       , Just child <- uiint.component = map (cons uiint.arg) $ snap child v
--       | inside v uiint.outer = goC (L.fromFoldable uiint.conns)
--       | otherwise = Nothing
-- 
--     goC (L.Cons (l × v' × ai) ts)
--       | inradius 10.0 v v' = Just [ai]
--       | otherwise          = goC ts
--     goC L.Nil = Nothing
--   | otherwise = Nothing
-- 
-- snap' :: UIComponent' -> Vec -> Maybe Path
-- snap' (UIComponent' rcmp) v = go 0 rcmp.utype
--   where
--     go index (L.Cons t ts) = case t of
--       Left exconn
--         | inradius 10.0 v exconn.pos -> Just $ Go index Done
--         | otherwise                  -> go (index + 1) ts
--       Right (int × layout)
--         | inside v layout.inner
--         , Just child' <- layout.child -> map (Go index) $ snap' child' v
--         | inside v layout.outer       -> goI 0 int
--         | otherwise                   -> go (index + 1) ts
--     go index L.Nil = Nothing
-- 
--     goI :: Int -> L.List (Either InConnLayout InCmpLayout) -> Maybe Path
--     goI index (L.Cons t ts) = case t of
--       Left inconn
--         | inradius 10.0 v inconn.pos -> Just $ Go index Done
--         | otherwise                  -> goI (index + 1) ts
--       Right incmp
--         | inside v incmp.bounds      -> Just $ Go index Done
--         | otherwise                  -> goI (index + 1) ts
--     goI _ L.Nil = Nothing
-- 
-- uicomponent' :: forall eff st. UIComponent' -> Component eff st
-- uicomponent' (UIComponent' rcmp) = g [] $
--  []

-- layoutUIComponent :: L.List Substitution -> Rect -> RComponent -> UIComponent
-- layoutUIComponent substs bounds@(bx × by × bw × bh) cmp@(RComponent rcmp) = UIComponent
--   { component: cmp
--   , bounds   : bounds
--   , external :
--     { conns: map (connE ((bx - gap) × by)) (indexedRange rcmp.external)
--     }
--   , internal : map internal (indexedRange rcmp.internal)
--   , substs   : substs
--   }
--   where
--     ccount = I.toNumber (length rcmp.internal)
--     gap    = 30.0
-- 
--     cw     = (bw - (gap * (ccount + 1.0))) / ccount
--     cy     = by + gap
-- 
--     connE :: Vec -> Int × RType × RArgIndex -> Label × Vec × RArgIndex
--     connE (ox × oy) (index × t × aindex) = show t × (ox × (oy + gap + I.toNumber index * gap)) × aindex
-- 
--     connI :: Vec -> Int × Either RType RComponent × RArgIndex -> Label × Vec × Either RType RComponent × RArgIndex
--     connI (ox × oy) (index × t × aindex) = case t of
--       Left  t -> show t × (ox × (oy + I.toNumber index * gap)) × Left t × aindex
--       Right t -> "HOC" × (ox × (oy + I.toNumber index * gap)) × Right t × aindex
-- 
--     internal :: Int × Array (Either RType RComponent × RArgIndex) × RArgIndex -> UIInternal
--     internal (index × args × iai) =
--       { outer    : cx × (by + gap) × cw × (bh - 2.0 * gap)
--       , inner    : inner
--       , conns    : map (connI ((cx + gap) × (cy + gap))) (indexedRange args)
--       , component: firstJustL substs \(v × s) -> if s == iai
--           then let
--             vt  = getType v rcmp.rtype
--             rch = extractComponents vt
--             in case rch of
--               Left _    -> Nothing
--               Right rch -> Just $ layoutUIComponent L.Nil inner rch
--           else Nothing
--       }
--       where
--         index' = I.toNumber index
--         cx     = bx + (index' + 1.0) * gap + index' * cw
--         inner  = (cx + gap * 5.0) × (by + gap * 4.0) × (cw - gap * 6.0) × (bh - 6.0 * gap)
-- 
-- type CtxE = L.List UIInternal
-- 
-- snapToInternal :: Vec -> UIInternal -> Maybe RArgIndex
-- snapToInternal v uiint = goC (L.fromFoldable uiint.conns)
--   where
--      goC (L.Cons (l × v' × _ × ai) ts)
--        | inradius 10.0 v v' = Just ai
--        | otherwise          = goC ts
--      goC L.Nil = Nothing
-- 
-- uicomponent :: forall eff. CtxE -> UIComponent -> Component eff AppState
-- uicomponent ctxE (UIComponent uicmp) = g [ ] $
--   [ rect
--     [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", fill "transparent" ]
--     []
--   ]
--   <> map (conn "end" Nothing (Just uicmp.bounds)) uicmp.external.conns
--   <> map container uicmp.internal
--   where
--     bx × by × bw × bh = uicmp.bounds
-- 
--     container :: UIInternal -> Component eff AppState
--     container uiint = g [] $
--       [ rect
--         [ x (px ox), y (px oy), width (px ow), height (px oh), rx (px 7.0), ry (px 7.0), stroke "#333", strokeWidth "3", fill "transparent" ]
--         []
--       ]
--       <> case uiint.component of
--            Just child -> [ uicomponent (L.Cons uiint ctxE) child ]
--            Nothing    ->
--              [ rect
--                [ x (px ix), y (px iy), width (px iw), height (px ih), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", strokeDashArray "5, 5", fill "transparent" ]
--                []
--              ]
--       <> map (connI "start") uiint.conns
--       where
--         ox × oy × ow × oh = uiint.outer
--         ix × iy × iw × ih = uiint.inner
-- 
--     connI :: String -> Label × Vec × Either RType RComponent × RArgIndex -> Component eff AppState
--     connI align (name × (x × y) × t × aindex) = g [] $
--       [ circle
--         [ case t of
--             Left _ -> onMouseDrag \ds -> pure unit
--             Right hoc' -> onMouseDrag \ds -> case ds of
--               DragStart e -> modify \st -> st
--                 { debug     = (show aindex)
--                 , dragState = Just $ DragHOC
--                     { hoc : layoutUIComponent L.Nil (e.pageX × e.pageY × 200.0 × 100.0) hoc'
--                     , pos : e.pageX × e.pageY
--                     , rarg: aindex
--                     }
--                 }
--               DragMove e -> modify \st -> st
--                 { dragState = flip map st.dragState \ds -> case ds of
--                     DragHOC ds -> DragHOC $ ds
--                       { hoc = layoutUIComponent L.Nil (e.pageX × e.pageY × 200.0 × 100.0) hoc'
--                       , pos = e.pageX × e.pageY
--                       }
--                     _ -> ds
--                 }
--               DragEnd e -> let
--                 getBounds (UIComponent cmp) = cmp.bounds
-- 
--                 chint :: Maybe DragState -> Array UIInternal
--                 chint (Just (DragHOC ds)) = flip map uicmp.internal \chint -> if true -- intersect chint.inner (getBounds ds.hoc)
--                   then chint { component = Just ds.hoc }
--                   else chint
--                 chint _ = undefined
-- 
--                 in modify \st -> layout $ st
--                   { debug = stringify_ $ UIComponent $ uicmp { internal = chint st.dragState }
--                   , dragState = Nothing
--                   , component = (\(UIComponent uicmp) -> UIComponent $ uicmp { internal = chint st.dragState }) st.component
--                   }
--         , cx (px x'), cy (px y'), r (px 5.0), fill "transparent", stroke "#d90e59", strokeWidth (px 3.0)
--         ]
--         []
--       , label (x' + offset align × y' + 4.0) align name
--       ]
--       where
--         x' = x - 0.0
--         y' = y + 0.0
-- 
--         offset "start" = 20.0
--         offset "end"   = -20.0
--         offset _       = 0.0
-- 
--     conn :: String -> Maybe RArgIndex -> Maybe Rect -> Label × Vec × RArgIndex -> Component eff AppState
--     conn align aindex' bounds (name × (x × y) × aindex) = g [] $
--       [ circle
--         [ onMouseDrag \ds -> case ds of
--             DragStart e -> modify \st -> st
--               { debug     = (show (aindex' × aindex))
--               , dragState = Just $ DragConn
--                   { start: e.pageX × e.pageY
--                   , end  : e.pageX × e.pageY
--                   }
--               }
--             DragMove e -> modify \st -> st
--               { dragState = flip map st.dragState \ds -> case ds of
--                   DragConn ds -> DragConn $ ds
--                     { end = e.pageX × e.pageY
--                     }
--                   _ -> ds
--               }
--             DragEnd  e -> modify \st -> st
--                 { debug = case firstJustL ctxE (\uiint -> snapToInternal (e.pageX × e.pageY) uiint) of
--                     Just ai -> let
--                       t = getType ai componentType
--                       c = getType aindex $ (\(RComponent rcmp) -> rcmp.rtype) uicmp.component
--                       tr = unifyType c t
--                       t' = specifyType tr componentType
--                       in show t'
--                     Nothing        -> "None"
--                 , component = case firstJustL ctxE (\uiint -> snapToInternal (e.pageX × e.pageY) uiint) of
--                     Just ai -> let
--                       t = getType ai componentType
--                       c = getType aindex $ (\(RComponent rcmp) -> rcmp.rtype) uicmp.component
--                       tr = unifyType c t
--                       t' = specifyType tr componentType
--                       in mkCmp t'
--                     Nothing        -> undefined
--                 }
--         , cx (px x'), cy (px y'), r (px 5.0), fill "transparent", stroke "#d90e59", strokeWidth (px 3.0)
--         ]
--         []
--       , label (x' + offset align × y' + 4.0) align name
--       ]
--       <> case bounds of
--            Just _  -> [ line (x' + 5.0 × y') (x' + 30.0 × y') ]
--            Nothing -> []
--       where
--         x' = x - 0.0
--         y' = y + 0.0
-- 
--         offset "start" = 20.0
--         offset "end"   = -20.0
--         offset _       = 0.0

uirect :: forall eff st. Rect -> Component eff st
uirect (bx × by × bw × bh) = rect [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", fill "transparent" ] []

uirectDashed :: forall eff st. Rect -> Component eff st
uirectDashed (bx × by × bw × bh) = rect [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", strokeDashArray "5, 5", fill "transparent" ] []

data UILabel = UILabelLeft String | UILabelRight String

uicircle :: forall eff st. Vec -> UILabel -> Component eff st
uicircle (x' × y') label' = g [] $
  [ circle [ cx (px x'), cy (px y'), r (px 5.0), fill "transparent", stroke "#d90e59", strokeWidth (px 3.0) ] []
  , uilabel label'
  ]
  where
    uilabel (UILabelLeft str)  = label (x' - 20.0 × y' + 4.0) "end" str
    uilabel (UILabelRight str) = label (x' + 20.0 × y' + 4.0) "start" str

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

subdivide' :: forall eff st a. Rect -> (Rect -> Rect) -> Array a -> (Rect -> Int -> a -> Component eff st) -> ((Rect -> Maybe a) × Component eff st)
subdivide' (bx × by × bw × bh) snapf as f = snap × (g [] $ map (\(i × a × r) -> f r i a) rects)
  where
     rects   = flip map (indexedRange as) \(i × a) -> i × a × (cx (tn i) × (by + gap) × cw × (bh - 2.0 * gap))

     snap r' = firstJust rects (\(_ × a × r) -> if intersect (snapf r) r' then Just a else Nothing)

     count = A.length as
     cx i  = bx + (i + 1.0) * gap + i * cw
     cw    = (bw - (gap * (tn count + 1.0))) / tn count
     cy    = by + gap

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

typeComponent :: forall eff. Rect -> Lens' AppState (L.List Substitution) -> RType -> Component eff AppState
typeComponent r ss t = typeComponent' t r ss t
  where
    typeComponent' :: RType -> Rect -> Lens' AppState (L.List Substitution) -> RType -> Component eff AppState
    typeComponent' tt bounds@(bx × by × bw × bh) substs rtype
      | Just (incoming × children) <- extract rtype = state \st -> g [] $ concat
          [ [ uirect bounds ]
          , inc (A.fromFoldable incoming)
          , [ snd $ snch st ]
          ]
      where
        inc :: Array (Label × RType) -> Array (Component eff AppState)
        inc incoming = flip map (indexedRange incoming) \(i × l × t) ->
          uicircle (bx - gap × by + (tn i * gap)) (UILabelLeft $ show t)
    
        child :: AppState -> Rect -> Int -> Maybe Substitution × Label × RType -> Component eff AppState
        child st bounds@(ix × iy × _ × _) index (s × l × t@(RFun args _))
          | isHOC t = g [] $ concat
            [ [ uirect bounds ]
            , case s of
                Just (SApp fs ss) -> case labeltype fs tt of
                  Just t'   -> [ typeComponent' tt shrunkBounds (substs <<< lensAtL index <<< _SApp' <<< _2) t' ]
                  otherwise -> [ uirectDashed shrunkBounds ]
                otherwise         -> [ uirectDashed shrunkBounds ]
            , ext st (ix × (iy + gap)) (A.fromFoldable args)
            ]
            where
              shrunkBounds = shrink ((7.0 * gap) × (1.0 * gap) × gap × gap) bounds
          | otherwise = undefined
        child _ _ _ _ = g [] []

        -- TODO: very inefficient
        snch :: AppState -> ((Rect -> Maybe (Maybe Substitution × Label × RType)) × Component eff AppState)
        snch st = subdivide' bounds (shrink ((7.0 * gap) × (1.0 * gap) × gap × gap)) (A.fromFoldable $ zipMaybe (st ^. substs) children) (child st)

        ext :: AppState -> Vec -> Array (Label × RType) -> Array (Component eff AppState)
        ext st (ox × oy) external = map ext' (indexedRange external)
          where
            ext' (i × l × t@(RFun _ (RConst (Const "Component")))) = g
              [ onMouseDrag \e -> case e of
                  DragStart e -> modify \st -> st { dragState = Just $ DragHOC { hoc: t, label: l, pos: meToV e } }
                  DragMove  e -> modify \st -> st { dragState = Just $ DragHOC { hoc: t, label: l, pos: meToV e } }
                  DragEnd   e -> do
                    modify \st -> st { dragState = Nothing, debug = show $ map snd $ (fst (snch st)) (e.pageX × e.pageY × 200.0 × 100.0) }
                    modify' substs \st' -> let
                      a = 5
                      in case (fst (snch st)) (e.pageX × e.pageY × 200.0 × 100.0) of
                          Just (_ × l × t) -> L.Cons (SApp l L.Nil) L.Nil
                          _ -> st'
              ]
              [ uicircle (ox + gap × oy + (tn i * gap)) (UILabelRight "HOC") ]
            ext' (i × l × t) = uicircle (ox + gap × oy + (tn i * gap)) (UILabelRight $ show t)
      | otherwise = g [] []
    typeComponent _ _ _ = g [] []
