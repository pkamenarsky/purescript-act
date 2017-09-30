module Act where

import Control.Alternative
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free
import Control.Monad.Trans.Class
import Data.Argonaut.Core
import Data.Array
import Data.Either
import Data.Functor
import Data.Exists
import Data.Generic
import Data.Traversable
import Data.Lazy
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
import Path
import Control.Monad.State as ST
import DOM as D
import DOM.Node.Types as D
import Data.Array as A
import Data.Int as I
import Data.List as L
import Data.Map as M
import Data.String as S
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

foreign import three :: R.ReactClass { geometry :: String }

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
  , subst     :: Substitution
  , unfcs     :: M.Map Var Const
  }

_subst :: Lens' AppState Substitution
_subst = lens (_.subst) (\st s -> st { subst = s })

_const :: forall st a. a -> Lens' st a
_const a = lens (const a) (\st s -> st)

emptyAppState :: AppState
emptyAppState =
  { debug     : "Debug: "
  , dragState : Nothing
  , rtype     : rtypeFromRefs refArray -- type2
  , subst     : Placeholder
  , unfcs     : M.empty
  }

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui')
  where ui' = R.createFactory (R.createClass (mkSpec emptyAppState mainUI)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

mainUI :: forall eff. Component eff AppState
mainUI = div [] [ {- testUI -} ui ]

showUnfcs :: M.Map Var Const -> String
showUnfcs m = S.joinWith ", " $ map (\(Var v × Const c) -> v <> " -> " <> c) (M.toUnfoldable m :: Array (Var × Const))

ui :: forall eff. Component eff AppState
ui = state \st -> let snap × cmp' = cmp st in div [] $
 [ div [ class_ "wire-split" ]
   [ svg [ shapeRendering "geometricPrecision", width "2000px", height "600px" ]
     $ [ -- snapValue $ typeComponent st M.empty (specialize st.unfcs st.rtype) (50.5 × 100.5 × 700.0 × 400.0) _substs (specialize st.unfcs st.rtype)
         cmp'
       ]
   , code [] [ text $ st.debug <> " # " <> showUnfcs st.unfcs <> " # " <> show st.subst <> " # " <> show (substituteC st.rtype st.subst) ]
   -- , state \st -> code [] [ text st.debug ]
   ]
 , div [ class_ "search-split" ]
   [ searchComponent st snap
   ]
 , div [ class_ "component-split" ]
     [ div [ class_ "component-container" ]
       [ componentFromRefs (substituteC st.rtype st.subst) refArray ]
       -- [ wrapClass three unit ]
     ]
 ]
 <> case st.dragState of
      Just (DragConn ds) -> [ svg [ shapeRendering "geometricPrecision", class_ "overlay" ] [ line ds.start ds.end ] ]
      Just (DragHOC { hoc, label, pos: (px × py) }) -> [ svg [ shapeRendering "geometricPrecision", class_ "overlay" ]
        [ snapValue $ typeComponent st Compact M.empty (specialize st.unfcs st.rtype) ((px + 0.5) × (py + 0.5) × dropSize × dropSize) (_const L.Nil) hoc
        ] ]
      _ -> []
 where
   -- offset :: SnapF -> SnapF
   -- offset snap (Left (bx × by × bw × bh)) = snap $ Left (bx - 300.0 × by × bw × bh)
   -- offset snap (Right (vx × vy))          = snap $ Right (vx - 300.0 × vy)

   cmp st
     | Just (incTypes × L.Cons chType@(_ × _ × RFun args _) L.Nil) <- extract st.rtype = (snap cmp) × (g [] $ concat
         [ [ snapValue cmp ]
         , snapValue exts
         ])
         where
           pos i = (20.0 + (3.0 * gap) × (50.0 + gap) + (tn i * gap))
           exts  = traverse (ext (snap cmp) UILabelLeft (300.0 × (50.0 + gap))) (indexedRange $ A.fromFoldable args)
           ctx'  = M.fromFoldable $ map (\(i × l × _) -> l × pos i)  (indexedRange $ A.fromFoldable args)
           cmp   = child st Full ctx' (specialize st.unfcs st.rtype) (500.5 × 150.5 × 300.0 × 300.0) (_subst × Just st.subst × chType)
     | otherwise = const Nothing × g [] []

--------------------------------------------------------------------------------

repeat :: forall a. Int -> a -> L.List a
repeat 0 _ = L.Nil
repeat n a = L.Cons a (repeat (n - 1) a)

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

bezier :: forall eff st. Vec -> Path -> Component eff st
bezier start es = path [ strokeWidth (px 3.0), fill "transparent", stroke "#d90e59", d (pathToString start es)] []

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

uirectDashed' :: forall eff st. Rect -> String -> Component eff st
uirectDashed' (bx × by × bw × bh) color
  | bw > 0.0 && bh > 0.0 = rect [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke color, strokeWidth "3", strokeDashArray "5, 5", fill "transparent" ] []
  | otherwise            = g [] []

uirectDashed :: forall eff st. Rect -> Component eff st
uirectDashed (bx × by × bw × bh)
  | bw > 0.0 && bh > 0.0 = rect [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", strokeDashArray "5, 5", fill "transparent" ] []
  | otherwise            = g [] []

data UILabel = UILabelLeft String | UILabelRight String | UILabelTopLeft String | UILabelTopRight String

uicircle :: forall eff st. Vec -> UILabel -> Component eff st
uicircle (x' × y') label' = g [] $
  [ circle [ cx (px x'), cy (px y'), r (px 5.0), fill "transparent", stroke "#d90e59", strokeWidth (px 3.0) ] []
  , uilabel label'
  ]
  where
    uilabel (UILabelLeft str)     = label (x' - 20.0 × y' + 4.0) "end" str
    uilabel (UILabelRight str)    = label (x' + 20.0 × y' + 4.0) "start" str
    uilabel (UILabelTopLeft str)  = label (x' - 5.0 × y' - 15.0) "start" str
    uilabel (UILabelTopRight str) = label (x' + 5.0 × y' - 15.0) "end" str

tn :: Int -> Number
tn = I.toNumber

gap :: Number
gap = 30.0

subdivide :: forall eff st a r. Rect -> Array a -> (Rect -> a -> Component eff st) -> Component eff st
subdivide (bx × by × bw × bh) as f = g [] $
  flip map (indexedRange as) \(i × a) -> f (cx (tn i) × (by + gap) × cw × (bh - 2.0 * gap)) a
  where
     count = A.length as
     cx i  = bx + (i + 1.0) * gap + i * cw
     cw    = (bw - (gap * (tn count + 1.0))) / tn count
     cy    = by + gap

subdivide' :: forall eff st a b c m r. Monad m => Rect -> (Rect -> Rect) -> Array a -> (Rect -> Int -> a -> m r) -> m (Array r)
subdivide' (bx × by × bw × bh) snapf as f = flip traverse (indexedRange as) \(i × a) -> f (cx (tn i) × (by + gap) × cw × (bh - 2.0 * gap)) i a
   where
      count     = A.length as
      cx i      = bx + (i + 1.0) * gap + i * cw
      cw        = (bw - (gap * (tn count + 1.0))) / tn count
      cy        = by + gap

subdivide'' :: forall eff st a b c m r. Monad m => Rect -> (Rect -> Rect) -> Array a -> (Rect -> a -> m r) -> m (Array r)
subdivide'' (bx × by × bw × bh) snapf as f = flip traverse (indexedRange as) \(i × a) -> f (bx × (by + tn i * ch) × bw × ch) a
   where
      count  = A.length as
      ch     = bh / tn count

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

-- (c -> Maybe b) -> (a -> a) × (c -> Maybe b)
-- SnapM \f -> id × f

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

instance monadSnapM :: Monad (SnapM c b)

snappable :: forall a b c. (c -> Maybe b) -> a -> SnapM c b a
snappable f a = SnapM f a

snap :: forall a b c. SnapM c b a -> c -> Maybe b
snap (SnapM f _) c = f c

withSnap :: forall a b c. SnapM c b a -> ((c -> Maybe b) -> SnapM c b a)
withSnap = undefined

snapValue :: forall a b c. SnapM c b a -> a
snapValue (SnapM _ a) = a

--------------------------------------------------------------------------------

type AppF = Label -> RType -> AppState -> AppState

type SnapComponent'    = SnapM (Either Rect Vec) (Either AppF AppF)
type SnapComponent eff = SnapComponent' (Component eff AppState)

type SnapF = Either Rect Vec -> Maybe (Either AppF AppF)

snappableRect :: forall a b c d. Rect -> b -> a -> SnapM (Either Rect c) (Either b d) a
snappableRect bounds b a = flip SnapM a \bounds' -> case bounds' of
  Left bounds' -> if intersect bounds bounds' then (Just (Left b)) else Nothing
  Right       _ -> Nothing

snappableCircle :: forall a b c d m. Number -> Vec -> b -> a -> SnapM (Either c Vec) (Either d b) a
snappableCircle radius pos b a = flip SnapM a \pos' -> case pos' of
  Left     _ -> Nothing
  Right pos' -> if inradius radius pos pos' then (Just (Right b)) else Nothing

type Context = M.Map Label Vec

type Level = Int

ext :: forall eff. SnapF -> (String -> UILabel) -> Vec -> (Int × Label × RType) -> SnapComponent eff
ext snap labelf (ox × oy) (i × l × t@(RFun _ (RConst (Const "Component")))) = pure $ state \st -> g
  [ onMouseDrag \e -> case e of
      DragStart e -> modify \st -> st { debug = "DRAG", dragState = Just $ DragHOC { hoc: t, label: l, pos: meToV e } }
      DragMove  e -> modify \st -> st { dragState = Just $ DragHOC { hoc: t, label: l, pos: meToV e } }
      DragEnd   e -> do
        modify \st -> st { dragState = Nothing, debug = "" {- show $ map snd $ (fst (snch st)) (e.pageX × e.pageY × 200.0 × 100.0) -} }

        case snap (Left (e.pageX × e.pageY × dropSize × dropSize)) of
          Just (Left f) -> modify (f l t)
          _             -> modify \st -> st { debug = "no drop target" }
  ]
  -- [ uicircle (ox + (3.0 * gap) × oy + (tn i * gap)) (labelf "HOC") ]
  [ snapValue $ typeComponent st Compact M.empty (specialize st.unfcs st.rtype) (ox × oy × dropSize × dropSize) (_const L.Nil) t ]
ext snap labelf (ox × oy) (i × l × t) = pure $ g
  [ onMouseDrag \e -> case e of
      DragStart e -> modify \st -> st { debug = "DRAG", dragState = Just $ DragConn { start: meToV e, end: meToV e } }
      DragMove  e -> modify \st -> st
        { dragState = flip map st.dragState \ds -> case ds of
            DragConn dc -> DragConn $ dc { end = meToV e }
            _ -> ds
        }
      DragEnd   e -> do
        modify \st -> st { dragState = Nothing, debug = "" {- show $ map snd $ (fst (snch st)) (e.pageX × e.pageY × 200.0 × 100.0) -} }

        case snap (Right (e.pageX × e.pageY)) of
          Just (Right f) -> modify (f l t)
          _              -> pure unit
  ]
  [ uicircle (pos i) (labelf $ show t) ]
  where
    pos i = (ox + (2.0 * gap) × (oy + gap * 2.0) + (tn i * gap))

data ChildStyle = Full | Compact

dropSize :: Number
dropSize = 36.0

child :: forall eff. AppState -> ChildStyle -> Context -> RType -> Rect -> ALens' AppState Substitution × Maybe Substitution × RArgIndex × Label × RType -> SnapComponent eff
child st style ctx tt bounds@(bx × by × bw × bh) (substlens × s × RArgIndex ai × l × t@(RFun args _))
  | isHOC t = do
     
    let pos i = (bx + (2.0 * gap) × (by + gap * 2.0) + (tn i * gap))
        midy  = by + bh / 2.0
        r     = 15.0
        incpath start@(sx × sy) = bezier (sx - 5.0 × sy) [H (-20.0), TL, V (midy - sy - r * 2.0), BR, H (-(sx - bx - r * 2.0 - 20.0 - 5.0))]
        ctx'  = case style of
          Full    -> M.empty
          Compact -> M.fromFoldable $ map (\(i × l × _) -> l × pos i)  (indexedRange $ A.fromFoldable args)
        cmp   = snappableRect dropBounds insertChild =<< case s of
          Just (SApp fs ss) -> case labeltype fs tt of
            Just t' -> typeComponent st style (M.union ctx ctx') tt dropBounds (cloneLens substlens <<< _SApp' <<< _2) t'
            Nothing -> pure $ uirectDashed' dropBounds "#ccc"
          _ -> pure $ uirectDashed' dropBounds "#ccc"

    exts <- traverse (ext (snap cmp) UILabelTopRight (bx × by)) (indexedRange $ A.fromFoldable args)
    cmp' <- cmp

    pure $ g [] $ concat
      [ case style of
          Compact -> [] -- [ line (ix × (iy + ih)) ((ix + iw) × (iy + ih)) ]
          _       -> []
      , [ cmp' ]
      , case style of
          Compact -> exts
          _       -> []
      , case style of
          Compact -> flip map (0 .. (L.length args - 1)) \i -> incpath (pos i)
          _       -> []
      ]
    where
      -- shrunkBounds = shrink childMargin bounds
      -- childMargin = ((8.0 * gap) × (1.0 * gap) × gap × gap)

      dropGap      = 24.0
      dropBounds   = case style of
        Full    -> bounds
        Compact -> ((bx + bw - dropGap - dropSize) × (by + bh / 2.0 - dropSize / 2.0) × dropSize × dropSize)

      insertChild l t st = flip (set $ cloneLens substlens) st (SApp l (repeat (argCount t) Placeholder))

      argCount :: RType -> Int
      argCount (RFun args _) = L.length args
      argCount _ = 0

  | otherwise = pure $ g [] []
child _ _ _ _ _ _ = pure $ g [] []

typeComponent :: forall eff.
                 AppState
              -> ChildStyle
              -> Context
              -> RType
              -> Rect
              -> Lens' AppState (L.List Substitution)
              -> RType
              -> SnapComponent eff
typeComponent st style ctx tt r ss t = typeComponent' tt r ss t
  where
    typeComponent' :: RType
                   -> Rect
                   -> Lens' AppState (L.List Substitution)
                   -> RType
                   -> SnapComponent eff
    typeComponent' tt bounds@(bx × by × bw × bh) substs rtype
      | Just (incTypes × chTypes) <- extract rtype = do
          children' <- case style of
            Full    -> Just <$> children
            Compact -> pure Nothing
          inc'      <- inc (A.fromFoldable incTypes)

          pure $ g [] $ concat
            [ [ uirect bounds ]
            , [ inc' ]
            , fromMaybe [] children'
            ]
      where
        childMargin = ((8.0 * gap) × (1.0 * gap) × gap × gap)

        inc :: Array (RArgIndex × Label × RType) -> SnapComponent eff
        inc incTypes = g [] <$> flip traverse (indexedRange incTypes) \(i × ai × l × t) -> do
          snappableCircle 10.0 (pos i) (insertArg t ai) $ g [] $ concat
            [ [ uicircle (pos i) (UILabelTopLeft $ show t) ]
            , case connected ctx ai of
                Just pos' -> [ line pos' (pos i) ]
                Nothing   -> []
            , [ incpath (pos i) ]
            ]
          where
            midy = by + bh / 2.0
            r    = 15.0
            incpath start@(sx × sy) = bezier (sx + 5.0 × sy) [H 20.0, TR, V (midy - sy - r * 2.0), BL, H (bx - sx - r * 2.0 - 20.0 - 5.0)]
            pos i = (bx - (gap * 2.0) × by - gap + (tn i * gap))
            insertArg t (RArgIndex ai) l t' st = case unify t t' of
              UEq -> flip (over substs) st \sss -> if L.length sss == 0
                then sss
                else fromMaybe sss (L.updateAt ai (SArg l) sss)
              UUnify v c -> flip (over substs) (addUnification st v c) \sss -> if L.length sss == 0
                then sss
                else fromMaybe sss (L.updateAt ai (SArg l) sss)
              UNone -> st

            addUnification st v c = st { unfcs = M.insert v c st.unfcs }

            connected ctx (RArgIndex ai) = do
              l' <- case (st ^. substs) L.!! ai of
                Just (SArg l') -> pure l'
                _ -> Nothing
              M.lookup l' ctx

        children :: SnapComponent' (Array (Component eff AppState))
        children = subdivide'' bounds id (A.fromFoldable $ zipSubsts (st ^. substs) chTypes) (child st Compact ctx tt)
          where
            zipSubsts :: L.List Substitution -> L.List (RArgIndex × Label × RType) -> L.List (ALens' AppState Substitution × Maybe Substitution × RArgIndex × Label × RType)
            zipSubsts ss' (L.Cons ch@(RArgIndex ai × _ × _) chs) = case L.index ss' ai of
              Just s  -> L.Cons (ss <<< lensAtL ai × Just s × ch) (zipSubsts ss' chs)
              Nothing -> L.Cons (ss <<< lensAtL ai × Nothing × ch) (zipSubsts ss' chs)
            zipSubsts _ L.Nil = L.Nil
      | otherwise = pure $ g [] []

--------------------------------------------------------------------------------

type Tweet =
  { user :: String
  , text :: String
  , date :: String
  }

tweet :: Tweet
tweet =
  { user: "User1"
  , text: "If the result of an expensive computation is invalidated by a small change to the input, the old result should be updated incrementally instead of reexecuting the whole computation."
  , date: "01.01.1999"
  }

tweets :: Array Tweet
tweets = A.fromFoldable $ repeat 10 tweet

testUI :: forall eff st. Component eff st
testUI = div [ class_ "component-split" ]
  [ div [ class_ "component-container" ]
    [ -- listComponent tweets tweetComponent
      -- listComponent'
    ]
  ]

--------------------------------------------------------------------------------

type Ref  = Unit × TypeM RType
type Ref' = Unit

mkRef :: forall a. TypeM RType -> a -> Ref
mkRef rtype cmp = unsafeCoerce cmp × rtype

mkRef' :: forall a. a -> Ref'
mkRef' = unsafeCoerce

rtypeFromRefs :: Array Ref -> RType
rtypeFromRefs refs = runType $ fun [ fun (map snd refs) component] component 

componentFromRefs :: forall eff st. Expr -> Array Ref -> Component eff st
componentFromRefs e args
  | Just js <- exprToJS e = applyJSFun (jsFunFromString js) (map fst args)
  | otherwise             = div [] []

--------------------------------------------------------------------------------

threeCR :: Ref
threeCR = mkRef listCT threeComponent 
  where
    listCT = fun [ ] component

    threeComponent :: forall eff st. Component eff st
    threeComponent = wrapClass three { geometry: "cube" }

listCR :: Ref
listCR = mkRef listCT listComponent 
  where
    listCT = fun [ pure $ array a, fun [ pure a ] component ] component

    listComponent :: forall a eff st. Array a -> (a -> Component eff st) -> Component eff st
    listComponent as cmp = div [ class_ "list" ] $ flip map as \a -> div [ class_ "cell" ] [ cmp a ]

tweetCR :: Ref
tweetCR = mkRef tweetCT tweetComponent
  where
    tweetCT = fun [ pure tweetT ] component

    tweetComponent :: forall eff st. Tweet -> Component eff st
    tweetComponent tweet = div [ class_ "tweet" ]
      [ div [ class_ "user" ] [ text tweet.user ]
      , div [ class_ "text" ] [ text tweet.text ]
      , div [ class_ "icon1" ] [ ]
      , div [ class_ "icon2" ] [ ]
      , div [ class_ "icon3" ] [ ]
      ]

tweetT :: RType
tweetT = RConst (Const "Tweet")

tweetsR :: Ref
tweetsR = mkRef (pure $ array tweetT) tweets

refArray :: Array Ref
refArray = [ listCR, threeCR, tweetCR, tweetsR ]

--listComponentExpr :: Expr
--listComponentExpr = ELam (L.fromFoldable ["listC", "tweets", "tweetC"]) (EApp (EVar "listC") (L.fromFoldable [EVar "tweets", EVar "tweetC"]))
--
--listComponent' :: forall eff st. Component eff st
--listComponent' = componentFromRef listComponentExpr [ listR, tweetsR, tweetR ]

--------------------------------------------------------------------------------

searchComponent :: forall eff. AppState -> SnapF -> Component eff AppState
searchComponent st snap
  | Just (incTypes × L.Cons chType@(_ × _ × RFun args@(L.Cons arg (L.Cons arg1 _)) _) L.Nil) <- extract st.rtype = div [] $ concat
    [ [ input [ onChange \e -> modify \st -> st { debug = (unsafeCoerce e).target.value } ] [] ]
    , [ div [ class_ "container" ] 
        [ div [ class_ "cell" ]
          [ div [ class_ "title" ] [ text "ListComponent" ]
          , svg [ class_ "svg", shapeRendering "geometricPrecision" ]
              [ snapValue $ ext snap UILabelTopLeft (230.0 × 73.0) (0 × arg)
              ]
          ]
        , div [ class_ "cell" ]
          [ div [ class_ "title" ] [ text "TweetComponent" ]
          , svg [ class_ "svg", shapeRendering "geometricPrecision" ]
              [ snapValue $ ext snap UILabelTopLeft (230.0 × 73.0) (1 × arg1)
              ]
          ]
        ]
      ]
    ]
    where
      pos i = (20.0 + (3.0 * gap) × (50.0 + gap) + (tn i * gap))
      exts  = traverse (ext snap UILabelLeft (50.0 × (80.0 + gap))) (indexedRange $ A.fromFoldable args)
      ctx'  = M.fromFoldable $ map (\(i × l × _) -> l × pos i)  (indexedRange $ A.fromFoldable args)
  | otherwise = div [] []
