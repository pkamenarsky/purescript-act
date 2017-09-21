module Act where

import Control.Monad
import Control.Monad.State
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free
import Data.Argonaut.Core
import Data.Array
import Data.Functor
import Data.Exists
import Data.Generic.Rep
import Data.Traversable
import Data.Lens
import Data.Maybe
import Data.Map
import Data.Lens.Index
import Data.Tuple
import Type.Proxy
import Unsafe.Coerce
import Prelude
import Match
import DOM as D
import DOM.Node.Types as D
import Data.Array as A
import Data.Int as I
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

foreign import traceAny :: forall a b. a -> (Unit -> b) -> b

foreign import dragStart :: forall eff a. (Int -> R.Event -> Eff eff a) -> Eff eff Unit

traceAnyM :: forall m a. Monad m => a -> m a
traceAnyM s = traceAny s \_ -> pure s

undefined :: forall a. a
undefined = unsafeCoerce unit

lensAt :: forall a. Int -> Lens' (Array a) a
lensAt index = lens (\arr -> unsafePartial $ arr `unsafeIndex` index) (unsafeUpdateAt index)

unsafeUpdateAt index arr a = unsafePartial $ fromJust $ updateAt index a arr

unitLens :: forall a. Lens' a Unit
unitLens = lens (const unit) (\s _ -> s)

--------------------------------------------------------------------------------

type StaticPtrTable = forall a. Map Int a

type Static = State StaticPtrTable

data StaticPtr a = StaticPtr Int

foreign import static_ :: forall a. a -> Int
foreign import derefStatic_ :: forall a. Int -> a

static :: forall a. a -> StaticPtr a
static = StaticPtr <<< static_

derefStatic :: forall a. StaticPtr a -> a
derefStatic (StaticPtr ptr) = derefStatic_ ptr

--------------------------------------------------------------------------------

data MouseDragState = DragStart R.MouseEvent | DragMove R.MouseEvent | DragEnd R.MouseEvent

instance shoeMouseDragState :: Show MouseDragState where
  show (DragStart _) = "DragStart"
  show (DragMove _) = "DragMove"
  show (DragEnd _) = "DragEnd"

data EffectF eff st next =
    Modify (st -> st) next
  | ModifyRemotely (StaticPtr (Json -> st -> (Tuple st Json))) Json (Json -> next)
  | Effect Json (Json -> Eff eff Json) (Json -> next)
  | Log String next
  | PreventDefault R.Event next
  | StopPropagation R.Event next
  | OnDragStart (MouseDragState -> Effect eff st next)

derive instance functorEffectF :: Functor (EffectF eff st)

type Effect eff st = Free (EffectF eff st)

mapEffectF :: forall eff st stt next. Lens' st stt -> EffectF eff stt next -> EffectF eff st next
mapEffectF lns (ModifyRemotely a f next) = undefined
mapEffectF lns (Modify f next) = Modify (over lns f) next
mapEffectF lns (Log str next) = Log str next
mapEffectF lns (PreventDefault e next) = PreventDefault e next
mapEffectF lns (StopPropagation e next) = StopPropagation e next
mapEffectF lns (OnDragStart next) = OnDragStart (map (mapEffect lns) next)
mapEffectF lns (Effect json eff next) = Effect json eff next

mapEffect :: forall eff st stt a. Lens' st stt -> Effect eff stt a -> Effect eff st a
mapEffect lns m = hoistFree (mapEffectF lns) m

modify :: forall eff st. (st -> st) -> Effect eff st Unit
modify f = liftF $ Modify f unit

modify' :: forall eff st stt. Lens' st stt -> (stt -> stt) -> Effect eff st Unit
modify' l f = liftF $ Modify (over l f) unit

getHTTP :: forall eff st. String -> Effect eff st (Maybe String)
getHTTP url = liftF $ Effect (fromString url) (\_ -> pure $ fromString "Result'") toString

log :: forall eff st. String -> Effect eff st Unit
log str = liftF $ Log str unit

preventDefault :: forall eff st. R.Event -> Effect eff st Unit
preventDefault e = liftF $ PreventDefault e unit

stopPropagation :: forall eff st. R.Event -> Effect eff st Unit
stopPropagation e = liftF $ StopPropagation e unit

onDragStart :: forall eff st. (MouseDragState -> Effect eff st Unit) -> Effect eff st Unit
onDragStart f = liftF $ OnDragStart f

interpretEffect :: forall eff st a. R.ReactThis Unit st -> Effect eff st a -> Eff (state :: R.ReactState R.ReadWrite, console :: CONSOLE | eff) a
interpretEffect this m = runFreeM go m
  where
    go (Modify f next) = do
      _ <- (unsafeCoerce R.transformState) this f
      -- void $ traceAnyM $ static f
      pure next
    go (ModifyRemotely f a next) = undefined
    go (Log str next) = do
      logShow str
      pure next
    go (PreventDefault e next) = do
      _ <- R.preventDefault e
      pure next
    go (StopPropagation e next) = do
      _ <- R.stopPropagation e
      pure next
    go (OnDragStart f) = do
      _ <- dragStart \st e -> case st of
        1 -> interpretEffect this $ f (DragMove $ unsafeCoerce e)
        2 -> interpretEffect this $ f (DragEnd $ unsafeCoerce e)
        _ -> undefined
      pure $ pure undefined
    go (Effect json eff next) = do
      -- dump json here
      res <- unsafeCoerce $ eff json
      pure $ next res

--------------------------------------------------------------------------------

type Element st = R.ReactElement

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type Component eff st =
  { render :: (Effect eff st Unit -> Handler st) -> st -> Array R.ReactElement
  -- , onfetchstart :: st -> st
  -- , onfetchend :: st -> st
  }

type Props eff st = (Effect eff st Unit -> Handler st) -> P.Props

state :: forall eff st. (st -> Component eff st) -> Component eff st
state f = { render: \effect st -> (f st).render effect st }

state' :: forall eff st stt. Lens' st stt -> (stt -> Component eff st) -> Component eff st
state' l f = { render: \effect st -> (f (st ^. l)).render effect st }

zoom :: forall eff st stt. Lens' st stt -> Component eff stt -> Component eff st
zoom lns cmp = { render: \effect st -> cmp.render (\e -> effect (mapEffect lns e)) (view lns st) }

zoomProps :: forall eff st stt. Lens' st stt -> Props eff stt -> Props eff st
zoomProps lns effect f = effect \b -> f (mapEffect lns b)

zoomState :: forall eff st stt. Lens' st stt -> (stt -> Component eff stt) -> Component eff st
zoomState lns f = { render: \effect st -> (f (view lns st)).render (\e -> effect (mapEffect lns e)) (view lns st) }

foreach :: forall eff st stt. Lens' st (Array stt) -> Component eff stt -> Component eff st
foreach lns cmp = { render }
  where
    render effect st = concat $ do
      Tuple index item <- zip (range 0 (length items - 1)) items
      pure $ cmp.render (\f -> effect (mapEffect (lns <<< lensAt index) f)) (unsafePartial $ fromJust $ items !! index)
      where
        items = view lns st

foreach_ :: forall eff st stt. Lens' st (Array stt) -> (Int -> (st -> st) -> Lens' st stt -> Component eff st) -> Component eff st
foreach_ lns f = { render }
  where
    render effect st = concat $ do
      Tuple index item <- zip (range 0 (length items - 1)) items
      pure $ (f index (over lns (\arr -> unsafePartial $ fromJust $ deleteAt index arr)) (lns <<< lensAt index)).render effect st
      where
        items = view lns st

--------------------------------------------------------------------------------

onClick :: forall eff st. (R.Event -> Effect eff st Unit) -> Props eff st
onClick f effect = P.onClick \e -> effect (f e)

onMouseDown :: forall eff st. (R.MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseDown f effect = P.onMouseDown \e -> effect (f e)

onMouseUp :: forall eff st. (R.MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseUp f effect = P.onMouseUp \e -> effect (f e)

onMouseMove :: forall eff st. (R.MouseEvent -> Effect eff st Unit) -> Props eff st
onMouseMove f effect = P.onMouseMove \e -> effect (f e)

onMouseDrag :: forall eff st. (MouseDragState -> Effect eff st Unit) -> Props eff st
onMouseDrag f = onMouseDown \e -> do
  f (DragStart e)
  onDragStart f

shapeRendering :: forall eff st. String -> Props eff st
shapeRendering v _ = P.unsafeMkProps "shapeRendering" v

width :: forall eff st. String -> Props eff st
width v _ = P.width v

height :: forall eff st. String -> Props eff st
height v _ = P.height v

d :: forall eff st. String -> Props eff st
d v _ = P.unsafeMkProps "d" v

x :: forall eff st. String -> Props eff st
x v _ = P.unsafeMkProps "x" v

y :: forall eff st. String -> Props eff st
y v _ = P.unsafeMkProps "y" v

cx :: forall eff st. String -> Props eff st
cx v _ = P.unsafeMkProps "cx" v

cy :: forall eff st. String -> Props eff st
cy v _ = P.unsafeMkProps "cy" v

r :: forall eff st. String -> Props eff st
r v _ = P.unsafeMkProps "r" v

rx :: forall eff st. String -> Props eff st
rx v _ = P.unsafeMkProps "rx" v

ry :: forall eff st. String -> Props eff st
ry v _ = P.unsafeMkProps "ry" v

stroke :: forall eff st. String -> Props eff st
stroke v _ = P.unsafeMkProps "stroke" v

strokeWidth :: forall eff st. String -> Props eff st
strokeWidth v _ = P.unsafeMkProps "strokeWidth" v

strokeDashArray :: forall eff st. String -> Props eff st
strokeDashArray v _ = P.unsafeMkProps "strokeDasharray" v

fill :: forall eff st. String -> Props eff st
fill v _ = P.unsafeMkProps "fill" v

textAlign :: forall eff st. String -> Props eff st
textAlign v _ = P.unsafeMkProps "textAlign" v

textAnchor :: forall eff st. String -> Props eff st
textAnchor v _ = P.unsafeMkProps "textAnchor" v

fontFamily :: forall eff st. String -> Props eff st
fontFamily v _ = P.unsafeMkProps "fontFamily" v

fontSize :: forall eff st. String -> Props eff st
fontSize v _ = P.unsafeMkProps "fontSize" v

fontWeight :: forall eff st. String -> Props eff st
fontWeight v _ = P.unsafeMkProps "fontWeight" v

color :: forall eff st. String -> Props eff st
color v _ = P.unsafeMkProps "color" v

div :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
div props children = { render: \effect st -> [ R.div (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

style :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
style props children = { render: \effect st -> [ R.style (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

svg :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
svg props children = { render: \effect st -> [ SVG.svg (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

g :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
g props children = { render: \effect st -> [ SVG.g (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

circle :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
circle props children = { render: \effect st -> [ SVG.circle (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

rect :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
rect props children = { render: \effect st -> [ SVG.rect (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

path :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
path props children = { render: \effect st -> [ SVG.path (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

svgtext :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
svgtext props children = { render: \effect st -> [ SVG.text (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

text :: forall eff st. String -> Component eff st
text str = { render: \_ _ -> [ R.text str ] }

--------------------------------------------------------------------------------

elementIndex :: forall eff st. Int -> Props eff st
elementIndex index _ = P.unsafeMkProps "data-element-index" (show index)

getElementChildren :: forall eff. R.ReactElement -> Eff (props :: R.ReactProps | eff) (Array R.ReactElement)
getElementChildren e = R.getChildren (unsafeCoerce e)

getElementAllChildren :: forall eff. R.ReactElement -> Eff (props :: R.ReactProps | eff) (Array R.ReactElement)
getElementAllChildren e = do
  cs <- R.getChildren (unsafeCoerce e)
  cs' <- concat <$> traverse getElementAllChildren cs
  pure $ cs <> cs'

mkSpec :: forall eff st. st -> Component eff st -> R.ReactSpec Unit st eff
mkSpec st cmp = R.spec st \this -> do
  st' <- R.readState this
  pure $ R.div [] (cmp.render (unsafeCoerce interpretEffect $ this) st')

type AppState = (Maybe Vec × Maybe Vec) × (Maybe String × Array Int)

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui)
  -- where ui = R.createFactory (R.createClass (mkSpec (Tuple Nothing []) list)) unit
  where ui = R.createFactory (R.createClass (mkSpec (Tuple (Tuple Nothing Nothing) (Tuple Nothing [])) list)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          -- void $ traceAnyM $ show $ from (undefined :: A)
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

uicmp :: UIComponent
uicmp = UIComponent c1
  where
   UIComponent c1 = layoutUIComponent (300.5 × 100.5 × 800.0 × 400.0) testComponent
   c2 = case c1.internal A.!! 0 of
     Just i  -> layoutUIComponent i.inner testComponent2
     Nothing -> undefined
   c1' = (c1 { internal = fromMaybe undefined (A.modifyAt 0 (\uii -> uii { component = Just c2 }) c1.internal) })

list :: forall eff. Component eff AppState
list = div
 []
 [ svg [ shapeRendering "geometricPrecision", width "2000px", height "1000px" ]
   [ uicomponent uicmp
   ]
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

type UIExternal = 
  { conns   :: Array (Label × Vec × RArgIndex)
  }

type UIInternal = 
  { outer     :: Rect
  , inner     :: Rect
  , arg       :: RArgIndex
  , conns     :: Array (Label × Vec × RArgIndex)
  , component :: Maybe UIComponent
  }

newtype UIComponent = UIComponent
  { component :: RComponent
  , bounds    :: Rect
  , external  :: UIExternal
  , internal  :: Array UIInternal
  }

layoutUIComponent :: Rect -> RComponent -> UIComponent
layoutUIComponent bounds@(bx × by × bw × bh) cmp@(RComponent rcmp) = UIComponent
  { component: cmp
  , bounds   : bounds
  , external :
    { conns: map (conn ((bx - gap) × by)) (indexedRange rcmp.external)
    }
  , internal : [] -- map internal (indexedRange rcmp.internal)
  }
  where
    ccount = I.toNumber (length rcmp.internal)
    gap    = 30.0

    cw     = (bw - (gap * (ccount + 1.0))) / ccount
    cy     = by + gap

    conn :: Vec -> Int × RType × RArgIndex -> Label × Vec × RArgIndex
    conn (ox × oy) (index × t × aindex) = show t × (ox × (oy + I.toNumber index * gap)) × aindex

    internal :: Int × Array (RType × RArgIndex) × RArgIndex -> UIInternal
    internal (index × args × aindex) =
      { outer    : cx × (by + gap) × cw × (bh - 2.0 * gap)
      , inner    : (cx + gap * 5.0) × (by + gap * 4.0) × (cw - gap * 6.0) × (bh - 6.0 * gap)
      , arg      : aindex
      , conns    : map (conn ((cx + gap) × (cy + gap))) (indexedRange args)
      , component: Nothing
      }
      where
        index' = I.toNumber index
        cx     = bx + (index' + 1.0) * gap + index' * cw

uicomponent :: forall eff st. UIComponent -> Component eff st
uicomponent (UIComponent uicmp) = g [] $
  [ rect
    [ x (px bx), y (px by), width (px bw), height (px bh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", fill "transparent" ]
    []
  ]
  <> map (conn "end") uicmp.external.conns
  <> map container uicmp.internal
  where
    bx × by × bw × bh = uicmp.bounds

    container :: UIInternal -> Component eff st
    container uiint = g [] $
      [ rect
        [ x (px ox), y (px oy), width (px ow), height (px oh), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", fill "transparent" ]
        []
      ]
      <> case uiint.component of
           Just child -> [ uicomponent child ]
           Nothing    ->
             [ rect
               [ x (px ix), y (px iy), width (px iw), height (px ih), rx (px 7.0), ry (px 7.0), stroke "#d90e59", strokeWidth "3", strokeDashArray "5, 5", fill "transparent" ]
               []
             ]
      <> map (conn "start") uiint.conns
      where
        ox × oy × ow × oh = uiint.outer
        ix × iy × iw × ih = uiint.inner

    conn :: String -> Label × Vec × RArgIndex -> Component eff st
    conn align (name × (x × y) × _) = g []
      [ circle
        [ cx (px x'), cy (px y'), r (px 5.0), fill "transparent", stroke "#d90e59", strokeWidth (px 2.0) ]
        []
      , label (x' + offset align × y' + 4.0) align name
      ]
      where
        x' = x - 7.0
        y' = y + 7.0

        offset "start" = 20.0
        offset "end"   = -20.0
        offset _       = 0.0
