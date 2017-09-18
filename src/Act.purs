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
import Data.Int as I
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
import DOM as D
import DOM.Node.Types as D
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

modify :: forall eff st. (st -> st) -> (Effect eff st Unit)
modify f = liftF $ Modify f unit

getHTTP :: forall eff st. String -> Effect eff st (Maybe String)
getHTTP url = liftF (Effect (fromString url) (\_ -> pure $ fromString "Result'") toString)

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

fill :: forall eff st. String -> Props eff st
fill v _ = P.unsafeMkProps "fill" v

div :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
div props children = { render: \effect st -> [ R.div (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

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

deleteButton :: forall st eff. (st -> st) -> Lens' st Unit -> Component eff st
deleteButton delete lns = div [ onClick $ const $ modify delete ] [ text "Delete Button" ]

counter_ :: forall eff st. (st -> st) -> Lens' st Int -> Component eff st
counter_ delete lns =
  div [ ]
    [ div [ zoomProps lns $ onClick $ const $ modify (_ + 1) ] [ text "++" ]
    , div [ ] [ zoomState lns \ st -> text $ show st ]
    , div [ zoomProps lns $ onClick $ const $ modify (_ - 1) ] [ text "--" ]
    , div [ onClick $ const $ modify delete ] [ text "Delete" ]
    , deleteButton delete (lns <<< unitLens)
    ]

counter :: forall eff. Component eff Int
counter =
  div [ ]
    [ div [ onClick $ const $ modify (_ + 1) ] [ text "++" ]
    , div [ ] [ state \st -> text $ show st ]
    , div [ onClick $ const $ modify (_ - 1) ] [ text "--" ]
    ]

list :: forall eff. Component eff (Tuple (Tuple (Maybe RPosition) (Maybe RPosition)) (Tuple (Maybe String) (Array Int)))
list =
  div
    [ ]
    [ svg [ shapeRendering "geometricPrecision", width "500px", height "500px" ]
      [ zoom _1 $ rcomponent testRComponent
      ]
    -- , state $ text <<< show
    -- , div [ onClick $ const ajax ] [ text "+" ]
    -- , div [ onClick $ const $ modify \(Tuple str arr) -> (Tuple str (cons 0 arr)) ] [ text "+" ]
    -- , zoom _1 counter
    -- , foreach (_2 >>> _2) counter
    -- , foreach_ (_2 >>> _2) \_ d l -> counter_ d l
    ]
  where
    ajax = do
      res <- getHTTP "http://google.com"
      modify \(Tuple _ arr) -> (Tuple res (cons 0 arr))

--------------------------------------------------------------------------------

testRComponent :: RComponent
testRComponent =
  { pos: { x: 100.0, y: 100.0 }
  , label: "Add"
  , args: [ { name: "add", type: RForall "A" } ]
  }

type RPosition = { x :: Number, y :: Number }

data RType = RForall String | RNamed String | RArray RType

type RArg = { name :: String, type :: RType }

type RComponent =
  { pos   :: RPosition
  , label :: String
  , args  :: Array RArg
  }

px :: Number -> String
px x = show x <> "px"

line :: forall eff st. RPosition -> RPosition -> Component eff st
line start end = path [ strokeWidth (px 3.0), stroke "#333333", d ("M" <> show start.x <> " " <> show start.y <> " L" <> show end.x <> " " <> show end.y)] []

rcomponent :: forall eff. RComponent -> Component eff (Tuple (Maybe RPosition) (Maybe RPosition))
rcomponent rcmp = state \st -> g [] $
  [ rect
    [ x (px rcmp.pos.x), y (px rcmp.pos.y), width (px 150.0), height (px 50.0), rx (px 5.0), ry (px 5.0), fill  "#d90e59" ]
    []
  ]
  <> map arg (zip (range 0 (length rcmp.args)) (rcmp.args))
  <> case st of
       Tuple (Just start) (Just end)  -> [ line start end ]
       _ -> []
  where
    arg :: Tuple Int RArg -> Component eff (Tuple (Maybe RPosition) (Maybe RPosition))
    arg (Tuple index rarg) = circle
      [ onMouseDrag drag
      , cx (px $ rcmp.pos.x - 20.0), cy (px $ rcmp.pos.y - 0.0 + I.toNumber index * 15.0), r (px 7.0), fill "transparent", stroke "#d90e59", strokeWidth (px 3.0) ]
      []
      where
        drag (DragStart e) = modify $ const (Tuple (Just { x: e.pageX, y: e.pageY }) Nothing)
        drag (DragMove e)  = modify \(Tuple start _) -> Tuple start (Just { x: e.pageX, y: e.pageY })
        drag (DragEnd e)   = modify \(Tuple start _) -> Tuple start (Just { x: e.pageX, y: e.pageY })
