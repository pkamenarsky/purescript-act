module Act where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Free
import Data.Argonaut.Core
import Data.Array
import Data.Functor
import Data.Traversable
import Data.Lens
import Data.Maybe
import Data.Lens.Index
import Data.Tuple
import Unsafe.Coerce
import Prelude
import DOM as D
import DOM.Node.Types as D
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactDOM as RD
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import React (transformState)

foreign import traceAny :: forall a b. a -> (Unit -> b) -> b

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

data StaticPtr a = StaticPtr Int

foreign import static_ :: forall a. a -> Int
foreign import derefStatic_ :: forall a. Int -> a

static :: forall a. a -> StaticPtr a
static = static_ >>> StaticPtr

derefStatic :: forall a. StaticPtr a -> a
derefStatic (StaticPtr ptr) = derefStatic_ ptr

--------------------------------------------------------------------------------

data EffectF eff st next =
    Modify (st -> st) next
  | Effect Json (Json -> Eff eff Json) (Json -> next)

derive instance functorEffectF :: Functor (EffectF eff st)

type Effect eff st = Free (EffectF eff st)

mapEffectF :: forall eff st stt next. Lens' st stt -> EffectF eff stt next -> EffectF eff st next
mapEffectF lns (Modify f next) = Modify (over lns f) next
mapEffectF lns (Effect json eff next) = Effect json eff next

mapEffect :: forall eff st stt a. Lens' st stt -> Effect eff stt a -> Effect eff st a
mapEffect lns m = hoistFree (mapEffectF lns) m

modify :: forall eff st. (st -> st) -> (Effect eff st Unit)
modify f = liftF $ Modify f unit

getHTTP :: forall eff st. String -> Effect eff st (Maybe String)
getHTTP url = liftF (Effect (fromString url) (\_ -> pure $ fromString "Result'") toString)

interpretEffect :: forall eff st a. R.ReactThis Unit st -> Effect eff st a -> Eff (state :: R.ReactState R.ReadWrite | eff) a
interpretEffect this m = runFreeM go m
  where
    go (Modify f next) = do
      _ <- (unsafeCoerce R.transformState) this f
      void $ traceAnyM $ static f
      pure next
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

div :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
div props children = { render: \effect st -> [ R.div (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

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
  where ui = R.createFactory (R.createClass (mkSpec (Tuple Nothing []) list)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
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

list :: forall eff. Component eff (Tuple (Maybe String) (Array Int))
list =
  div
    [ ]
    [ state $ text <<< show
    , div [ onClick $ const ajax ] [ text "+" ]
    -- , zoom _1 counter
    , foreach _2 counter
    , foreach_ _2 \_ d l -> counter_ d l
    ]
  where
    ajax = do
      res <- getHTTP "http://google.com"
      modify \(Tuple _ arr) -> (Tuple res (cons 0 arr))
