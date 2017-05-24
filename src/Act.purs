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
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import React (transformState)
import ReactDOM as RD

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

data EffectF eff pst st next =
    Modify (st -> st) next
  | ModifyParent (pst -> pst) next
  | Effect (Eff eff Json) (Json -> next)
  | GetHTTP String (String -> next)

derive instance functorEffectF :: Functor (EffectF eff pst st)

mapEffectF :: forall eff st st' st'' next. Lens' st' st -> EffectF eff st' st next -> EffectF eff st'' st' next
mapEffectF lns (Modify f next) = Modify (over lns f) next
mapEffectF lns (ModifyParent f next) = Modify f next
mapEffectF lns (Effect eff next) = Effect eff next
mapEffectF lns (GetHTTP url next) = GetHTTP url next

type EffectM eff pst st a = Free (EffectF eff pst st) a

mapEffectM :: forall eff st st' st'' a. Lens' st' st -> EffectM eff st' st a -> EffectM eff st'' st' a
mapEffectM lns m = hoistFree (mapEffectF lns) m

modify :: forall eff pst st. (st -> st) -> (EffectM eff pst st Unit)
modify f = liftF $ Modify f unit

modifyParent :: forall eff pst st. (pst -> pst) -> (EffectM eff pst st Unit)
modifyParent f = liftF $ ModifyParent f unit

getHTTP :: forall eff pst st. String -> EffectM eff pst st String
getHTTP url = liftF $ GetHTTP url id

getHTTP' :: forall eff pst st. String -> EffectM eff pst st (Maybe String)
getHTTP' url = liftF (Effect (pure $ fromString "Result'") toString)

interpretEffect :: forall eff pst st a. R.ReactThis Unit st -> EffectM eff pst st a -> Eff (state :: R.ReactState R.ReadWrite | eff) a
interpretEffect this m = runFreeM go m
  where
    go (Modify f next) = do
      _ <- (unsafeCoerce R.transformState) this f
      void $ traceAnyM $ static f
      pure next
    go (Effect eff next) = do
      json <- unsafeCoerce eff
      -- dump json here
      pure $ next json
    go (ModifyParent _ _) = unsafeCrashWith "ModifyParent"
    go (GetHTTP url next) = do
      pure $ next "Result"

--------------------------------------------------------------------------------

type Element st = R.ReactElement

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type Component eff st =
  { render :: ((st -> st) -> Handler st) -> st -> Array R.ReactElement
  -- , onfetchstart :: st -> st
  -- , onfetchend :: st -> st
  }

type Props eff st = ((st -> st) -> Handler st) -> P.Props

state :: forall eff st. (st -> Component eff st) -> Component eff st
state f = { render: \effect st -> (f st).render effect st }

zoom :: forall eff st stt. Lens' st stt -> Component eff stt -> Component eff st
zoom lns cmp = { render: \effect st -> cmp.render (\e -> effect (over lns e)) (view lns st) }

zoomProps :: forall eff st stt. Lens' st stt -> Props eff stt -> Props eff st
zoomProps lns effect f = effect \b -> f (lns b)

zoomState :: forall eff st stt. Lens' st stt -> (stt -> Component eff stt) -> Component eff st
zoomState lns f = { render: \effect st -> (f (view lns st)).render (\e -> effect (over lns e)) (view lns st) }

foreach :: forall eff st stt. Lens' st (Array stt) -> Component eff stt -> Component eff st
foreach lns cmp = { render }
  where
    render effect st = concat $ do
      Tuple index item <- zip (range 0 (length items - 1)) items
      pure $ cmp.render (\f -> effect (over (lns <<< lensAt index) f)) (unsafePartial $ fromJust $ items !! index)
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

onClick :: forall eff st. (R.Event -> st -> st) -> Props eff st
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

mkSpec :: forall eff st. st -> Component Unit st -> R.ReactSpec Unit st eff
mkSpec st cmp = R.spec st \this -> do
  st' <- R.readState this
  pure $ R.div [] (cmp.render (R.transformState this) st')

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui)
  where ui = R.createFactory (R.createClass (mkSpec (Tuple 666 []) list)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

deleteButton :: forall st eff. (st -> st) -> Lens' st Unit -> Component eff st
deleteButton delete lns = div [ onClick $ const delete ] [ text "Delete Button" ]

counter_ :: forall eff st. (st -> st) -> Lens' st Int -> Component eff st
counter_ delete lns =
  div [ ]
    [ div [ zoomProps lns $ onClick (\_ st -> st + 1) ] [ text "++" ]
    , div [ ] [ zoomState lns \ st -> text $ show st ]
    , div [ zoomProps lns $ onClick (\_ st -> st - 1) ] [ text "--" ]
    , div [ onClick $ const delete ] [ text "Delete" ]
    , deleteButton delete (lns <<< unitLens)
    ]

counter :: forall eff. Component eff Int
counter =
  div [ ]
    [ div [ onClick $ const (_ + 1) ] [ text "++" ]
    , div [ ] [ state \st -> text $ show st ]
    , div [ onClick $ const (_ - 1) ] [ text "--" ]
    ]

list :: forall eff. Component eff (Tuple Int (Array Int))
list =
  div
    [ ]
    [ state $ text <<< show
    , div [ onClick \_ (Tuple str arr) -> Tuple str (cons 0 arr) ] [ text "+" ]
    , zoom _1 counter
    , foreach _2 counter
    , foreach_ _2 \_ d l -> counter_ d l
    ]
