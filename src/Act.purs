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
import React.DOM.Props (onClick)
import ReactDOM as RD

foreign import traceAny :: forall a b. a -> (Unit -> b) -> b

traceAnyM :: forall m a. Monad m => a -> m a
traceAnyM s = traceAny s \_ -> pure s

undefined :: forall a. a
undefined = unsafeCoerce unit

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

type Element st = R.ReactElement

type Props st = P.Props

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

lensAt :: forall a. Int -> Lens' (Array a) a
lensAt index = lens (\arr -> unsafePartial $ arr `unsafeIndex` index) (unsafeUpdateAt index)

unsafeUpdateAt index arr a = unsafePartial $ fromJust $ updateAt index a arr

--------------------------------------------------------------------------------

type Component eff st =
  { render :: ((st -> st) -> Handler st) -> st -> Element st
  }

render :: forall eff st. Component eff st -> ((st -> st) -> Handler st) -> st -> Element st
render cmp = cmp.render

zoomC :: forall eff st stt. Lens' st stt -> ((st -> st) -> Handler st) -> Component eff stt -> Component eff st
zoomC lns effect cmp =
  { render: \effect st -> cmp.render (\f -> effect (over lns f)) (view lns st)
  }

zoom :: forall eff st stt. Lens' st stt -> ((st -> st) -> Handler st) -> st -> Component eff stt -> Element st
zoom lns effect st cmp = cmp.render (\e -> effect (over lns e)) (view lns st)

foreach_ :: forall eff st stt. Lens' st (Array stt) -> ((st -> st) -> Handler st) -> st -> Component eff stt -> Array (Element st)
foreach_ lns effect st cmp = do
  Tuple index item <- zip (range 0 (length items - 1)) items
  pure $ (cmp' index).render effect st
  where
    items = view lns st
    cmp' index = zoomC (lns <<< lensAt index) effect cmp

foreach :: forall eff st stt. Lens' st (Array stt) -> ((st -> st) -> Handler st) -> st -> (Int -> (st -> st) -> Lens' st stt -> Component eff st) -> Array (Element st)
foreach lns effect st f = do
  Tuple index item <- zip (range 0 (length items - 1)) items
  pure $ (cmp index).render effect st
  where
    items = view lns st
    cmp index = f index (over lns (\arr -> unsafePartial $ fromJust $ deleteAt index arr)) (lensAt index >>> lns)

--------------------------------------------------------------------------------

elementIndex :: forall st. Int -> Props st
elementIndex = P.unsafeMkProps "data-element-index" <<< show

onClick :: forall eff props state result. (R.Event -> R.EventHandlerContext eff props state result) -> P.Props
onClick = P.onClick

div :: forall st. Array (Props st) -> Array (Element st) -> Element st
div = R.div

unitLens :: forall a. Lens' a Unit
unitLens = lens (const unit) (\s _ -> s)

getElementChildren :: forall eff. R.ReactElement -> Eff (props :: R.ReactProps | eff) (Array R.ReactElement)
getElementChildren e = R.getChildren (unsafeCoerce e)

getElementAllChildren :: forall eff. R.ReactElement -> Eff (props :: R.ReactProps | eff) (Array R.ReactElement)
getElementAllChildren e = do
  cs <- R.getChildren (unsafeCoerce e)
  cs' <- concat <$> traverse getElementAllChildren cs
  pure $ cs <> cs'

mkSpec :: forall eff st. st -> ComponentR Unit st -> R.ReactSpec Unit st eff
mkSpec st cmp = R.spec st \this -> do
  st' <- R.readState this
  pure $ div [] (cmp.render (R.transformState this) st')

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui)
  where ui = R.createFactory (R.createClass (mkSpec (Tuple 666 []) listR)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

data StaticPtr a = StaticPtr Int

foreign import static_ :: forall a. a -> Int
foreign import derefStatic_ :: forall a. Int -> a

static :: forall a. a -> StaticPtr a
static = static_ >>> StaticPtr

derefStatic :: forall a. StaticPtr a -> a
derefStatic (StaticPtr ptr) = derefStatic_ ptr

--------------------------------------------------------------------------------

test :: forall st eff. Lens' st String -> (st -> st) -> Component eff st
test lns delete =
  { render: \effect st -> div [ P.onClick \_ -> effect delete ] [ R.text (view lns st) ]
  }

deleteButton :: forall st eff. (st -> st) -> Component eff st
deleteButton delete =
  { render: \effect a -> div [ P.onClick \_ -> effect delete ] [ R.text "Delete" ]
  }

deleteButtonUst :: forall st eff. (st -> st) -> Lens' st Unit -> Component eff st
deleteButtonUst delete lns =
  { render: \effect a -> div [ P.onClick \_ -> effect delete ] [ R.text "Delete" ]
  }

nested :: forall eff st stt. Lens' st stt -> ((st -> st) -> Handler st) -> st -> (((stt -> stt) -> Handler st) -> stt -> Element st) -> Element st
nested lns effect st f = f (\f -> effect (over lns f)) (view lns st)

counter_ :: forall eff st. (st -> st) -> Lens' st Int -> Component eff st
counter_ delete lns =
  { render: \effectPrn stPrn -> nested lns effectPrn stPrn \effect st ->
     div [ elementIndex 666 ]
       [ div [ P.onClick \_ -> effect (_ + 1) ] [ R.text "++" ]
       , div [ ] [ R.text $ show st ]
       , div [ P.onClick \_ -> effect (_ - 1) ] [ R.text "--" ]
       -- , div [ P.onClick \_ -> effect delete ] [ R.text "delete" ]
       , render (deleteButtonUst delete (lns <<< unitLens)) effectPrn stPrn
       ]
  }

counter :: forall eff. Component eff Int
counter =
  { render: \effect st ->
     div [ elementIndex 666 ]
       [ div [ P.onClick \_ -> effect (_ + 1) ] [ R.text "++" ]
       , div [ ] [ R.text $ show st ]
       , div [ P.onClick \_ -> effect (_ - 1) ] [ R.text "--" ]
       ]
  }

list :: forall eff. Component eff (Tuple Int (Array Int))
list =
  { render: \effect st ->
     div
       [ ] $ concat
       [ [ div [ P.onClick \_ -> effect (\(Tuple str arr) -> Tuple str (cons 0 arr)) ] [ R.text "+" ]
         ]
       , foreach_ _2 effect st counter
       , foreach _2 effect st \_ d l -> zoomC l effect counter
       , foreach _2 effect st \_ d l -> counter_ d l
       , [ zoom _1 effect st counter ]
       ]
  }

--------------------------------------------------------------------------------

type ComponentR eff st =
  { render :: ((st -> st) -> Handler st) -> st -> Array R.ReactElement
  -- , onfetchstart :: st -> st
  -- , onfetchend :: st -> st
  }

type PropsR eff st = ((st -> st) -> Handler st) -> P.Props

onClickR :: forall eff st. (R.Event -> st -> st) -> PropsR eff st
onClickR f effect = P.onClick \e -> effect (f e)

divR :: forall eff st. Array (PropsR eff st) -> Array (ComponentR eff st) -> ComponentR eff st
divR props children = { render: \effect st -> [ div (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

textR :: forall eff st. String -> ComponentR eff st
textR str = { render: \_ _ -> [ R.text str ] }

stateR :: forall eff st. (st -> ComponentR eff st) -> ComponentR eff st
stateR f = { render: \effect st -> (f st).render effect st }

--------------------------------------------------------------------------------

zoomR :: forall eff st stt. Lens' st stt -> ComponentR eff stt -> ComponentR eff st
zoomR lns cmp = { render: \effect st -> cmp.render (\e -> effect (over lns e)) (view lns st) }

foreachR :: forall eff st stt. Lens' st (Array stt) -> ComponentR eff stt -> ComponentR eff st
foreachR lns cmp = { render }
  where
    render effect st = concat $ do
      Tuple index item <- zip (range 0 (length items - 1)) items
      pure $ cmp.render (\f -> effect (over (lns <<< lensAt index) f)) (unsafePartial $ fromJust $ items !! index)
      where
        items = view lns st

foreachR_ :: forall eff st stt. Lens' st (Array stt) -> (Int -> (st -> st) -> Lens' st stt -> Component eff st) -> Array (Component eff st)
foreachR_ lns f = undefined -- do
  -- Tuple index item <- zip (range 0 (length items - 1)) items
  -- pure { render: \effect st -> (cmp index).render effect st }
  -- where
  --   render st index effect st (cmp index).render effect st
  --   items = view lns st
  --   cmp index = f index (over lns (\arr -> unsafePartial $ fromJust $ deleteAt index arr)) (lensAt index >>> lns)

--------------------------------------------------------------------------------

counterR :: forall eff. ComponentR eff Int
counterR =
  divR [ ]
    [ divR [ onClickR $ const (_ + 1) ] [ textR "++" ]
    , divR [ ] [ stateR \st -> textR $ show st ]
    , divR [ onClickR $ const (_ - 1) ] [ textR "--" ]
    ]

listR :: forall eff. ComponentR eff (Tuple Int (Array Int))
listR =
  divR
    [ ]
    [ stateR $ textR <<< show
    , divR [ onClickR \_ (Tuple str arr) -> Tuple str (cons 0 arr) ] [ textR "+" ]
    , zoomR _1 counterR
    , foreachR _2 counterR
    ]
