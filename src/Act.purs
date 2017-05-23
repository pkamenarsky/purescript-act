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
import ReactDOM (render)

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

type Element pst st = R.ReactElement

type Props pst st = P.Props

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type ComponentU eff st stt =
  { render :: ((st -> st) -> Handler st) -> stt -> Element Unit st
  }

type ComponentU' eff st =
  { render :: ((st -> st) -> Handler st) -> st -> Element Unit st
  }

zoomU :: forall eff st stt. Lens' st stt -> ((st -> st) -> Handler st) -> st -> ComponentU' eff stt -> Element Unit st
zoomU = undefined

testU :: forall st stt eff. Lens' st String -> Lens' st stt -> (stt -> stt) -> ComponentU eff st String
testU lns dellens delete =
  { render: \effect a -> div [ P.onClick \_ -> effect $ over dellens delete ] [ R.text a ]
  }

deleteButtonU :: forall st stt eff. Lens' st (Array stt) -> (Array stt -> Array stt) -> ComponentU eff st Unit
deleteButtonU dellens delete =
  { render: \effect a -> div [ P.onClick \_ -> effect $ over dellens delete ] [ R.text "Delete" ]
  }

type ChildComponent eff pst st =
  { render :: (EffectM eff pst st Unit -> Handler st) -> st -> Element pst st
  }

type Component eff st = forall pst. ChildComponent eff pst st

elementIndex :: forall pst st. Int -> Props pst st
elementIndex = P.unsafeMkProps "data-element-index" <<< show

onClick :: forall eff props state result. (R.Event -> R.EventHandlerContext eff props state result) -> P.Props
onClick = P.onClick

div :: forall pst st. Array (Props pst st) -> Array (Element pst st) -> Element pst st
div = R.div

zoom :: forall eff ppst pst st. Lens' pst st -> (EffectM eff ppst pst Unit -> Handler pst) -> pst -> ChildComponent eff pst st -> Element ppst pst
zoom lns effect pst cmp = cmp.render (\e -> effect (mapEffectM lns e)) (view lns pst) 

foreach :: forall eff ppst pst st. Lens' pst (Array st) -> (EffectM eff ppst pst Unit -> Handler pst) -> pst -> ((pst -> pst) -> ChildComponent eff pst st) -> Array (Element ppst pst)
foreach lns effect pst cmp = do
  Tuple index item <- zip (range 0 (length items - 1)) items
  pure $ (cmp (over lns (\arr -> unsafePartial $ fromJust $ deleteAt index arr))).render (\e -> effect (mapEffectM (lensAt index >>> lns) e)) (view (lensAt index >>> lns) pst)
  where
    items = view lns pst

foreach_ :: forall eff ppst pst st. Lens' pst (Array st) -> (EffectM eff ppst pst Unit -> Handler pst) -> pst -> (ChildComponent eff pst st) -> Array (Element ppst pst)
foreach_ lns effect pst cmp = do
  Tuple index item <- zip (range 0 (length items - 1)) items
  pure $ cmp.render (\e -> effect (mapEffectM (lensAt index >>> lns) e)) (view (lensAt index >>> lns) pst)
  where
    items = view lns pst

lensAt :: forall a. Int -> Lens' (Array a) a
lensAt index = lens (\arr -> unsafePartial $ arr `unsafeIndex` index) (unsafeUpdateAt index)

unsafeUpdateAt index arr a = unsafePartial $ fromJust $ updateAt index a arr

unitLens :: forall a. Lens' a Unit
unitLens = lens (const unit) (\s _ -> s)

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
  let e = cmp.render (unsafeCoerce interpretEffect $ this) st'
  -- ch <- getElementAllChildren e
  -- _ <- traceAnyM ch
  pure (cmp.render (unsafeCoerce interpretEffect $ this) st')

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= render ui)
  where ui = R.createFactory (R.createClass (mkSpec (Tuple "QM" []) list)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

deleteButton :: forall eff pst. (pst -> pst) -> ChildComponent eff pst Unit
deleteButton delete =
  { render: \effect _ -> div [ P.onClick \_ -> effect $ modifyParent delete ] [ R.text "delete" ]
  }

counter :: forall eff st. Lens' st Int -> (st -> st) -> ChildComponent eff st Int
counter lns delete =
  { render: \effect st ->
     div [ elementIndex 666 ]
       [ div [ P.onClick \_ -> effect dinc ] [ R.text "++" ]
       , div [ ] [ R.text $ show st ]
       , div [ P.onClick \_ -> effect (modify (_ - 1)) ] [ R.text "--" ]
       , div [ P.onClick \_ -> effect (modifyParent delete) ] [ R.text "delete" ]
       , (deleteButton (over undefined delete)).render (\e -> effect (mapEffectM unitLens e)) unit
       ]
  }
  where
    one = 1
    dinc = do
      modify (_ + one)
      modify (_ + 1)
      res <- getHTTP' "http://google.com"
      when (res == Just "Result'") (modify (_ + 1))
      pure unit

counter_ :: forall eff. Component eff Int
counter_ =
  { render: \effect st ->
     div [ ]
       [ div [ P.onClick \_ -> effect (modify (_ + 1)) ] [ R.text "+" ]
       , div [ ] [ R.text $ show st ]
       , div [ P.onClick \_ -> effect (modify (_ - 1)) ] [ R.text "-" ]
       ]
  }

list :: forall eff. Component eff (Tuple String (Array Int))
list =
  { render: \effect st ->
     div
       [ ] $ concat
       [ [ div [ P.onClick \_ -> effect (modify (\(Tuple str arr) -> Tuple str (cons 0 arr))) ] [ R.text "+" ]
         ]
       , foreach _2 effect st (counter undefined)
       , foreach_ _2 effect st counter_
       ]

  }

--------------------------------------------------------------------------------

data StaticPtr a = StaticPtr Int

foreign import static_ :: forall a. a -> Int
foreign import derefStatic_ :: forall a. Int -> a

static :: forall a. a -> StaticPtr a
static = static_ >>> StaticPtr

derefStatic :: forall a. StaticPtr a -> a
derefStatic (StaticPtr ptr) = derefStatic_ ptr
