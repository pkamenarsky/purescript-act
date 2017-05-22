module Act where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Free
import Data.Array
import Data.Functor
import Data.Lens
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

undefined :: forall a. a
undefined = unsafeCoerce unit

data Effect st' st =
    Pure (st -> st)
  | Parent (st' -> st')
  | GetHTTP String (String -> Effect st' st)

data EffectF pst st next =
    Modify (st -> st) next
  | ModifyParent (pst -> pst) next
  | GetHTTP' String (String -> next)

derive instance functorEffectF :: Functor (EffectF pst st)

mapEffectF :: forall st st' st'' next. Lens' st' st -> EffectF st' st next -> EffectF st'' st' next
mapEffectF lns (Modify f next) = Modify (over lns f) next
mapEffectF lns (ModifyParent f next) = Modify f next
mapEffectF lns (GetHTTP' url next) = GetHTTP' url next

type EffectM pst st a = Free (EffectF pst st) a

mapEffectM :: forall st st' st'' a. Lens' st' st -> EffectM st' st a -> EffectM st'' st' a
mapEffectM lns m = hoistFree (mapEffectF lns) m

modify :: forall pst st. (st -> st) -> (EffectM pst st Unit)
modify f = liftF $ Modify f unit

modifyParent :: forall pst st. (pst -> pst) -> (EffectM pst st Unit)
modifyParent f = liftF $ ModifyParent f unit

getHTTP :: forall pst st. String -> EffectM pst st String
getHTTP url = liftF $ GetHTTP' url id

interpretEffect :: forall eff pst st a. R.ReactThis Unit st -> EffectM pst st a -> Eff (state :: R.ReactState R.ReadWrite | eff) a
interpretEffect this m = runFreeM go m
  where
    go (Modify f next) = do
      R.transformState this f
      pure next
    go (ModifyParent _ _) = unsafeCrashWith "ModifyParent"
    go (GetHTTP' url next) = do
      pure $ next "Result"

type Element pst st = R.ReactElement

type Props pst st = P.Props

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type ChildComponent pst st =
  { render :: (Effect pst st -> Handler st) -> st -> Element pst st
  }

type Component st = forall pst. ChildComponent pst st

div :: forall pst st. Array (Props pst st) -> Array (Element pst st) -> Element pst st
div = R.div

mapEffect :: forall st st' st''. Lens' st' st -> Effect st' st -> Effect st'' st'
mapEffect lns (Pure f) = Pure (over lns f)
mapEffect lns (GetHTTP url eff) = GetHTTP url \r -> (mapEffect lns (eff r))
mapEffect lns (Parent f) = Pure f

zoom :: forall ppst pst st. Lens' pst st -> (Effect ppst pst -> Handler pst) -> pst -> ChildComponent pst st -> Element ppst pst
zoom lns dispatch pst cmp = cmp.render (\e -> dispatch (mapEffect lns e)) (view lns pst) 

foreach :: forall ppst pst st. Lens' pst (Array st) -> (Effect ppst pst -> Handler pst) -> pst -> ((pst -> pst) -> ChildComponent pst st) -> Array (Element ppst pst)
foreach lns dispatch pst cmp = do
  Tuple index item <- zip (range 0 (length items - 1)) items
  pure $ (cmp (over lns (\arr -> unsafePartial $ fromJust $ deleteAt index arr))).render (\e -> dispatch (mapEffect (lensAt index >>> lns) e)) (view (lensAt index >>> lns) pst)
  where
    items = view lns pst

foreach_ :: forall ppst pst st. Lens' pst (Array st) -> (Effect ppst pst -> Handler pst) -> pst -> (ChildComponent pst st) -> Array (Element ppst pst)
foreach_ lns dispatch pst cmp = do
  Tuple index item <- zip (range 0 (length items - 1)) items
  pure $ cmp.render (\e -> dispatch (mapEffect (lensAt index >>> lns) e)) (view (lensAt index >>> lns) pst)
  where
    items = view lns pst

lensAt :: forall a. Int -> Lens' (Array a) a
lensAt index = lens (\arr -> unsafePartial $ arr `unsafeIndex` index) (unsafeUpdateAt index)

unsafeUpdateAt index arr a = unsafePartial $ fromJust $ updateAt index a arr

mkSpec :: forall eff st. st -> Component st -> R.ReactSpec Unit st eff
mkSpec st cmp = R.spec st \this -> do
  st' <- R.readState this
  pure (cmp.render (dispatch this) st')
  where
    dispatch this (Pure f) = R.transformState this f
    dispatch this (Parent _) = unsafeCrashWith "Parent"
    dispatch this (GetHTTP url next) = do
      dispatch this (next "Result")

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

counter :: forall st. (st -> st) -> ChildComponent st Int
counter delete =
  { render: \dispatch st ->
     div [ ]
       [ div [ P.onClick \_ -> dispatch (Pure (_ + 1)) ] [ R.text "+" ]
       , div [ ] [ R.text $ show st ]
       , div [ P.onClick \_ -> dispatch (Pure (_ - 1)) ] [ R.text "-" ]
       , div [ P.onClick \_ -> dispatch (Parent delete) ] [ R.text "delete" ]
       ]
  }

counter_ :: Component Int
counter_ =
  { render: \dispatch st ->
     div [ ]
       [ div [ P.onClick \_ -> dispatch (Pure (_ + 1)) ] [ R.text "+" ]
       , div [ ] [ R.text $ show st ]
       , div [ P.onClick \_ -> dispatch (Pure (_ - 1)) ] [ R.text "-" ]
       ]
  }

list :: Component (Tuple String (Array Int))
list =
  { render: \dispatch st ->
     div
       [ ] $ concat
       [ [ div [ P.onClick \_ -> dispatch (Pure (\(Tuple str arr) -> Tuple str (cons 0 arr))) ] [ R.text "+" ]
         ]
       , foreach _2 dispatch st counter
       , foreach_ _2 dispatch st counter_
       ]

  }
