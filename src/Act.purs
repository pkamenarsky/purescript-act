module Act where

import Control.Monad.Eff
import Data.Lens
import Data.Maybe (fromJust)
import Data.Tuple
import Unsafe.Coerce
import Partial.Unsafe (unsafePartial)
import Prelude
import DOM as D
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import DOM.Node.Types as D
import React as R
import React.DOM as R
import React.DOM.Props as P
import React (transformState)
import React.DOM.Props (onClick)
import ReactDOM (render)

data Effect st' st =
    Pure (st -> st)
  | Parent (st' -> st')
  | GetHTTP String (String -> Effect st' st)

type Element pst st = R.ReactElement

type Props pst st = P.Props

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type ChildComponent pst st =
  { render :: st -> (Effect pst st -> Handler st) -> Element pst st
  }

type Component st = forall pst. ChildComponent pst st

div :: forall pst st. Array (Props pst st) -> Array (Element pst st) -> Element pst st
div = R.div

counter :: Component Int
counter =
  { render: \st dispatch -> div [ P.onClick \e -> dispatch $ Pure (_ + 1) ] [ R.text ("counter: " <> show st) ]
  }

mapEffect :: forall st st' st''. Lens' st' st -> Effect st' st -> Effect st'' st'
mapEffect lns (Pure f) = Pure (over lns f)
mapEffect lns (GetHTTP url eff) = GetHTTP url \r -> (mapEffect lns (eff r))
mapEffect lns (Parent f) = Pure f

mkSpec :: forall eff st. st -> Component st -> R.ReactSpec Unit st eff
mkSpec st cmp = R.spec st \this -> do
  st' <- R.readState this
  pure (cmp.render st' (dispatch this))
  where
    dispatch this (Pure f) = R.transformState this f
    dispatch this _ = unsafeCoerce unit

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= render ui)
  where ui = R.createFactory (R.createClass (mkSpec 0 counter)) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm
