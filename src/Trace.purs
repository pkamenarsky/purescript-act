module Trace where

import Prelude

foreign import trace :: forall a b. b -> a -> a

foreign import traceAny :: forall a b. a -> (Unit -> b) -> b

traceAnyM :: forall m a. Monad m => a -> m a
traceAnyM s = traceAny s \_ -> pure s

trace_ :: forall a b. b -> a -> a
trace_ = trace
