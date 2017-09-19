module Match where

import Prelude

import Data.Array
import Data.Maybe
import Data.Tuple

import Unsafe.Coerce

import Data.Generic

undefined :: forall a. a
undefined = unsafeCoerce unit

data RType = RVar String | RConst String | RArray RType

derive instance genericRType :: Generic RType

instance showRType :: Show RType
  where show = gShow

type RArg = { name :: String, type :: RType }

matchArgs :: RType -> Array RType -> Array (Tuple RType (Array RType))
matchArgs t args = catMaybes $ do
  arg <- args
  case matchArg t arg args of
    Just args' -> pure $ Just $ Tuple arg args'
    Nothing    -> pure Nothing
  where
    replaceArg :: String -> String -> RType -> RType
    replaceArg a b (RConst a') = RConst a'
    replaceArg a b (RVar a')
      | a == a'   = RConst b
      | otherwise = RVar a'
    replaceArg a b (RArray a') = RArray $ replaceArg a b a'

    matchArg :: RType -> RType -> Array RType -> Maybe (Array RType)
    matchArg (RConst a) (RConst b) args
      | a == b    = Just args
      | otherwise = Nothing
    matchArg (RConst a) (RVar b) args = Just $ map (replaceArg b a) args
    matchArg (RArray a) (RArray b) args = matchArg a b args
    matchArg _ _ _ = Nothing

testArgs :: Array RType
testArgs = [RVar "a", RArray (RVar "a")]
