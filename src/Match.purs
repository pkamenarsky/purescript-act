module Match where

import Prelude

import Data.Array
import Data.Maybe
import Data.Tuple

import Unsafe.Coerce

import Data.Generic

undefined :: forall a. a
undefined = unsafeCoerce unit

data RType = RVar String | RConst String | RApp RType RType | RFun RType RType

derive instance genericRType :: Generic RType

instance showRType :: Show RType
  where show = gShow

instance showREq :: Eq RType
  where eq = gEq

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
    replaceArg a b (RApp t u) = RApp (replaceArg a b t) (replaceArg a b u)
    replaceArg a b (RFun f x) = undefined

    matchArg :: RType -> RType -> Array RType -> Maybe (Array RType)
    matchArg (RConst a) (RConst b) args
      | a == b    = Just args
      | otherwise = Nothing
    matchArg (RConst a) (RVar b) args = Just $ map (replaceArg b a) args
    matchArg (RApp a (RConst b)) (RApp a' (RVar b')) args
      | a == a' = Just $ map (replaceArg b' b) args
    matchArg _ _ _ = Nothing

testArgs :: Array RType
testArgs = [RVar "a", RApp (RConst "array") (RVar "a")]
