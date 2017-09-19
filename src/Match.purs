module Match where

import Prelude
import Data.Array
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Unsafe.Coerce
import Data.Generic
import Data.Map as M

undefined :: forall a. a
undefined = unsafeCoerce unit

type T = Tuple

newtype Var = Var String

derive instance genericVar :: Generic Var

instance eqVar :: Eq Var where
  eq = gEq

instance ordVar :: Ord Var where
  compare = gCompare

newtype Const = Const String

derive instance genericConst :: Generic Const

instance eqConst :: Eq Const where
  eq = gEq

data RType = RVar Var
           | RConst Const
           | RApp RType RType
           | RFun (List RType) RType

derive instance genericRType :: Generic RType

instance showRType :: Show RType
  where show = gShow

instance eqRType :: Eq RType
  where eq = gEq

data RCoercion = RCoercion RType RType

derive instance genericRCoercion :: Generic RCoercion

data RTransform = REqual
                | RSpecify Var Const
                | RMap Const Const
                | RCoerce RCoercion
                | RNoMatch
                | RTFun (List RTransform) RTransform

derive instance genericRTransform :: Generic RTransform

instance showRTransform :: Show RTransform
  where show = gShow

type RCost = Number

-- generalization: i.e. Person -> a
generalizeType :: RType -> RType -> RTransform
generalizeType = undefined

-- specification: i.e. a -> Person
specifyType :: RType -> RType -> RTransform
specifyType (RConst t) (RConst u)
  | t == u    = REqual
  | otherwise = RMap t u
specifyType (RVar t) (RConst u) = RSpecify t u
specifyType (RApp t x) (RApp u y)
  | t == u    = specifyType x y
  | otherwise = RNoMatch
-- TODO: apply context to result type
specifyType (RFun args x) (RFun args' x') = RTFun (match M.empty args args' Nil) (specifyType x x')
  where
    match :: M.Map Var Const -> List RType -> List RType -> List RTransform -> List RTransform
    match ctx (Cons (RVar a) as) b'@(Cons (RConst b) bs) tr
      | Just v <- M.lookup a ctx
                  = match ctx (Cons (RConst v) as) b' tr
      | otherwise = match (M.insert a b ctx) as bs (Cons (RSpecify a b) tr) 
    match ctx (Cons a as) (Cons b bs) tr = match ctx as bs (Cons (specifyType a b) tr) 
    match _ _ _ tr = tr
specifyType _ _ = RNoMatch

type ArgList = Array RType

specifyFun :: RType -> ArgList -> Array RType -> Array (RTransform /\ ArgList)
specifyFun = undefined

testSpecify :: RTransform
testSpecify = specifyType (RApp (RConst (Const "array")) (RVar (Var "a"))) (RApp (RConst (Const "array")) (RConst (Const "person")))
