module Match where

import Control.Apply

import Prelude
import Data.Array
import Data.List
import Data.List as L
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested
import Data.Traversable
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

type RCost = Int

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
    match ctx (Cons a as) b'@(Cons b bs) tr
      | Just (v /\ c) <- matchVar a b
        = case M.lookup v ctx of
            Nothing -> match (M.insert v c ctx) as bs (Cons (specifyType a b) tr)
            Just c' -> if c == c'
              then match ctx as bs (Cons (specifyType a b) tr)
              else match ctx as bs (Cons RNoMatch tr)
      | otherwise = match ctx as bs (Cons (specifyType a b) tr) 
    match _ _ _ tr = tr
specifyType _ _ = RNoMatch

matchVar :: RType -> RType -> Maybe (Var /\ Const)
matchVar (RVar a) (RConst b)     = Just (a /\ b)
matchVar (RApp a b) (RApp a' b')
                     | a == a'   = matchVar b b'
                     | otherwise = Nothing
matchVar _ _                     = Nothing

type ArgList = Array RType

specifyFun :: RType -> ArgList -> Array RType -> Array (RTransform /\ ArgList)
specifyFun = undefined

cost :: RTransform -> Maybe RCost
cost REqual = Just 0
cost (RSpecify _ _) = Just 0
cost (RMap _ _) = Just 1
cost (RCoerce _) = Just 1
cost RNoMatch = Nothing
cost (RTFun args result) = lift2 (+) (map (foldr (+) 0) (sequence (map cost args))) (cost result)

--------------------------------------------------------------------------------

testSpecify :: RTransform
testSpecify = specifyType
  (RApp (RConst (Const "array")) (RVar (Var "a")))
  (RApp (RConst (Const "array")) (RConst (Const "person")))

a :: RType
a = RVar (Var "a")

person :: RType
person = RConst (Const "person")

component :: RType
component = RConst (Const "component")

array :: RType -> RType
array t = RApp (RConst (Const "array")) t

fun :: Array RType -> RType -> RType
fun args t = RFun (L.fromFoldable args) t

testSpecifyFun :: RTransform
testSpecifyFun = specifyType
  (fun [a, array a] component)
  (fun [person, array person] component)
