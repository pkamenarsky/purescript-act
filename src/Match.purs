module Match where

import Control.Apply

import Prelude
import Data.Array
import Data.Array as A
import Data.List
import Data.List as L
import Data.Maybe
import Data.String
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

instance showRType :: Show RType where
  show (RVar (Var a))     = a
  show (RConst (Const a)) = a
  show (RApp a b)         = show a <> " " <> show b
  show (RFun as r)        = "(" <> joinWith " -> " (A.fromFoldable (map show as <> (Cons (show r) Nil))) <> ")"

instance eqRType :: Eq RType where
  eq = gEq

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

type Ctx = M.Map Var Const

cost :: RTransform -> Maybe RCost
cost REqual = Just 0
cost (RSpecify _ _) = Just 0
cost (RMap _ _) = Just 1
cost (RCoerce _) = Just 1
cost RNoMatch = Nothing
cost (RTFun args result) = lift2 (+) (map (foldr (+) 0) (sequence (map cost args))) (cost result)

--------------------------------------------------------------------------------

a :: RType
a = RVar (Var "a")

person :: RType
person = RConst (Const "person")

address :: RType
address = RConst (Const "address")

location :: RType
location = RConst (Const "location")

component :: RType
component = RConst (Const "component")

array :: RType -> RType
array t = RApp (RConst (Const "array")) t

fun :: Array RType -> RType -> RType
fun args t = RFun (L.fromFoldable args) t

sidebarTypes :: Array RType
sidebarTypes =
  [ person
  , array person
  , address
  , array address
  , location
  , array location
  ]

componentType :: RType /\ RType
componentType = array a /\ fun [ array a, fun [a] component ] component
