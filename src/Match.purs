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
    match :: Ctx -> List RType -> List RType -> List RTransform -> List RTransform
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

realizeType :: Ctx -> RType -> RType
realizeType ctx (RVar a)
  | Just t <- M.lookup a ctx = RConst t
  | otherwise = RVar a
realizeType ctx (RConst a)  = RConst a
realizeType ctx (RApp a b)  = RApp a (realizeType ctx b)
realizeType ctx (RFun as r) = RFun (map (realizeType ctx) as) (realizeType ctx r)

realizeType' :: RTransform -> RType -> RType
realizeType' tr a = realizeType (ctxFromTransform tr M.empty) a

ctxFromTransform :: RTransform -> Ctx -> Ctx
ctxFromTransform (RSpecify a b) ctx = M.insert a b ctx
ctxFromTransform (RTFun as r) ctx
  | RSpecify a b <- r = fold as (M.insert a b ctx)
  | otherwise = fold as ctx
ctxFromTransform  _ ctx = ctx

fold Nil ctx = ctx
fold (Cons (RSpecify a b) as) ctx = fold as (M.insert a b ctx)
fold (Cons _ as) ctx = fold as ctx

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

testSpecifyFun :: RTransform
testSpecifyFun = specifyType
  (fun [a, array a] component)
  (fun [person, array person] component)

testRealize :: RType
testRealize = realizeType'
  testSpecifyFun
  (fun [a, array a, a, a] component)

findMatches :: RType -> Array RType -> Array RTransform
findMatches t = map (specifyType t)

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

testComponentMatches :: Array RType
testComponentMatches = map (flip realizeType' args) matches
  where
    arg /\ args = componentType
    matches     = findMatches arg sidebarTypes
