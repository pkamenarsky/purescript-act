module Match where

import Control.Apply
import Prelude
import Data.Array
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Tuple
import Data.Tuple.Nested
import Data.Traversable
import Unsafe.Coerce
import Data.Generic
import Data.Array as A
import Data.List as L
import Data.Map as M

undefined :: forall a. a
undefined = unsafeCoerce unit

infixr 6 Tuple as ×
infixr 6 type Tuple as ×

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
           | RRecord (Array (String × String))
           | RApp RType RType
           | RFun (List RType) RType

derive instance genericRType :: Generic RType

instance showRType :: Show RType where
  show (RVar (Var a))     = a
  show (RConst (Const a)) = a
  show (RRecord m)        = "{" <> joinWith "," (map (\(f × t) -> f <> ": " <> t) m) <> "}"
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

derive instance genericRTransform :: Generic RTransform

instance showRTransform :: Show RTransform
  where show = gShow

type RCost = Int

type Ctx = M.Map Var Const

type Index = Int

newtype RComponent = RComponent
  { external :: Array (RType × Index)
  , internal :: Array (Array (RType × Index) × Index)
  }

derive instance genericRComponent :: Generic RComponent

instance showRComponent :: Show RComponent where
  show = gShow

instance monoidRComponent :: Monoid RComponent where
  mempty = RComponent { external: [], internal: [] }

instance semigroupRComponent :: Semigroup RComponent where
  append (RComponent cmp1) (RComponent cmp2) = RComponent
    { external: cmp1.external <> cmp2.external
    , internal: cmp1.internal <> cmp2.internal
    }

extractComponents :: RType -> RComponent
extractComponents t = extractComponents' t mempty
  where
    extractComponents' :: RType -> RComponent -> RComponent
    extractComponents' (RConst (Const "component")) cmp' = cmp'
    extractComponents' (RFun args (RConst (Const "component"))) cmp'
      = foldr extractArg cmp' (L.zip args (0 L... (L.length args - 1)))
      where
        extractArg :: RType × Index -> RComponent -> RComponent
        extractArg (t@(RConst _) × i)  cmp = cmp <> RComponent { external: [t × i], internal: [] }
        extractArg (t@(RVar   _) × i)  cmp = cmp <> RComponent { external: [t × i], internal: [] }
        extractArg (t@(RRecord _) × i) cmp = cmp <> RComponent { external: [t × i], internal: [] }
        extractArg (t@(RApp _ _) × i)  cmp = cmp <> RComponent { external: [t × i], internal: [] }
        extractArg (t@(RFun args (RConst (Const "component"))) × i) cmp
                                     = cmp <> RComponent { external: [], internal: [A.fromFoldable (L.zip args (0 L... (L.length args - 1))) × i] }
        extractArg (t@(RFun _ _) × i)  cmp = cmp <> RComponent { external: [t × i], internal: [] }
    extractComponents' _ _ = undefined

unifyType :: Const -> RType -> RTransform
unifyType t@(Const c) (RConst t'@(Const c'))
  | c == c'   = REqual
  | otherwise = RMap t t'
unifyType t@(Const _) (RVar v) = RSpecify v t
unifyType _ _ = RNoMatch

specifyType :: RTransform -> RType -> RType
specifyType (RSpecify v c) t@(RConst _) = t
specifyType (RSpecify v c) t@(RVar v')
  | v == v'   = RConst c
  | otherwise = t
specifyType tr@(RSpecify v c) (RApp f x)  = RApp (specifyType tr f) (specifyType tr x)
specifyType tr@(RSpecify v c) (RFun as r) = RFun (map (specifyType tr) as) (specifyType tr r)
specifyType _ t = t

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

componentType :: RType
componentType = fun [ array a, fun [a, person] component ] component
