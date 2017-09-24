module Match where

import Control.Apply
import Prelude
import Data.Array
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Tuple
import Data.Tuple.Nested
import Data.Traversable
import Unsafe.Coerce
import Data.Generic
import Undefined
import Trace
import Data.Array as A
import Data.List as L
import Data.Map as M

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
  show (RApp a b)         = show a <> "<" <> show b <> ">"
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

newtype RArgIndex = RArgIndex (List Int)

instance eqRArgIndex :: Eq RArgIndex where
  eq (RArgIndex ai) (RArgIndex ai') = ai == ai'

instance showRArgIndex :: Show RArgIndex where
  show (RArgIndex index) = show index

instance semigroupRArgIndex :: Semigroup RArgIndex where
  append (RArgIndex ai) (RArgIndex ai') = RArgIndex (ai <> ai')

instance monoidRArgIndex :: Monoid RArgIndex where
  mempty = RArgIndex Nil

derive instance genericRArgIndex :: Generic RArgIndex

argRange :: Int -> Array RArgIndex
argRange x = map (RArgIndex <<< L.singleton) $ A.range 0 (x - 1)

zipArgRange :: forall a. Array a -> Array (a × RArgIndex)
zipArgRange a = A.zip a (argRange $ A.length a)

newtype RComponent = RComponent
  { rtype    :: RType
  , external :: Array (RType × RArgIndex)
  , internal :: Array (Array (Either RType RComponent × RArgIndex) × RArgIndex)
  }

newtype RComponent' = RComponent'
  { rtype    :: RType
  , utype    :: List (Either RType (List (Either RType RComponent')))
  }

extractComponents'' :: RType -> Either RType RComponent'
extractComponents'' = undefined

instance showRComponent :: Show RComponent where
  show = show' 0
    where
      show' :: Int -> RComponent -> String
      show' indent (RComponent rcmp) = joinWith "\n" $
        [ prefix <> "<external>: "
        , joinWith "\n" $ map (\(t × RArgIndex a) -> prefix' <> show a <> " × " <> show t) rcmp.external
        , joinWith "\n" $ flip map rcmp.internal \(args × iai) -> joinWith "\n" $
           [ prefix <> "<" <> show iai <> " internal>:"
           , joinWith "\n" $ flip map args \(x × RArgIndex a') -> case x of
               Left  t -> prefix' <> show a' <> " × " <> show t
               Right c -> prefix' <> show a' <> " × <component>:\n" <> show' (indent + 4) c
           ]
        ]
        where
          prefix  = fromCharArray $ A.replicate indent ' '
          prefix' = fromCharArray $ A.replicate (indent + 2) ' '

instance semigroupRComponent :: Semigroup RComponent where
  append (RComponent cmp1) (RComponent cmp2) = RComponent
    { rtype   : cmp1.rtype
    , external: cmp1.external <> cmp2.external
    , internal: cmp1.internal <> cmp2.internal
    }

extractComponents :: RType -> Either RType RComponent
extractComponents t = extractComponents' t $ RComponent { rtype: t, external: [], internal: [] }
  where
    extractComponents' :: RType -> RComponent -> Either RType RComponent
    extractComponents' (RConst (Const "Component")) cmp' = Right cmp'
    extractComponents' (RFun args (RConst (Const "Component"))) cmp'
      = Right $ foldr extractArg cmp' $ zipArgRange (A.fromFoldable args)
      where
        extractArg :: RType × RArgIndex -> RComponent -> RComponent
        extractArg (t@(RConst _) × i)  cmp = cmp <> RComponent { rtype: undefined, external: [t × i], internal: [] }
        extractArg (t@(RVar   _) × i)  cmp = cmp <> RComponent { rtype: undefined, external: [t × i], internal: [] }
        extractArg (t@(RRecord _) × i) cmp = cmp <> RComponent { rtype: undefined, external: [t × i], internal: [] }
        extractArg (t@(RApp _ _) × i)  cmp = cmp <> RComponent { rtype: undefined, external: [t × i], internal: [] }
        extractArg (t@(RFun args (RConst (Const "Component"))) × i) cmp
          = cmp <> RComponent
              { rtype   : undefined
              , external: []
              , internal: [ map (\(x × ai) -> x × (i <> ai)) (zipArgRange (map extractComponents (A.fromFoldable args))) × i]
              }
        extractArg (t@(RFun _ _) × i)  cmp = cmp <> RComponent { rtype: undefined, external: [t × i], internal: [] }
    extractComponents' t _ = Left t

unifyType :: RType -> RType -> RTransform
unifyType (RConst t@(Const c)) (RConst t'@(Const c'))
  | c == c'   = REqual
  | otherwise = RMap t t'
unifyType (RConst t@(Const _)) (RVar v) = RSpecify v t
unifyType t@(RApp _ _) t'@(RApp _ _) = unifyApp t t'
  where
    unifyApp (RApp f x) (RApp f' x')
      | f == f'   = unifyApp x x'
      | otherwise = RNoMatch
    unifyApp u u' = unifyType u u'
unifyType _ _ = RNoMatch

specifyType :: RTransform -> RType -> RType
specifyType (RSpecify v c) t@(RConst _) = t
specifyType (RSpecify v c) t@(RVar v')
  | v == v'   = RConst c
  | otherwise = t
specifyType tr@(RSpecify v c) (RApp f x)  = RApp (specifyType tr f) (specifyType tr x)
specifyType tr@(RSpecify v c) (RFun as r) = RFun (map (specifyType tr) as) (specifyType tr r)
specifyType _ t = t

getType :: RArgIndex -> RType -> RType
getType as r | trace_ (as × r) false = undefined
getType (RArgIndex (Cons 0 as)) (RFun (Cons t _) _) = getType (RArgIndex as) t
getType (RArgIndex (Cons 0 as)) t = getType (RArgIndex as) t
getType (RArgIndex (Cons i as)) (RFun (Cons f fs) r)
  = getType (RArgIndex (Cons (i - 1) as)) (RFun fs r)
getType (RArgIndex Nil) t = t
getType _ _ = undefined

--------------------------------------------------------------------------------

a :: RType
a = RVar (Var "A")

b :: RType
b = RVar (Var "B")

person :: RType
person = RConst (Const "Person")

address :: RType
address = RConst (Const "Address")

location :: RType
location = RConst (Const "Location")

component :: RType
component = RConst (Const "Component")

array :: RType -> RType
array t = RApp (RConst (Const "Array")) t

navigationStack :: RType
navigationStack = RConst (Const "NavigationStack")

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
componentType = fun
  [ navigationStack
  , array a
  , fun [a] component
  , fun
    [ a
    , fun
      [ location
      , a
      ]
      component
    ]
    component
  ]
  component

componentType2 :: RType
componentType2 = fun [ person, fun [a] component ] component

testComponent :: RComponent
testComponent = either undefined id (extractComponents componentType)

testComponent2 :: RComponent
testComponent2 = either undefined id (extractComponents componentType2)

testUnify :: RTransform
testUnify = unifyType (array person) (array a)

testSpecify :: RType
testSpecify = specifyType testUnify componentType

--------------------------------------------------------------------------------

type Label = String

data Expr = EVar Label | EApp Expr Expr | ELam Label Expr

data Substitution = Negative Int (List Substitution) | Positive Int | Placeholder

substitute substs (RFun (Cons a as) r) = ELam "label" (substitute substs (RFun as r))
substitute (Negative f args') (RFun args r) = foldl (\e _ -> ELam "label" e) (EApp (EVar "") undefined) args
substitute _ _ = undefined
