module Match where

import Control.Apply
import Control.Alternative
import Control.Monad.State
import Prelude
import Data.Array
import Data.Either
import Data.Lens
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

foreign import data JSFun :: Type

foreign import jsFunFromString :: String -> JSFun

foreign import applyJSFun :: forall a. JSFun -> Array Unit -> a

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
           | RRecord (List (Label × RType))
           | RApp RType RType
           | RFun (List (Label × RType)) RType

derive instance genericRType :: Generic RType

instance showRType :: Show RType where
  show (RVar (Var a))     = a
  show (RConst (Const a)) = a
  show (RRecord m)        = "{" <> joinWith ", " (A.fromFoldable $ map (\(f × t) -> f <> ": " <> show t) m) <> "}"
  show (RApp (RConst (Const "Array")) b) = "▓ " <> show b <> ""
  show (RApp (RConst (Const "Lens")) b)  = "◈ " <> show b <> ""
  show (RApp a b)         = show a <> "<" <> show b <> ">"
  show (RFun as r)        = "(" <> joinWith " -> " (A.fromFoldable (map (\(l × t) -> l <> " : " <> show t) as <> (Cons (show r) Nil))) <> ")"

instance eqRType :: Eq RType where
  eq = gEq

--------------------------------------------------------------------------------

type TypeM = State Int

a :: RType
a = RVar (Var "A")

b :: RType
b = RVar (Var "B")

tweet :: RType
tweet = RVar (Var "Tweet")

person :: RType
person = RConst (Const "Person")

address :: RType
address = RConst (Const "Address")

location :: RType
location = RConst (Const "Location")

component :: RType
component = RConst (Const "Component")

record :: Array (Label × RType) -> RType
record m = RRecord (L.fromFoldable m)

array :: RType -> RType
array t = RApp (RConst (Const "Array")) t

navigationStack :: RType
navigationStack = RConst (Const "NavigationStack")

runType :: forall a. TypeM a -> a
runType = flip evalState 0

fun :: Array (TypeM RType) -> RType -> TypeM RType
fun args t = do
  args' <- sequence args
  index <- get
  modify (_ + A.length args')
  pure $ RFun (L.fromFoldable (A.zip (map (\a -> "a" <> show a) (index A... (index + A.length args' - 1))) args')) t

componentType :: RType
componentType = runType $ fun
  [ pure navigationStack
  , pure $ array a
  , fun [pure a] component
  , fun
    [ pure a
    , fun
      [ pure location
      , pure a
      ]
      component
    ]
    component
  ]
  component

componentType2 :: RType
componentType2 = runType $ fun [ pure person, fun [ pure a ] component ] component

--------------------------------------------------------------------------------

type Label = String

data Expr = EVar Label
          | EApp Expr (List Expr)
          | ELam (List Label) Expr
          | EPlaceholder

instance showExpr :: Show Expr where
  show (EVar v)     = v
  show (EApp f x)   = show f <> " " <> joinWith " " (map show $ A.fromFoldable x)
  show (ELam a e)   = "(λ" <> joinWith " " (A.fromFoldable a) <> " -> " <> show e <> ")"
  show EPlaceholder = "_"

data Substitution = SApp Label (List Substitution)
                  | SArg Label
                  | Placeholder

instance showSubstitution :: Show Substitution where
   show Placeholder = "_"
   show (SArg l)    = l
   show (SApp l ss) = "(" <> l <> " " <> joinWith " " (map show $ A.fromFoldable ss) <> ")"
                 
_SApp :: Prism' Substitution (Label × List Substitution)
_SApp = prism (\(l × s) -> SApp l s) $ \s -> case s of
  SApp l s -> Right (l × s)
  s        -> Left s

_SApp' :: Lens' Substitution (Label × List Substitution)
_SApp' = lens ex (\_ (l × s) -> SApp l s)
  where
    ex (SApp l s) = l × s
    ex _ = "(_SApp' error)" × L.Nil

substitute :: RType -> Substitution -> Expr
substitute tt subst = lam tt subst
  where
    lam :: RType -> Substitution -> Expr
    lam (RFun args _) subst = ELam (map fst args) (substitute' subst)
    lam _ subst             = substitute' subst

    substitute' :: Substitution -> Expr
    substitute' (SApp f xs)
      | Just t <- labeltype f tt = case t of
          RFun args _ -> EApp (EVar f) $ flip map (L.zip args xs) \((l × _) × x) -> case labeltype l tt of
            Just t' -> lam t' x
            Nothing -> EVar "substitute' error: labeltype"
          _           -> EVar "substitute' error: invalid SApp"
      | otherwise = EVar "substitute' error: labeltype"
    substitute' (SArg a)    = EVar a
    substitute' Placeholder = EPlaceholder

substituteC :: RType -> Substitution -> Expr
substituteC (RFun ((_ × tt@(RFun args _)) L.: Nil) _) subst = substitute tt subst
substituteC _ _ = EVar "substituteC error: no RFun"

data Unification = UEq | UUnify Var Const | UNone

instance showUnification :: Show Unification where
  show UEq                        = " == "
  show (UNone)                    = "0"
  show (UUnify (Var v) (Const c)) = v <> " -> " <> c

unify :: RType -> RType -> Unification
unify (RConst c) (RConst c')
  | c == c'   = UEq
  | otherwise = UNone
unify (RVar v) (RConst c)   = UUnify v c
unify (RConst c) (RVar v)   = UUnify v c
unify (RApp f x) (RApp f' x')
  | f == f'   = unify x x'
  | otherwise = UNone
unify _ _ = UNone

specialize :: M.Map Var Const -> RType -> RType
specialize ctx t = foldr (uncurry specialize') t (M.toUnfoldable ctx :: L.List (Var × Const))

specialize' :: Var -> Const -> RType -> RType
specialize'  _ _ t@(RConst _) = t
specialize' (Var v) c@(Const _) t@(RVar (Var v'))
  | v == v'   = RConst c
  | otherwise = t
specialize' v c (RApp f x)  = RApp f (specialize' v c x)
specialize' v c (RFun f xs) = RFun (map (\(l × t) -> l × specialize' v c t) f) (specialize' v c xs)
specialize' v c (RRecord fs) = RRecord (map (\(l × t) -> l × specialize' v c t) fs)

--------------------------------------------------------------------------------

labeltype :: Label -> RType -> Maybe RType
labeltype label (RFun args r) = go args
  where
    go (Cons (a × t) as)
      | a == label = Just t
      | otherwise  = labeltype label t <|> go as
    go Nil = Nothing
labeltype label (RApp f xs) = labeltype label f <|> labeltype label xs
labeltype _ _ = Nothing

type1 :: RType
type1 = runType $ fun
  [ pure a
  , pure component
  , fun [ pure a, pure location ] component
  , fun [ fun [ pure a ] component ] component
  , fun [ fun [ pure location ] component ] component
  ]
  component

type2 :: RType
type2 = runType $ fun
  [ fun
    [ fun [ pure (array tweet), fun [ pure tweet ] component ] component
    , fun [ pure location, pure tweet ] component
    , fun [ pure tweet ] component
    , pure (array tweet)
    ] component
  ]
  component
-- type2 = runType $ fun
--   [ fun
--     [ fun [ pure (array a), fun [ pure a ] component ] component
--     , fun [ pure location, pure a ] component
--     , fun [ pure a ] component
--     ] component
--   ]
--   component

type C = Int

t0 :: forall a. a -> C -> (a -> C) -> C
t0 = \a1 a2 a3 -> a3 a1

r0 :: RType
r0 = runType $ fun
  [ pure a
  , pure component
  , fun [ pure a ] component
  ]
  component

s0 :: Expr
s0 = substitute r0 (SApp "a3" (SArg "a1" L.: Nil))

t1 :: forall a. (a -> C) -> C -> ((a -> C) -> C) -> C
t1 = \a3 a4 a5 -> a5 (\a1 -> a3 a1)

r1 :: RType
r1 = runType $ fun
  [ fun [ pure a ] component
  , pure component
  , fun [ fun [ pure a ] component ] component
  ]
  component

s1 :: Expr
s1 = substitute r1 (SApp "a5" ((SApp "a3" (SArg "a2" L.: Nil)) L.: Nil))

t2 :: forall a. a -> C -> (((a -> C) -> (a -> C) -> C) -> C) -> C
t2 = \a5 a6 a7 -> a7 (\a2 a3 -> a2 a5)

r2 :: RType
r2 = runType $ fun
  [ pure a
  , pure component
  , fun [ fun [ fun [ pure a ] component, fun [ pure a ] component ] component  ] component
  ]
  component

s2 :: Expr
s2 = substitute r2 (SApp "a7" ((SApp "a2" (SArg "a5" L.: Nil)) L.: Nil))

t3 :: forall a. a -> C -> (((a -> C) -> C) -> ((a -> C) -> C) -> C) -> C
t3 = \a2 a3 a4 -> a4 (\x -> x a2) (\x -> a3)

r3 :: RType
r3 = runType $ fun
  [ pure a
  , pure component
  , fun [ fun [ fun [ pure a ] component ] component, fun [ fun [ pure a ] component ] component ] component
  ]
  component


--------------------------------------------------------------------------------

newtype RArgIndex = RArgIndex Int

instance showRArgIndex :: Show RArgIndex where
  show (RArgIndex index) = show index


extract :: RType -> Maybe (L.List (RArgIndex × Label × RType) × L.List (RArgIndex × Label × RType))
extract (RFun args (RConst (Const "Component"))) = Just (incoming 0 args L.Nil × children 0 args L.Nil)
  where
    incoming :: Int -> L.List (Label × RType) -> L.List (RArgIndex × Label × RType) -> L.List (RArgIndex × Label × RType)
    incoming index (L.Cons (_ × RFun _ _) ls) ts = incoming (index + 1) ls ts
    incoming index (L.Cons t ls) ts = (RArgIndex index × t) L.: incoming (index + 1) ls ts
    incoming index L.Nil ts = ts
    
    children :: Int -> L.List (Label × RType) -> L.List (RArgIndex × Label × RType) -> L.List (RArgIndex × Label × RType)
    children index (L.Cons t@(_ × RFun _ _) ls) ts = (RArgIndex index × t) L.: children (index + 1) ls ts
    children index (L.Cons t ls) ts = children (index + 1) ls ts
    children index L.Nil ts = ts
extract _ = Nothing

--------------------------------------------------------------------------------

exprToJS :: forall eff st. Expr -> Maybe String
exprToJS (EVar x)   = Just x
exprToJS (ELam (Cons a as) e) = do
  e' <- exprToJS (ELam as e)
  pure $ "function(" <> a <> ") { return " <> e' <> "; }"
exprToJS (ELam Nil e) = exprToJS e
exprToJS (EApp f as) = do
  f'  <- exprToJS f
  as' <- traverse exprToJS as
  pure $ "(" <> f' <> ")" <> joinWith "" (A.fromFoldable $ map (\a -> "(" <> a <> ")") as')
exprToJS EPlaceholder = Nothing
