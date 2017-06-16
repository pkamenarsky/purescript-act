module Act where

import Control.Monad
import Control.Monad.State
import Control.Monad.Eff
import Control.Monad.Free
import Data.Argonaut.Core
import Data.Array
import Data.Functor
import Data.Exists
import Data.Generic.Rep
import Data.Traversable
import Data.Lens
import Data.Maybe
import Data.Map
import Data.Lens.Index
import Data.Tuple
import Type.Proxy
import Unsafe.Coerce
import Prelude
import DOM as D
import DOM.Node.Types as D
import React as R
import React.DOM as R
import React.DOM.Props as P
import ReactDOM as RD
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import React (transformState)

foreign import traceAny :: forall a b. a -> (Unit -> b) -> b

traceAnyM :: forall m a. Monad m => a -> m a
traceAnyM s = traceAny s \_ -> pure s

undefined :: forall a. a
undefined = unsafeCoerce unit

lensAt :: forall a. Int -> Lens' (Array a) a
lensAt index = lens (\arr -> unsafePartial $ arr `unsafeIndex` index) (unsafeUpdateAt index)

unsafeUpdateAt index arr a = unsafePartial $ fromJust $ updateAt index a arr

unitLens :: forall a. Lens' a Unit
unitLens = lens (const unit) (\s _ -> s)

--------------------------------------------------------------------------------

type StaticPtrTable = forall a. Map Int a

type Static = State StaticPtrTable

data StaticPtr a = StaticPtr Int

foreign import static_ :: forall a. a -> Int
foreign import derefStatic_ :: forall a. Int -> a

static :: forall a. a -> StaticPtr a
static = StaticPtr <<< static_

derefStatic :: forall a. StaticPtr a -> a
derefStatic (StaticPtr ptr) = derefStatic_ ptr

--------------------------------------------------------------------------------

data EffectF eff st next =
    Modify (st -> st) next
  | ModifyRemotely (StaticPtr (Json -> st -> (Tuple st Json))) Json (Json -> next)
  | Effect Json (Json -> Eff eff Json) (Json -> next)

derive instance functorEffectF :: Functor (EffectF eff st)

type Effect eff st = Free (EffectF eff st)

mapEffectF :: forall eff st stt next. Lens' st stt -> EffectF eff stt next -> EffectF eff st next
mapEffectF lns (ModifyRemotely a f next) = undefined
mapEffectF lns (Modify f next) = Modify (over lns f) next
mapEffectF lns (Effect json eff next) = Effect json eff next

mapEffect :: forall eff st stt a. Lens' st stt -> Effect eff stt a -> Effect eff st a
mapEffect lns m = hoistFree (mapEffectF lns) m

modify :: forall eff st. (st -> st) -> (Effect eff st Unit)
modify f = liftF $ Modify f unit

getHTTP :: forall eff st. String -> Effect eff st (Maybe String)
getHTTP url = liftF (Effect (fromString url) (\_ -> pure $ fromString "Result'") toString)

interpretEffect :: forall eff st a. R.ReactThis Unit st -> Effect eff st a -> Eff (state :: R.ReactState R.ReadWrite | eff) a
interpretEffect this m = runFreeM go m
  where
    go (Modify f next) = do
      _ <- (unsafeCoerce R.transformState) this f
      void $ traceAnyM $ static f
      pure next
    go (ModifyRemotely f a next) = undefined
    go (Effect json eff next) = do
      -- dump json here
      res <- unsafeCoerce $ eff json
      pure $ next res

--------------------------------------------------------------------------------

type Element st = R.ReactElement

type Handler st = R.EventHandlerContext R.ReadWrite Unit st Unit

type Component eff st =
  { render :: (Effect eff st Unit -> Handler st) -> st -> Array R.ReactElement
  -- , onfetchstart :: st -> st
  -- , onfetchend :: st -> st
  }

type Props eff st = (Effect eff st Unit -> Handler st) -> P.Props

state :: forall eff st. (st -> Component eff st) -> Component eff st
state f = { render: \effect st -> (f st).render effect st }

zoom :: forall eff st stt. Lens' st stt -> Component eff stt -> Component eff st
zoom lns cmp = { render: \effect st -> cmp.render (\e -> effect (mapEffect lns e)) (view lns st) }

zoomProps :: forall eff st stt. Lens' st stt -> Props eff stt -> Props eff st
zoomProps lns effect f = effect \b -> f (mapEffect lns b)

zoomState :: forall eff st stt. Lens' st stt -> (stt -> Component eff stt) -> Component eff st
zoomState lns f = { render: \effect st -> (f (view lns st)).render (\e -> effect (mapEffect lns e)) (view lns st) }

foreach :: forall eff st stt. Lens' st (Array stt) -> Component eff stt -> Component eff st
foreach lns cmp = { render }
  where
    render effect st = concat $ do
      Tuple index item <- zip (range 0 (length items - 1)) items
      pure $ cmp.render (\f -> effect (mapEffect (lns <<< lensAt index) f)) (unsafePartial $ fromJust $ items !! index)
      where
        items = view lns st

foreach_ :: forall eff st stt. Lens' st (Array stt) -> (Int -> (st -> st) -> Lens' st stt -> Component eff st) -> Component eff st
foreach_ lns f = { render }
  where
    render effect st = concat $ do
      Tuple index item <- zip (range 0 (length items - 1)) items
      pure $ (f index (over lns (\arr -> unsafePartial $ fromJust $ deleteAt index arr)) (lns <<< lensAt index)).render effect st
      where
        items = view lns st

--------------------------------------------------------------------------------

onClick :: forall eff st. (R.Event -> Effect eff st Unit) -> Props eff st
onClick f effect = P.onClick \e -> effect (f e)

div :: forall eff st. Array (Props eff st) -> Array (Component eff st) -> Component eff st
div props children = { render: \effect st -> [ R.div (map (\p -> p effect) props) (concatMap (\e -> e.render effect st) children) ] }

text :: forall eff st. String -> Component eff st
text str = { render: \_ _ -> [ R.text str ] }

--------------------------------------------------------------------------------

elementIndex :: forall eff st. Int -> Props eff st
elementIndex index _ = P.unsafeMkProps "data-element-index" (show index)

getElementChildren :: forall eff. R.ReactElement -> Eff (props :: R.ReactProps | eff) (Array R.ReactElement)
getElementChildren e = R.getChildren (unsafeCoerce e)

getElementAllChildren :: forall eff. R.ReactElement -> Eff (props :: R.ReactProps | eff) (Array R.ReactElement)
getElementAllChildren e = do
  cs <- R.getChildren (unsafeCoerce e)
  cs' <- concat <$> traverse getElementAllChildren cs
  pure $ cs <> cs'

mkSpec :: forall eff st. st -> Component eff st -> R.ReactSpec Unit st eff
mkSpec st cmp = R.spec st \this -> do
  st' <- R.readState this
  pure $ R.div [] (cmp.render (unsafeCoerce interpretEffect $ this) st')

mkSpec' :: forall eff st. Local st -> Remote st -> Component' eff st -> R.ReactSpec Unit (Local st) _
mkSpec' lst rst cmp = R.spec lst \this -> do
  st' <- R.readState this
  pure $ R.div [] (cmp.render (unsafeCoerce interpretEffect $ this) st' rst)

main :: forall eff. Eff (dom :: D.DOM | eff) Unit
main = void (elm' >>= RD.render ui)
  -- where ui = R.createFactory (R.createClass (mkSpec (Tuple Nothing []) list)) unit
  where ui = R.createFactory (R.createClass (mkSpec' localUsers remoteUsers userComponent')) unit

        elm' :: Eff (dom :: D.DOM | eff) D.Element
        elm' = do
          -- void $ traceAnyM $ show $ from (undefined :: A)
          mkPairs a a
          win <- window
          doc <- document win
          elm <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
          pure $ unsafePartial fromJust elm

--------------------------------------------------------------------------------

deleteButton :: forall st eff. (st -> st) -> Lens' st Unit -> Component eff st
deleteButton delete lns = div [ onClick $ const $ modify delete ] [ text "Delete Button" ]

counter_ :: forall eff st. (st -> st) -> Lens' st Int -> Component eff st
counter_ delete lns =
  div [ ]
    [ div [ zoomProps lns $ onClick $ const $ modify (_ + 1) ] [ text "++" ]
    , div [ ] [ zoomState lns \ st -> text $ show st ]
    , div [ zoomProps lns $ onClick $ const $ modify (_ - 1) ] [ text "--" ]
    , div [ onClick $ const $ modify delete ] [ text "Delete" ]
    , deleteButton delete (lns <<< unitLens)
    ]

counter :: forall eff. Component eff Int
counter =
  div [ ]
    [ div [ onClick $ const $ modify (_ + 1) ] [ text "++" ]
    , div [ ] [ state \st -> text $ show st ]
    , div [ onClick $ const $ modify (_ - 1) ] [ text "--" ]
    ]

list :: forall eff. Component eff (Tuple (Maybe String) (Array Int))
list =
  div
    [ ]
    [ state $ text <<< show
    , div [ onClick $ const ajax ] [ text "+" ]
    -- , zoom _1 counter
    , foreach _2 counter
    , foreach_ _2 \_ d l -> counter_ d l
    ]
  where
    ajax = do
      res <- getHTTP "http://google.com"
      modify \(Tuple _ arr) -> (Tuple res (cons 0 arr))

--------------------------------------------------------------------------------

type User = { name :: String, age :: Int }
type Session = { session :: String }

data Masked a = Masked
newtype Identity a = Identity a

unwrap :: forall a. Identity a -> a
unwrap = undefined

type Remote st = st Masked Identity
type Local st = st Identity Masked

data CurrentSession (local :: Type -> Type) (remote :: Type -> Type) = CurrentSession { currentSession :: remote String }

data Users local remote = Users
  { users          :: local  (Array User)

  , sessions       :: remote (Array Session)
  , currentSession :: CurrentSession local remote
  }

_currentSession :: forall local remote. Lens' (Users local remote) (CurrentSession local remote)
_currentSession = undefined

localUsers :: Local Users
localUsers = Users { users: Identity [], sessions: Masked, currentSession: CurrentSession { currentSession: Masked } }

remoteUsers :: Remote Users
remoteUsers = Users { users: Masked, sessions: Identity [], currentSession: CurrentSession { currentSession: Identity "" } }

data A = A
  { b :: Array String
  , c :: Array { d :: String }
  }

a :: A
a = A { b: [], c: [] }

derive instance genericUsers :: Generic (Users Masked Identity) _

derive instance genericA :: Generic A _

data Effect' eff (st :: (Type -> Type) -> (Type -> Type) -> Type) a

data Handler' (st :: (Type -> Type) -> (Type -> Type) -> Type)

type Component' eff (st :: (Type -> Type) -> (Type -> Type) -> Type) =
  { render :: (Effect' eff st Unit -> Handler' st) -> Local st -> Remote st -> Array R.ReactElement
  }

type Props' eff (st :: (Type -> Type) -> (Type -> Type) -> Type) = (Effect' eff st Unit -> Handler' st) -> P.Props

zoom' :: forall eff local remote st stt. Lens' (st local remote) (stt local remote) -> Component' eff stt -> Component' eff st
zoom' lns cmp = undefined

div' :: forall eff remotes st. Array (Props' eff st) -> Array (Component' eff st) -> Component' eff st
div' props children = { render: \effect lst rst -> [ R.div (map (\p -> p effect) props) (concatMap (\e -> e.render effect lst rst) children) ] }

text' :: forall eff st. String -> Component' eff st
text' str = { render: \_ _ _ -> [ R.text str ] }

getUser :: StaticPtr (String -> Remote CurrentSession -> Maybe String)
getUser = static \a (CurrentSession st) -> Just (unwrap st.currentSession <> a)

remoteState :: forall eff st a b. (StaticPtr (a -> Remote st -> b)) -> a -> (b -> Component' eff st) -> Component' eff st
remoteState ptr a f = { render: \effect lst rst -> (f $ derefStatic ptr a rst).render effect lst rst }

userComponent' :: forall eff. Component' eff Users
userComponent' = div' [] [ zoom' _currentSession $ remoteState getUser "(y)" (text' <<< fromMaybe "") ]

--------------------------------------------------------------------------------

class Pairable a where
  pair :: forall eff. a -> a -> Eff eff Unit

class GenericPair a where
  gPair :: forall eff. a -> a -> Eff eff Unit

instance genericPairNoConstructors :: GenericPair NoConstructors where
  gPair _ _ = pure unit

instance genericPairNoArguments :: GenericPair NoArguments where
  gPair _ _ = pure unit

instance genericPairSum :: (GenericPair a, GenericPair b) => GenericPair (Sum a b) where
  gPair (Inl a1) (Inl a2) = gPair a1 a2
  gPair (Inr b1) (Inr b2) = gPair b1 b2
  gPair _ _ = pure unit

instance genericPairProduct :: (GenericPair a, GenericPair b) => GenericPair (Product a b) where
  gPair (Product a1 b1) (Product a2 b2) = do
    gPair a1 a2
    gPair b1 b2

instance genericPairConstructor :: GenericPair a => GenericPair (Constructor name a) where
  gPair (Constructor a1) (Constructor a2) = gPair a1 a2

instance genericPairArgument :: Pairable a => GenericPair (Argument a) where
  gPair (Argument a1) (Argument a2) = pair a1 a2

instance genericPairRec :: GenericPair a => GenericPair (Rec a) where
  gPair (Rec a1) (Rec a2) = gPair a1 a2

instance genericPairFieldArray :: (Pairable a, IsSymbol name) => GenericPair (Field name a) where
  gPair (Field a1) (Field a2) = do
    void $ traceAnyM $ "Field: " <> reflectSymbol (SProxy :: SProxy name)
    pair a1 a2

mkPairs :: forall eff st rep. Generic st rep => GenericPair rep => st -> st -> Eff eff Unit
mkPairs x y = gPair (from x) (from y)

--------------------------------------------------------------------------------

instance pairableArray :: Pairable (Array a) where
  pair a b = do
    void $ traceAnyM "Array trace"
    pure unit

--------------------------------------------------------------------------------

data UserG = UserG
  { name :: Guarded (AdminP' || UserP && AdminP') String
  }

data Guarded perm a = Guarded a

data Permission read write create = Permission
  { read :: read
  , write :: write
  , create :: create
  }

data AdminP a = AdminP a
type AdminP' = AdminP Unit
data UserP
data GuestP

data Or a b

infixl 4 type Or as ||

data And a b = And a b

infixl 4 type And as &&

data Perm a = Perm a

data Nil = Nil
data Cons a b = Cons a b

infixr 4 type Cons as :::
infixr 4 Cons as :::

class ElemOf a b where
  elemOf :: Proxy a -> Proxy b -> Unit

instance elemOfA :: ElemOf a (Cons a b) where
  elemOf _ _ = unit

instance elemOfB :: ElemOf a c => ElemOf a (Cons b c) where
  elemOf _ _ = unit

--------------------------------------------------------------------------------

class ElimUnit a b | a -> b where
  elimUnit :: Proxy a -> Proxy b

instance elimOp0 :: ElimUnit Unit Unit where
  elimUnit _ = Proxy

instance elimOp1 :: ElimUnit (Perm a) (Perm a) where
  elimUnit _ = Proxy

instance elimOp2 :: ElimUnit b c => ElimUnit (Unit && b) c where
  elimUnit _ = Proxy

instance elimOp3 :: ElimUnit b c => ElimUnit (b && Unit) c where
  elimUnit _ = Proxy

instance elimOp4 :: ElimUnit b c => ElimUnit (Unit || b) Unit where
  elimUnit _ = Proxy

instance elimOp5 :: ElimUnit b c => ElimUnit (b || Unit) Unit where
  elimUnit _ = Proxy

instance elimOp6 :: (ElimUnit b d, ElimUnit c e) => ElimUnit (op b c) (op d e) where
  elimUnit _ = Proxy

--------------------------------------------------------------------------------

class Elim a b c | a b -> c where
  elim :: Proxy a -> Proxy b -> Proxy c

instance elimId :: Elim (Perm a) (Perm a) Unit where
  elim _ _ = Proxy

instance elimUnit0 :: Elim (Perm a) Unit Unit where
  elim _ _ = Proxy

instance elimNotId :: Elim (Perm a) (Perm b) (Perm b) where
  elim _ _ = Proxy

instance elimOp :: (Elim a b d, Elim a c e, ElimUnit (op d e) g) => Elim a (op b c) g where
  elim _ _ = Proxy

testElim :: _
testElim = elim (Proxy :: Proxy (Perm Char)) (Proxy :: Proxy (Perm Int || (Perm String || (Perm Boolean && Perm Char))))

--------------------------------------------------------------------------------

class ElimList a b c | a b -> c where
  elimList :: Proxy a -> Proxy b -> Proxy c

instance elimNil :: ElimList Nil a a where
  elimList _ _ = Proxy

instance elimCons :: (Elim a c c', ElimList b c' d) => ElimList (Cons a b) c d where
  elimList _ _ = Proxy

testElimList :: _
testElimList = elimList (Proxy :: Proxy (Perm Char ::: Perm Boolean ::: Perm String ::: Nil)) (Proxy :: Proxy (Perm Int || (Perm String && (Perm Boolean && Perm Char))))

--------------------------------------------------------------------------------

proxy :: forall a. a
proxy = undefined

pmodify :: forall perms permList a rest. (ElimList permList perms Unit) => { modify :: permList | rest } -> Lens' (Guarded perms a) a
pmodify = undefined

pread :: forall perms permList a rest. (ElimList permList perms Unit) => { read :: Proxy permList | rest } -> Guarded perms a -> a
pread = undefined

pcreate :: forall perms permList a rest. (ElimList permList perms Unit) => { create :: Proxy permList | rest } -> a -> Guarded perms a
pcreate = undefined

testPerm :: Lens' (Guarded (Perm String && Perm Int) Boolean) _
testPerm = pmodify { modify: proxy :: Perm String ::: Perm Int ::: Nil }

--------------------------------------------------------------------------------

data PMap perms k v

-- returns Nothing if key already present
minsert :: forall k v perms rest permList. (ElimList permList perms Unit) => Proxy permList -> k -> v -> PMap { insert :: perms | rest } k v -> Maybe (PMap { insert :: perms | rest } k v)
minsert = undefined

mlookup :: forall k v perms rest permList. (ElimList permList perms Unit) => (k -> Maybe (Proxy permList)) -> k -> PMap { read :: perms | rest } k v -> Maybe v
mlookup = undefined

mlookup' :: forall k v permKey otherPerms allPerms perms rest.
     ElimList (permKey ::: otherPerms) perms Unit
  => Key permKey k
  => permKey
  -> otherPerms
  -> PMap { read :: perms | rest } k v
  -> Maybe v
mlookup' = undefined

mmodify :: forall k v perms rest permList. (ElimList permList perms Unit) => (k -> Maybe (Proxy permList)) -> k -> (Maybe v -> Maybe v) -> PMap { modify :: perms | rest } k v -> PMap { modify :: perms | rest } k v
mmodify = undefined

pmap :: PMap { read :: Perm (AdminP String) && Perm UserP } String Int
pmap = undefined

testMlookup :: _
testMlookup = mlookup' (proxy :: Perm (AdminP String)) (proxy :: Perm UserP ::: Nil) pmap

--------------------------------------------------------------------------------

class Key a b where
  key :: a -> b

instance keyAdminP :: Key (AdminP a) a where
  key _ = undefined

instance keyAdminPerm :: Key a b => Key (Perm a) b where
  key _ = undefined

data Self a

self :: forall st a. st -> Maybe (Self a)
self = undefined

--------------------------------------------------------------------------------

class Append a b c | a b -> c where
  append :: Proxy a -> Proxy b -> Proxy c

instance append0 :: Append a Nil a where
  append _ _ = Proxy

instance append1 :: Append Nil a a where
  append _ _ = Proxy

instance append3 :: Append b d e => Append (Cons a b) (Cons c d) (Cons a (Cons c e)) where
  append _ _ = Proxy

testAppend :: _
testAppend = append (Proxy :: Proxy (Int ::: Char ::: Nil)) (Proxy :: Proxy (String ::: Boolean ::: Nil))

--------------------------------------------------------------------------------

class AppendPerm st a b | a -> b where
  appendPerm :: st -> a -> b

instance appendPerm0 :: AppendPerm st Nil Nil where
  appendPerm st Nil = Nil

instance appendPerm1 :: AppendPerm st b c => AppendPerm st (Cons (st -> Maybe a) b) (Cons (Maybe (Perm a)) c) where
  appendPerm st (x ::: xs) = undefined -- Perm (x st) ::: appendPerm st xs

testAppendPerm :: _
testAppendPerm = appendPerm 6 (proxy :: (Int -> Maybe Char) ::: (Int -> Maybe String) ::: Nil)

--------------------------------------------------------------------------------

testList :: _
testList = case appendPerm 6 (proxy :: (Int -> Maybe Char) ::: (Int -> Maybe String) ::: Nil) of
  Just (Perm char) ::: Just (Perm string) ::: _ -> string
  otherwise -> ""
