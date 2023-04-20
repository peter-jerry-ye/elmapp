{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Following is not needed in GHC 2021
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use <$>" #-}

module Elmlens where

import           Control.Category (Category (..))
import           Data.Kind        (Type)
import           Data.Proxy       (Proxy (..))
import           Data.IntMap.Strict (IntMap, fromList, foldlWithKey, toList)
import           Prelude          hiding (id, (.), product)

import           Miso             hiding (View)
import qualified Miso.Html        as H
import           Miso.String      (MisoString)

class (Monoid m) => ElmlensMsg m where
  checkMempty :: m -> Bool

-- Update Structure
class (Monoid (Msg u), Eq (Model u), Eq (Msg u)) => UpdateStructure (u :: Type) where
  type Model u :: Type
  type Msg u :: Type
  -- NB: data Proxy (u :: k) = Proxy
  -- is used to determine type u
  act :: Proxy u -> Model u -> Msg u -> Model u

data ProdU u1 u2

instance (UpdateStructure u1, UpdateStructure u2) => UpdateStructure (ProdU u1 u2) where
  type Model (ProdU u1 u2) = (Model u1, Model u2)
  type Msg (ProdU u1 u2) = (Msg u1, Msg u2)

  act _ (m1, m2) (msg1, msg2) = (act (Proxy @u1) m1 msg1, act (Proxy @u2) m2 msg2)

embFst :: (UpdateStructure u2) => Proxy u1 -> Proxy u2 -> Msg u1 ->  Msg (ProdU u1 u2)
embFst _ _ m = (m, mempty)

embSnd :: (UpdateStructure u1) => Proxy u1 -> Proxy u2 -> Msg u2 -> Msg (ProdU u1 u2)
embSnd _ _ m = (mempty, m)

data DupU u1 u2

instance (UpdateStructure u1, UpdateStructure u2) => UpdateStructure (DupU u1 u2) where
  type Model (DupU u1 u2) = (Model u1, Model u2)
  type Msg (DupU u1 u2) = (Msg u1, Msg u2)

  act _ (m1, m2) (msg1, msg2) = 
    if msg1 /= mempty && msg2 /= mempty then error "Inconsistent act in DupU" 
    else (act (Proxy @u1) m1 msg1, act (Proxy @u2) m2 msg2)

data ULens u1 u2 =
  ULens { get    :: Model u1 -> Model u2,
          trans  :: Model u1 -> Msg u2 -> Msg u1,
          create :: Model u2 -> Model u1 }

instance Category ULens where
  id = ULens { get = id, trans = const id, create = id  }
  ULens { get = g2, trans = tr2, create = c2 } . ULens { get = g1, trans = tr1, create = c1 } =
    ULens { get = g2 . g1, trans = tr, create = c1 . c2  }
    where
      tr s = tr1 s . tr2 (g1 s)

productL :: ULens u1 u2 -> ULens t1 t2 -> ULens (u1 `ProdU` t1) (u2 `ProdU` t2)
productL l1 l2 =
  ULens {
    get = \(a, b) -> (get l1 a, get l2 b),
    trans = \(a, b) (da, db) -> (trans l1 a da, trans l2 b db),
    create = \(a,b) -> (create l1 a, create l2 b)
  }

splitL :: forall u uv1 uv2. (UpdateStructure u, UpdateStructure uv1, UpdateStructure uv2) => ULens u uv1 -> ULens u uv2 -> ULens u (uv1 `DupU` uv2)
splitL l1 l2 =
  ULens {
    get = \s -> (get l1 s, get l2 s),
    trans = \s (da, db) -> 
      if da /= mempty && db /= mempty then error "Inconsistent trans in DupU"
      else trans l1 s da <> trans l2 s db,
    create = \(a, b) -> 
      let a' = create l1 a
          b' = create l2 b in
      if a' /= b' then error "Inconsistent create in DupU" else a'
  }

proj1L :: UpdateStructure u2 => Model u2 -> ULens (ProdU u1 u2) u1
proj1L m2 = 
  ULens {
    get = fst,
    trans = \_ m -> (m, mempty),
    create = \m1 -> (m1, m2)
  }

proj2L :: UpdateStructure u1 => Model u1 -> ULens (ProdU u1 u2) u2
proj2L m1 = 
  ULens {
    get = snd,
    trans = \_ m -> (mempty, m),
    create = \m2 -> (m1, m2)
  }

type VType = Type
class Functor (View v) => ViewType (v :: VType) where
  data View v :: Type -> Type

data HTML

instance ViewType HTML where
  newtype View HTML m = Base (H.View m)
    deriving newtype Functor

data Attr
instance ViewType Attr where
  newtype View Attr m = Property (H.Attribute m)
    deriving newtype Functor

data (a :: VType) :~> (b :: VType)
data ProdV a b

instance (ViewType a, ViewType b) => ViewType (a :~> b) where
  data View (a :~> b) msg = Holed (forall m. (msg -> m) -> View a m -> View b m)
instance Functor (View (a :~> b)) where
  fmap f (Holed h) = Holed $ \emb -> h (emb . f)

instance (ViewType a, ViewType b) => ViewType (ProdV a b) where
  data View (ProdV a b) msg = Pair (View a msg) (View b msg)
instance (Functor (View a), Functor (View b)) => Functor (View (ProdV a b)) where
  fmap f (Pair a b) = Pair (fmap f a) (fmap f b)

data ElmApp u uview (v :: VType) where
  ElmApp :: (UpdateStructure uview, ViewType v) => ULens u uview -> (Model uview -> View v (Msg uview)) -> ElmApp u uview v

fromView :: (UpdateStructure u, ViewType v) => (Model u -> View v (Msg u)) -> ElmApp u u v
fromView = ElmApp id

lmap :: ULens u u' -> ElmApp u' uv v -> ElmApp u uv v
lmap ul (ElmApp l h) = ElmApp (l . ul) h

vmap :: ViewType v' => (View v (Msg uv) -> View v' (Msg uv)) -> ElmApp u uv v -> ElmApp u uv v'
vmap f (ElmApp l h) = ElmApp l (f . h)

vmap' :: ViewType v' => ((Model uv -> View v (Msg uv)) -> (Model uv -> View v' (Msg uv))) -> ElmApp u uv v -> ElmApp u uv v'
vmap' f (ElmApp l h) = ElmApp l (f h)

vmix :: forall u uv uv' v v'. UpdateStructure u => ElmApp u uv v -> ElmApp u uv' v' -> ElmApp u (DupU uv uv') (ProdV v v')
vmix (ElmApp l1 view1) (ElmApp l2 view2) = 
  ElmApp (splitL l1 l2)
         (\(a, b) -> Pair (fmap (embFst (Proxy @uv) (Proxy @uv')) (view1 a)) (fmap (embSnd (Proxy @uv) (Proxy @uv')) (view2 b)))

product :: forall u1 uv1 v1 u2 uv2 v2. ElmApp u1 uv1 v1 -> ElmApp u2 uv2 v2 -> ElmApp (ProdU u1 u2) (ProdU uv1 uv2) (ProdV v1 v2)
product (ElmApp l1 view1) (ElmApp l2 view2) =
  ElmApp (productL l1 l2)
         (\(a, b) -> Pair (fmap (embFst (Proxy @uv1) (Proxy @uv2)) (view1 a)) (fmap (embSnd (Proxy @uv1) (Proxy @uv2)) (view2 b)))

data ListU u

data AtomicListMsg model msg
  = ALIns Int model
  | ALDel Int
  | ALRep Int msg
  | ALReorder (IntMap Int) deriving Eq

-- For simplicity, we treat out-of-bound updates as identity updates
actAtomicListMsg :: UpdateStructure u => Proxy u -> [ Model u ] -> AtomicListMsg (Model u) (Msg u) -> [ Model u ]
actAtomicListMsg pu xs0 (ALRep i msg) = case splitAt i xs0 of
        (xs, [])   -> xs
        (xs, y:ys) -> xs ++ act pu y msg : ys
actAtomicListMsg _ xs0 (ALDel i) = case splitAt i xs0 of
        (xs, [])   -> xs
        (xs, _:ys) -> xs ++ ys
actAtomicListMsg _ xs0 (ALIns i a) = case splitAt i xs0 of
        (xs, ys) -> xs ++ a : ys
actAtomicListMsg _ xs0 (ALReorder f) = foldlWithKey (\ls key n -> case splitAt key ls of
        (xs, []) -> xs
        (xs, _:ys) -> xs ++ (xs0 !! n) : ys) xs0 f

instance UpdateStructure u => UpdateStructure (ListU u) where
  type Model (ListU u) = [ Model u ]
  type Msg (ListU u) = [ AtomicListMsg (Model u) (Msg u) ]

  act _ = foldl (actAtomicListMsg (Proxy @u))

data ListV (v :: VType)

instance ViewType v => ViewType (ListV v) where
  newtype View (ListV v) msg = ViewList [ View v msg ]

instance Functor (View v) => Functor (View (ListV v)) where
  fmap f (ViewList xs) = ViewList $ map (fmap f) xs

mapL :: forall u1 u2. UpdateStructure u1 => ULens u1 u2 -> ULens (ListU u1) (ListU u2)
mapL l = ULens { get = map (get l), trans = tr, create = map (create l) }
  where
    tr :: Model (ListU u1) -> Msg (ListU u2) -> Msg (ListU u1)
    tr _ []         = mempty
    tr s (db : dbs) = let da = trA s db
                      in da <> tr (act (Proxy @(ListU u1)) s da) dbs
    trA :: Model (ListU u1) -> AtomicListMsg (Model u2) (Msg u2) -> Msg (ListU u1)
    trA _ (ALIns i a)   = [ALIns i (create l a)]
    trA _ (ALDel i)     = [ALDel i]
    trA xs (ALRep i da) = case splitAt i xs of
      (_xs1 , [] )      -> mempty
      (_xs1, xi : _xs2) -> [ALRep i (trans l xi da)]
    trA _ (ALReorder f) = [ALReorder f]

list :: forall u uv v. UpdateStructure u => ElmApp u uv v -> ElmApp (ListU u) (ListU uv) (ListV v)
list (ElmApp lens h) =
  ElmApp (mapL lens) viewList
  where
    viewList :: Model (ListU uv) -> View (ListV v) (Msg (ListU uv))
    viewList xs = ViewList $ zipWith (\x i -> fmap (\msg -> [ALRep i msg]) $ h x) xs [0..]

filterList :: forall u uv v. (ViewType v, UpdateStructure u) => (Model uv -> Bool) -> ElmApp u (ListU uv) v -> ElmApp u (ListU uv) v
filterList predicate  = vmap' viewFilteredList
  where
    viewFilteredList ::  (Model (ListU uv) -> View v (Msg (ListU uv))) -> Model (ListU uv) -> View v (Msg (ListU uv))
    viewFilteredList h ls = fmap (f (length ls) (fmap snd $ filter (\(x, _) -> predicate x) $ zip ls [0..])) (h $ filter predicate ls)
    f :: Int -> [ Int ] -> Msg (ListU uv) -> Msg (ListU uv)
    f _n _ls [] = []
    f n ls (ALIns i a : dbs) = case splitAt i ls of 
      (_xs1, []) -> ALIns n a : f (n + 1) (ls ++ [ n ]) dbs
      (xs1, xi : xs2) -> ALIns (xi + 1) a : f (n + 1) (xs1 ++ xi : (xi + 1) : fmap (+1) xs2) dbs
    f n ls (ALDel i : dbs) = case splitAt i ls of
      (_xs1, []) -> f n ls dbs
      (xs1, xi : xs2) -> ALDel xi : f (n - 1) (xs1 ++ fmap (\x -> x - 1) xs2) dbs
    f n ls (ALRep i da : dbs) = case splitAt i ls of
      (_xs1, []) -> f n ls dbs
      (_xs1, xi : _xs2) -> ALRep xi da : f n ls dbs
    f n ls (ALReorder reorder : dbs) = ALReorder (fromList $ fmap (\(from, to) -> (ls !! from, ls !! to)) $ toList reorder) : f n ls dbs

conditional :: forall u uv v. UpdateStructure u => (Model uv -> Bool) -> ElmApp u uv v -> ElmApp u uv v -> ElmApp u uv v
conditional predicate (ElmApp lens v1) (ElmApp _ v2) = ElmApp lens (\s -> if predicate s then v1 s else v2 s)
    
render :: forall u uv. UpdateStructure u => ElmApp u uv HTML -> Model u -> Maybe MisoString -> App (Model u) (Msg u)
render (ElmApp l v) model mountPoint = App {
  model  = model
, update = updateModel
, view   = viewModel
, subs   = []
, events = defaultEvents
, initialAction = mempty
, mountPoint = mountPoint
, logLevel   = DebugPrerender
}
  where
    updateModel :: Msg u -> Model u -> Effect (Msg u) (Model u)
    updateModel = \m s -> noEff $ act (Proxy @u) s m
    viewModel :: Model u -> H.View (Msg u)
    viewModel = \s -> (\(Base h) -> h) (fmap (trans l s) $ v (get l s))
