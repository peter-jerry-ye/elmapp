{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Elmlens where

import           Control.Category (Category (..))
import           Data.Kind        (Type)
import           Data.Proxy       (Proxy (..))
import           Data.IntMap.Strict (IntMap, (!))
import           Prelude          hiding (id, (.), product)

import           Miso             hiding (View)
import qualified Miso.Html        as H
import           Miso.String      (MisoString)
import Control.Applicative (liftA2)
import Data.Bifunctor (first, bimap)
import qualified Data.IntMap as IntMap
import Debug.Trace (trace)

class (Eq m, Monoid m) => ElmlensMsg m where
  checkMempty :: m -> Bool
  checkFail   :: m -> Bool

-- Update Structure
class (ElmlensMsg (Msg u), Eq (Model u)) => UpdateStructure (u :: Type) where
  type Model u :: Type
  type Msg u :: Type
  -- NB: data Proxy (u :: k) = Proxy
  -- is used to determine type u
  act :: Proxy u -> Model u -> Msg u -> Effect (Msg u) (Model u)

instance (ElmlensMsg m1, ElmlensMsg m2) => ElmlensMsg (m1, m2) where
  checkMempty (m1, m2) = checkMempty m1 && checkMempty m2
  checkFail (m1, m2) = checkFail m1 || checkFail m2

data ProdU u1 u2

instance (UpdateStructure u1, UpdateStructure u2) => UpdateStructure (ProdU u1 u2) where
  type Model (ProdU u1 u2) = (Model u1, Model u2)
  type Msg (ProdU u1 u2) = (Msg u1, Msg u2)

  act _ (m1, m2) (msg1, msg2) = liftA2 (,) (first (embFst (Proxy @u1) (Proxy @u2)) $ act (Proxy @u1) m1 msg1) (first (embSnd (Proxy @u1) (Proxy @u2)) $ act (Proxy @u2) m2 msg2)

embFst :: (UpdateStructure u2) => Proxy u1 -> Proxy u2 -> Msg u1 ->  Msg (ProdU u1 u2)
embFst _ _ m = (m, mempty)

embSnd :: (UpdateStructure u1) => Proxy u1 -> Proxy u2 -> Msg u2 -> Msg (ProdU u1 u2)
embSnd _ _ m = (mempty, m)

data DupU u1 u2

data Dup a b = Dup a b deriving (Eq, Show)

data DupMsg ma mb = MNone | MLeft ma | MRight mb | MConflict deriving (Eq, Show)

instance (ElmlensMsg ma, ElmlensMsg mb) => Semigroup (DupMsg ma mb) where
  MNone <> m = m
  m <> MNone = m
  MConflict <> _ = MConflict
  _ <> MConflict = MConflict
  MLeft ma <> MLeft ma' = MLeft $ ma <> ma'
  MRight mb <> MRight mb' = MRight $ mb <> mb'
  MLeft ma <> MRight mb | checkMempty ma = MRight mb
                        | checkMempty mb = MLeft ma
                        | otherwise      = MConflict
  MRight mb <> MLeft ma | checkMempty ma = MRight mb
                        | checkMempty mb = MLeft ma
                        | otherwise      = MConflict

instance (ElmlensMsg ma, ElmlensMsg mb) => Monoid (DupMsg ma mb) where
  mempty = MNone

instance (ElmlensMsg ma, ElmlensMsg mb) => ElmlensMsg (DupMsg ma mb) where
  checkMempty MNone = True
  checkMempty _     = False
  checkFail   MConflict = True
  checkFail (MLeft ma)  | checkFail ma = True
  checkFail (MRight mb) | checkFail mb = True
  checkFail   _         = False

instance (UpdateStructure u1, UpdateStructure u2) => UpdateStructure (DupU u1 u2) where
  type Model (DupU u1 u2) = Dup (Model u1) (Model u2)
  type Msg (DupU u1 u2) = DupMsg (Msg u1) (Msg u2)

  act _ (Dup m1 m2) MNone = noEff $ Dup m1 m2
  act _ (Dup m1 m2) (MLeft ma) = bimap MLeft (`Dup` m2) (act (Proxy @u1) m1 ma)
  act _ (Dup m1 m2) (MRight mb) = bimap MRight (Dup m1) (act (Proxy @u2) m2 mb)
  act _ _ MConflict = error "Inconsistent message"

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
    get = \s -> Dup (get l1 s) (get l2 s),
    trans = \s d -> case d of
      MNone -> mempty
      MLeft dl -> trans l1 s dl
      MRight dr -> trans l2 s dr
      MConflict -> mempty,
    create = \(Dup a b) -> 
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

infixr 1 :~>

type VType = Type
data Html
data Attr
data (a :: VType) :~> (b :: VType)
data ProdV a b
data ListV v

data View (v :: VType) m where
  Html :: H.View m -> View Html m
  Attr :: H.Attribute m -> View Attr m
  Holed :: (forall m. (msg -> m) -> View v1 m -> View v2 m) -> View (v1 :~> v2) msg
  ProdV :: View v1 msg -> View v2 msg -> View (ProdV v1 v2) msg
  ListV :: [ View v msg ] -> View (ListV v) msg

instance Functor (View v) where
  fmap f (Html m) = Html $ fmap f m
  fmap f (Attr m) = Attr $ fmap f m
  fmap f (Holed h) = Holed $ \emb -> h (emb . f)
  fmap f (ProdV v1 v2) = ProdV (fmap f v1) (fmap f v2)
  fmap f (ListV vs) = ListV $ fmap (fmap f) vs

(<~|) :: View (v1 :~> v2) m -> View v1 m -> View v2 m
(<~|) (Holed f) = f id

data ElmApp u uview v = ElmApp (ULens u uview) (Model uview -> View v (Msg uview))

fromView :: (UpdateStructure u) => (Model u -> View v (Msg u)) -> ElmApp u u v
fromView = ElmApp id

lmap :: ULens u u' -> ElmApp u' uv v -> ElmApp u uv v
lmap ul (ElmApp l h) = ElmApp (l . ul) h

vmap :: (forall m. View v m -> View v' m) -> ElmApp u uv v -> ElmApp u uv v'
vmap f (ElmApp l h) = ElmApp l (f . h)

vmap' :: ((Model uv -> View v (Msg uv)) -> (Model uv -> View v' (Msg uv))) -> ElmApp u uv v -> ElmApp u uv v'
vmap' f (ElmApp l h) = ElmApp l (f h)

split :: (UpdateStructure u, UpdateStructure uv1, UpdateStructure uv2) => ULens u u1 -> ElmApp u1 uv1 v1 -> ULens u u2 -> ElmApp u2 uv2 v2 -> ElmApp u (DupU uv1 uv2) (ProdV v1 v2)
split l1 e1 l2 e2 = dup (lmap l1 e1) (lmap l2 e2)

dup :: forall u uv uv' v v'. (UpdateStructure u, UpdateStructure uv, UpdateStructure uv') => ElmApp u uv v -> ElmApp u uv' v' -> ElmApp u (DupU uv uv') (ProdV v v')
dup (ElmApp l1 view1) (ElmApp l2 view2) = 
  ElmApp (splitL l1 l2)
         (\(Dup a b) -> ProdV (fmap MLeft (view1 a)) (fmap MRight (view2 b)))

product :: forall u1 uv1 v1 u2 uv2 v2. (UpdateStructure uv2, UpdateStructure uv1) => ElmApp u1 uv1 v1 -> ElmApp u2 uv2 v2 -> ElmApp (ProdU u1 u2) (ProdU uv1 uv2) (ProdV v1 v2)
product (ElmApp l1 view1) (ElmApp l2 view2) =
  ElmApp (productL l1 l2)
         (\(a, b) -> ProdV (fmap (embFst (Proxy @uv1) (Proxy @uv2)) (view1 a)) (fmap (embSnd (Proxy @uv1) (Proxy @uv2)) (view2 b)))

data ListU u

data AtomicListMsg model msg
  = ALIns Int model
  | ALDel Int
  | ALRep Int msg
  | ALReorder (IntMap Int) deriving (Eq, Show)

instance (Eq model, ElmlensMsg msg) => ElmlensMsg [ AtomicListMsg model msg ] where
  checkMempty [] = True
  checkMempty _  = False
  checkFail _  = False

data IList m = IList Int (IntMap Int) (IntMap m) deriving (Eq, Show)

instance Functor IList where
  fmap f (IList n indexMap mMap) = IList n indexMap (f <$> mMap)

iFromList :: [ m ] -> IList m
iFromList ls = IList (length ls) (IntMap.fromAscList $ zip [0..(length ls-1)] [0..(length ls-1)]) (IntMap.fromAscList $ zip [0..] ls)

iToList :: IList m -> [ m ]
iToList (IList _ indexMap mMap) = (\i -> mMap ! (indexMap ! i)) <$> [0 .. (IntMap.size indexMap - 1)]

iLength :: IList m -> Int
iLength (IList _ indexMap _) = IntMap.size indexMap

actAtomicListMsg :: UpdateStructure u => Proxy u -> IList (Model u) -> AtomicListMsg (Model u) (Msg u) -> Effect [ AtomicListMsg (Model u) (Msg u) ] (IList (Model u))
actAtomicListMsg pu (IList n indexMap mMap) (ALRep key msg) = case IntMap.lookup key mMap of
  Just m -> bimap (\msg' -> [ ALRep key msg']) (\m' -> IList n indexMap (IntMap.insert key m' mMap)) (act pu m msg)
  Nothing -> noEff (IList n indexMap mMap)
actAtomicListMsg _ (IList n indexMap mMap) (ALDel i) = case IntMap.lookup i indexMap of
  Just j -> let updatedMap = IntMap.mapKeys (\k -> if k >= i then k - 1 else k) (IntMap.delete i indexMap) in noEff (IList n updatedMap (IntMap.delete j mMap))
  Nothing -> noEff (IList n indexMap mMap)
actAtomicListMsg _ (IList n indexMap mMap) (ALIns i a) = 
  let updatedMap = IntMap.mapKeys (\k -> if k >= i then k + 1 else k) indexMap in 
    noEff (IList (n + 1) (IntMap.insert i n updatedMap) (IntMap.insert n a mMap))
actAtomicListMsg _ (IList n indexMap mMap) (ALReorder f) = noEff (IList n (IntMap.mapKeys (f !) indexMap) mMap)

-- For simplicity, we treat out-of-bound updates as identity updates
-- actAtomicListMsg :: UpdateStructure u => Proxy u -> [ Model u ] -> AtomicListMsg (Model u) (Msg u) -> [ Model u ]
-- actAtomicListMsg pu xs0 (ALRep i msg) = case splitAt i xs0 of
--         (xs, [])   -> xs
--         (xs, y:ys) -> xs ++ act pu y msg : ys
-- actAtomicListMsg _ xs0 (ALDel i) = case splitAt i xs0 of
--         (xs, [])   -> xs
--         (xs, _:ys) -> xs ++ ys
-- actAtomicListMsg _ xs0 (ALIns i a) = case splitAt i xs0 of
--         (xs, ys) -> xs ++ a : ys
-- actAtomicListMsg _ xs0 (ALReorder f) = foldlWithKey (\ls key n -> case splitAt key ls of
--         (xs, []) -> xs
--         (xs, _:ys) -> xs ++ (xs0 !! n) : ys) xs0 f

instance UpdateStructure u => UpdateStructure (ListU u) where
  type Model (ListU u) = IList (Model u)
  type Msg (ListU u) = [ AtomicListMsg (Model u) (Msg u) ]

  act _ ilist= foldl (\eff msgs -> eff >>= (\l -> actAtomicListMsg (Proxy @u) l msgs)) (noEff ilist)

mapL :: forall u1 u2. UpdateStructure u1 => ULens u1 u2 -> ULens (ListU u1) (ListU u2)
mapL l = ULens { get = \(IList n indexMap mMap) -> IList n indexMap (IntMap.map (get l) mMap), trans = tr, create = \(IList n indexMap mMap) -> IList n indexMap (IntMap.map (create l) mMap) }
  where
    tr :: Model (ListU u1) -> Msg (ListU u2) -> Msg (ListU u1)
    tr _ []         = mempty
    tr s (db : dbs) = let da = trA s db
                      in da <> tr ((\(Effect m _) -> m) $ act (Proxy @(ListU u1)) s da) dbs
    trA :: Model (ListU u1) -> AtomicListMsg (Model u2) (Msg u2) -> Msg (ListU u1)
    trA _ (ALIns i a)   = [ALIns i (create l a)]
    trA _ (ALDel i)     = [ALDel i]
    trA (IList _ _ mMap) (ALRep i da) = case IntMap.lookup i mMap of
      Nothing -> mempty
      Just m -> [ALRep i (trans l m da)]
    trA _ (ALReorder f) = [ALReorder f]

list :: forall u uv v. UpdateStructure u => ElmApp u uv v -> ElmApp (ListU u) (ListU uv) (ListV v)
list (ElmApp lens h) =
  ElmApp (mapL lens) viewList
  where
    viewList :: Model (ListU uv) -> View (ListV v) (Msg (ListU uv))
    viewList (IList _ indexMap mMap) = ListV $ map (\i -> fmap (\msg -> [ ALRep (indexMap ! i) msg ] ) $ h (mMap ! (indexMap ! i))) [0..(IntMap.size indexMap - 1)]

filterE :: forall u uv v. (UpdateStructure u, UpdateStructure uv) => (Model u -> Bool) -> ElmApp (ListU u) uv (ListV v) -> ElmApp (ListU u) (DupU (ListU u) uv) (ListV v)
filterE predicate e = vmap f $ dup eFilter e
  where
    f :: View (ProdV (ListV v :~> ListV v) (ListV v)) msg -> View (ListV v) msg
    f (ProdV (Holed template) v) = template id v
    eFilter :: ElmApp (ListU u) (ListU u) (ListV v :~> ListV v)
    eFilter = fromView $ \(IList _ indexMap mMap) -> Holed (\_ (ListV vs) -> ListV $ fmap fst $ filter (predicate . snd) $ zip vs (map (\i -> mMap ! (indexMap ! i)) [0..(IntMap.size indexMap - 1)]))
    

conditional :: forall u uv1 uv2 v. (UpdateStructure u, UpdateStructure uv1, UpdateStructure uv2) => (Model u -> Bool)
  -> ElmApp u uv1 v -> ElmApp u uv2 v -> ElmApp u (DupU u (DupU uv1 uv2)) v
conditional predicate e1 e2 = vmap f $ dup eConditional $ dup e1 e2
  where
    f :: View (ProdV (v :~> (v :~> v)) (ProdV v v)) msg -> View v msg
    f (ProdV template (ProdV v1 v2)) = template <~| v1 <~| v2
    eConditional :: ElmApp u u (v :~> (v :~> v))
    eConditional = fromView $ \s -> Holed (\_ v1 -> 
                                    Holed (\f' v2 -> if predicate s then fmap f' v1 else v2))
    
render :: forall u uv. (UpdateStructure u, Show (Model u)) => ElmApp u uv Html -> Model u -> Maybe MisoString -> App (Model u) (Msg u)
render (ElmApp l v) model mountPoint = App {
  subs   = []
, events = defaultEvents
, initialAction = mempty
, logLevel   = Off
, ..
}
  where
    update :: Msg u -> Model u -> Effect (Msg u) (Model u)
    update msg m = trace (show m) (act (Proxy @u) m msg)
    view :: Model u -> H.View (Msg u)
    view = \s -> (\(Html h) -> h) (fmap (trans l s) $ v (get l s))
