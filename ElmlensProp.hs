{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeFamilies               #-}

-- Following is not needed in GHC 2021
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use <$>" #-}

module ElmlensProp where

import           Data.Proxy       (Proxy (..))
import           Data.Kind        (Type)
import           Elmlens
import Data.Semigroup (Sum)
import Apps (ScratchMsg(Keep))
import Miso.String (MisoString, null)
import Todo (EditMsg)

class (Eq m, Monoid m) => ElmlensMsg m where
  checkMempty :: m -> Bool
  checkFail   :: m -> Bool

instance (ElmlensMsg ma, ElmlensMsg mb) => ElmlensMsg (DupMsg ma mb) where
  checkMempty MNone = True
  checkMempty _     = False
  checkFail   MConflict = True
  checkFail (MLeft ma)  | checkFail ma = True
  checkFail (MRight mb) | checkFail mb = True
  checkFail   _         = False

instance (Eq model, ElmlensMsg msg) => ElmlensMsg [ AtomicListMsg model msg ] where
  checkMempty [] = True
  checkMempty _  = False
  checkFail _  = False

instance (ElmlensMsg m1, ElmlensMsg m2) => ElmlensMsg (m1, m2) where
  checkMempty (m1, m2) = checkMempty m1 && checkMempty m2
  checkFail (m1, m2) = checkFail m1 || checkFail m2

instance (Eq a, Num a) => ElmlensMsg (Sum a) where
  checkMempty s = s == mempty
  checkFail _ = False

instance Eq a => ElmlensMsg (ScratchMsg a) where
  checkMempty Keep = True
  checkMempty _    = False
  checkFail _ = False

instance ElmlensMsg MisoString where
  checkMempty = Miso.String.null
  checkFail = const False

instance ElmlensMsg [ EditMsg ] where
  checkMempty [] = True
  checkMempty _  = False
  checkFail _  = False

class (ElmlensMsg (Msg u), Semigroup (Mask u), UpdateStructure u) => MaskedUpdateStructure (u :: Type) where
  type Mask u :: Type
  mask :: Proxy u -> Msg u -> Mask u
  eqv :: Proxy u -> Mask u -> Model u -> Model u -> Bool

instance (MaskedUpdateStructure u1, MaskedUpdateStructure u2) => MaskedUpdateStructure (DupU u1 u2) where
  type Mask (DupU u1 u2) = (Maybe (Mask u1), Maybe (Mask u2))
  mask _ MNone = (Nothing, Nothing)
  mask _ MConflict = (Nothing, Nothing)
  mask _ (MLeft m) | checkMempty m = (Nothing, Nothing)
                   | otherwise   = (Just (mask (Proxy @u1) m), Nothing)
  mask _ (MRight m) | checkMempty m  = (Nothing, Nothing)
                    | otherwise      = (Nothing, Just (mask (Proxy @u2) m))

  eqv _ (mask1, mask2) (Dup m1 m2) (Dup m1' m2') = eqv' (Proxy @u1) mask1 m1 m1' && eqv' (Proxy @u2) mask2 m2 m2'
    where eqv' _ Nothing _ _ = True
          eqv' proxy (Just msg) m m' = eqv proxy msg m m'

instance (MaskedUpdateStructure u1, MaskedUpdateStructure u2) => MaskedUpdateStructure (ProdU u1 u2) where
  type Mask (ProdU u1 u2) = (Mask u1, Mask u2)
  mask _ (m1, m2) = (mask (Proxy @u1) m1, mask (Proxy @u2) m2)
  eqv _ (mask1, mask2) (m1, m2) (m1', m2') = eqv (Proxy @u1) mask1 m1 m1' && eqv (Proxy @u2) mask2 m2 m2'

instance (MaskedUpdateStructure u) => MaskedUpdateStructure (ListU u) where
  type Mask (ListU u) = Maybe (Mask u)
  mask _ ms = foldl (<>) Nothing $ fmap f ms
    where 
      f (ALIns _ _) = Nothing
      f (ALDel _) = Nothing
      f (ALReorder _) = Nothing
      f (ALRep _ m) = Just (mask (Proxy @u) m)
      
  eqv _ (Just m) ms ms' = and (zipWith (eqv (Proxy @u) m) ms ms')
  eqv _ Nothing ms ms' = ms == ms'

translateEmptyProp :: forall u1 u2. (UpdateStructure u1, MaskedUpdateStructure u2) => ULens u1 u2 -> Model u1 -> Bool
translateEmptyProp lens model = mempty == trans lens model mempty

translateCombineProp :: forall u1 u2. (UpdateStructure u1, MaskedUpdateStructure u2) => ULens u1 u2 -> Model u1 -> Msg u2 -> Msg u2 -> Bool
translateCombineProp lens m1 msg2 msg2' = 
  checkFail (msg2 <> msg2') || (msg1 <> msg1') == trans lens m1 (msg2 <> msg2')
    where
      msg1 = trans lens m1 msg2
      msg1' = trans lens (upd (Proxy @u1) m1 msg1) msg2'

consistencyProp :: forall u1 u2. (MaskedUpdateStructure u1, MaskedUpdateStructure u2) => ULens u1 u2 -> Model u1 -> Msg u2 -> Bool
consistencyProp lens m1 msg2 =
  eqv (Proxy @u2) m (get lens (upd (Proxy @u1) m1 msg1)) (upd (Proxy @u2) m2 msg2)
  where
    msg1 = trans lens m1 msg2
    m2 = get lens m1
    m  = mask (Proxy @u2) msg2

-- get create get
createProp :: forall u1 u2. MaskedUpdateStructure u2 => ULens u1 u2 -> Model u1 -> Bool
createProp lens m1 =
  get lens (create lens m2) == m2
    where m2 = get lens m1
