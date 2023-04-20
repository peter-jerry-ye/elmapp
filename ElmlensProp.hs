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

class (Semigroup (Mask u), UpdateStructure u) => MaskedUpdateStructure (u :: Type) where
  type Mask u :: Type
  mask :: Proxy u -> Msg u -> Mask u
  eqv :: Proxy u -> Mask u -> Model u -> Model u -> Bool

instance (MaskedUpdateStructure u1, MaskedUpdateStructure u2) => MaskedUpdateStructure (DupU u1 u2) where
  type Mask (DupU u1 u2) = (Maybe (Mask u1), Maybe (Mask u2))
  mask _ (m1, m2) | m1 /= mempty && m2 /= mempty = (Just (mask (Proxy @u1) m1), Just (mask (Proxy @u2) m2))
                  | m1 /= mempty                 = (Just (mask (Proxy @u1) m1), Nothing)
                  | m2 /= mempty                 = (Nothing, Just (mask (Proxy @u2) m2))
                  | otherwise                    = (Nothing, Nothing)

  eqv _ (Nothing, Nothing) _ _ = True
  eqv _ (Just mask1, Nothing) (m1, _) (m1', _) = eqv (Proxy @u1) mask1 m1 m1'
  eqv _ (Nothing, Just mask2) (_, m2) (_, m2') = eqv (Proxy @u2) mask2 m2 m2'
  eqv _ (Just mask1, Just mask2) (m1, m2) (m1', m2') = eqv (Proxy @u1) mask1 m1 m1' && eqv (Proxy @u2) mask2 m2 m2'

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

translateEmptyProp :: (UpdateStructure u1, UpdateStructure u2) => ULens u1 u2 -> Model u1 -> Bool
translateEmptyProp lens model = trans lens model mempty == mempty

translateCombineProp :: forall u1 u2. (UpdateStructure u1, UpdateStructure u2) => ULens u1 u2 -> Model u1 -> Msg u2 -> Msg u2 -> Bool
translateCombineProp lens m1 msg2 msg2' = 
  (msg1 <> msg1') == trans lens m1 (msg2 <> msg2')
    where
      msg1 = trans lens m1 msg2
      msg1' = trans lens (act (Proxy @u1) m1 msg1) msg2'

consistencyProp :: forall u1 u2. (MaskedUpdateStructure u1, MaskedUpdateStructure u2) => ULens u1 u2 -> Model u1 -> Msg u2 -> Bool
consistencyProp lens m1 msg2 =
  eqv (Proxy @u2) m (get lens (act (Proxy @u1) m1 msg1)) (act (Proxy @u2) m2 msg2)
  where
    msg1 = trans lens m1 msg2
    m2 = get lens m1
    m  = mask (Proxy @u2) msg2

createProp :: forall u1 u2. UpdateStructure u2 => ULens u1 u2 -> Model u2 -> Bool
createProp lens m2 =
  get lens (create lens m2) == m2
