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
import           Elmlens

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
