{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}

module Apps where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String (MisoString, pack, ms)
import Data.Semigroup   (Sum (..))
import Data.List        (zipWith)
import Prelude hiding (id, product, (.))
import Elmlens
import Data.Map (singleton)

data IntU

instance (Eq a, Num a) => ElmlensMsg (Sum a) where
  checkMempty s = s == mempty
  checkFail _ = False

instance UpdateStructure IntU where
  type Model IntU = Int
  type Msg IntU = Sum Int

  act _ n (Sum m) = n + m

counter :: ElmApp IntU IntU HTML
counter = fromView (\x -> Base $ H.div_ [] [
      H.button_ [ H.onClick $ Sum 1 ] [ H.text "+" ],
      H.text (ms x),
      H.button_ [ H.onClick $ Sum (-1) ] [ H.text "-" ] ])

counterApp :: Maybe MisoString -> App (Model IntU) (Msg IntU)
counterApp = render counter 0

data RepU (a :: Type)
data ScratchMsg a = Replace a | Keep deriving (Eq, Show)

instance Semigroup (ScratchMsg a) where
  Keep <> b              = b
  Replace a <> Keep      = Replace a
  Replace _ <> Replace b = Replace b

instance Monoid (ScratchMsg a) where
  mempty = Keep

instance Eq a => ElmlensMsg (ScratchMsg a) where
  checkMempty Keep = True
  checkMempty _    = False
  checkFail _ = False

instance (Eq a) => UpdateStructure (RepU a) where
  type Model (RepU a) = a
  type Msg (RepU a) = ScratchMsg a

  act _ a Keep        = a
  act _ _ (Replace a) = a

type BoolU = RepU Bool

checkButton :: ElmApp BoolU BoolU HTML
checkButton = fromView (\b -> Base $ H.input_ [ H.type_ "checkbox", H.checked_ b, H.onChecked (\(Checked x) -> Replace x) ]  )

type StringU = RepU MisoString

highlightButton :: ElmApp (RepU ()) (RepU ()) (ListV Attr :~> HTML)
highlightButton = fromView (\_ -> Holed $ \f (ViewList properties) -> Base $ H.button_ (fmap (\(Property p) -> p) properties) ["Click here"])

highlightProperties :: ElmApp (ProdU (RepU Int) (ListU (RepU ()))) (ProdU (RepU Int) (ListU (RepU ()))) (ListV (ListV Attr))
highlightProperties = fromView (\(i, list) -> ViewList $ fmap (f i) [0..(Prelude.length list)])
  where
    f i n = if n == i then ViewList [Property $ H.class_ "chosen"]
                    else ViewList [Property $ H.class_ "not_chosen", Property $ H.onClick (Replace n, mempty)]

highlightDemo = vmap f $ vmix (lmap (proj2L 0) (list highlightButton )) highlightProperties 
  where
    f (Pair (ViewList buttons) (ViewList properties)) = 
      Base $ H.div_ [] ((\(Base h) -> h) <$> Prelude.zipWith (\(Holed template) ps -> template id ps) buttons properties)

highlightDemoApp = render highlightDemo (0, [(), (), (), (), ()])

data UnitU (a :: Type)

instance (ElmlensMsg a) => UpdateStructure (UnitU a) where
  type Model (UnitU a) = ()
  type Msg (UnitU a) = a

  act _ _ _ = ()

unitL :: (UpdateStructure u) => Model u -> ULens u (UnitU a)
unitL m = ULens { get = const (), trans = const mempty, create = const m }


type NameU = RepU MisoString

type AddrU = RepU MisoString

name :: ElmApp NameU NameU HTML
name = fromView $ \name -> Base $ H.div_ [] [
  H.label_ [] [ H.text "Name: " ],
  H.input_ [ H.value_ name, H.onInput Replace ] ]

addr :: ElmApp AddrU AddrU HTML
addr = fromView $ \addr -> Base $ H.div_ [] [
  H.label_ [] [ H.text "Addr: " ],
  H.input_ [ H.value_ addr, H.onInput Replace ] ]

form :: ElmApp (ProdU NameU AddrU) (ProdU NameU AddrU) HTML
form = vmap f $ product name addr
  where f (Pair (Base vname) (Base vaddr)) = Base $ H.div_ [] [ vname, vaddr ]

data ChildU
newtype Child = Child Int deriving Eq

instance UpdateStructure ChildU where
  type Model ChildU = Child
  type Msg ChildU = Sum Int

  act _ (Child model) (Sum msg) = Child (model + msg)
 
data ParentU
newtype Parent = Parent Bool deriving Eq

instance UpdateStructure ParentU where
  type Model ParentU = Parent
  type Msg ParentU = Sum Int

  act _ (Parent True) (Sum msg) = Parent (even msg)
  act _ (Parent False) (Sum msg) = Parent (odd msg)

child :: ElmApp ChildU ChildU (Attr :~> HTML)
child = fromView $ \(Child model) -> Holed $ \f (Property attr) -> Base $ H.div_ [] [
  H.button_ [ H.onClick $ f $ Sum 1 ] [ H.text "To Child " ],
  H.label_ [] [ H.text $ pack ("Child: " ++ show model) ],
  H.button_ [ attr ] [ H.text "To Parent" ] ]

parent :: ElmApp ParentU ParentU (ProdV Attr (HTML :~> HTML))
parent = fromView $ \(Parent model) -> Pair (Property $ onClick $ Sum 1) (Holed $ \f (Base child) -> Base $ 
  H.div_ [ H.style_ $ singleton "background" $ if model then "red" else "blue" ] [ child ] )

decorated :: ElmApp (ProdU ChildU ParentU) (ProdU ChildU ParentU) HTML
decorated = vmap f $ product child parent
  where
    f (Pair childTemplate (Pair onClick parentTemplate)) = parentTemplate <~| (childTemplate <~| onClick)