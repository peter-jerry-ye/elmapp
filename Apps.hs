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

instance UpdateStructure IntU where
  type Model IntU = Int
  type Msg IntU = Sum Int

  upd _ n (Sum m) = n + m

counter :: ElmApp IntU IntU Html
counter = fromView (\x -> Html $ H.div_ [] [
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

instance (Eq a) => UpdateStructure (RepU a) where
  type Model (RepU a) = a
  type Msg (RepU a) = ScratchMsg a

  upd _ a Keep        = a
  upd _ _ (Replace a) = a

type BoolU = RepU Bool

checkButton :: ElmApp BoolU BoolU Html
checkButton = fromView (\b -> Html $ H.input_ [ H.type_ "checkbox", H.class_ "uk-checkbox", H.checked_ b, H.onChecked (\(Checked x) -> Replace x) ]  )

type StringU = RepU MisoString

highlightButton :: ElmApp (RepU ()) (RepU ()) (ListV Attr :~> Html)
highlightButton = fromView (\_ -> Holed $ \f (ListV properties) -> Html $ H.button_ (fmap (\(Attr p) -> p) properties) ["Click here"])

highlightProperties :: ElmApp (ProdU (RepU Int) (ListU (RepU ()))) (ProdU (RepU Int) (ListU (RepU ()))) (ListV (ListV Attr))
highlightProperties = fromView (\(i, list) -> ListV $ fmap (f i) [0..(Prelude.length list)])
  where
    f i n = if n == i then ListV [Attr $ H.class_ "chosen"]
                    else ListV [Attr $ H.class_ "not_chosen", Attr $ H.onClick (Replace n, mempty)]

highlightDemo = vmap f $ dup (lmap (proj2L 0) (list highlightButton )) highlightProperties 
  where
    f :: View (ProdV (ListV (v :~> Html)) (ListV v)) m -> View Html m
    f (ProdV (ListV buttons) (ListV properties)) = 
      Html $ H.div_ [] ((\(Html h) -> h) <$> Prelude.zipWith (<~|) buttons properties)

highlightDemoApp = render highlightDemo (0, [(), (), (), (), ()])

data UnitU (a :: Type)

instance (Monoid a, Eq a) => UpdateStructure (UnitU a) where
  type Model (UnitU a) = ()
  type Msg (UnitU a) = a

  upd _ _ _ = ()

unitL :: (UpdateStructure u) => Model u -> ULens u (UnitU a)
unitL m = ULens { get = const (), trans = const mempty, create = const m }


type NameU = RepU MisoString

type AddrU = RepU MisoString

nameApp :: ElmApp NameU NameU Html
nameApp = fromView $ \name -> Html $ H.div_ [] [
  H.label_ [] [ H.text "Name: " ],
  H.input_ [ H.value_ name, H.onInput Replace ] ]

addrApp :: ElmApp AddrU AddrU Html
addrApp = fromView $ \addr -> Html $ H.div_ [] [
  H.label_ [] [ H.text "Addr: " ],
  H.input_ [ H.value_ addr, H.onInput Replace ] ]

formApp :: ElmApp (ProdU NameU AddrU) (ProdU NameU AddrU) Html
formApp = vmap f $ product nameApp addrApp
  where 
    f :: View (ProdV Html Html) m -> View Html m
    f (ProdV (Html vname) (Html vaddr)) = Html $ H.div_ [] [ vname, vaddr ]

data ChildU
newtype Child = Child Int deriving Eq

instance UpdateStructure ChildU where
  type Model ChildU = Child
  type Msg ChildU = Sum Int

  upd _ (Child model) (Sum msg) = Child (model + msg)
 
data ParentU
newtype Parent = Parent Bool deriving Eq

instance UpdateStructure ParentU where
  type Model ParentU = Parent
  type Msg ParentU = Sum Int

  upd _ (Parent True) (Sum msg) = Parent (even msg)
  upd _ (Parent False) (Sum msg) = Parent (odd msg)

childApp :: ElmApp ChildU ChildU (Attr :~> Html)
childApp = fromView $ \(Child model) -> Holed $ \f (Attr attr) -> Html $ H.div_ [] [
  H.button_ [ H.onClick $ f $ Sum 1 ] [ H.text "To Child " ],
  H.label_ [] [ H.text $ pack ("Child: " ++ show model) ],
  H.button_ [ attr ] [ H.text "To Parent" ] ]

parentApp :: ElmApp ParentU ParentU (ProdV Attr (Html :~> Html))
parentApp = fromView $ \(Parent model) -> ProdV (Attr $ onClick $ Sum 1) (Holed $ \f (Html child) -> Html $ 
  H.div_ [ H.style_ $ singleton "background" $ if model then "red" else "blue" ] [ child ] )

themeApp:: ElmApp (ProdU ChildU ParentU) (ProdU ChildU ParentU) Html
themeApp = vmap f $ product childApp parentApp 
  where
    f :: View (ProdV (Attr :~> Html) (ProdV Attr (Html :~> Html))) m -> View Html m
    f (ProdV childTemplate (ProdV onClick parentTemplate)) = parentTemplate <~| (childTemplate <~| onClick)

clickToUpdateChild :: View Attr ()
clickToUpdateChild = Attr $ H.onClick ()

-- template :: View (Html :~> Html) ()
-- template = Holed $ \f (Html child) -> Html $ H.div_ [] [ child ]
