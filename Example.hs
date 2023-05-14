{-# LANGUAGE OverloadedStrings #-}

module Example where
import Miso.String (MisoString, pack)
import qualified Miso.Html as H
import Data.Bifunctor (second, Bifunctor (first))
import Miso (View)
import Data.Map (singleton)

newtype Name = Name MisoString
newtype NameMsg = NameMsg MisoString

updateName :: NameMsg -> Name -> Name
updateName (NameMsg newName) _ = Name newName
viewName :: Name -> View NameMsg
viewName (Name name) = H.div_ [] [
  H.label_ [] [ H.text "Name: " ],
  H.input_ [ H.value_ name, H.onInput NameMsg ] ]

newtype Addr = Addr MisoString
newtype AddrMsg = AddrMsg MisoString

updateAddr :: AddrMsg -> Addr -> Addr
updateAddr (AddrMsg newAddr) _ = Addr newAddr
viewAddr :: Addr -> View AddrMsg
viewAddr (Addr name) = H.div_ [] [
  H.label_ [] [ H.text "Addr: " ],
  H.input_ [ H.value_ name, H.onInput AddrMsg ] ]

type Form = (Name, Addr)
type FormMsg = Either NameMsg AddrMsg

updateForm :: FormMsg -> Form -> Form
updateForm formMsg form = case formMsg of
  Left nameMsg -> first (updateName nameMsg) form
  Right addrMsg -> second (updateAddr addrMsg) form
viewForm :: Form -> View FormMsg
viewForm form = H.div_ [] [
  Left <$> viewName (fst form),
  Right <$> viewAddr (snd form) ]

newtype Child = Child Int
newtype ChildMsg = ChildMsg ()

data Args msg = Args { toSelf :: ChildMsg -> msg, toParent :: msg }

updateChild :: ChildMsg -> Child -> Child
updateChild _ (Child child) = Child $ child + 1

viewChild :: Args msg -> Child -> View msg
viewChild args (Child child) = H.div_ [] [
  H.button_ [ H.onClick (toSelf args $ ChildMsg ()) ] [ H.text "To Child " ],
  H.label_ [] [ H.text $ pack ("Child: " ++ show child) ],
  H.button_ [ H.onClick (toParent args) ] [ H.text "To Parent" ] ]

newtype Parent = Parent (Bool, Child)
data ParentMsg = ParentMsg () | UpdateChild ChildMsg

updateParent :: ParentMsg -> Parent -> Parent 
updateParent (ParentMsg _) (Parent pair) = Parent $ first not pair
updateParent (UpdateChild childMsg) (Parent pair) = Parent $ second (updateChild childMsg) pair

viewParent :: Parent -> View ParentMsg
viewParent (Parent pair) = 
  H.div_ [ H.style_ $ singleton "background" $ if fst pair then "red" else "blue" ] [ 
    viewChild (Args { toSelf = UpdateChild, toParent = ParentMsg ()}) (snd pair) ]

