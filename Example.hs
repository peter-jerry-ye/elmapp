{-# LANGUAGE OverloadedStrings #-}

module Example where
import Miso.String (MisoString, pack)
import qualified Miso.Html as H
import Data.Bifunctor (second, Bifunctor (first))
import Miso
import Data.Map (singleton)

-- type Counter = Int
-- type CounterMsg = Int

-- update :: CounterMsg -> Counter -> Counter
-- update m i = i + m
-- view :: Counter -> View CounterMsg
-- view model = H.div_ [] [
--   H.button_ [ H.onClick 1 ] [ H.text "+" ],
--   H.label_ [] [ H.text $ (pack . show) model ],
--   H.button_ [ H.onClick (-1) ] [ H.text "-"] ]

newtype Name = Name MisoString deriving Eq
newtype NameMsg = ReplaceName MisoString

updateName :: NameMsg -> Name -> Name
updateName (ReplaceName newName) _ = Name newName
viewName :: Name -> View NameMsg
viewName (Name name) = H.div_ [] [
  H.label_ [] [ H.text "Name: " ],
  H.input_ [ H.value_ name, H.onInput $ \newName -> ReplaceName newName],
  H.label_ [] [ H.text ("Current value: " <> name)] ]

newtype Addr = Addr MisoString deriving Eq
newtype AddrMsg = AddrMsg MisoString

updateAddr :: AddrMsg -> Addr -> Addr
updateAddr (AddrMsg newAddr) _ = Addr newAddr
viewAddr :: Addr -> View AddrMsg
viewAddr (Addr addr) = H.div_ [] [
  H.label_ [] [ H.text "Address: " ],
  H.input_ [ H.value_ addr, H.onInput AddrMsg ],
  H.label_ [] [ H.text ("Current value: " <> addr)] ]

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

formApp :: App Form FormMsg
formApp = App {
  model = (Name "", Addr ""),
  update = \m msg -> noEff $ updateForm m msg,
  view = viewForm,
  subs = [],
  events = defaultEvents,
  initialAction = Left $ ReplaceName "",
  mountPoint = Nothing,
  logLevel = Off
}

newtype Child = Child Int deriving Eq
data ChildMsg = ChildMsg

data Args msg = Args { toSelf :: ChildMsg -> msg, toParent :: msg }

updateChild :: ChildMsg -> Child -> Child
updateChild _ (Child child) = Child $ child + 1

viewChild :: Args msg -> Child -> View msg
viewChild args (Child child) = H.div_ [] [
  H.button_ [ H.onClick (toSelf args ChildMsg) ] [ H.text "Update Child " ],
  H.label_ [] [ H.text $ pack ("Child: " ++ show child) ],
  H.button_ [ H.onClick (toParent args) ] [ H.text "Update Parent" ] ]

type IsDarkMode = Bool

newtype Parent = Parent (IsDarkMode, Child) deriving Eq
data ParentMsg = ParentMsg | UpdateChild ChildMsg

updateParent :: ParentMsg -> Parent -> Parent 
updateParent ParentMsg (Parent pair) = Parent $ first not pair
updateParent (UpdateChild childMsg) (Parent pair) = Parent $ second (updateChild childMsg) pair

viewParent :: Parent -> View ParentMsg
viewParent (Parent pair) = 
  H.div_ [ H.class_ $ if fst pair then "dark" else "light" ] [ 
    viewChild (Args { toSelf = UpdateChild, toParent = ParentMsg }) (snd pair) ]

themeApp:: App Parent ParentMsg
themeApp = App {
  model = Parent (True, Child 0),
  update = \m msg -> noEff $ updateParent m msg,
  view = viewParent,
  subs = [],
  events = defaultEvents,
  initialAction = ParentMsg,
  mountPoint = Nothing,
  logLevel = Off
}
