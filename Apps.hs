{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Apps where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String
import Data.Semigroup   (Sum (..))
import Data.List        (zipWith)
import Prelude hiding (id, product, (.))
import Elmlens

data IntU

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
data ScratchMsg a = Replace a | Keep deriving Eq

instance Semigroup (ScratchMsg a) where
  Keep <> b              = b
  Replace a <> Keep      = Replace a
  Replace _ <> Replace b = Replace b

instance Monoid (ScratchMsg a) where
  mempty = Keep

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

data Menu msg = Menu {
  selectedItem :: Maybe String,
  items :: [MenuItem msg]
}

data MenuItem msg = 
    MenuItem {
      label_ :: String,
      key :: String,
      message :: msg
    }
  | SubMenu {
      label_ :: String,
      key :: String,
      selectedKey :: Maybe String,
      children :: [MenuItem msg]
    }

data UnitU (a :: Type)

instance (Monoid a, Eq a) => UpdateStructure (UnitU a) where
  type Model (UnitU a) = ()
  type Msg (UnitU a) = a

  act _ _ _ = ()

unitL :: (UpdateStructure u) => Model u -> ULens u (UnitU a)
unitL m = ULens { get = const (), trans = const mempty, create = const m }
