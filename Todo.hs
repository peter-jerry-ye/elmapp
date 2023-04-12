{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Todo where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String hiding (filter)
import Data.Semigroup   (Sum (..))
import qualified Data.Sequence as S
import Data.List        (zipWith)
import Prelude hiding (id, product, (.))
import Elmlens
import Apps
import Data.Foldable (toList)

data TaskInputU

data EditMsg = 
    Focus
  | Cancel
  | Commit
  | Edit MisoString

instance UpdateStructure TaskInputU where
  type Model TaskInputU = (MisoString, Maybe MisoString)
  type Msg TaskInputU = [ EditMsg ]

  act _ model [] = model
  act pu (str, Nothing) (Focus : xs) = act pu (str, Just str) xs
  act pu (str, Just edit) (Cancel : xs) = act pu (str, Nothing) xs
  act pu (str, Just edit) (Commit : xs) = act pu (edit, Nothing) xs
  act pu (str, Just _)    (Edit e : xs) = act pu (str, Just e) xs
  act pu m (_ : xs) = act pu m xs -- Maybe runtime error?

taskInput :: ElmApp TaskInputU TaskInputU HTML
taskInput = fromView viewTask
  where
    viewTask :: Model TaskInputU -> View HTML (Msg TaskInputU)
    viewTask (str, Nothing) = Base $ H.label_ [ H.onDoubleClick [ Focus ] ] [ H.text str ]
    viewTask (str, Just ed) = Base $ H.input_ [ H.value_ ed, H.onInput $ \s -> [ Edit s ], H.onChange $ const [ Commit ], H.onKeyDown $ \(KeyCode code) -> [ Cancel | code == 27 ] ]

taskRow :: ElmApp (ProdU BoolU TaskInputU) (ProdU BoolU TaskInputU) (HTML :~> HTML)
taskRow = vmap f (product checkButton taskInput)
  where
    f :: View (ProdV HTML HTML) (Msg (ProdU BoolU TaskInputU)) -> View (HTML :~> HTML) (Msg (ProdU BoolU TaskInputU))
    f (Pair (Base h1) (Base h2)) = Holed $ \f (Base h) -> Base $ H.div_ [] [ fmap f h1, fmap f h2, h ]

deleteButtons = fromView view
  where
    view :: Model (ListU (ProdU BoolU TaskInputU)) -> View (ListV HTML) (Msg (ListU (ProdU BoolU TaskInputU)))
    view list = ViewList $ toList $ S.mapWithIndex (\index _ -> Base $ H.button_ [ H.onClick [ ALDel index ] ] [ H.text "Delete" ]) list

tasks = vmap f $ vmix (list taskRow) deleteButtons
  where
    f (Pair (ViewList v1) (ViewList v2)) = ViewList $ Data.List.zipWith (\(Holed template) button -> template id button) v1 v2

newTask :: ElmApp (ProdU StringU (ListU (ProdU BoolU TaskInputU))) (ProdU StringU (ListU (ProdU BoolU TaskInputU))) HTML
newTask = fromView (\(str, ls) -> 
    Base $ H.input_ [ 
      H.value_ str, 
      H.onInput $ \s -> (Replace s, [] ), 
      H.onChange $ const (Replace "", [ ALIns (Prelude.length ls) (False, (str, Nothing)) ] ) ] )

todoWithoutFilter = vmap f $ lmap (splitL id (proj2L "")) $ product newTask tasks
  where
    f (Pair v1 (ViewList v2)) = Base $ H.div_ [] $ fmap (\(Base h) -> h) (v1 : v2)

todoWithoutFilterApp = render todoWithoutFilter ("", S.Empty)

data TaskFilter = 
    DisplayAll
  | Doing
  | Done deriving (Eq, Show)

type TaskFilterU = RepU TaskFilter

taskFilterSwitch :: ElmApp TaskFilterU TaskFilterU HTML
taskFilterSwitch = fromView view
  where
    view :: Model TaskFilterU -> View HTML (Msg TaskFilterU)
    view filter = Base $ H.div_ [] [
      H.input_ [ H.type_ "radio", H.checked_ $ filter == DisplayAll, H.onChange $ \_ -> Replace DisplayAll ],
      H.label_ [] [ text "All" ],
      H.input_ [ H.type_ "radio", H.checked_ $ filter == Doing, H.onChange $ \_ -> Replace Doing ],
      H.label_ [] [ text "Doing" ],
      H.input_ [ H.type_ "radio", H.checked_ $ filter == Done, H.onChange $ \_ -> Replace Done ],
      H.label_ [] [ text "Done" ] ]

allTasks = vmap' f tasks
  where f view = \(ls1, ls2) -> view (ls1, ls2)
doingTasks = vmap' f tasks
  where f view = \(ls1, ls2) -> view (S.filter (not . fst) ls1, S.filter (not . fst) ls2)
doneTasks = vmap' f tasks
  where f view = \(ls1, ls2) -> view (S.filter fst ls1, S.filter fst ls2)

filteredTasks = 
  conditional (\(filter, _) -> filter == DisplayAll)
              (product taskFilterSwitch allTasks )
              $ conditional (\(filter, _) -> filter == Doing)
                            (product taskFilterSwitch doingTasks )
                            (product taskFilterSwitch doneTasks )

todomvc = vmap f $ lmap (splitL (productL id (proj2L DisplayAll)) (proj2L "") ) $ product newTask filteredTasks 
  where
    f (Pair (Base inputV) (Pair (Base filterV) (ViewList tasksV))) = 
        Base $ H.div_ [] $ [ inputV, filterV ] ++ fmap (\(Base v) -> v) tasksV

todomvcapp = render todomvc ("", (DisplayAll, S.Empty))

