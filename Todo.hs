{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances #-}

module Todo where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String hiding (filter)
import Data.Semigroup   (Sum (..))
import Data.List        (zipWith)
import Prelude hiding (id, product, (.))
import Elmlens
import Apps

data TaskInputU

data EditMsg = 
    Focus
  | Cancel
  | Commit
  | Edit MisoString deriving (Eq, Show)

instance ElmlensMsg [ EditMsg ] where
  checkMempty [] = True
  checkMempty _  = False
  checkFail _  = False

instance UpdateStructure TaskInputU where
  type Model TaskInputU = (MisoString, Maybe MisoString)
  type Msg TaskInputU = [ EditMsg ]

  act _ model [] = model
  act pu (str, Nothing) (Focus : xs) = act pu (str, Just str) xs
  act pu (str, Just edit) (Cancel : xs) = act pu (str, Nothing) xs
  act pu (str, Just edit) (Commit : xs) = act pu (edit, Nothing) xs
  act pu (str, Just _)    (Edit e : xs) = act pu (str, Just e) xs
  act pu m (_ : xs) = act pu m xs -- Maybe runtime error?

taskInput :: ElmApp TaskInputU TaskInputU Html
taskInput = fromView viewTask
  where
    viewTask :: Model TaskInputU -> View Html (Msg TaskInputU)
    viewTask (str, Nothing) = Html $ H.label_ [ H.onDoubleClick [ Focus ] ] [ H.text str ]
    viewTask (str, Just ed) = Html $ H.input_ [ H.value_ ed, H.onInput $ \s -> [ Edit s ], H.onChange $ const [ Commit ], H.onKeyDown $ \(KeyCode code) -> [ Cancel | code == 27 ] ]

taskRow :: ElmApp (ProdU BoolU TaskInputU) (ProdU BoolU TaskInputU) (Html :~> Html)
taskRow = vmap f (product checkButton taskInput)
  where
    f :: View (ProdV Html Html) (Msg (ProdU BoolU TaskInputU)) -> View (Html :~> Html) (Msg (ProdU BoolU TaskInputU))
    f (ProdV (Html h1) (Html h2)) = Holed $ \f (Html h) -> Html $ H.div_ [] [ fmap f h1, fmap f h2, h ]

deleteButtons = fromView view
  where
    view :: Model (ListU (ProdU BoolU TaskInputU)) -> View (ListV Html) (Msg (ListU (ProdU BoolU TaskInputU)))
    view list = ListV $ Data.List.zipWith (\index _ -> Html $ H.button_ [ H.onClick [ ALDel index ] ] [ H.text "Delete" ]) [0..] list

tasks = vmap f $ vmix (list taskRow) deleteButtons
  where
    f :: View (ProdV (ListV (v :~> Html)) (ListV v)) m -> View (ListV Html) m
    f (ProdV (ListV v1) (ListV v2)) = ListV $ Data.List.zipWith (<~|) v1 v2

unfinishedTasks = filterE (not . fst) tasks

finishedTasks = filterE fst tasks

newTask :: ElmApp (ProdU StringU (ListU (ProdU BoolU TaskInputU))) (ProdU StringU (ListU (ProdU BoolU TaskInputU))) Html
newTask = fromView (\(str, ls) -> 
    Html $ H.input_ [ 
      H.value_ str, 
      H.onInput $ \s -> (Replace s, [] ), 
      H.onChange $ const (Replace "", [ ALIns (Prelude.length ls) (False, (str, Nothing)) ] ) ] )

todoWithoutFilter = vmap f $ vmix newTask (lmap (proj2L "") tasks)
  where
    f :: View (ProdV Html (ListV Html)) m -> View Html m
    f (ProdV v1 (ListV v2)) = Html $ H.div_ [] $ fmap (\(Html h) -> h) (v1 : v2)

todoWithoutFilterApp = render todoWithoutFilter ("", [])

data TaskFilter = 
    DisplayAll
  | Doing
  | Done deriving (Eq, Show)

type TaskFilterU = RepU TaskFilter

taskFilterSwitch :: ElmApp TaskFilterU TaskFilterU Html
taskFilterSwitch = fromView view
  where
    view :: Model TaskFilterU -> View Html (Msg TaskFilterU)
    view filter = Html $ H.div_ [] [
      H.input_ [ H.type_ "radio", H.checked_ $ filter == DisplayAll, H.onChange $ \_ -> Replace DisplayAll ],
      H.label_ [] [ text "All" ],
      H.input_ [ H.type_ "radio", H.checked_ $ filter == Doing, H.onChange $ \_ -> Replace Doing ],
      H.label_ [] [ text "Doing" ],
      H.input_ [ H.type_ "radio", H.checked_ $ filter == Done, H.onChange $ \_ -> Replace Done ],
      H.label_ [] [ text "Done" ] ]

filteredTasks = 
  conditional (\(filter, _) -> filter == DisplayAll)
              (product taskFilterSwitch tasks)
              $ conditional (\(filter, _) -> filter == Doing)
                            (product taskFilterSwitch unfinishedTasks)
                            (product taskFilterSwitch finishedTasks)

todomvc = vmap f $ vmix (lmap (productL id (proj2L DisplayAll)) newTask) (lmap (proj2L "") filteredTasks)
  where
    f :: View (ProdV Html (ProdV Html (ListV Html))) m -> View Html m
    f (ProdV (Html inputV) (ProdV (Html filterV) (ListV tasksV))) = 
        Html $ H.div_ [] $ [ inputV, filterV ] ++ fmap (\(Html v) -> v) tasksV

todomvcapp = render todomvc ("", (DisplayAll, []))

