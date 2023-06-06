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
import qualified Data.Map as M

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

taskChecker :: ElmApp BoolU BoolU Html
taskChecker = fromView (\b -> Html $ H.label_ [] [
  H.input_ [ H.type_ "checkbox", H.class_ "uk-checkbox", H.checked_ b, H.onChecked (\(Checked x) -> Replace x) ],
  text $ if b then " Done" else " Doing" ] )

taskRow :: ElmApp (ProdU BoolU TaskInputU) (ProdU BoolU TaskInputU) (Html :~> Html)
taskRow = vmap f (product taskChecker taskInput)
  where
    f :: View (ProdV Html Html) ~> View (Html :~> Html) 
    f (ProdV (Html h1) (Html h2)) = Holed $ \f (Html h) -> Html $ 
      H.div_ [ H.class_ "uk-grid uk-width-1-1 uk-child-width-1-3" ] [ 
        H.div_ [] [ fmap f h1 ], 
        H.div_ [] [ fmap f h2 ], 
        H.div_ [] [ h ] ]

deleteButtons = fromView view
  where
    view :: Model (ListU (ProdU BoolU TaskInputU)) -> View (ListV Html) (Msg (ListU (ProdU BoolU TaskInputU)))
    view list = ListV $ Data.List.zipWith (\index _ -> Html $ H.button_ [ H.class_ "uk-button", H.class_ "uk-button-danger", H.onClick [ ALDel index ] ] [ H.text "Delete" ]) [0..] list

tasks = vmap f $ dup (list taskRow) deleteButtons
  where
    f :: View (ProdV (ListV (v :~> Html)) (ListV v)) ~> View (ListV Html)
    f (ProdV (ListV v1) (ListV v2)) = ListV $ Data.List.zipWith (<~|) v1 v2

newTask :: ElmApp (ProdU StringU (ListU (ProdU BoolU TaskInputU))) (ProdU StringU (ListU (ProdU BoolU TaskInputU))) Html
newTask = fromView (\(str, ls) -> 
    Html $ H.div_ [ H.class_ "uk-width-1-1" ] [ H.input_ [ 
      H.class_ "uk-input",
      H.value_ str, 
      H.onInput $ \s -> (Replace s, [] ), 
      H.onChange $ const (Replace "", [ ALIns (Prelude.length ls) (False, (str, Nothing)) ] ) ] ] )

todoWithoutFilter = vmap f $ dup newTask (lmap (proj2L "") tasks)
  where
    f :: View (ProdV Html (ListV Html)) ~> View (ListV Html)
    f (ProdV v1 (ListV v2)) = ListV $ v1 : v2

theme :: ElmApp BoolU BoolU (ListV Html :~> Html)
theme = fromView $ \isDark -> Holed $ \f (ListV children) -> Html $ 
  H.div_ [ if isDark then H.class_ "uk-background-secondary uk-light" else H.class_ "uk-background-default uk-dark", H.class_ "uk-grid", H.style_ $ M.singleton "padding" "20px"] ( 
    H.div_ [ H.class_ "uk-width-1-1"] [
      H.label_ [ ] [
        H.input_ [ H.class_ "uk-checkbox", H.id_ "darkModeSwitch", H.type_ "checkbox", H.textProp "role" "switch", H.checked_ isDark, H.onInput $ \str -> f $ Replace $ not isDark ],
        H.text " Dark Mode" ] ] : fmap (\(Html c) -> c) children)

themed = vmap f $ product theme todoWithoutFilter
  where
    f :: View (ProdV (ListV Html :~> Html) (ListV Html)) ~> View Html
    f (ProdV template h) = template <~| h

unfinishedTasks = filterE (not . fst) tasks

finishedTasks = filterE fst tasks

data TaskFilter = 
    DisplayAll
  | Doing
  | Done deriving (Eq, Show)

type TaskFilterU = RepU TaskFilter

taskFilterSwitch :: ElmApp TaskFilterU TaskFilterU Html
taskFilterSwitch = fromView view
  where
    view :: Model TaskFilterU -> View Html (Msg TaskFilterU)
    view filter = Html $ H.div_ [ H.class_ "uk-grid uk-width-1-1" ] [
      H.label_ [] [
        H.input_ [ H.class_ "uk-radio", H.type_ "radio", H.checked_ $ filter == DisplayAll, H.onChange $ \_ -> Replace DisplayAll ],
        text " All"
      ],
      H.label_ [] [
        H.input_ [ H.class_ "uk-radio", H.type_ "radio", H.checked_ $ filter == Doing, H.onChange $ \_ -> Replace Doing ],
        text " Doing"
      ],
      H.label_ [] [
        H.input_ [ H.class_ "uk-radio", H.type_ "radio", H.checked_ $ filter == Done, H.onChange $ \_ -> Replace Done ],
        text " Done"
      ] ]

filteredTasks = 
  conditional (\(filter, _) -> filter == DisplayAll)
              (product taskFilterSwitch tasks)
              $ conditional (\(filter, _) -> filter == Doing)
                            (product taskFilterSwitch unfinishedTasks)
                            (product taskFilterSwitch finishedTasks)

todomvc = vmap f $ dup (lmap (productL id (proj2L DisplayAll)) newTask) (lmap (proj2L "") filteredTasks)
  where
    f :: View (ProdV Html (ProdV Html (ListV Html))) m -> View Html m
    f (ProdV (Html inputV) (ProdV (Html filterV) (ListV tasksV))) = 
        Html $ H.div_ [ H.class_ "uk-grid uk-width-1-2" ] $ [ inputV, filterV ] ++ fmap (\(Html v) -> v) tasksV

themedTodoMVC = vmap f $ product theme todomvc
  where
    f :: View (ProdV (ListV Html :~> Html) Html) ~> View Html
    f (ProdV template todo) = template <~| ListV [ todo ]

todomvcapp = render todomvc ("", (DisplayAll, []))

-- todoWithoutFilterApp = render todoWithoutFilter ("", [])

-- themedApp = render themed (False, ("", []))

themedApp = render themedTodoMVC (False, ("", (DisplayAll, [])))

