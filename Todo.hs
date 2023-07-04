{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- Following is not needed in GHC 2021
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Todo where

import Data.Kind        (Type)
import Data.Proxy       (Proxy (..))
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String hiding (filter)
import Data.Semigroup   (Sum (..))
import Data.List        (zipWith, foldl, filter, map, splitAt, length, zip)
import Prelude hiding (id, product, (.))
import Elmlens
import Apps
import qualified Data.Map as M
import Data.Bifunctor (second)
import Data.IntMap (fromList)
import Data.IntMap.Strict (toList)

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

  upd _ model [] = model
  upd pu (str, Nothing) (Focus : xs) = upd pu (str, Just str) xs
  upd pu (str, Just edit) (Cancel : xs) = upd pu (str, Nothing) xs
  upd pu (str, Just edit) (Commit : xs) = upd pu (edit, Nothing) xs
  upd pu (str, Just _)    (Edit e : xs) = upd pu (str, Just e) xs
  upd pu m (_ : xs) = upd pu m xs -- Maybe runtime error?

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

type TaskU = ProdU BoolU TaskInputU

taskRow :: ElmApp TaskU TaskU (Html :~> Html)
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
    view :: Model TaskListU -> View (ListV Html) (Msg TaskListU)
    view list = ListV $ Data.List.zipWith (\index _ -> Html $ H.button_ [ H.class_ "uk-button", H.class_ "uk-button-danger", H.onClick [ ALDel index ] ] [ H.text "Delete" ]) [0..] list

type TaskListU = ListU TaskU
type TaskListViewU = DupU TaskListU TaskListU
tasks :: ElmApp TaskListU TaskListViewU (ListV Html)
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

-- type FilteredTaskListViewU = DupU TaskListU TaskListViewU
type FilteredTaskListViewU = FilteredListViewU TaskU TaskListViewU

unfinishedTasks :: ElmApp TaskListU FilteredTaskListViewU (ListV Html)
unfinishedTasks = filterE (not . fst) tasks

finishedTasks :: ElmApp TaskListU FilteredTaskListViewU (ListV Html)
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

type FilterAndTasksU = ProdU TaskFilterU TaskListU
type FilterAndTasksViewU = 
  ConditionalViewU FilterAndTasksU 
    (ProdU TaskFilterU TaskListViewU)
    (ConditionalViewU FilterAndTasksU (ProdU TaskFilterU FilteredTaskListViewU) (ProdU TaskFilterU FilteredTaskListViewU))
filteredTasks :: ElmApp  FilterAndTasksU FilterAndTasksViewU (ProdV Html (ListV Html))
filteredTasks = 
  conditional (\(filter, _) -> filter == DisplayAll)
              (product taskFilterSwitch tasks)
              $ conditional (\(filter, _) -> filter == Doing)
                            (product taskFilterSwitch unfinishedTasks)
                            (product taskFilterSwitch finishedTasks)

type FilterAndTasksViewU' =
  DupU TaskFilterU
    (ConditionalViewU FilterAndTasksU
      TaskListViewU
      (ConditionalViewU FilterAndTasksU
        FilteredTaskListViewU FilteredTaskListViewU))
filteredTasks'' :: ElmApp FilterAndTasksU FilterAndTasksViewU'
                  (ProdV Html (ListV Html))
filteredTasks'' = 
  dup (lmap (proj1L []) taskFilterSwitch)
      (conditional (\(filter, _) -> filter == DisplayAll) 
                   (lmap (proj2L DisplayAll) tasks)
                 $ conditional (\(filter, _) -> filter == Doing)
                               (lmap (proj2L Doing) unfinishedTasks)
                               (lmap (proj2L Done) finishedTasks))

inputbox :: ElmApp TaskListU TaskListU Html
inputbox = undefined
                  
type TodoViewU = DupU TaskListU FilterAndTasksViewU
todomvc :: ElmApp FilterAndTasksU TodoViewU Html
todomvc = vmap f (dup (lmap (proj2L DisplayAll) inputbox) filteredTasks)
-- todomvc = vmap f $ dup (lmap (productL id (proj2L DisplayAll)) newTask) (lmap (proj2L "") filteredTasks)
  where
    f :: View (ProdV Html (ProdV Html (ListV Html))) m -> View Html m
    f (ProdV (Html inputV) (ProdV (Html filterV) (ListV tasksV))) = 
        Html $ H.div_ [ H.class_ "uk-grid uk-width-1-2" ] $ [ inputV, filterV ] ++ fmap (\(Html v) -> v) tasksV

themedTodoMVC = vmap f $ product theme todomvc
  where
    f :: View (ProdV (ListV Html :~> Html) Html) ~> View Html
    f (ProdV template todo) = template <~| ListV [ todo ]

-- todomvcapp = render todomvc ("", (DisplayAll, []))

-- todoWithoutFilterApp = render todoWithoutFilter ("", [])

-- themedApp = render themed (False, ("", []))

-- themedApp = render themedTodoMVC (False, ("", (DisplayAll, [])))

class (UpdateStructure filterU, UpdateStructure dataU) => FilterOperation filterU dataU where
  filter:: Proxy filterU -> Proxy dataU -> Model filterU -> Model dataU -> Bool

instance FilterOperation TaskFilterU (ProdU BoolU TaskInputU) where
  filter _ _ DisplayAll = const True
  filter _ _ Doing = not . fst
  filter _ _ Done = fst

data FilterListU filterU u

instance forall u filterU. (UpdateStructure u, UpdateStructure filterU, FilterOperation filterU u) => UpdateStructure (FilterListU filterU u) where
  type Model (FilterListU filterU u) = (Model filterU, [ Model u ])
  type Msg (FilterListU filterU u) = [ AtomicListMsg (Model u) (Msg u) ]

  upd _ model msgs = (fst model, Data.List.filter (Todo.filter (Proxy @filterU) (Proxy @u) (fst model)) $ Data.List.foldl (updAtomicListMsg (Proxy @u)) (snd model) msgs)

filterL :: forall filterU u1 u2. UpdateStructure u1 => ULens u1 u2 -> ULens (FilterListU filterU u1) (FilterListU filterU u2)
filterL l = ULens { get = second $ Data.List.map (get l), trans = tr . snd, create = second $ Data.List.map (create l) }
  where
    tr :: Model (ListU u1) -> Msg (ListU u2) -> Msg (ListU u1)
    tr _ []         = mempty
    tr s (db : dbs) = let da = trA s db
                      in da <> tr (upd (Proxy @(ListU u1)) s da) dbs
    trA :: Model (ListU u1) -> AtomicListMsg (Model u2) (Msg u2) -> Msg (ListU u1)
    trA _ (ALIns i a)   = [ALIns i (create l a)]
    trA _ (ALDel i)     = [ALDel i]
    trA xs (ALRep i da) = case Data.List.splitAt i xs of
      (_xs1 , [] )      -> mempty
      (_xs1, xi : _xs2) -> [ALRep i (trans l xi da)]
    trA _ (ALReorder f) = [ALReorder f]

filterApp :: forall filterU u uv v. (UpdateStructure filterU, UpdateStructure u) => ElmApp u uv v -> ElmApp (FilterListU filterU u) (FilterListU filterU uv) (ListV v)
filterApp (ElmApp lens h) =
  ElmApp (filterL lens) (viewList . snd)
  where
    viewList :: Model (ListU uv) -> View (ListV v) (Msg (ListU uv))
    viewList xs = ListV $ Data.List.zipWith (\x i -> fmap (\msg -> [ALRep i msg]) $ h x) xs [0..]

filterTaskL :: ULens (ProdU TaskFilterU (ListU (ProdU BoolU TaskInputU))) (FilterListU TaskFilterU (ProdU BoolU TaskInputU))
filterTaskL = ULens { 
  get = \(taskFilter, ls) -> case taskFilter of
    DisplayAll -> (taskFilter, ls)
    Doing -> (taskFilter, Data.List.filter fst ls)
    Done -> (taskFilter, Data.List.filter (not . fst) ls),
  trans = tr,
  create = id }
  where 
    tr :: Model (ProdU TaskFilterU (ListU (ProdU BoolU TaskInputU))) -> Msg (FilterListU TaskFilterU (ProdU BoolU TaskInputU)) ->Msg (ProdU TaskFilterU (ListU (ProdU BoolU TaskInputU)))
    tr (filter, _) [] = mempty
    tr (filter, s) (db : dbs) = let da = trA filter s db
                                in (mempty, da) <> tr (filter, upd (Proxy @(ListU (ProdU BoolU TaskInputU))) s da) dbs

    trA :: Model TaskFilterU -> Model (ListU (ProdU BoolU TaskInputU)) -> AtomicListMsg (Model (ProdU BoolU TaskInputU)) (Msg (ProdU BoolU TaskInputU)) -> Msg (ListU (ProdU BoolU TaskInputU))
    trA DisplayAll _ msg = [msg]
    trA Doing ls msg = f (Data.List.length ls) (fmap snd $ Data.List.filter (not . fst . fst) $ Data.List.zip ls [0..]) msg
    trA Done ls msg = f (Data.List.length ls) (fmap snd $ Data.List.filter (fst . fst) $ Data.List.zip ls [0..]) msg
    f n ls (ALIns i a) = case Data.List.splitAt i ls of 
      (_xs1, []) -> [ALIns n a]
      (xs1, xi : xs2) -> [ALIns (xi + 1) a]
    f n ls (ALDel i) = case Data.List.splitAt i ls of
      (_xs1, []) -> []
      (xs1, xi : xs2) -> [ALDel xi]
    f n ls (ALRep i da) = case Data.List.splitAt i ls of
      (_xs1, []) -> []
      (_xs1, xi : _xs2) -> [ALRep xi da]
    f n ls (ALReorder reorder) = [ALReorder (fromList $ fmap (\(from, to) -> (ls !! from, ls !! to)) $ toList reorder)]

unfinishedTasks' = vmap f $ dup (lmap filterTaskL $ filterApp taskRow) (lmap (proj2L DisplayAll) deleteButtons)
  where
    f :: View (ProdV (ListV (v :~> Html)) (ListV v)) ~> View (ListV Html)
    f (ProdV (ListV v1) (ListV v2)) = ListV $ Data.List.zipWith (<~|) v1 v2

filteredTasks' = 
  conditional (\(filter, _) -> filter == DisplayAll)
              (product taskFilterSwitch tasks)
              $ conditional (\(filter, _) -> filter == Doing)
                            (dup (lmap (proj1L []) taskFilterSwitch) unfinishedTasks')
                            (product taskFilterSwitch finishedTasks)

todomvc' = vmap f $ dup (lmap (productL id (proj2L DisplayAll)) newTask) (lmap (proj2L "") filteredTasks')
  where
    f :: View (ProdV Html (ProdV Html (ListV Html))) m -> View Html m
    f (ProdV (Html inputV) (ProdV (Html filterV) (ListV tasksV))) = 
        Html $ H.div_ [ H.class_ "uk-grid uk-width-1-2" ] $ [ inputV, filterV ] ++ fmap (\(Html v) -> v) tasksV

themedTodoMVC' = vmap f $ product theme todomvc
  where
    f :: View (ProdV (ListV Html :~> Html) Html) ~> View Html
    f (ProdV template todo) = template <~| ListV [ todo ]

-- todomvcapp' = render todomvc ("", (DisplayAll, []))

-- themedApp' = render themedTodoMVC (False, ("", (DisplayAll, [])))

