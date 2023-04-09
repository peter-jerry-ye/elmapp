-- Implemented according to https://github.com/krausest/js-framework-benchmark/blob/master/frameworks/non-keyed/elm/src/Main.elm
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Benchbase where

import Miso
import qualified Miso.Html as H
import Miso.String (MisoString, unwords, pack)
import System.Random
import Prelude hiding (unwords)

adjectives :: [ MisoString ]
adjectives = [
   "pretty"
 , "large"
 , "big"
 , "small"
 , "tall"
 , "short"
 , "long"
 , "handsome"
 , "plain"
 , "quaint"
 , "clean"
 , "elegant"
 , "easy"
 , "angry"
 , "crazy"
 , "helpful"
 , "mushy"
 , "odd"
 , "unsightly"
 , "adorable"
 , "important"
 , "inexpensive"
 , "cheap"
 , "expensive"
 , "fancy" ]

colours :: [ MisoString ]
colours = [
   "red"
 , "yellow"
 , "blue"
 , "green"
 , "pink"
 , "brown"
 , "purple"
 , "brown"
 , "white"
 , "black"
 , "orange" ]

nouns :: [ MisoString ]
nouns = [ 
   "table"
 , "chair"
 , "house"
 , "bbq"
 , "desk"
 , "car"
 , "pony"
 , "cookie"
 , "sandwich"
 , "burger"
 , "pizza"
 , "mouse"
 , "keyboard" ]

data Msg = 
    Create Int
  | AppendOneThousand
  | UpdateEveryTenth
  | Clear
  | Swap
  | Remove Int
  | Select Int
  | NoOp

data Row = Row {
  ident :: Int,
  label :: MisoString
} deriving (Eq)

data Model = Model {
  seed :: StdGen,
  rows :: [ Row ],
  lastId :: Int,
  selectedId :: Int
} deriving (Eq)

appendRandomEntries :: Int -> Int -> ([ Row ], StdGen) -> ([ Row ], StdGen)
appendRandomEntries amount lastId (rows, seed) = 
  if amount == 0 
    then (rows, seed) 
    else appendRandomEntries (amount - 1) newId (newRows, newSeed)
  where
    newId = lastId + 1
    (ia, s1) = uniformR (0, length adjectives - 1) seed
    (ic, s2) = uniformR (0, length colours - 1) s1
    (in_, newSeed) = uniformR (0, length nouns - 1) s2
    adjective :: MisoString
    adjective = adjectives !! ia
    colour :: MisoString
    colour = colours !! ic
    noun :: MisoString
    noun = nouns !! in_
    newRows = rows ++ [ Row { ident = newId, label = unwords [adjective, colour, noun] } ]

updateEveryTenth :: Int -> [ Row ] -> [ Row ]
updateEveryTenth index rows =
  case splitAt index rows of
    (_, [])       -> rows
    (hd, row : tl) -> updateEveryTenth (index + 10) (hd ++ ((row { label = newLabel }) : tl))
      where 
        newLabel :: MisoString
        newLabel = unwords [label row, "!!!"]

updateModel :: Msg -> Model -> Effect Msg Model
updateModel msg model = 
  case msg of
    Create amount -> 
      let (newRows, newSeed) = appendRandomEntries amount (lastId model) ( [], seed model )
      in noEff ( model { rows = newRows, seed = newSeed, lastId = lastId model + amount } )
    AppendOneThousand ->
      let 
        amount = 1000
        (newRows, newSeed) = appendRandomEntries amount (lastId model) (rows model, seed model)
      in noEff ( model { rows = newRows, seed = newSeed, lastId = lastId model + amount} )
    UpdateEveryTenth ->
      noEff ( model { rows = updateEveryTenth 0 (rows model ) } )
    Clear -> noEff (model { rows = [] })
    Swap ->
      case splitAt 1 (rows model) of
        (_, []) -> noEff model
        (hd, from : tl) -> case splitAt 996 tl of
          (_, []) -> noEff model
          (hd', to : tl') -> noEff (model { rows = hd ++ to : (hd' ++ from : tl') })
    Remove id -> noEff (model { rows = filter (\r -> ident r /= id) (rows model)})
    Select id -> noEff (model { selectedId = id })
    NoOp -> noEff model

buttons = [
  ("run", "Create 1,000 rows", Create 1000),
  ("runlots", "Create 10,000 rows", Create 10000),
  ("add", "Append 1,000 rows", AppendOneThousand),
  ("update", "Update every 10th row", UpdateEveryTenth),
  ("clear", "Clear", Clear),
  ("swaprows", "Swap Rows", Swap) ]

btnPrimaryBlock :: ( MisoString, MisoString, Msg ) -> View Msg
btnPrimaryBlock (buttonId, labelText, msg) = 
  H.div_ [ class_ "col-sm-6 smallpad" ]
         [ H.button_ [ H.type_ "button",
                       H.class_ "btn btn-primary btn-block",
                       H.id_ buttonId,
                       H.onClick msg,
                       H.textProp "ref" "text"]
                     [ H.text labelText ]]

viewRow :: Int -> Row -> View Msg
viewRow selectedId row =
  let ident_ = ident row
      label_ = label row in
  H.tr_ [ H.classList_ [ ("danger", selectedId == ident_)]]
        [ H.td_ colMd1 [ H.text (pack $ show ident_) ],
          H.td_ colMd4 [ H.a_ [ H.onClick (Select ident_) ] [ H.text label_ ]],
          H.td_ colMd1 [ H.a_ [ H.onClick (Remove ident_) ] removeIcon ],
          spacer]
  where colMd1 = [ H.class_ "col-md-1" ]
        colMd4 = [ H.class_ "col-md-4" ]
        spacer = H.td_ [ H.class_ "col-md-6" ] []
        removeIcon = [ H.span_ [ H.class_ "glyphicon glyphicon-remove",
                                 H.boolProp "aria-hidden" True] [] ]

viewModel :: Model -> View Msg
viewModel model = 
  H.div_ containerClasses
         [ jumbotron,
           H.table_ tableClasses
                    [ H.tbody_ [] (foldr (f (selectedId model)) [] (rows model))],
           footer]
  where
    containerClasses = [ H.class_ "container" ]
    tableClasses = [ H.class_ "table table-hover table-striped test-data" ]
    footer = H.span_ [ H.class_ "preloadicon glyphicon glyphicon-remove",
                       H.boolProp "aria-hidden" True] []
    jumbotron = H.div_ [ H.class_ "jumbotron" ]
                       [ H.div_ [ H.class_ "row" ]
                                [ H.div_ [ H.class_ "col-md-6" ]
                                         [ H.h1_ [] [ text "Miso (non-keyed)" ]],
                                  H.div_ [ H.class_ "col-md-6" ]
                                         (fmap btnPrimaryBlock buttons)]]
    f :: Int -> Row -> [ View Msg ] -> [ View Msg ]
    f selectedId row elems = viewRow selectedId row : elems
    

benchbaseApp :: StdGen -> App Model Msg
benchbaseApp seed = App {
  model = Model { seed = seed, rows = [], lastId = 0, selectedId = -1 },
  update = updateModel,
  view = viewModel,
  subs = [],
  events = defaultEvents,
  initialAction = NoOp,
  mountPoint = Nothing,
  logLevel = Off
}
