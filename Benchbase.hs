{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module BenchBase where

import Miso
import qualified Miso.Html as H
import Miso.String (MisoString, unwords)
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

data Row = Row {
  ident :: Int,
  label :: MisoString
}

data Model = Model {
  seed :: StdGen,
  rows :: [ Row ],
  lastId :: Int,
  selectedId :: Int
}

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

viewModel :: Model -> View Msg
viewModel = undefined

benchbaseApp :: StdGen -> App Model Msg
benchbaseApp seed = App {
  model = Model { seed = seed, rows = [], lastId = 0, selectedId = -1 },
  update = updateModel,
  view = viewModel,
  subs = [],
  events = defaultEvents,
  initialAction = Clear,
  mountPoint = Nothing,
  logLevel = Off
}
