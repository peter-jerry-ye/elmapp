-- Implemented according to https://github.com/krausest/js-framework-benchmark/blob/master/frameworks/non-keyed/elm/src/Main.elm
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE StrictData #-}

module Benchbase where

import Miso
import qualified Miso.Html as H
-- import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Sequence (Seq((:<|), (:|>)), (|>), (<|), (><))
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
  -- rows :: V.Vector Row,
  rows :: S.Seq Row,
  lastId :: Int,
  selectedId :: Int
} deriving (Eq)

-- appendRandomEntries :: Int -> Int -> (V.Vector Row, StdGen) -> (V.Vector Row, StdGen)
appendRandomEntries :: Int -> Int -> (S.Seq Row, StdGen) -> (S.Seq Row, StdGen)
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
    -- newRows = V.snoc rows $ Row { ident = newId, label = unwords [adjective, colour, noun] } 
    newRows = rows |> Row { ident = newId, label = unwords [adjective, colour, noun]}

-- updateEveryTenth :: Int -> V.Vector Row -> V.Vector Row
updateEveryTenth :: Int -> S.Seq Row -> S.Seq Row
updateEveryTenth index rows =
  -- if V.null tl then hd
  -- else updateEveryTenth (index + 10) (hd V.++ V.cons (row { label = newLabel }) (V.tail tl))
  --   where 
  --     (hd, tl) = V.splitAt index rows
  --     row = V.head tl
  --     newLabel = unwords [label row, "!!!"]
  case S.splitAt index rows of
    (hd, S.Empty) -> rows
    (hd, row :<| tl) -> updateEveryTenth (index + 10) (hd >< row { label = unwords [label row, "!!!"] } <| tl)

updateModel :: Msg -> Model -> Effect Msg Model
updateModel msg model = 
  case msg of
    Create amount -> 
      -- let (newRows, newSeed) = appendRandomEntries amount (lastId model) ( V.empty, seed model )
      let (newRows, newSeed) = appendRandomEntries amount (lastId model) (S.Empty, seed model)
      in noEff ( model { rows = newRows, seed = newSeed, lastId = lastId model + amount } )
    AppendOneThousand ->
      let 
        amount = 1000
        (newRows, newSeed) = appendRandomEntries amount (lastId model) (rows model, seed model)
      in noEff ( model { rows = newRows, seed = newSeed, lastId = lastId model + amount} )
    UpdateEveryTenth ->
      noEff ( model { rows = updateEveryTenth 0 (rows model ) } )
    -- Clear -> noEff (model { rows = V.empty })
    Clear -> noEff (model { rows = S.Empty })
    Swap ->
      -- let (hd, tl) = V.splitAt 1 (rows model) in
      --   if V.null tl then noEff model
      --   else let (hd', tl') = V.splitAt 996 $ V.tail tl in
      --          if V.null tl' then noEff model
      --          else noEff (model { rows = V.concat [hd, V.singleton $ V.head tl, hd', V.singleton $ V.head tl', V.tail tl'] })
        case S.splitAt 1 (rows model) of
          (_, S.Empty) -> noEff model
          (hd, from :<| tl) -> case S.splitAt 996 tl of
            (_, S.Empty) -> noEff model
            (hd', to :<| tl') -> noEff (model { rows = hd >< to <| hd' >< from <| tl' })
    Remove id -> noEff (model { rows = S.filter (\r -> ident r /= id) (rows model)})
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
  model = Model { seed = seed, rows = S.Empty, lastId = 0, selectedId = -1 },
  update = updateModel,
  view = viewModel,
  subs = [],
  events = defaultEvents,
  initialAction = NoOp,
  mountPoint = Nothing,
  logLevel = Off
}
