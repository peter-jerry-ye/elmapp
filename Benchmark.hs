{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Benchmark where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String      (MisoString, unwords, pack)
import Data.Semigroup   (Sum (..))
import Data.List (zipWith4)
import System.Random
import Prelude hiding (id, product, (.), unwords)
import Elmlens
import Apps

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

generateRows :: Int -> Int -> ([ (Int, MisoString) ], StdGen) -> ([ (Int, MisoString ) ], StdGen)
generateRows amount lastId (rows, seed) = if amount == 0 then (rows, seed) else generateRows (amount - 1) id (newRows, newSeed)
  where
    id = lastId + 1
    (ia, s1) = uniformR (0, length adjectives - 1) seed
    (ic, s2) = uniformR (0, length colours - 1) s1
    (in_, newSeed) = uniformR (0, length nouns - 1) s2
    adjective :: MisoString
    adjective = adjectives !! ia
    colour :: MisoString
    colour = colours !! ic
    noun :: MisoString
    noun = nouns !! in_
    newRows = rows ++ [ (id, unwords [adjective, colour, noun]) ]
    
data LabelU
instance UpdateStructure LabelU where
  type Model LabelU = MisoString
  type Msg LabelU = MisoString

  act _ n m = n <> m

buttonsConfig :: [(MisoString, MisoString, ULens (ProdU (RepU Int) (ProdU (RepU StdGen) (ListU (ProdU (RepU Int) LabelU)))) (UnitU (Maybe ())))]
buttonsConfig = [
  ("run", "Create 1,000 rows", 
    ULens { get = const (),
            trans = \(index, (gen, ls)) m -> case m of
              Nothing -> mempty
              Just _ -> (Replace $ index + 1000, (Replace newGen, replicate (length ls) (ALDel 0) ++ zipWith ALIns [0..] newRows)) 
                where (newRows, newGen) = generateRows 1000 index ([], gen),
            create = const (0, (mkStdGen 0, []))}),
  ("runlots", "Create 10,000 rows",
    ULens { get = const (),
            trans = \(index, (gen, ls)) m -> case m of
              Nothing -> mempty
              Just _ -> (Replace $ index + 10000, (Replace newGen, replicate (length ls) (ALDel 0) ++ zipWith ALIns [0..] newRows)) 
                where (newRows, newGen) = generateRows 10000 index ([], gen),
            create = const (0, (mkStdGen 0, []))}),
  ("add", "Append 1,000 rows",
    ULens { get = const (),
            trans = \(index, (gen, ls)) m -> case m of
              Nothing -> mempty
              Just _ -> (Replace $ index + 1000, (Replace newGen, zipWith ALIns [(length ls)..] newRows)) 
                where (newRows, newGen) = generateRows 1000 index ([], gen),
            create = const (0, (mkStdGen 0, []))}),
  ("update", "Update every 10th row", 
    ULens { get = const (), 
            trans = \(_, (_, ls)) m -> case m of 
              Nothing -> mempty
              Just _  -> (mempty, (mempty, [ ALRep i (mempty, " !!!") | i <- [0 .. (length ls)], i `mod` 10 == 0])),
            create = const (0, (mkStdGen 0, []))}),
  ("clear", "Clear", 
    ULens { get = const (),
            trans = \(_, (_, ls)) m -> case m of
              Nothing -> mempty
              Just _  -> (mempty, (mempty, replicate (length ls) (ALDel 0))),
            create = const (0, (mkStdGen 0, []))}),
  ("swaprows", "Swap Rows", 
    ULens { get = const (),
            trans = \(_, (_, ls)) m -> case m of
              Nothing -> mempty
              Just _  -> (mempty, (mempty, [ if length ls >= 999 then ALReorder (\i -> if i == 1 then 998 else if i == 998 then 1 else i) else ALReorder id ])),
            create = const (0, (mkStdGen 0, []))})]

-- TODO When a button is clicked multiple times, will the message be truncated into one?

btnPrimaryBlock :: MisoString -> MisoString -> ElmApp (UnitU (Maybe ())) (UnitU (Maybe ())) HTML
btnPrimaryBlock buttonId label = fromView $ \_ -> Base $ 
    H.div_ [ H.class_ "col-sm-6 smallpad" ]
           [ H.button_ [ H.type_ "button",
                         H.class_ "btn btn-primary btn-block",
                         H.id_ buttonId,
                         H.onClick $ Just (),
                         H.textProp "ref" "text" ]
                       [ text label ] ]

-- buttons = fmap (\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) buttonsConfig
buttons = vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ head buttonsConfig)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 1)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 2)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 3)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 4)
               ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 5)
                    
jumbotronTemplate :: ElmApp (UnitU (Maybe ())) (UnitU (Maybe ())) (ListV HTML :~> HTML)
jumbotronTemplate = fromView $ \_ ->  Holed (\_f (ViewList buttons) -> 
    Base (H.div_ [ H.class_ "jumbotron" ]
                 [ H.div_ [ H.class_ "row" ]
                          [ H.div_ [ H.class_ "col-md-6" ]
                                   [ H.h1_ [] [ H.text "Elmlens (non-keyed)" ]],
                            H.div_ [ H.class_ "col-md-6"] (fmap (\(Base b) -> b) buttons) ]]))

-- jumbotron = vmap (\(Pair (Holed template) h) -> template id h ) $ lmap (splitL (unitL (0, (mkStdGen 0, []))) id) $ product jumbotronTemplate buttonList
--   where
--     buttonList = foldl1 f buttons
--     f :: ElmApp u uv1 HTML -> ElmApp u uv2 HTML -> ElmApp u (ProdU uv1 uv2) HTML
--     f e1 e2 = vmap (\(Pair v1 v2) -> v2) $ vmix e1 e2

jumbotron = vmap (\(Pair (Holed template) (Pair h1 (Pair h2 (Pair h3 (Pair h4 (Pair h5 h6)))))) -> template id $ ViewList [h1, h2, h3, h4, h5, h6] ) 
  $ lmap (splitL (unitL (0, (mkStdGen 0, []))) id) $ product jumbotronTemplate buttons

deletes :: UpdateStructure u => ElmApp (ListU u) (ListU u) (ListV HTML)
deletes = fromView $ \ls -> ViewList $ fmap (\i -> Base $ 
    H.a_ [ H.onClick [ALDel i]] 
         [ H.span_ [ H.class_ "glyphicon glyphicon-remove", H.boolProp "aria-hidden" True ] [] ] ) 
    [0 .. (length ls)]

highlights :: ElmApp (ProdU (RepU Int) (ListU (ProdU (RepU Int) LabelU)))
                     (ProdU (RepU Int) (ListU (ProdU (RepU Int) LabelU)))
                     (ListV (ProdV Attr HTML))
highlights = fromView $ \(selected, ls) -> ViewList $ fmap (\(id, label) -> 
    Pair ( Property $ H.classList_ [ ("danger", selected == id) ])
         ( Base $ H.a_ [H.onClick (Replace id, mempty)] [ text label ]) )
    ls

rowTemplate :: ElmApp (UnitU (Maybe ())) (UnitU (Maybe ())) (Attr :~> (HTML :~> (HTML :~> (HTML :~> HTML))))
rowTemplate = fromView $ \_ -> 
  Holed (\_f (Property attr) -> 
  Holed (\f1 (Base h1) ->
  Holed (\f2 (Base h2) ->
  Holed (\f3 (Base h3) -> 
    Base (H.tr_ [ f3 . f2 . f1 <$> attr]
         [ H.td_ [ H.class_ "col-md-1" ] [ f3 . f2 <$> h1 ],
           H.td_ [ H.class_ "col-md-4" ] [ f3 <$> h2 ],
           H.td_ [ H.class_ "col-md-1" ] [ h3 ],
           H.td_ [ H.class_ "col-md-6" ] [] ])))))

tableTemplate = list rowTemplate

rows :: ElmApp (ListU (ProdU (RepU Int) LabelU)) (ListU (ProdU (RepU Int) LabelU)) (ListV (ProdV HTML HTML))
rows = fromView $ \ls -> ViewList $ fmap (\(index, label) -> Pair (Base $ H.text $ pack $ show index) (Base $ H.text label)) ls

table = vmap mapView $ lmap (splitL id (proj2L 0)) $ product highlights (lmap (splitL id (mapL (unitL (0, "")))) $ product (vmix rows deletes) tableTemplate)
  where
    mapView :: View (ProdV (ListV (ProdV Attr HTML)) (ProdV (ProdV (ListV (ProdV HTML HTML)) (ListV HTML)) (ListV (Attr :~> (HTML :~> (HTML :~> (HTML :~> HTML))))))) m -> View (ListV HTML) m
    mapView (Pair (ViewList highlights) (Pair (Pair (ViewList rows) (ViewList deletes)) (ViewList templates))) = ViewList $ zipWith4 f highlights rows deletes templates
    f :: View (ProdV Attr HTML) m -> View (ProdV HTML HTML) m -> View HTML m -> View (Attr :~> (HTML :~> (HTML :~> (HTML :~> HTML)))) m -> View HTML m
    f (Pair attr button) (Pair index label) delete (Holed template) = 
      let Holed template2 = template id attr
          Holed template3 = template2 id index
          Holed template4 = template3 id button
          in template4 id delete

template :: ElmApp (UnitU (Maybe ())) (UnitU (Maybe ())) (HTML :~> (ListV HTML :~> HTML))
template = fromView $ \_ -> Holed (\_f (Base jumbotron) -> Holed (\f1 (ViewList rows) -> 
    Base $ H.div_ [ H.class_ "container" ] 
                  [ f1 <$> jumbotron,
                    H.table_ [ H.class_ "table table-hover table-striped test-data" ] [ 
                      H.tbody_ [] (fmap (\(Base b) -> b) rows), 
                      H.span_ [ H.class_ "preloadicon glyphicon glyphicon-remove",
                                H.boolProp "aria-hidden" True ] [] ]]))

benchmark = vmap f $ lmap (splitL id (unitL (0, (mkStdGen 0, (0, []))))) $ product (lmap (splitL (productL id (productL id (proj2L 0))) (proj2L (mkStdGen 0) . proj2L 0)) $ product jumbotron table) template
  where
    f :: View (ProdV (ProdV HTML (ListV HTML)) (HTML :~> (ListV HTML :~> HTML))) m -> View HTML m
    f (Pair (Pair jumbo rows) (Holed template)) =
      let Holed template2 = template id jumbo
      in template2 id rows

benchmarkApp seed = render benchmark (0, (seed, (-1, [])))
