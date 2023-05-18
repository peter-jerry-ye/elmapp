{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- Following is not needed in GHC 2021
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Benchmark where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String      (MisoString, unwords, pack, replicate, null)
import Data.Semigroup   (Sum (..))
import Data.List (zipWith4)
import System.Random
import Prelude hiding (id, product, (.), unwords)
import Elmlens
import Apps
import Data.IntMap.Strict (fromList)
import Numeric.Natural

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

instance ElmlensMsg MisoString where
  checkMempty = Miso.String.null
  checkFail = const False
    
data LabelU
instance UpdateStructure LabelU where
  type Model LabelU = MisoString
  type Msg LabelU = MisoString

  act _ n m = n <> m

buttonsConfig :: [(MisoString, MisoString, ULens (ProdU (RepU Int) (ProdU (RepU StdGen) (ListU (ProdU (RepU Int) LabelU)))) (UnitU (Sum Natural)))]
buttonsConfig = [
  ("run", "Create 1,000 rows", 
    ULens { get = const (),
            trans = \(index, (gen, ls)) (Sum m) -> if m == 0 then mempty else let n = 1 in (\(msgs, _, _, _) -> msgs) $ 
                foldl (\(cumul, index, gen, l) _ -> 
                        let (newRows, newGen) = generateRows n index ([], gen) 
                        in (cumul <> (Replace $ index + n, (Replace newGen, Prelude.replicate l (ALDel 0) ++ zipWith ALIns [0..] newRows)), index + n, newGen, length newRows)) 
                      (mempty, index, gen, length ls) [0 .. m - 1],
            create = const (0, (mkStdGen 0, []))}),
  ("runlots", "Create 10,000 rows",
    ULens { get = const (),
            trans = \(index, (gen, ls)) (Sum m) -> if m == 0 then mempty else let n = 10 in (\(msgs, _, _, _) -> msgs) $ 
                foldl (\(cumul, index, gen, l) _ -> 
                        let (newRows, newGen) = generateRows n index ([], gen) 
                        in (cumul <> (Replace $ index + n, (Replace newGen, Prelude.replicate l (ALDel 0) ++ zipWith ALIns [0..] newRows)), index + n, newGen, length newRows)) 
                      (mempty, index, gen, length ls) [0 .. m - 1],
            create = const (0, (mkStdGen 0, []))}),
  ("add", "Append 1,000 rows",
    ULens { get = const (),
            trans = \(index, (gen, ls)) (Sum m) -> 
              let n = 1000 * fromIntegral m
                  (newRows, newGen) = generateRows n index ([], gen) in
              if n == 0 then mempty else (Replace $ index + n, (Replace newGen, zipWith ALIns [(length ls)..] newRows)),
            create = const (0, (mkStdGen 0, []))}),
  ("update", "Update every 10th row", 
    ULens { get = const (), 
            trans = \(_, (_, ls)) (Sum m) -> if m == 0 then mempty else (mempty, (mempty, [ ALRep i (mempty, Miso.String.replicate (fromIntegral m) $ pack " !!!") | i <- [0 .. (length ls)], i `mod` 10 == 0])),
            create = const (0, (mkStdGen 0, []))}),
  ("clear", "Clear", 
    ULens { get = const (),
            trans = \(_, (_, ls)) m -> if m == 0 then mempty else (mempty, (mempty, Prelude.replicate (length ls) (ALDel 0))),
            create = const (0, (mkStdGen 0, []))}),
  ("swaprows", "Swap Rows", 
    ULens { get = const (),
            trans = \(_, (_, ls)) (Sum m) -> if m == 0 then mempty else (mempty, (mempty, Prelude.replicate (fromIntegral m) $ ALReorder $ fromList [(1, 998), (998, 1)])),
            create = const (0, (mkStdGen 0, []))})]

-- TODO When a button is clicked multiple times, will the message be truncated into one?

btnPrimaryBlock :: MisoString -> MisoString -> ElmApp (UnitU (Sum Natural)) (UnitU (Sum Natural)) Html
btnPrimaryBlock buttonId label = fromView $ \_ -> Html $ 
    H.div_ [ H.class_ "col-sm-6 smallpad" ]
           [ H.button_ [ H.type_ "button",
                         H.class_ "btn btn-primary btn-block",
                         H.id_ buttonId,
                         H.onClick 0,
                         H.textProp "ref" "text" ]
                       [ text label ] ]

{-

class Mixable x u uv v | x -> u, x -> uv, x -> v  where
  mix :: x -> ElmApp u uv v 

instance Mixable (ElmApp u1 uv1 v1) u1 uv1 v1 where 
  mix = id 

instance (Mixable a u1 uv1 v1, Mixable b u2 uv2 v2) => Mixable (a, b) (ProdU u1 u2) (ProdU uv1 uv1) (ProdV v1 v2) where
  mix a b = vmix (mix a) (mix b)

pattern a |> b = (a, b)
infixr |> 4 

mix (d1 |> d2 |> d3 |> d4 |> d5) ...

-}

-- buttons = fmap (\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) buttonsConfig
buttons = vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ head buttonsConfig)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 1)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 2)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 3)
        $ vmix ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 4)
               ((\(buttonId, label, lens) -> lmap lens $ btnPrimaryBlock buttonId label) $ buttonsConfig !! 5)
                    
jumbotronTemplate :: ElmApp (UnitU (Sum Natural)) (UnitU (Sum Natural)) (ListV Html :~> Html)
jumbotronTemplate = fromView $ \_ ->  Holed (\_f (ListV buttons) -> 
    Html (H.div_ [ H.class_ "jumbotron" ]
                 [ H.div_ [ H.class_ "row" ]
                          [ H.div_ [ H.class_ "col-md-6" ]
                                   [ H.h1_ [] [ H.text "Elmlens (non-keyed)" ]],
                            H.div_ [ H.class_ "col-md-6"] (fmap (\(Html b) -> b) buttons) ]]))

jumbotron = vmap (\(ProdV (Holed template) (ProdV h1 (ProdV h2 (ProdV h3 (ProdV h4 (ProdV h5 h6)))))) -> template id $ ListV [h1, h2, h3, h4, h5, h6] ) 
  $ vmix (lmap (unitL (0, (mkStdGen 0, []))) jumbotronTemplate) buttons

deletes :: UpdateStructure u => ElmApp (ListU u) (ListU u) (ListV Html)
deletes = fromView $ \ls -> ListV $ fmap (\i -> Html $ 
    H.a_ [ H.onClick [ALDel i]] 
         [ H.span_ [ H.class_ "glyphicon glyphicon-remove", H.boolProp "aria-hidden" True ] [] ] ) 
    [0 .. (length ls)]

highlights :: ElmApp (ProdU (RepU Int) (ListU (ProdU (RepU Int) LabelU)))
                     (ProdU (RepU Int) (ListU (ProdU (RepU Int) LabelU)))
                     (ListV (ProdV Attr Html))
highlights = fromView $ \(selected, ls) -> ListV $ fmap (\(id, label) -> 
    ProdV ( Attr $ H.classList_ [ ("danger", selected == id) ])
         ( Html $ H.a_ [H.onClick (Replace id, mempty)] [ text label ]) )
    ls

rowTemplate :: ElmApp (UnitU (Sum Natural)) (UnitU (Sum Natural)) (Attr :~> Html :~> Html :~> Html :~> Html)
rowTemplate = fromView $ \_ -> 
  Holed (\_f (Attr attr) -> 
  Holed (\f1 (Html h1) ->
  Holed (\f2 (Html h2) ->
  Holed (\f3 (Html h3) -> 
    Html (H.tr_ [ f3 . f2 . f1 <$> attr]
         [ H.td_ [ H.class_ "col-md-1" ] [ f3 . f2 <$> h1 ],
           H.td_ [ H.class_ "col-md-4" ] [ f3 <$> h2 ],
           H.td_ [ H.class_ "col-md-1" ] [ h3 ],
           H.td_ [ H.class_ "col-md-6" ] [] ])))))

tableTemplate = list rowTemplate

rows :: ElmApp (ListU (ProdU (RepU Int) LabelU)) (ListU (ProdU (RepU Int) LabelU)) (ListV (ProdV Html Html))
rows = fromView $ \ls -> ListV $ fmap (\(index, label) -> ProdV (Html $ H.text $ pack $ show index) (Html $ H.text label)) ls

table = vmap mapView $ vmix highlights (lmap (proj2L 0) (vmix (vmix rows deletes) (lmap (mapL (unitL (0, ""))) tableTemplate)))
  where
    mapView :: View (ProdV (ListV (ProdV Attr Html)) (ProdV (ProdV (ListV (ProdV Html Html)) (ListV Html)) (ListV (Attr :~> (Html :~> (Html :~> (Html :~> Html))))))) m -> View (ListV Html) m
    mapView (ProdV (ListV highlights) (ProdV (ProdV (ListV rows) (ListV deletes)) (ListV templates))) = ListV $ zipWith4 f highlights rows deletes templates
    f :: View (ProdV Attr Html) m -> View (ProdV Html Html) m -> View Html m -> View (Attr :~> (Html :~> (Html :~> (Html :~> Html)))) m -> View Html m
    f (ProdV attr button) (ProdV index label) delete template = 
      template <~| attr <~| index <~| button <~| delete

template :: ElmApp (UnitU (Sum Natural)) (UnitU (Sum Natural)) (Html :~> ListV Html :~> Html)
template = fromView $ \_ -> Holed (\_f (Html jumbotron) -> Holed (\f1 (ListV rows) -> 
    Html $ H.div_ [ H.class_ "container" ] 
                  [ f1 <$> jumbotron,
                    H.table_ [ H.class_ "table table-hover table-striped test-data" ] [ 
                      H.tbody_ [] (fmap (\(Html b) -> b) rows), 
                      H.span_ [ H.class_ "preloadicon glyphicon glyphicon-remove",
                                H.boolProp "aria-hidden" True ] [] ]]))

benchmark = vmap f $ vmix (vmix (lmap (productL id (productL id (proj2L 0))) jumbotron) (lmap (proj2L (mkStdGen 0) . proj2L 0) table)) (lmap (unitL (0, (mkStdGen 0, (0, [])))) template)
  where
    f :: View (ProdV (ProdV Html (ListV Html)) (Html :~> ListV Html :~> Html)) m -> View Html m
    f (ProdV (ProdV jumbo rows) template) = template <~| jumbo <~| rows

benchmarkApp seed = render benchmark (0, (seed, (-1, [])))
