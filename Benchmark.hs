{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Benchmark where

import Data.Kind        (Type)
import Control.Category (Category (..))
import Miso hiding (View)
import qualified Miso.Html as H
import Miso.String
import Data.Semigroup   (Sum (..))
import Data.List        (zipWith)
import Prelude hiding (id, product, (.))
import Elmlens
import Apps

colMd1 = [ H.class_ "col-md-1" ]
colMd4 = [ H.class_ "col-md-4" ]
spacer = H.td_ [ H.class_ "col-md-6" ] []
containerClasses = [ H.class_ "container" ]
tableClasses = [ H.class_ "table table-hover table-striped test-data" ]
footer = H.span_ [ H.class_ "preloadicon glyphicon glyphicon-remove", 
                  H.boolProp "aria-hidden" True]

rowTemplate :: ElmApp (UnitU ()) (UnitU ()) (ListV Attr :~> (HTML :~> (HTML :~> (HTML :~> HTML))))
rowTemplate = fromView $ \_ -> 
  Holed (\_f (ViewList attrs) -> 
  Holed (\f1 (Base h1) ->
  Holed (\f2 (Base h2) ->
  Holed (\f3 (Base h3) -> 
    Base (H.tr_ (fmap (\(Property p) -> f3 . f2 . f1 <$> p) attrs) 
         [ H.td_ [ H.class_ "col-md-1" ] [ f3 . f2 <$> h1 ],
           H.td_ [ H.class_ "col-md-4" ] [ f3 <$> h2 ],
           H.td_ [ H.class_ "col-md-1" ] [ h3 ],
           H.td_ [ H.class_ "col-md-6" ] [] ])))))

tableTemplate = list rowTemplate

jumbotronTemplate :: ElmApp (UnitU ()) (UnitU ()) (ListV HTML :~> HTML)
jumbotronTemplate = fromView $ \_ ->  Holed (\_f (ViewList buttons) -> 
    Base (H.div_ [ H.class_ "jumbotron" ]
                 [ H.div_ [ H.class_ "col-md-6" ]
                          [ H.h1_ [] [ H.text "Elmlens (non-keyed)" ]],
                   H.div_ [ H.class_ "col-md-6"] (fmap (\(Base b) -> b) buttons) ]))

template :: ElmApp (UnitU ()) (UnitU ()) (HTML :~> (ListV HTML :~> HTML))
template = fromView $ \_ -> Holed (\_f (Base jumbotron) -> Holed (\f1 (ViewList rows) -> 
    Base $ H.div_ [ H.class_ "container" ] 
                  [ f1 <$> jumbotron,
                    H.table_ [ H.class_ "table table-hover table-striped test-data" ] [ 
                      H.tbody_ [] (fmap (\(Base b) -> b) rows), 
                      H.span_ [ H.class_ "preloadicon glyphicon glyphicon-remove",
                                H.boolProp "aria-hidden" True ] [] ]]))