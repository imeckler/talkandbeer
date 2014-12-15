module Draw where

import Graphics.Collage(..)
import Graphics.Element (Element)
import Color
import List
import Dict
import Types(..)

drawPerson : Person -> Form
drawPerson p = circle 3 |> filled Color.red |> move (p.pos.x, p.pos.y)

drawBeerSource : BeerSource -> Form
drawBeerSource s = circle s.radius |> filled Color.brown |> move (s.pos.x, s.pos.y)

draw : Env -> State -> Element
draw e s =
  let beers = List.map drawBeerSource  e.beerSources
  in collage e.dims.w e.dims.h <| ((++) beers) <| List.map drawPerson <| Dict.values s.people

