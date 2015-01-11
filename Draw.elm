module Draw where

import Graphics.Collage(..)
import Graphics.Element (Element)
import Color
import List
import List((::))
import Dict
import Types(..)
import Utils(..)

drawPerson : Person -> Form
drawPerson p = circle 3 |> filled Color.black |> move (p.pos.x, p.pos.y)

drawBeerSource : BeerSource -> Form
drawBeerSource s = circle s.radius |> filled Color.brown |> move (s.pos.x, s.pos.y)

toTuple pos = (pos.x, pos.y)

drawEdges : Env -> State -> Form
drawEdges e s =
  let maxDistance = sqrt ((toFloat e.dims.w)^2 + (toFloat e.dims.h)^2)
      drawEdgesFrom p =
        let net = s.network ! p.idNum in
        List.filterMap (\q ->
          let d = dist p.pos q.pos in
          if d > 100 then Nothing else 
            let opacity = d / maxDistance
                hue     = degrees (60 + 60 * (net ! q.idNum))
            in
            traced (solid (Color.hsla hue 1 0.5 opacity))
              (segment (toTuple p.pos) (toTuple q.pos))
            |> Just)
          (Dict.values s.people)
  in
  group (List.concatMap (\i -> drawEdgesFrom (s.people ! i)) [0..99])

draw : Env -> State -> Element
draw e s =
  let beers  = group (List.map drawBeerSource e.beerSources)
      people = group (List.map drawPerson (Dict.values s.people))
  in collage e.dims.w e.dims.h [beers, people, drawEdges e s]

