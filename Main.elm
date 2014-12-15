module Main where

import Types(..)
import Utils(..)
import Draw
import List
import List((::))
import Maybe
import Dict
import Time
import Signal
import Random
import Graphics.Element (Element)

isOld : Person -> Bool
isOld = let oldAge = 10000 in \p -> p.age > oldAge

isUnhappy : Person -> Bool
isUnhappy = let sad = 0.1 in \p -> p.happiness < sad

-- Perhaps they should have to ask someone they know where the beer is
nearestBeerSource : Env -> Pos -> BeerSource
nearestBeerSource e p = minimumOn (\s -> dist p s.pos) e.beerSources

anti x = 1 - x

baseSpeed = 1
-- TODO: At every instance, the person should select their new goal
-- via a utility calculation. Utility includes things like how turntocious
-- they wish to become, how much time would be spent approaching the goal,
-- how much they like certain individuals, etc.

findPersonPos : State -> Int -> Maybe Pos
findPersonPos s pid = Maybe.map (.pos) (Dict.get pid s.people)

closePeople : State -> Pos -> List Person
closePeople s p =
  let r = 10 in
  Dict.foldl (\_ q ps -> if dist p q.pos < r then q :: ps else ps) [] s.people

newGoal k = {kind = k, age = 0}

updateGoal : State -> Person -> Person
updateGoal s p = if p.goal.age < 1000 then p else if
  | p.turntocity - p.predilection > 0.5 -> {p | goal <- newGoal Drink}
  | True -> 
    let m               = s.network ! p.idNum
        friendishness q = m ! q.idNum
    in {p | goal <- newGoal (TalkTo (maximumOn friendishness (closePeople s p.pos)).idNum)}

incrAge p = {p | age <- p.age + 1}

stepPerson : Env -> State -> Person -> Maybe Person
stepPerson =
  let stepToGoal e s p = case p.goal.kind of
    Drink ->
      let src   = nearestBeerSource e p.pos
          speed = anti p.turntocity + p.happiness + baseSpeed
          dir   = normed (src.pos #- p.pos)
      in
      if dist p.pos src.pos < src.radius
      then { p | turntocity <- min 1 (p.turntocity + 0.1) }
      else { p | pos <- p.pos #+ (speed #* dir) }
    TalkTo q -> p
  in
  \e s p ->
    if isOld p && isUnhappy p
    then Nothing
    else Just (incrAge <| stepToGoal e s <| updateGoal s p)

removeFromNetwork : PersonID -> Network -> Network
removeFromNetwork pid n =
  Dict.remove pid n |> Dict.map (\_ d -> Dict.remove pid d)

step : Env -> State -> State
step e s0 = 
  List.map (\p -> (p.idNum, stepPerson e s0 p)) (Dict.values s0.people)
  |> List.foldl (\mp s ->
      case mp of
        (idNum, Just p)  -> {s | people <- Dict.insert idNum p s.people}
        (idNum, Nothing) -> {s | people <- Dict.remove idNum s.people
                            , network <- removeFromNetwork idNum s.network})
      s0

env : Env
env =
  { dims = {w = 200, h = 200}
  , beerSources =
      [ {pos = {x=20, y=20}, radius = 3}
      , {pos = {x=-60, y=-70}, radius = 3}
      ]
  }

-- randomPerson : Random.Generator Person
randomPerson = Random.customGenerator <| \s ->
  let (predilection, s') = Random.generate (Random.float 0 1) s
      (w, h)             = (toFloat env.dims.w, toFloat env.dims.h)
      ((x,y), s'')       = Random.generate (Random.pair (Random.float (-w/2) (w/2)) (Random.float (-h/2) (h/2))) s'
  in
  ({turntocity   = 0
  , predilection = predilection
  , happiness    = 0
  , age          = 0
  , goal         = { kind = Drink, age = 0 }
  , pos          = {x = x, y = y}
  }
  , s'')

rmap f g = Random.customGenerator (\s -> let (x, s') = (Random.generate g s) in (f x, s'))

rreturn : a -> Random.Generator a
rreturn x = Random.customGenerator (\s -> (x, s))

randThen : Random.Generator a -> (a -> Random.Generator b) -> Random.Generator b
randThen g f = Random.customGenerator <| \seed ->
  let (a, seed') = Random.generate g seed
      gb = f a
  in Random.generate gb seed'

randomNetwork : Int -> Random.Generator Network
randomNetwork n =
  let enum = Dict.fromList << List.map2 (,) [0..(n-1)]
      randomPrefs = rmap enum (Random.list n (Random.float 0 1))
  in rmap enum (Random.list n randomPrefs)

initialState : State
initialState =
  let g : Random.Generator State
      g = Random.list 100 randomPerson `randThen` \ps ->
            randomNetwork 100 `randThen` \net ->
              rreturn {
                network = net,
                people = Dict.fromList <| List.indexedMap (\i p -> (i, {p | idNum = i})) ps
              }
  in
  fst <| Random.generate g (Random.initialSeed 0)

main : Signal Element
main = Signal.map (Draw.draw env) (Signal.foldp (\_ -> step env) initialState (Time.fps 30))

