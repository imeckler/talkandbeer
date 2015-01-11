module Main where

import Html
import Html.Events(onClick)
import Debug
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
import Graphics.Element (Element, above)
import Graphics.Collage (..)
import Color

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

-- Sort of a differential equations model
utility : Env -> State -> Person -> Goal -> (Float, Dir)
utility e s p g =
  case g.kind of
    TalkTo q ->
      let r = s.network ! p.idNum ! q.idNum
          sat = p.socialSatiation ! q.idNum
      in
      ( socialConstant * abs r / (max 0.01 (dist q.pos p.pos)) - 1000 * sat -- WTF are there clusters
      , sign r #* (q.pos #- p.pos)
      )
      -- ( socialConstant * (s.network ! p.idNum ! q.idNum) / (dist q.pos p.pos)^2, q.pos)
    Drink ->
      let src    = nearestBeerSource e p.pos
          d      = dist src.pos p.pos - src.radius
          desire = (p.predilection - p.turntocity)
      in 
      if | d > 0      -> (desire / d, src.pos #- p.pos)
         | otherwise  -> (desire, {x=0,y=0})
      {-
    Avoid q ->
      (-socialConstant * (s.network ! p.idNum ! q.idNum) / (dist q.pos p.pos)^2, q.pos)
      -}

-- I iterate many times over the pairs of people when doing an update for
-- modularity. Consider merging into one pass if necessary.

diff : Env -> State -> Person -> Pos
diff =
  let epsilon = 0.001 in
  \e s p ->
    let
      talkUtility =
        Dict.foldl (\_ q (u,x) ->
          let (u',x') = utility e s p {kind=TalkTo q, age=0}
          in
          if | q.idNum == p.idNum -> (u, x)
             | u' > u             -> (u', x')
             | otherwise          -> (u, x))
          (0, {x=0,y=0}) s.people
      others = List.map (utility e s p) [{kind=Drink, age=0}]
      dstDir = List.foldl (maxOn fst) talkUtility others |> snd
    in
    epsilon #* normed dstDir

clampPerson : Env -> Person -> Person
clampPerson e p =
  let hw = toFloat (e.dims.w) / 2
      hh = toFloat (e.dims.h) / 2
  in
  {p | pos <- {x=clamp -hw hw p.pos.x, y=clamp -hh hh p.pos.y}}

{-
updateGoal : State -> Person -> Person
updateGoal s p = if p.goal.age < 1000 then p else if
  | p.turntocity - p.predilection > 0.5 -> {p | goal <- newGoal Drink}
  | True -> 
    let m               = s.network ! p.idNum
        friendishness q = m ! q.idNum
    in {p | goal <- newGoal (TalkTo (maximumOn friendishness (closePeople s p.pos)).idNum)}
-}
incrAge p = {p | age <- p.age + 1}

stepPerson : Env -> State -> Person -> Maybe Person
stepPerson =
  let stepToGoal e s p = {p | pos <- p.pos #+ diff e s p}
      areClose p q = dist p.pos q.pos < 8
      satiate s p =
        let sat =
          List.filter (areClose p) (Dict.values s.people)
          |> List.foldl (\q sat ->
               Dict.update q.idNum (Just << ((+) 0.1) << Maybe.withDefault 0) sat)
               p.socialSatiation
        in
        {p | socialSatiation <- sat}
      turntocify e p =
        if List.any (\src -> dist src.pos p.pos < src.radius) e.beerSources
        then {p | turntocity <- min 1 (p.turntocity + 0.1)}
        else p
  in
  \e s p ->
    if isOld p && isUnhappy p
    then Nothing
    else Just (incrAge <| satiate s <| turntocify e <| stepToGoal e s p)
  {-
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
-}

removeFromNetwork : PersonID -> Network -> Network
removeFromNetwork pid n =
  Dict.remove pid n |> Dict.map (\_ d -> Dict.remove pid d)

socialConstant : Float
socialConstant = 1

step : Env -> State -> State
step e s0 = 
  List.map (\p -> (p.idNum, stepPerson e s0 p)) (Dict.values s0.people)
  |> List.foldl (\mp s ->
      case mp of
        (idNum, Just p)  -> {s | people <- Dict.insert idNum p s.people}
        (idNum, Nothing) -> {s | people <- Dict.remove idNum s.people
                            , network <- removeFromNetwork idNum s.network})
      s0

-- randomPerson : Random.Generator Person

initialPopulation = 100

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
  , socialSatiation = Dict.fromList <| List.map (\i -> (i, 0)) [0..initialPopulation - 1]
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

randomNetwork : Random.Generator Network
randomNetwork =
  let enum = Dict.fromList << List.map2 (,) [0..(initialPopulation-1)]
      randomPrefs = rmap enum (Random.list initialPopulation (Random.float -1 1))
  in rmap enum (Random.list initialPopulation randomPrefs)

randomState : Random.Generator State
randomState = Random.list initialPopulation randomPerson `randThen` \ps ->
  randomNetwork `randThen` \net ->
    rreturn {
      network = net,
      people = Dict.fromList <| List.indexedMap (\i p -> (i, {p | idNum = i})) ps
    }

env : Env
env =
  { dims = {w = 800, h = 500}
  , beerSources =
      [ {pos = {x=20, y=20}, radius = 3}
      , {pos = {x=-60, y=-70}, radius = 3}
      ]
  }

type Update
  = Tick Time.Time
  | ResetState
  | NoOp

updateChan : Signal.Channel Update
updateChan = Signal.channel NoOp

scene s = 
  let reset = 
        Html.button [ onClick (Signal.send updateChan ResetState) ]
          [ Html.text "Reset" ]
  in
  Draw.draw env s `above` Html.toElement 100 30 reset

update : Update -> (Random.Seed, State) -> (Random.Seed, State)
update u (se, s) = case u of
  Tick t     -> (Random.initialSeed (floor t), step env s)
  ResetState -> (se, fst <| Random.generate randomState se)
  NoOp       -> (se, s)

main : Signal Element
main =
  let ticks        = Signal.map Tick (Time.fps 30)
      updates      = Signal.merge ticks (Signal.subscribe updateChan)
      se0          = Random.initialSeed 16
      initialState = fst <| Random.generate randomState se0
  in
  Signal.foldp update (se0, initialState) updates
  |> Signal.map (\(_, s) -> scene s)

