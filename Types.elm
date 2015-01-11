module Types where

import Graph(..)
import Dict(..)

type alias Pos = {x : Float, y : Float}

type alias Dir = {x : Float, y : Float}

type alias PersonID = Int

type GoalKind
  = TalkTo Person
  | Drink

type alias Goal = { kind : GoalKind, age : Int }

type alias Person =
  { turntocity      : Float
  , predilection    : Float
  -- Turns out happiness is just a floating point number
  , happiness       : Float
  -- How many ticks they've been at the party for
  , age             : Int
  , socialSatiation : Dict Int Float
  , goal            : Goal
  , idNum           : PersonID
  , pos             : Pos
  }

type alias Network = Dict Int (Dict Int Float)

type alias BeerSource = {pos : Pos, radius : Float}

type alias Env =
  { dims        : {w : Int, h : Int}
  , beerSources : List BeerSource
  }

type alias State =
  { people    : Dict Int Person
  , network   : Network
  }

