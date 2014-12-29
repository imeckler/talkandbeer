module Utils where

import Dict
import List
import Types(..)

maxOn : (a -> comparable) -> a -> a -> a
maxOn p x y = if p x > p y then x else y

maximumOn : (a -> comparable) -> List a -> a
maximumOn p = List.foldl1 (\x m -> if p x > p m then x else m)

minimumOn : (a -> comparable) -> List a -> a
minimumOn p = List.foldl1 (\x m -> if p x < p m then x else m)

norm : Pos -> Float
norm p = sqrt <| p.x^2 + p.y^2

(#*) : Float -> Pos -> Pos
(#*) s {x, y} = {x = s * x, y = s * y}

(#+) : Pos -> Pos -> Pos
(#+) p1 p2 = { x = p1.x + p2.x, y = p1.y + p2.y}

(#-) : Pos -> Pos -> Pos
(#-) p1 p2 = { x = p1.x - p2.x, y = p1.y - p2.y}

dist : Pos -> Pos -> Float
dist p1 p2 = norm (p1 #- p2)

normed : {x : Float, y : Float} -> {x : Float, y : Float}
normed v = let m = norm v in if m == 0 then {x=0,y=0} else m #* v

(!) : Dict.Dict comparable v -> comparable -> v
(!) d k = case Dict.get k d of Just x -> x
infixl 9 !

sign : number -> number
sign x = if | x < 0     -> -1
            | x == 0    -> 0
            | otherwise -> 1

