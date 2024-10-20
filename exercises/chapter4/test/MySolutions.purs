module Test.MySolutions where

import Prelude
import Data.Array (tail)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)

import Data.Picture
  ( Bounds
  , Picture
  , bounds
  , intersect
  )

import Partial.Unsafe (unsafePartial)

type Address = {street :: String, city :: String}

type Person = {name :: String, address :: Address}

type Point = {
    x :: Number,
    y :: Number
    }


data Shape = 
    Circle Point Number
    | Rectangle Point Number Number
    | Line Point Point
    | Text Point String


origin :: Point
origin = { x, y }
  where
    x = 0.0
    y = 0.0


factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

binomial :: Int -> Int -> Int
binomial 0 1 = 1
binomial 0 _ = 0
binomial n k | k > (n+1) = 0
             | otherwise = (factorial n)/((factorial k) * (factorial (n-k)))   

pascal :: Int -> Int -> Int
pascal 0 1 = 1
pascal 0 _ = 0
pascal _ 0 = 1
pascal n k | k > n = 0
           | otherwise = pascal (n-1) (k-1) + pascal (n-1) (k)         

sameCity :: Person -> Person -> Boolean
sameCity p1 p2 | p1.address.city == p2.address.city = true
               | otherwise = false


fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [b] = b
fromSingleton default _ = default



circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

getCenter :: Shape -> Point
getCenter (Circle c r) = c
getCenter (Rectangle c l w) = c 
getCenter (Line p1 p2) = (p1 + p2) * {x:0.5 , y:0.5}
getCenter (Text p s) = p

centerShape :: Shape -> Shape
centerShape (Circle c r) = Circle origin r
centerShape (Rectangle c l w) = Rectangle origin l w
centerShape (Line p1 p2) = Line (p1 - delta) (p2 - delta)
    where   
        delta = getCenter (Line p1 p2)
centerShape (Text p s) = Text origin s

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r*i)
scaleShape i (Rectangle c l w) = Rectangle c (l*i) (w*i)
scaleShape i (Line p1 p2) = Line (p1 * scale) (p2 * scale)
  where
    scale = {x: i, y: i}
scaleShape i (Text p s) = Text p s  


doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter shape = scaleShape 2.0 (centerShape shape)


showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"


showPoint :: Point -> String
showPoint { x, y } =
  "(" <> show x <> ", " <> show y <> ")"


derive instance Eq Shape

instance Show Shape where
  show shape = showShape shape
-- ANCHOR_END: showShape

newtype Watt = Watt Number
newtype Amp = Amp Number
newtype Volt = Volt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = (Watt $ a * v)


shapeText :: Shape -> Maybe String
shapeText (Text p s) = Just s
shapeText _ = Nothing






