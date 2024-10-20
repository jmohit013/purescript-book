module Test.MySolutions where

import Prelude
import Control.Alternative (guard)
import Data.Array (concat,length, cons, filter, head, length, null, tail, (..), (:))
import Data.Foldable (foldl, product)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path, filename, isDirectory, ls, size)
import Data.Tuple (Tuple(..))



isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (mod n 2)


countEven :: Array Int -> Int
countEven [] = 0
countEven arr | isEven (fromMaybe 1 (head arr)) = 1 + (countEven (fromMaybe [] (tail arr)))
              | otherwise = 0 + (countEven (fromMaybe [] (tail arr)))


squared :: Array Number -> Array Number
squared [] = [] 
squared arr = map (\n -> n*n) arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative [] = []
keepNonNegative arr = filter (\n -> n >= 0.0) arr


infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite [] = []
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr


factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) do
    i <- 1 .. n
    j <- i .. n
    pure [i,j]

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime 1 = false
isPrime n | length (factors n) == 1 = true
          | otherwise = false

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arrx arry = do
    i <- arrx
    j <- arry
    pure [i,j]

triples :: Int -> Array (Array Int)
triples n = do
    a <- 1 .. n
    b <- a .. n
    c <- b .. n
    guard $ a*a + b*b == c*c
    [[a,b,c]]


primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
  factorize :: Int -> Int -> Array Int
  factorize _ 1 = []
  factorize divisor dividend =
    if dividend `mod` divisor == 0 then
      cons divisor $ factorize (divisor) (dividend / divisor)
    else
      factorize (divisor + 1) dividend


allTrue :: Array Boolean -> Boolean
allTrue [] = false
allTrue arr = foldl (\acc n -> acc && n) true arr

fibTailRec :: Int -> Int
fibTailRec n = fib 2 n 0 1
    where
        fib :: Int -> Int -> Int -> Int -> Int
        fib acc 0 a b = 0
        fib acc 1 a b = 1
        fib acc n a b | acc == n = a + b
                      | otherwise = fib (acc+1) n b (a+b)


reverse :: forall a. Array a -> Array a
reverse [] = [] 
reverse arr = foldl (\acc x -> [x] <> acc ) [] arr

onlyFiles :: Path -> Array Path
onlyFiles path = path : addFiles path []
    where 
        addFiles :: Path -> Array Path
        addFiles path acc = do    
            child <- ls path
            if isDirectory child == true then
                addFiles child acc
            else 
                cons child acc 
