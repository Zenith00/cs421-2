--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake a [] = []
mytake a (x:xs) | a <= 0 = []
                | otherwise = (x: (mytake (a-1) xs))

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop a [] = []
mydrop a (x:xs) | a <= 0 = (x:xs)
                | otherwise = mydrop (a-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = tmp [] (x:xs)
   where tmp a [] = a
         tmp b (c:cs) = tmp (c:b) cs

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] [] = []
app (a:as) [] = (a:as)
app [] (b:bs) = (b:bs)
app (x:xs) (y:ys) = x : app xs (y : ys)


--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a] 
inclist [] = []
inclist (x:xs) = (x+1 : inclist xs)

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x: xs)= x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip (a : as) [] = []
myzip [] (b : bs) = []
myzip (x : xs) (y : ys)= (x, y) : myzip xs ys 

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] [] = []
addpairs (a : as) [] = []
addpairs [] (b : bs) = []
addpairs (x : xs) (y : ys) = pairsum (myzip (x : xs) (y : ys))
          where pairsum :: (Num a) => [(a, a)] -> [a]
                pairsum [] = []
                pairsum ((a, b) : as) = (a+b) : pairsum as
                

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = (1 : ones)

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0, 1 ..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = rec_fib (0, 1)
 where rec_fib (a, b) = a : rec_fib (b, a+b)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add x (y:ys) | x < y = x : (add y ys)
             | x > y = y : (add x ys)
             | otherwise = y : ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union (a:as) [] = a : as
union [] (b:bs) = b : bs
union (x:xs) (y:ys)   | x < y = x : union xs (y : ys)
                      | x > y = y : union (x : xs) ys
                      | otherwise = x : (union xs ys)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect (a:as) [] = []
intersect [] (b:bs) = []
intersect (x:xs) (y:ys)   | x < y = intersect xs (y:ys)
                          | x > y = intersect (x:xs) ys
                          | otherwise = x : (intersect xs ys)

--- ### powerset

-- don't forget to put the type declaration or you will lose points!

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (distribute x (powerset xs)) (powerset xs)
    where   distribute :: Ord a => a -> [[a]] -> [[a]]
            distribute a [] = []
            distribute x (y:ys) = (add x y) : distribute x ys


--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' [] = []
inclist' (x:xs) = P.map (+1) (x:xs)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' [] = 0
sumlist' (x:xs) = P.foldl (+) x xs 
