module Signatures.Queue (
    Queue (..),
    drop,
    filter,
    fmapQ,
    foldQ,
    insert,
    insertFront,
    pop,
    tail,
    take
) where

import Prelude hiding (head, tail, take, drop, filter)
import qualified Data.List as List

data Queue a = Queue {
    front :: [a],
    back :: [a]
} deriving Show

instance Functor Queue where
    fmap = fmapQ

instance Foldable Queue where
    foldr = foldQ

insert :: a -> Queue a -> Queue a
insert e queue = queue {back = e : back queue}

head :: Queue a -> a
head (Queue f b) | null f = List.head $ reverse b
                 | otherwise = List.head f

tail :: Queue a -> Queue a
tail queue | null $ front queue = queue {front = List.tail . reverse $ back queue, back = []}
           | otherwise = queue {front = List.tail $ front queue}

pop :: Queue a -> (a, Queue a)
pop queue | null $ front queue = let newQueue = normalize queue in (head newQueue, tail newQueue)
          | otherwise = (head queue, tail queue)

fmapQ :: (a -> b) -> Queue a -> Queue b
fmapQ fun queue = queue {front = List.map fun $ front queue, back = List.map fun $ back queue}

foldQ :: (a -> b -> b) -> b -> Queue a -> b
foldQ fun st (Queue f b) = List.foldr fun st (f ++ reverse b)

take :: Queue a -> Int -> Queue a
take (Queue f b) n = Queue (List.take n (f ++ reverse b)) []

drop :: Queue a -> Int -> Queue a
drop (Queue f b) n = Queue (List.drop n (f ++ reverse b)) []

normalize :: Queue a -> Queue a
normalize (Queue f b) = Queue (f ++ reverse b) []

filter :: (a -> Bool) -> Queue a -> Queue a
filter fun queue = queue {front = List.filter fun $ front queue, back = List.filter fun $ back queue}

insertFront :: a -> Queue a -> Queue a
insertFront e queue = queue {front = e : front queue}