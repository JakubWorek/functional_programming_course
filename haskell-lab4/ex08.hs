module Stack
  ( Stack
  , empty   -- :: Stack a
  , isEmpty -- :: Stack a -> Bool
  , push    -- :: a -> Stack a -> Stack a
  , top     -- :: Stack a -> a
  , pop     -- :: Stack a -> (a,Stack a)
  ) where

-- interface (signature, contract)
empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)

-- implementation
newtype Stack a = MkStack [a] deriving Show -- hidden constructor (see the module export list)

empty = MkStack []
isEmpty (MkStack s) = null s
push x (MkStack s) = MkStack (x:s)
top (MkStack s) = head s
pop (MkStack (s:ss)) = (s,MkStack ss)


--------------
module Queue
  ( Queue
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  ) where

-- interface (signature, contract)
emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a,Queue a)

-- implementation
newtype Queue a = MkQueue [a] deriving Show -- hidden constructor (see the module export list)

emptyQ = MkQueue []
isEmptyQ (MkQueue q) = null q
addQ x (MkQueue q) = MkQueue (q ++ [x])
remQ (MkQueue (q:qs)) = (q, MkQueue qs)

--------------

module Dequeue
 ( Dequeue
 , emptyDEQ     -- :: Dequeue a
 , isEmptyDEQ   -- :: Dequeue a -> Bool
 , lengthDEQ    -- :: Dequeue a -> Int, O(1)
 , firstDEQ     -- :: Dequeue a -> Maybe a,  O(1)
 , lastDEQ      -- :: Dequeue a -> Maybe a, O(1)
 , takeFrontDEQ -- :: Int -> Dequeue a -> [a], O(n)
 , takeBackDEQ  -- :: Int -> Dequeue a -> [a], O(n)
 , pushFrontDEQ -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popFrontDEQ  -- :: Dequeue a -> Maybe (a, Dequeue a), O(1) amortised
 , pushBackDEQ  -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popBackDEQ   -- :: Dequeue a -> Maybe (a, q a), O(1) amortised
 , fromListDEQ  -- :: [a] -> Dequeue a, O(n)
 ) where

-- interface (signature, contract)
emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a

-- implementation
newtype Dequeue a = MkDequeue [a] deriving Show -- hidden constructor (see the module export list)

emptyDEQ = MkDequeue []

isEmptyDEQ (MkDequeue q) = null q

lengthDEQ (MkDequeue q) = length q

firstDEQ (MkDequeue []) = Nothing
firstDEQ (MkDequeue (x:xs)) = Just x

lastDEQ (MkDequeue []) = Nothing
lastDEQ (MkDequeue q) = Just (last q)

takeFrontDEQ n (MkDequeue q) = take n q
takeBackDEQ n (MkDequeue q) = reverse (take n (reverse q))

pushFrontDEQ (MkDequeue q) x = MkDequeue (x:q)

popFrontDEQ (MkDequeue []) = Nothing
popFrontDEQ (MkDequeue (x:xs)) = Just (x, MkDequeue xs)

pushBackDEQ (MkDequeue q) x = MkDequeue (q ++ [x])

popBackDEQ (MkDequeue []) = Nothing
popBackDEQ (MkDequeue q) = Just (last q, MkDequeue (init q))

fromListDEQ xs = MkDequeue xs
