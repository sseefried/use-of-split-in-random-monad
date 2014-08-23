module UseOfSplit where

import Control.Monad.Random hiding (split)

{-

I wrote this small example to track down a problem I was having using the Rand monad
available in module Control.Monad.Random from the MonadRandom package on hackage.

Essentially the Rand monad is a state monad which threads through a "seed" to generate
each successive random number.

I ran into trouble when I defined a function to get an infinite list of random object. (The type
signature has been simplified for ease of reading.) I naively defined randObjects as

    randObject :: Rand Object

    randObjects :: Rand [Object]
    randObjects = sequence . repeat $ randObject

Now let's assume there is a function evalRand which takes an initial seed and then evaluates
the Rand monad. (A real function which does a similar thing is evalRandIO.)

I found that an expression such as

    > take 100 $ evalRand <seed> randObjects

would yield 100 objects just fine.

However, I ran into trouble when I tried to create an infinite list of pairs of random objects.

    zippedObjects :: Rand [(Object, Object)]
    zippedObjects = do
      os  <- randObjects
      os' <- randObjects
      return $ zip os os'

    > take 100 $ evalRand <seed> zippedObjects

went into an infinite loop almost immediately, only yielding the following in the terminal

   [(<random object>,

It seems obvious in retrospect why this happened, but I needed to write out the example below
to make it clear to me. Now, let me run through the reasoning behind why it happens.

First, I define a simple state monad. I then define a function nextFibo that given a state
representing the two predecessor integers will yield the next Fibonacci number by summing them
together. It will also update (setState) the state so that a subsequence call to nextFibo
will add up the next two previous numbers.

getSequence is analogous to evalRand. It takes a state monad and an initial state and then
evaluates it. However, it does not require the "final" state to be evaluated so one can
happily take a finite number of elements from an infinite sequence. e.g.

    > take 10 $ getSequence fibo (1,1)
    [2,3,5,8,13,21,34,55,89,144]

However

    > take 10 $ getSequence loopForever

does not work at all.

It's almost too obvious why this is the case. If you follow the logic of the state monad,
fs' requires a "seed" (i.e. the two previous numbers stored in the state) to begin
computing results. This can never be generated becase fs is infinitely long.

So, this begs the question, why does doesNotLoopForever not loop forever when
getRandomRs plainly returns an infinitel list of random numbers?

The secret lies in "split"ting the seed.

-}

-- My own hand rolled state monad.
data State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return a = State $ \s -> (a,s)
  State f >>= k = State $ \s ->
                    let (a,s') = f s
                        (State f') = k a
                    in f' s'

getState :: State s s
getState = State $ \s -> (s,s)

setState :: s -> State s ()
setState s = State $ const ((),s)

nextFibo :: State (Integer, Integer) Integer
nextFibo = do
  (a,b) <- getState
  let next = a + b
  setState (b, next)
  return next

fibo :: State (Integer, Integer) [Integer]
fibo = sequence . repeat $ nextFibo

getSequence :: State s [a] -> s -> [a]
getSequence m s = let (rs, _) = runState m s in rs

noLoop  = getSequence fibo (1,1)

loopForever :: State (Integer, Integer) [(Integer, Integer)]
loopForever = do
  fs  <- fibo
  fs' <- fibo
  return $ zip fs fs'

randDoesNotLoopForever :: RandomGen g => Rand g [(Int,Int)]
randDoesNotLoopForever = do
  xs <- getRandomRs (0, 10)
  ys <- getRandomRs (0, 10)
  return $ zip xs ys

--
-- get current state, run [m] with that state but then set the current state back to that.
--
split :: State s a -> State s a
split m = do
  s <- getState
  a <- m
  setState s
  return a

doesNotLoopForever :: State (Integer, Integer) [(Integer, Integer)]
doesNotLoopForever = do
  fs  <- split fibo
  fs' <- split fibo
  return $ zip fs fs'
