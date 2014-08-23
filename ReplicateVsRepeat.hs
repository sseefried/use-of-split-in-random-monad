module ReplicateVsRepeat where

import Control.Monad.Random

data D = D Int Float deriving Show

getRandomD :: RandomGen g => Rand g D
getRandomD = do
  i <- getRandomR (0,10)
  f <- getRandomR (0,1)
  return $ D i f

getRandomDs :: RandomGen g => Rand g [D]
getRandomDs = sequence . repeat $ getRandomD

getRandomDPairs :: RandomGen g => Rand g [(D,D)]
getRandomDPairs = do
  ds  <- getRandomDs
  ds' <- getRandomDs
  return $ zip ds ds'

productiveLoop = evalRandIO $ getRandomDs
nonproductiveLoop = evalRandIO $ getRandomDPairs

productiveAgain = do
  ds  <- evalRandIO $ getRandomDs
  ds' <- evalRandIO $ getRandomDs
  return $ zip ds ds'

productiveTheRightWay :: RandomGen g => Rand g [(D,D)]
productiveTheRightWay = do
  g <- getSplit
  let ds = evalRand getRandomDs g
  g' <- getSplit
  let ds' = evalRand getRandomDs g'
  return $ zip ds ds'