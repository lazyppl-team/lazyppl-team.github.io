-- Generated from examples/models/ControlFlowDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)

import LazyPPL
import LazyPPL.SingleSite
import LazyPPL.Distributions
import Control.Monad

prior1 :: Prob (Bool,Bool)
prior1 =
  do x <- bernoulli 0.5
     y <- if x then bernoulli 0.4 else bernoulli 0.7
     return (x,y)

model :: Prob (Bool,Bool) -> Meas Bool
model prior =
  do (x,y) <- sample prior
     score (if x==y then 1 else 0)
     return x

prior2 :: Prob (Bool,Bool)
prior2 =
  do x <- bernoulli 0.5
     ytrue <- bernoulli 0.4
     yfalse <- bernoulli 0.7
     return (if x then (x,ytrue) else (x,yfalse))

-- Take a sampler (m)  returning reals, regarded as an estimator,
-- and calculate the mean and precision for the estimator.
-- k is the number of samples to use in the estimator.
-- n is the number of runs to average over to approximate the mean, variance and ess.
meanPrec :: IO [(Double,a)] -> Int -> Int -> IO (Double,Double)
meanPrec m n k =
  do xwss <- replicateM n m
     let as = map ((\x -> x / (fromIntegral k)). sum . map fst . take k) xwss
     let mean = (sum as) / (fromIntegral n) -- sample mean
     let var = (sum (map (\x -> (x - mean)^2) as)) / (fromIntegral n) -- sample variance
     return (mean,1/var)

softModel :: Prob (Bool,Bool) -> Meas Bool
softModel prior =
  do (x,y) <- sample prior
     score (if x==y then 0.9 else 0.1)
     return x

fromBool b = if b then 1.0 else 0.0

linreg :: Meas (Double,Double)
linreg =
  do
    a <- sample $ normal 0 3
    b <- sample $ normal 0 3
    let dataset = [(0,5), (0.1,10), (0.2,0)]
    mapM (\(x, y) -> score $ normalPdf (a*x + b) 0.5 y) dataset
    return (a,b)

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  a <- mh 0.3 (model prior1)
  b <- mh 0.33 (model prior2)
  print (fromIntegral (length (filter fst (take 10000 a))) / 10000 :: Double)
  print (fromIntegral (length (filter fst (take 10000 b))) / 10000 :: Double)
