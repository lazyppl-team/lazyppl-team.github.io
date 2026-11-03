-- Generated from examples/models/WienerDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.GP (wiener)

import Data.List
import Data.Map (empty,lookup,insert,size,keys)
import Control.Monad
import Numeric.Log (Log(Exp))
import Data.IORef
import System.IO.Unsafe

dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

example :: Meas (Double -> Double)
example = do g <- sample wiener
             a <- sample $ normal 0 3
             let f x = a + 2 * (g x)
             forM_ dataset (\(x,y) -> scoreLog $ Exp $ normalLogPdf (f x) 0.3 y)
             return f

splice :: Prob [Double] -> Prob (Double -> Double) -> Prob (Double -> Double)
splice pointProcess randomFun =
  do
    xs <- pointProcess
    fs <- mapM (const randomFun) xs
    default_f <- randomFun
    let h :: [(Double, Double -> Double)] -> Double -> Double
        h [] x = default_f x
        h ((a, f) : xfs) x | x <= a = f x
        h ((a, f) : xfs) x | x > a = h xfs x
    return (h (zip xs fs))

poissonPP :: Double -> Double -> Prob [Double]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)

regress :: Double -> Prob (a -> Double) -> [(a, Double)] -> Meas (a -> Double)
regress sigma prior dataset =
  do
    f <- sample prior
    forM_ dataset (\(x, y) -> scoreLog $ Exp $ normalLogPdf (f x) sigma y)
    return f

jump :: Prob (Double -> Double)
jump = let p = do f <- wiener
                  a <- normal 0 3
                  return $ \x -> a + f x
       in splice (poissonPP 0 0.2) p

datasetB :: [(Double, Double)]
datasetB = [(0,0.6), (1, 0.7), (2,8.2), (3,9.1), (4,3.2), (5,4.9), (6,2.9)]

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

curves :: [(Double,Double)] -> [Double -> Double] -> IO ()
curves observations fs = do
  let grid = [fromIntegral i/5 | i <- [0..30::Int]]
  forM_ observations $ \(x,y) -> putStrLn $ intercalate "\t" ["observation",show x,show y]
  forM_ (zip [0::Int ..] fs) $ \(i,f) ->
    putStrLn $ intercalate "\t" (show i : map (show . f) grid)

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  do { draws <- mh 0.1 example; curves dataset (map fst $ take 20 $ every 1000 $ drop 10000 draws) }
