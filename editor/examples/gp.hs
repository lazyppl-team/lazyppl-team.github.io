-- Generated from examples/models/GaussianProcessDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import Data.List (intercalate)
import LazyPPL
import LazyPPL.Distributions (normal, normalLogPdf)
import LazyPPL.Distributions.GP
import Control.Monad
import Numeric.Log (Log(Exp))

rbf var l x1 x2 = var * exp (-(x1 - x2)^2 / (2 * l^2))

dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

example :: Meas (Double -> Double)
example = do g <- sample $ gp $ rbf 1 1
             a <- sample $ normal 0 3
             b <- sample $ normal 0 3
             let f x = a + b * x + (g x)
             forM_ dataset (\(x,y) -> scoreLog $ Exp $ normalLogPdf (f x) 0.3 y)
             return f

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
  do { draws <- mh 0.2 example; curves dataset (map fst $ take 5 $ every 1000 $ drop 10000 draws) }
