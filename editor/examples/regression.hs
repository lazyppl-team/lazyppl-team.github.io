-- Generated from examples/models/RegressionDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen,splitGen,StdGen)
import Text.Read (readMaybe)
import Data.List (intercalate)
import LazyPPL
import LazyPPL.Distributions
import Control.Monad
import Numeric.Log (Log(Exp))

linear :: Prob (Double -> Double)
linear =
  do
    a <- normal 0 3
    b <- normal 0 3
    let f = \x -> a * x + b
    return f

dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

regress :: Double -> Prob (a -> Double) -> [(a, Double)] -> Meas (a -> Double)
regress sigma prior dataset =
  do
    f <- sample prior
    forM_ dataset (\(x, y) -> scoreLog $ Exp $ normalLogPdf (f x) sigma y)
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

randConst :: Prob (Double -> Double)
randConst =
  do
    b <- normal 0 3
    let f = \x -> b
    return f

-- Plot the points drawn from weighted samples
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

regression :: Double -> StdGen -> IO ()
regression sigma predictiveGen = do
  samples <- mh 0.5 (regress sigma linear dataset)
  let fs = map fst $ take 1000 $ every 50 $ drop 100 samples
      grid = [fromIntegral i / 5 | i <- [0..30 :: Int]]
      predictions = runProb (mapM (\f -> mapM (\x -> normal (f x) sigma) grid) fs) (randomTree predictiveGen)
  forM_ dataset $ \(x,y) -> putStrLn $ "observation\t" ++ show x ++ "\t" ++ show y
  putStrLn "draw\tf(0)\tf(6)\tpredictive values at x=0,0.2,...,6"
  forM_ (zip3 [0..999 :: Int] fs predictions) $ \(i,f,ys) ->
    putStrLn $ intercalate "\t" (show i : map show (f 0 : f 6 : ys))

main :: IO ()
main = do
  seed <- seedFromArgs
  let (inferenceGen, predictiveGen) = splitGen (mkStdGen seed)
  setStdGen inferenceGen
  regression 0.5 predictiveGen
