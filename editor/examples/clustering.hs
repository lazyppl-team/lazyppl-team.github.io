-- Generated from examples/models/ClusteringDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import Control.Monad
import Data.List
import Data.Monoid
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.DirichletP
import LazyPPL.Distributions.Memoization
import LazyPPL.SingleSite
import Numeric.Log

-- In Ghci you will need to :set -fobject-code

dataset = [(7.7936387, 7.469271), (5.3105156, 7.891521), (5.4320135, 5.135559), (7.3844196, 7.478719), (6.7382938, 7.476735), (0.6663453, 4.460257), (3.2001898, 2.653919), (2.1231227, 3.758051), (3.3734472, 2.420528), (0.4699408, 1.835277)]

cluster :: [a] -> Prob b -> (b -> a -> Log Double) -> Meas [(a, Double, b)]
cluster xs pparam like =
  do
    -- sample a distribution from a Dirichlet process
    rest <- sample $ newRestaurant 0.3
    -- lazily sample an infinite list of cluster parameters
    param <- sample $ memoize $ const pparam
    -- lazily sample an infinite list of colors
    color <- sample $ memoize $ const $ uniformbounded 0.2 1
    -- for each data point, sample a cluster and return
    --   the corresponding color and parameter
    forM xs
      ( \x -> do
          i <- sample $ newCustomer rest
          scoreLog $ like (param i) x
          return (x, color i, param i)
      )

example :: Meas [((Double, Double), Double, (Double, Double, Double))]
example =
  cluster
    dataset
    (do x <- normal 5 4; y <- normal 5 4; prec <- gamma 2 4;
                                        return (x, y, 1 / sqrt prec))
    (\(x, y, s) (x', y') -> Exp (normalLogPdf x s x' + normalLogPdf y s y'))

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

clustering :: Int -> IO ()
clustering n = do
  draws <- take n <$> mh1 example
  let bestWeight = maximum (map snd draws)
  case [points | (points,weight) <- draws,weight==bestWeight] of
    points:_ -> forM_ (zip [0::Int ..] points) $ \(i,((x,y),c,(mx,my,s))) ->
      putStrLn $ intercalate "\t" (show i : map show [x,y,c,mx,my,s])
    [] -> fail "Clustering inference returned no samples"

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  clustering 1000
