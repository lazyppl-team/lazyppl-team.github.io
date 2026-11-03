-- Generated from examples/models/AdditiveClusteringDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)

import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.IBP
import LazyPPL.Distributions.Memoization

import Data.List
import Control.Monad
import Data.Monoid

import Data.Array
import Data.Maybe

data Country = China | Cuba | Germany | Indonesia | Iraq | Italy | Jamaica | Japan | Libya | Nigeria | Philippines | Russia | Spain | US | Vietnam | Zimbabwe deriving (Eq, Ord, Show, Enum, Bounded, Ix)

countries :: [Country]
countries = [China .. Zimbabwe]

similarityData :: Array (Country,Country) Double

similarityData = array ((minBound,minBound),(maxBound,maxBound)) [((i,j),if i > j then (countriesDataset !! (fromEnum i) !! (fromEnum j)) else if i == j then 1 else (countriesDataset !! (fromEnum j) !! (fromEnum i))) | i <- countries , j <- countries]
 where
 countriesDataset :: [[Double]] =
  [[ ],
  [0.11],
  [0.06, 0.04],
  [0.43, 0.06, 0.03],
  [0.06, 0.32, 0.04, 0.14],
  [0.02, 0.09, 0.70, 0.02, 0.03],
  [0.02, 0.59, 0.02, 0.14, 0.04, 0.10],
  [0.69, 0.01, 0.26, 0.35, 0.03, 0.06, 0.03],
  [0.03, 0.32, 0.01, 0.04, 0.70, 0.04, 0.11, 0.01],
  [0.01, 0.12, 0.01, 0.04, 0.20, 0.03, 0.31, 0.01, 0.45],
  [0.42, 0.12, 0.01, 0.87, 0.09, 0.02, 0.17, 0.31, 0.05, 0.04],
  [0.51, 0.35, 0.55, 0.01, 0.13, 0.22, 0.02, 0.17, 0.05, 0.02, 0.03],
  [0.02, 0.37, 0.58, 0.03, 0.04, 0.90, 0.20, 0.04, 0.04, 0.03, 0.04, 0.15],
  [0.30, 0.11, 0.42, 0.03, 0.06, 0.20, 0.12, 0.46, 0.02, 0.04, 0.01, 0.43, 0.20],
  [0.60, 0.12, 0.03, 0.55, 0.12, 0.01, 0.05, 0.45, 0.10, 0.03, 0.57, 0.08, 0.02, 0.12],
  [0.01, 0.08, 0.01, 0.11, 0.15, 0.02, 0.29, 0.01, 0.31, 0.83, 0.08, 0.01, 0.02, 0.01, 0.03]]

additiveClustering :: Double -> Double -> Double
                   -> Array (Country,Country) Double -> Meas (Array Country [Dish])

additiveClustering alpha lambda1 lambda2 similarityData = do
    (restaurant :: Restaurant) <- sample $ newRestaurant alpha

    (weights :: Dish -> Double) <- sample $ memoize (\d -> gamma lambda1 lambda2)

    (features :: Array Country [Dish])
           <- listArray (minBound,maxBound) <$>
              (replicateM (fromEnum (maxBound :: Country) + 1) $
              sample $ newCustomer restaurant)

    let similarity :: Country -> Country -> Double
        similarity i j = sum [weights a | a <- features!j, a `elem` features!i]

    mapM_ (\(i, j) -> score $ normalPdf (similarityData!(i,j)) 0.1 (similarity i j)) [ (i, j) | i <- countries, j <- countries, j < i ]

    return features

-- The sample with the highest weight, that is, the highest likelihood
maxap samples =
        let maxw = (maximum $ map snd samples) in
        fromJust $ Data.List.lookup maxw $ map (\(z,w) -> (w,z)) samples

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

features :: Int -> IO ()
features n = do
  draws <- mhirreducible 0.2 0.01 (additiveClustering 2 6 0.3 similarityData)
  let assignment = maxap (take n draws)
      dishes = nub (concat $ Data.Array.elems assignment)
  forM_ (zip [0::Int ..] countries) $ \(i,country) -> do
    putStrLn $ intercalate "\t" ["label",show i,show country]
    putStrLn $ intercalate "\t" (show i : [if dish `elem` (assignment Data.Array.! country) then "1" else "0" | dish <- dishes])

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  features 10000
