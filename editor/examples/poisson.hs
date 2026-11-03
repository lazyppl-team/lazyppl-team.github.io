-- Generated from examples/models/MinimalDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import LazyPPL
import LazyPPL.Distributions
import Data.List

poll :: [Bool]
poll = replicate 51 True ++ replicate 49 False

-- Likelihood function of Bernoulli distribution.
bernoulliPdf r x = if x then r else (1-r)

voteModel :: Meas (Bool)
voteModel = do
  -- prior belief: vote share is unknown, so uniform
  voteShare <- sample uniform
  -- incorporate each poll result as an observation, using bernoulli likelihood
  mapM_ (\actualVote-> score (bernoulliPdf voteShare actualVote)) poll
  -- I win if my vote share is more than 50%.
  return $ voteShare > 0.5

runVoteModel =
  do
    xws <- mh 1 voteModel
    let n=10000 
    let m = length $ filter id $ map fst $ take n $ xws
    putStrLn $ "Chance of winning = " ++ (show (100 * (fromIntegral m) / (fromIntegral n) )) ++ "%"

poissonPP :: Double -> Double -> Prob [Double]
poissonPP start rate = do
  step <- exponential rate
  let next = start + step
  rest <- poissonPP next rate 
  return $ next : rest

poissonSim :: Double -> Prob Int
poissonSim rate = do
  xs <- poissonPP 0 rate
  return $ length $ takeWhile (<1) xs

runPoissonSim =
  do
    xws <- mh 1 $ sample $ poissonSim 1
    let n=10000
    let xs = sort $ map fst $ take n $ xws
    -- histogram generator
    let categories = nub xs
    let counts = map (\c -> length $ filter (==c) xs) categories
    putStrLn $ "k\tpoisson(k)"
    putStrLn $ "--\t----------"
    mapM_ (\(c,k) -> putStrLn $ (show c) ++ "\t" ++ (show $ (fromIntegral k)/(fromIntegral n))) (zip categories counts)

demoMain :: IO ()
demoMain = do {runVoteModel ; putStrLn "" ; runPoissonSim }

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

-- The playground also shows the sampled point-process trace.
main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  runPoissonSim
