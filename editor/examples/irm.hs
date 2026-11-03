-- Generated from examples/models/IrmDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
 
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import Control.Monad (forM_)
 
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.DirichletP
import LazyPPL.Distributions.Memoization
import Data.List

data Person = Tom | Fred | Jim | Mary | Sue | Ann deriving (Show , Eq, Ord, Enum, Bounded)
instance MonadMemo Prob Person

model :: Meas (Person -> Table)
model = do

  r :: Restaurant <- sample $ newRestaurant 1.0

  table :: (Person -> Table) <- sample $ memoize $ \person -> newCustomer r

  near :: ((Table, Table) -> Double) <- sample $ memoize $ \(tableA, tableB) -> beta 0.5 0.5

  let talks :: (Person, Person) -> Meas () = \(personA, personB) ->
                                            score $ near (table personA, table personB)
  let nottalks :: (Person, Person) -> Meas () = \(personA, personB) ->
                                              score $ 1 - near (table personA, table personB)

  mapM_ talks [(Tom, Fred), (Tom, Jim), (Jim, Fred), (Mary, Sue), (Mary, Ann), (Ann, Sue)]
  mapM_ nottalks [(Mary, Fred), (Mary, Jim), (Sue, Fred), (Sue, Tom), (Ann, Jim), (Ann, Tom)]

  return table

-- The sample with the highest weight, that is, the highest likelihood, from a list of weighted samples
maxap xws =
    let maxw = (maximum $ map snd xws) in
    let (Just x) = Data.List.lookup maxw $
                                    map (\(z, w) -> (w, z)) xws in
   x

tableToDot :: (Person -> Table) -> String
tableToDot f = "graph tables {" ++ concat [ show a ++ " -- " ++ show b ++ "; " | a <- people , b <- people , a < b, f a == f b] ++ "}"
 where dotLine a b True = show a ++ " -- " ++ show b ++ "\n"
       people = [minBound..maxBound]
instance Show (Person -> Table) where show f = tableToDot f

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

relational :: Int -> IO ()
relational n = do
  draws <- take n <$> mh 0.2 model
  let people = [minBound..maxBound :: Person]
  forM_ (zip [0::Int ..] people) $ \(i,a) -> do
    putStrLn $ intercalate "\t" ["label",show i,show a]
    let probabilities = [fromIntegral (length [() | (table,_) <- draws,table a==table b])/fromIntegral n :: Double | b <- people]
    putStrLn $ intercalate "\t" (show i : map show probabilities)

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  relational 1000
