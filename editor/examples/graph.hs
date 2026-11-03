-- Generated from examples/models/GraphDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.Memoization
import Control.Monad
import Data.List
import Data.Maybe
import Numeric.Log (Log(Exp))

instance MonadMemo Prob Double where
  memoize = generalmemoize

test :: [[Bool]]
test = map (map $ \x -> x==1)
 [[0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0]]

class Show v => RandomGraph p v | p -> v where
  new :: p -> Prob v
  edge :: p -> v -> v -> Bool

data SphGrph = SG Int Double -- Graph parameters with dimension and distance
data SphVert = SV [Double]   -- Vertices are coordinates

instance Show SphVert where
  show (SV (x:y:xs)) = show x ++ "," ++ show (SV (y:xs))
  show (SV (x:[])) = show x

instance RandomGraph SphGrph SphVert where
  new (SG d theta) =
   do
     -- sample d-dimensional isotropic normal vectors
     xs <- replicateM d $ normal 0 1
     -- compute the length
     let r = sqrt $ sum $ map (^ 2) xs
     -- normalize, to get something on the sphere
     return $ SV $ map (/ r) xs
  edge (SG d theta) (SV xs) (SV ys) =
   theta > acos (max (-1) (min 1 (sum (zipWith (*) xs ys))))

data Graphon = Graphon (Double -> Double -> Double)

data GraphonVertex = GV Double (Double->Bool)

instance Show GraphonVertex where
  show (GV x _) = show x

instance RandomGraph Graphon GraphonVertex where
  new (Graphon f) = do
    x <- uniform
    p <- memoize $ \y -> bernoulli $ sqrt $ f x y
    return $ GV x p
  edge _ (GV x p) (GV y q) = p y && q x

erdosRenyi r = Graphon (\_ _ -> r)

isTriangle :: RandomGraph p v => p -> Prob Bool
isTriangle p = do
           x <- new p
           y <- new p
           z <- new p
           return (edge p x y && edge p y z && edge p x z)

ngraph :: RandomGraph p v => Int -> p -> Prob ([[Bool]],String)
ngraph n p = do vs <- replicateM n $ new p
                let matrix = [[edge p v w | w <- vs] | v <- vs]
                let csv = concat [show v ++ "\n" | v <- vs]
                return (matrix,csv)

like :: Double -> [[Bool]] -> [[Bool]] -> Meas Int
like r a b = do
   let matches = [a!!i!!j == b!!i!!j | i <- [0..length a-1] , j <- [0..(i-1)]]
   scoreLog $ Exp $ sum [bernoulliLogPmf r m | m <- matches]
   return $ length $ filter id matches

superuniformbounded n a b = do
   xs <- replicateM n uniform
   let x = sum xs
   return $ (x - fromIntegral (floor x :: Int)) * (b - a) + a

exampleA = do
     -- theta is uniformly distributed in [0,pi]
     theta <- sample (superuniformbounded 3 0 pi)
     -- We are looking at the 3d geometric graph
     let p = SG 3 theta
     -- Sample a 15 vertex graph x from this p
     (x,csv) <- sample (ngraph 15 p)
     -- Observe that the test graph is very similar to x
     n <- like 0.999 test x
     return ((n,theta),(x,csv))

instance (RandomGraph a v,RandomGraph b w)
  => RandomGraph (Either a b) (Either v w) where
  new (Left a) = do {x <- new a ; return $ Left x}
  new (Right b) = do {x <- new b ; return $ Right x}
  edge (Left a) (Left x) (Left y) = edge a x y
  edge (Right b) (Right x) (Right y) = edge b x y
  edge _ _ _ = False

exampleB = do
     -- Prior on the type of graph
     b <- sample $ bernoulli 0.5
     let p = if b then Left (erdosRenyi 0.5) else Right (SG 2 (pi / 2))
     -- Randomly sample two vertices and ask whether there is an edge
     c <- sample $ do {a <- new p ; b <- new p ; return $ edge p a b}
     -- Observation that there is in fact an edge
     if c then score 1 else score 0
     return b

exampleC = do
     -- Prior on the type of graph
     b <- sample $ bernoulli 0.5
     let p = if b then Left (erdosRenyi 0.5) else Right (SG 2 (pi / 2))
     -- Randomly sample three vertices and ask whether they form a triangle
     c <- sample $ isTriangle p
     -- Observation that there is a triangle
     if c then score 1 else score 0
     return b

graphToDot :: [[Bool]] -> String
graphToDot g = let vs = [0..length g -1] in
               "graph {node[shape=point] \n edge[len=0.5] \n " ++
               concat [show v ++ "\n" | v <- vs] ++
               concat [show v ++ " -- " ++ show w ++ "\n" | v <- vs , w <- [0..v] , g !! v !! w]
               ++ "}"

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

graph :: Int -> IO ()
graph n = do
  draws <- mh 0.1 exampleA
  case drop (n-1) draws of
    (((matches,theta),(adjacency,_)),_):_ -> do
      putStrLn $ intercalate "\t" ["graph_stat",show matches,show theta]
      forM_ (zip [0::Int ..] adjacency) $ \(i,row) ->
        putStrLn $ intercalate "\t" (show i : [if edge then "1" else "0" | edge <- row])
    [] -> fail "Graph inference returned no samples"

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  graph 5000
