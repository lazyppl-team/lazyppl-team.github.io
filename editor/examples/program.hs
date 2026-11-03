-- Generated from examples/models/ProgramInductionDemo.lhs at 859ee06149d0866068fd9e20e02e7f9efa7a4ccc.
-- Pass the playground Random seed as the program argument; the default is 42.
module Main where
import System.Environment (getArgs)
import System.Random (mkStdGen,setStdGen)
import Text.Read (readMaybe)
import Data.List (intercalate)
import LazyPPL
import LazyPPL.Distributions
import Control.Monad (replicateM,forM_)
import Numeric.Log (Log(Exp))

data Expr = Var | Constt Double | Add Expr Expr | Mult Expr Expr
          | IfLess Double Expr Expr

instance Show Expr where
  show Var = "x"
  show (Constt r)  = show $ (fromInteger $ round $ r * 10) / 10.0
  show (Add e1 e2) = "(" ++ show e1 ++ " + "++show e2++")"
  show (Mult e1 e2) = "(" ++ show e1 ++ " . "++show e2++")"
  show (IfLess r e1 e2) =
    "(if x<" ++ show (Constt r) ++ " then "++show e1 ++" else "++show e2++")"

eval :: Expr -> Double -> Double
eval Var x = x
eval (Constt r) _ = r
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mult e1 e2) x = (eval e1 x) * (eval e2 x)
eval (IfLess r e1 e2) x = if x < r then eval e1 x else eval e2 x

randexpralt :: Prob Expr
randexpralt = do
  i <- categorical [0.3,0.3,0.18,0.18,0.04]
  e <- [return Var ,
        do { n <- normal 0 5 ; return $ Constt n },
        do { e1 <- randexpralt ; e2 <- randexpralt ; return $ Add e1 e2},
        do { e1 <- randexpralt ; e2 <- randexpralt ; return $ Mult e1 e2},
        do { r <- normal 0 5 ; e1 <- randexpralt ; e2 <- randexpralt ; return $ IfLess r e1 e2}]
       !! i
  return e

randexpr :: Prob Expr
randexpr = do
  i <- categorical [0.3,0.3,0.18,0.18,0.04]
  es <- sequence
     [return Var ,
     do { n <- normal 0 5 ; return $ Constt n },
     do { e1 <- randexpr ; e2 <- randexpr ; return $ Add e1 e2},
     do { e1 <- randexpr ; e2 <- randexpr ; return $ Mult e1 e2},
     do { r <- normal 0 5 ; e1 <- randexpr ; e2 <- randexpr ; return $ IfLess r e1 e2}]
  return $ es !! i

randfun :: Prob (Double -> Double,String)
randfun = do
  e <- randexpr
  return (eval e,show e)

dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

regress :: Double -> Prob (a -> Double,String) -> [(a,Double)]
           -> Meas (a->Double,String)
regress sigma prior dataset =
  do (f,s) <- sample prior
     forM_ dataset $ \(x,y) -> scoreLog $ Exp $ normalLogPdf (f x) sigma y
     return (f,s)

-- Plot the points drawn
-- More graphing routines
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about
interestingPoints :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interestingPoints f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon
    then acc
    else interestingPoints f lower mid epsilon delta (mid : (interestingPoints f mid upper epsilon delta acc))

sampleFun f =
--  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]
  let xs = ((-0.25) : (interestingPoints f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs

seedFromArgs :: IO Int
seedFromArgs = do
  args <- getArgs
  case args of
    [] -> pure 42
    [value] | Just seed <- readMaybe value -> pure seed
    _ -> fail "Expected one integer seed"

induction :: Int -> IO ()
induction n = do
  draws <- mhirreducible 0.1 0.005 (regress 0.25 randfun dataset)
  let functions = map fst $ take n $ every 1000 $ drop 1000 draws
  forM_ dataset $ \(x,y) -> putStrLn $ intercalate "\t" ["observation",show x,show y]
  forM_ (zip [0::Int ..] functions) $ \(i,(f,expression)) -> do
    putStrLn $ intercalate "\t" ["expression",show i,expression]
    forM_ (sampleFun f) $ \(x,y) ->
      putStrLn $ intercalate "\t" [show i,show x,show y]

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  induction 1
