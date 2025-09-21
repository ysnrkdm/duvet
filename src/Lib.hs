{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Lib
  ( runDemo,
  )
where

import Data.List (zip4)
import Debug.Trace (trace)
import Internal.ADMMSolver (solveADMM)
import Internal.Options (Opt (..), defaultOpt)
import Internal.Types (QP (..))
import Internal.Utils
import Numeric.LinearAlgebra
import Text.Printf (printf)
import Prelude hiding ((<>))

-- Generate sigma & Q,q like Barra
-- sigma = B F B' + diag(d)
-- Q = lambda sigma + gamma I
-- q = -(alpha + gamma xPrev)
sigmaBarra :: Matrix Double -> Matrix Double -> Vector Double -> Matrix Double
sigmaBarra b f d = b <> f <> tr b + diag d

-- Active risk model
-- sigma = B F B' + diag(d)
-- Q = lambda sigma + gamma I
-- q = -((sigma wBmk) * lambda + gamma xPrev)
qBarraActiveRisk :: Double -> Double -> Matrix Double -> Vector Double -> Vector Double -> (Matrix Double, Vector Double)
qBarraActiveRisk lambda gamma sigma wBmk xPrev =
  ( scale lambda sigma + scale gamma (ident (rows sigma)),
    scale (-1) (scale lambda (sigma #> wBmk) + scale gamma xPrev)
  )

sumEqOne :: Int -> (Matrix Double, Vector Double, Vector Double)
sumEqOne n = (a, l, u)
  where
    a = (1 >< n) (replicate n 1.0) -- 1×n
    l = fromList [1.0] -- 1
    u = fromList [1.0] -- 1

boxBounds :: Vector Double -> Vector Double -> (Matrix Double, Vector Double, Vector Double)
boxBounds lB uB = (ident n, lB, uB)
  where
    n = size lB

-- v' M v (quadratic form)
quadForm :: Matrix Double -> Vector Double -> Double
quadForm m v = (v <.> (m #> v))

-- Tracking Error
trackingError :: Matrix Double -> Vector Double -> Vector Double -> Double
trackingError sigma x wBmk = sqrt deltaTEV
  where
    delta = x - wBmk
    tev = quadForm sigma delta
    deltaTEV = max 0 tev -- avoid negative due to numerical error

runDemo :: IO ()
runDemo = do
  putStrLn "Running demo..."
  let n = length sp11Benchmark -- number of assets
      seed = 42
      lambda = 0.05 -- risk aversion (common and specific)
      gamma = 5e-12 -- transaction cost coefficient

  -- -- More realistic B, F, d
  let b = mkExposure seed -- factor loadings (n×k)
      f = factorConvDaily -- factor covariance (k×k)
      d = mkSpecificVarDaily b -- specific risks (n-vector)
  putStrLn $ "B shape: " ++ sprintShape b
  putStrLn $ "F shape: " ++ sprintShape f
  putStrLn $ "d shape: " ++ show (size d)

  -- Generate Q, q
  let sigma = sigmaBarra b f d -- covariance matrix
      xPrev = vjoinV [(konst 0.0 (n - 1)), (konst 1.0 1)] -- previous holdings (all cash)
      wBmk = wBench -- benchmark weights
      (bigQ, smallq) = qBarraActiveRisk lambda gamma sigma wBmk xPrev

  -- box constraints
  -- CASH: max 1.5%, min 0%
  -- Others: max 20%, min 0%
  let uB = fromList (replicate (n - 1) 0.2 ++ [0.015]) -- upper bound
      lB = fromList (replicate n 0.0) -- lower bound

  -- constraints: sum(x) = 1, 0 <= x <= 0.2
  let (aEq, lEq, uEq) = sumEqOne n
      (aBox, lBox, uBox) = boxBounds lB uB

  let qp =
        QP
          { qQ = bigQ,
            qq = smallq,
            qA = vstackM [aEq, aBox],
            ql = vjoinV [lEq, lBox],
            qu = vjoinV [uEq, uBox]
          }

  let opt = defaultOpt {rho = 1.0, maxIt = 20_000, epsAbs = 1e-6, epsRel = 1e-4}

  let x0 = xPrev -- initial guess (warm start)
  let (xSol, iters, priRes, dualRes) = solveADMM opt qp x0
  let sumx = sumElements xSol
      minx = minElement xSol
      maxx = maxElement xSol

  putStrLn $ "iters=" ++ show iters ++ "  r_pri=" ++ show priRes ++ "  r_dual=" ++ show dualRes
  putStrLn $ "sum(x)=" ++ show sumx ++ "  min(x)=" ++ show minx ++ "  max(x)=" ++ show maxx
  putStrLn "x* ="
  disp 3 (asColumn xSol)

  let tev = trackingError sigma xSol wBmk
  printf "Tracking Error (TEV) = %.8f\n" tev

  mapM_
    ( \(nm, wb, wx, xp) ->
        printf "%8s  Bmk: %.4f, weight: %.6f, diff: %.6f, <- prev: %.6f\n" nm wb wx (wx - wb) xp
    )
    (zip4 sp11Names (toList wBench) (toList xSol) (toList xPrev))

  putStrLn ""
  putStrLn "Done."

sp11Benchmark :: [(String, Double)]
sp11Benchmark =
  [ -- Large Caps
    ("AAPL", 0.12),
    ("MSFT", 0.12),
    ("NVDA", 0.11),
    ("AMZN", 0.10),
    ("GOOGL", 0.09),
    ("META", 0.09),
    -- Mid to Small Caps
    ("BRK.B", 0.07),
    ("LLY", 0.07),
    ("GS", 0.11),
    ("JPM", 0.10),
    -- Cash
    ("CASH", 0.015)
  ]

sp11Names :: [String]
sp11Names = map fst sp11Benchmark

sp11NamesNoCash :: [String]
sp11NamesNoCash = init sp11Names

wBench :: Vector Double
wBench = fromList $ map snd sp11Benchmark

wBenchNoCash :: Vector Double
wBenchNoCash = fromList $ map snd (init sp11Benchmark)

-- z-score normalization
zscore :: Vector Double -> Vector Double
zscore v = scale (1 / s) (v - konst m (size v))
  where
    m = mean v
    s = std v
    mean u = sumElements u / fromIntegral (size u)
    std u = sqrt (sumElements ((u - konst (mean u) (size u)) ** 2) / fromIntegral (size u))

-- Proxies for size factor
-- Large cap = heavy on the benchmark -> negative SMB
sizeProxy :: Vector Double
sizeProxy = cmap (* (-1)) wBenchNoCash

-- Proxies for value factor
-- Value = high B/P (low P/B) = low price (high book) -> negative HML
valueProxy :: Vector Double
valueProxy = fromList [0.2, 0.1, -0.1, -0.2, 0.0, -0.1, 0.15, -0.25, -0.05, 0.05]

-- Add small Gaussian noise for reality
noiseV :: Int -> Int -> Vector Double
noiseV seed n = scale 0.05 (randnVec seed n)

mkExposure :: Int -> Matrix Double
mkExposure seed =
  trace ("size of B: " ++ sprintShape bStock) $
    vstackM [bStock, bCash]
  where
    n = length sp11NamesNoCash
    mkt = konst 1.0 n + noiseV (seed + 1) n -- market factor
    smb = zscore (sizeProxy + noiseV (seed + 2) n) -- size factor
    hml = zscore (valueProxy + noiseV (seed + 3) n) -- value factor
    bStock = fromColumns [mkt, smb, hml]
    bCash = (1 >< 3) [0.0, 0.0, 0.0] -- cash has no factor exposure

factorConvAnnual :: Matrix Double
factorConvAnnual = sd <> corrs <> sd
  where
    sd = diag vols -- standard deviations
    vols = fromList [0.20, 0.10, 0.08] -- annualized volatilities
    corrs =
      (3 >< 3)
        [ 1.0,
          0.2,
          -0.1,
          0.2,
          1.0,
          -0.2,
          -0.1,
          -0.2,
          1.0 -- correlation matrix
        ]

factorConvDaily :: Matrix Double
factorConvDaily = scale (1 / sqrt 252) factorConvAnnual

targetVolAnnual :: Vector Double
targetVolAnnual = fromList [0.30, 0.28, 0.35, 0.32, 0.27, 0.33, 0.25, 0.30, 0.29, 0.26]

mkSpecificVarDaily :: Matrix Double -> Vector Double
mkSpecificVarDaily b = vjoin [idioDaily, fromList [1e-12]] -- add tiny specific var for cash
  where
    n = rows b - 1
    rowsB = toRows b
    facVarA = fromList [quadForm factorConvAnnual (rowsB !! i) | i <- [0 .. n - 1]]
    targVarA = targetVolAnnual ** 2
    idioVarA = cmap (max (0.05 ** 2)) (targVarA - facVarA) -- ensure minimum specific variance > 0.05^2
    idioDaily = scale (1 / sqrt 252) idioVarA
