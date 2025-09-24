{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Lib
  ( runDemo,
    SolverOptions (..),
  )
where

import Control.Monad (when)
import Data.List (zip4)
import Internal.ADMMSolver (solveADMM)
import Internal.Options (Opt (..), defaultOpt)
import Internal.Types (QP (..))
import Internal.Utils
import Numeric.LinearAlgebra
import RiskModel.CsvStore
import System.FilePath ((</>))
import Text.Printf (printf)
import Prelude hiding ((<>))

data SolverOptions = SolverOptions
  { analysisDate :: String,
    annualized :: Bool
  }
  deriving (Show)

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
trackingError :: Matrix Double -> Vector Double -> Vector Double -> Bool -> Double
trackingError sigma x wBmk annualized = if annualized then tevAnnual else tevDaily
  where
    delta = x - wBmk
    tev = quadForm sigma delta
    deltaTEV = max 0 tev -- avoid negative due to numerical error
    tevDaily = sqrt deltaTEV
    tevAnnual = tevDaily * sqrt 252

runDemo :: SolverOptions -> IO ()
runDemo SolverOptions {analysisDate, annualized} = do
  putStrLn $ "Solving QP for analysis date: " ++ show analysisDate
  let dir = "out" </> analysisDate

  putStrLn $ "Loading F,B,D from " ++ dir
  loadedB <- loadMatrixWideCSV dir "B"
  loadedF <- loadMatrixWideCSV dir "F_daily"
  loadedD <- loadVectorCSV dir "D_daily"

  let (_syms, b, f, d) = validateFBD loadedB loadedF loadedD

  let n = length sp11Benchmark -- number of assets
      lambda = 0.5 -- risk aversion (common and specific)
      gamma = 5e-12 -- transaction cost coefficient

  -- -- More realistic B, F, d
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

  let tev = trackingError sigma xSol wBmk annualized
  printf "Tracking Error (%s) = %.8f\n" (if annualized then "TEV Annualized" else "TEV Daily") tev

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

wBench :: Vector Double
wBench = fromList $ map snd sp11Benchmark

validateFBD ::
  ([String], [String], Matrix Double) -> -- (rowNamesB, colNamesB, B)
  ([String], [String], Matrix Double) -> -- (rowNamesF, colNamesF, F)
  ([String], Vector Double) -> -- (namesD, D)
  ( [String], -- symbols
    Matrix Double, -- B
    Matrix Double, -- F
    Vector Double -- D
  )
validateFBD (rowsB, colsB, b) (_rowsF, colsF, f) (namesD, d) = do
  -- Verify dimensions
  let n = rows b
      k = cols b
  when (cols f /= k || rows f /= k) $
    error $
      "F size mismatch: expected " ++ show k ++ "x" ++ show k
  when (size d /= n) $
    error $
      "D size mismatch: expected " ++ show n
  -- Verify factor label alignment (it's okay if the order matches even if the column names differ)
  when (colsB /= colsF) $
    error $
      "[warn] factor names differ: B=" ++ show colsB ++ " F=" ++ show colsF
  -- Verify symbol name alignment (it's safer to error out here if they differ)
  when (rowsB /= namesD) $
    error $
      "symbol order mismatch between B and D"
  (rowsB, b, f, d)
