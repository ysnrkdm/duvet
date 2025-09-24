{-# LANGUAGE RecordWildCards #-}
-- since this is Internal, expose everything
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Internal.ADMMSolver where

import Data.Maybe (fromJust)
import Internal.Options (Opt (..))
import Internal.Types (QP (..))
import Numeric.LinearAlgebra (Vector, asColumn, flatten, ident, konst, linearSolve, norm_2, norm_Frob, scale, size, tr, (#>), (<>))
import Numeric.LinearAlgebra.Devel (zipVectorWith)
import Prelude hiding ((<>))

-- l2 norm
norm2 :: Vector Double -> Double
norm2 = norm_2

minVec, maxVec :: Vector Double -> Vector Double -> Vector Double
minVec = zipVectorWith min
maxVec = zipVectorWith max

-- clip v to [l, u], i.e., clip(l, u, v) = min(u, max(l, v))
clip :: Vector Double -> Vector Double -> Vector Double -> Vector Double
clip l u v = minVec u (maxVec l v)

-- compute the tolerance for stopping criteria
relTol :: Double -> Double -> Double -> Double
relTol epsAbs epsRel sc = epsAbs + epsRel * sc

-- ADMM solver for QP
-- returns (x, iters, primal residual norm, dual residual norm)
-- min (1/2) x' Q x + q' x  s.t.  l <= A x <= u
solveADMM :: Opt -> QP -> Vector Double -> (Vector Double, Int, Double, Double)
solveADMM Opt {..} QP {..} x0 =
  loop maxIt x0 z0 y0
  where
    (m, n) = size qA
    at = tr qA -- n×m
    -- KKT matrix (nxn)
    -- K = Q + ρ A' A
    k = qQ + scale rho (tr qA <> qA)
    -- regularization for numerical stability
    -- K_reg = K + 1e-10 I, i.e., K = Q + ρ A' A + 1e-10 I
    kReg = k + scale 1e-10 (ident n)
    z0 = konst 0 m -- initial z
    y0 = konst 0 m -- initial y
    aScale = max 1 (norm_Frob qA) -- scaling factor for A
    -- pre-factorization & solver for KKT system
    solveK :: Vector Double -> Vector Double
    solveK b = flatten $ fromJust $ linearSolve kReg (asColumn b)
    -- main ADMM loop
    loop 0 x z y = (x, maxIt, 1 / 0, 1 / 0) -- maxIt reached
    loop it x z y =
      if nPri <= epsP && nDual <= epsD
        then (x', maxIt - it + 1, nPri, nDual) -- converged
        else loop (it - 1) x' z' y'
      where
        rhs = scale (-1) qq + scale rho (at #> (z - y))
        x' = solveK rhs -- x-update
        aAx = qA #> x'
        z' = clip ql qu (aAx + y) -- z-update
        y' = y + aAx - z' -- y-update
        rPri = aAx - z' -- primal residual
        rDual = scale rho (at #> (z' - z)) -- dual residual
        nPri = norm2 rPri
        nDual = norm2 rDual
        epsP = relTol epsAbs epsRel (max (norm2 aAx) (norm2 z'))
        epsD = relTol epsAbs epsRel (aScale * norm2 (y'))
