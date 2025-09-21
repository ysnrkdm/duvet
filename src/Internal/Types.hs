-- since this is Internal, expose everything
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Internal.Types where

import Numeric.LinearAlgebra (Matrix, Vector)

-- ==============
-- Definition of QP (OSQP)
-- min  (1/2) x' Q x + q' x  s.t.  l <= A x <= u
-- where Q = lambda sigma + gamma I
--       q = -(alpha + gamma xPrev)
--       sigma = B F B' + diag(d)
--       A, l, u: constraints
--       x: decision variable (portfolio weights)
--       B: factor loadings
--       F: factor covariance matrix
--       d: idiosyncratic risks (n-vector)
--       alpha: expected returns
--       xPrev: previous holdings (for transaction cost)
--       lambda: risk aversion
--       gamma: transaction cost weight
-- ==============

data QP = QP
  { qQ :: Matrix Double, -- Q n×n (PSD)
    qq :: Vector Double, -- q n
    qA :: Matrix Double, -- A m×n
    ql :: Vector Double, -- l m
    qu :: Vector Double -- u m
  }
  deriving (Show)
