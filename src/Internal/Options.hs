{-# LANGUAGE NumericUnderscores #-}
-- since this is Internal, expose everything
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Internal.Options where

data Opt = Opt
  { rho :: Double, -- ADMM ρ
    maxIt :: Int,
    epsAbs :: Double,
    epsRel :: Double
  }
  deriving (Show)

defaultOpt :: Opt
defaultOpt = Opt {rho = 1.0, maxIt = 5_000, epsAbs = 1e-6, epsRel = 1e-4}
