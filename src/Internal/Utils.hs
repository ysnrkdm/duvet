module Internal.Utils
  ( vstackM,
    vjoinV,
    sym',
    randnVec,
    takeV,
    dropV,
    sprintShape,
  )
where

import Numeric.LinearAlgebra

vstackM :: [Matrix Double] -> Matrix Double
vstackM = fromRows . concatMap toRows

vjoinV :: [Vector Double] -> Vector Double
vjoinV = vjoin

-- Demo
sym' :: Matrix Double -> Matrix Double
sym' m = (m + tr m) / 2

-- Generate uniform random numbers in N(0,1) using Park-Miller LCG
randUniformVec :: Int -> Int -> Vector Double
randUniformVec seed n = fromList (go n (max 1 seed) [])
  where
    go 0 _ acc = reverse acc
    go k s acc = go (k - 1) s' (u : acc)
      where
        m = 2147483647
        a = 48271
        s' = (a * s) `mod` m
        u = fromIntegral s' / fromIntegral m

-- Box-Muller transform to generate N(0,1)
randnVec :: Int -> Int -> Vector Double
randnVec seed n = subVector 0 n z
  where
    n2 = n + (n `mod` 2) -- make n even
    u = randUniformVec seed n2
    u1 = cmap (\x -> max 1e-12 x) (subVector 0 (n2 `div` 2) u)
    u2 = subVector (n2 `div` 2) (n2 `div` 2) u
    r = cmap (sqrt . (* (-2)) . log) u1
    t = scale (2 * pi) u2
    z0 = r * cmap cos t
    z1 = r * cmap sin t
    z = vjoin [z0, z1]

takeV :: Int -> Vector Double -> Vector Double
takeV n v = subVector 0 n v

dropV :: Int -> Vector Double -> Vector Double
dropV n v = subVector n (size v - n) v

sprintShape :: Matrix Double -> String
sprintShape m = show (rows m) ++ "×" ++ show (cols m)
