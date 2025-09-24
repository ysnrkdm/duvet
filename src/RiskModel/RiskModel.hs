module RiskModel.RiskModel
  ( alignByDate,
    alignByDateStrict,
    logReturns,
    adv20,
    mom12m1,
    buildB,
    factorReturnCS,
    ewmaUpdateCov,
    ewmaUpdateVarVec,
  )
where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Time (Day)
import Data.Vector ()
import Numeric.LinearAlgebra
import Prelude hiding ((<>))

-- winsorize: median ± k * MAD
winsorMAD :: Double -> Vector Double -> Vector Double
winsorMAD k x =
  let xs = toList x
      ys = sort xs
      n = length ys
      med = if odd n then ys !! (n `div` 2) else (ys !! (n `div` 2 - 1) + ys !! (n `div` 2)) / 2
      mad = medianAbsDev med xs
      lo = med - k * mad
      hi = med + k * mad
   in cmap (\v -> max lo (min hi v)) x
  where
    medianAbsDev m vs =
      let zs = sort (map (abs . (subtract m)) vs)
          n' = length zs
       in if odd n'
            then zs !! (n' `div` 2)
            else (zs !! (n' `div` 2 - 1) + zs !! (n' `div` 2)) / 2

-- Weighted z-score
zscoreW :: Vector Double -> Vector Double -> Vector Double
zscoreW w x =
  let w' = w / scalar (sumElements w + 1e-16)
      mu = w' <.> x
      xc = x - scalar mu
      sd = sqrt $ w' <.> (xc * xc) + 1e-16
   in xc / scalar sd

alignByDateStrict ::
  [(String, [(Day, Double, Double)])] ->
  [(Day, [(String, (Double, Double))])]
alignByDateStrict xs =
  let perSym = [(s, M.fromList [(d, (c, v)) | (d, c, v) <- rows']) | (s, rows') <- xs]
      common :: S.Set Day
      common =
        foldl1
          S.intersection
          [S.fromList (M.keys m) | (_, m) <- perSym, not (null (M.keys m))]
      days = reverse (S.toList common) -- Remove reverse if you want new→old order
   in [ (d, [(s, M.findWithDefault (0, 0) d m) | (s, m) <- perSym])
      | d <- days
      ]

alignByDate :: [(String, [(Day, Double, Double)])] -> [(Day, [(String, (Double, Double))])]
alignByDate xs =
  let _syms = [s | (s, _) <- xs]
      byS = xs
      allDays = foldr (\(_, rows') acc -> nub (map (\(d, _, _) -> d) rows') ++ acc) [] byS
      days = reverse $ sort allDays
   in [ ( d,
          [ (s, (c, v))
          | (s, rows') <- byS,
            Just (c, v) <- [lookupDay d rows']
          ]
        )
      | d <- days
      ]
  where
    lookupDay d rows' = case filter (\(dd, _, _) -> dd == d) rows' of
      ((_, c, v) : _) -> Just (c, v)
      _ -> Nothing

logReturns ::
  [(Day, [(String, (Double, Double))])] ->
  [(Day, [(String, (Double, Double, Double))])] -- date, [(sym, r, close, vol)]
logReturns aligned =
  let prev = tail aligned
      curr = init aligned
   in [ let d = d2
            pairs =
              [ (s, (log (c2 / c1), c2, v2))
              | (s, (c1, _)) <- xs1,
                (s2, (c2, v2)) <- xs2,
                s2 == s,
                c1 > 0,
                c2 > 0
              ]
         in (d, pairs)
      | ((d1, xs1), (d2, xs2)) <- zip curr prev
      ]

-- ========= Construct industry-neutral factor exposure B (Size=log ADV, Mom=12-1, Industry dummy) =========

-- Average daily trading volume (20-day)
adv20 :: [(Day, [(String, (Double, Double, Double))])] -> Day -> [(String, Double)] -- (sym, adv)
adv20 hist d =
  let last20 = takeWhile (\(dd, _) -> dd <= d) hist
      win20 = take 20 last20
      -- (price≈close, vol)
      sums = foldr accumulate mempty win20
   in [ (s, (pSum / max 1 n))
      | (s, (pSum, n)) <- sums
      ]
  where
    accumulate (_, rows') m =
      foldr (\(s, (_, p, v)) acc -> add' s (p * v) acc) m rows'
    add' k x m = case lookup k m of
      Nothing -> (k, (x, 1)) : m
      Just (y, cnt) -> (k, (x + y, cnt + 1)) : filter ((/= k) . fst) m

-- 12-1 momentum(Accum in the past 252 days - 21 days)
mom12m1 :: [(Day, [(String, (Double, Double, Double))])] -> Day -> [(String, Double)]
mom12m1 hist d =
  let window n = take n (takeWhile (\(dd, _) -> dd <= d) hist)
      win252 = window 252
      win21 = window 21
      retMul rows' = product [exp r | (_, r, _, _) <- rows'] -- exp(logret)=gross
      m252 = foldr accum' mempty win252
      m21 = foldr accum' mempty win21
   in [ (s, log (max 1e-12 (g252 / max 1e-12 g21)))
      | (s, g252) <- m252,
        let g21 = fromMaybe 1 (lookup s m21)
      ]
  where
    accum' (_, rows') m = foldr (\(s, (r, _, _)) acc -> add s r acc) m rows'
    add k r m = case lookup k m of
      Nothing -> (k, exp r) : m
      Just g -> (k, g * exp r) : filter ((/= k) . fst) m

oneHot :: [String] -> [String] -> Matrix Double
oneHot labels xs =
  let n = length xs
      k = length labels
   in fromColumns
        [ fromList [if x == lab then 1 else 0 | x <- xs]
        | lab <- labels
        ]

buildB ::
  [String] -> -- symbols in this cross-section (ordered)
  Vector Double -> -- weights for standardization (bench or cap)
  [(String, Double)] -> -- adv20
  [(String, Double)] -> -- mom12-1
  (String -> String) -> -- industryOf
  (Matrix Double, [String]) -- B (N×K), column names
buildB syms w adv mom industryOf =
  let n = length syms
      -- Size proxy = log(ADV)
      sizeRaw = fromList [log (fromMaybe 1 (lookup s adv)) | s <- syms]
      sizeCol = zscoreW w (winsorMAD 3 sizeRaw)

      -- Momentum proxy
      momRaw = fromList [fromMaybe 0 (lookup s mom) | s <- syms]
      momCol = zscoreW w (winsorMAD 3 momRaw)

      -- Industry dummies
      inds = [industryOf s | s <- syms]
      ilabs = nub inds
      indMat = oneHot ilabs inds

      b = fromColumns ([sizeCol, momCol] ++ toColumns indMat)
      names = ["SIZE", "MOM"] ++ map ("IND:" ++) ilabs
   in (b, names)

-- ========= Estimate factor returns f_t and residuals ε by WLS (1 day) ========= ========

factorReturnCS :: Matrix Double -> Vector Double -> Vector Double -> Vector Double
factorReturnCS b r w =
  let wdiag = diag w
      lhs = tr b <> wdiag <> b
      rhs = tr b #> (wdiag #> r)
      kreg = lhs + scale 1e-8 (ident (cols b))
      sol = fromMaybe (error "linearSolve failed") (linearSolve kreg (asColumn rhs))
   in flatten sol

-- ========= EWMA update of F, D =========

ewmaUpdateCov :: Double -> Matrix Double -> Vector Double -> Matrix Double
ewmaUpdateCov lambda fCovPrev f_t =
  let ft = asColumn f_t
      inst = ft <> tr ft -- K×K outer
   in scale lambda fCovPrev + scale (1 - lambda) inst

ewmaUpdateVarVec :: Double -> Vector Double -> Vector Double -> Vector Double
ewmaUpdateVarVec lambda dPrev eps_t2 =
  scale lambda dPrev + scale (1 - lambda) eps_t2
