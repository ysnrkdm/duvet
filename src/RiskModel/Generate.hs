module RiskModel.Generate (generateRiskData) where

import Control.Monad (foldM, forM)
import Data.Maybe (fromMaybe)
import Data.Time (Day)
import Numeric.LinearAlgebra
import RiskModel.CsvSave (saveMatrixWideCSV, saveVectorCSV)
import RiskModel.FetchData (fetchStooq)
import RiskModel.RiskModel
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Prelude hiding ((<>))

-- Industry labels (Toy: Simple Map)
industryOf :: String -> String
industryOf s
  | s `elem` ["AAPL", "MSFT", "NVDA", "GOOGL", "META"] = "Tech"
  | s `elem` ["AMZN"] = "ConsDiscr"
  | s `elem` ["BRK.B", "JPM", "GS"] = "Financials"
  | s `elem` ["LLY"] = "HealthCare"
  | s `elem` ["CASH"] = "Cash"
  | otherwise = "Other"

generateRiskData :: IO ()
generateRiskData = do
  let syms = ["AAPL", "MSFT", "NVDA", "AMZN", "GOOGL", "META", "BRK.B", "LLY", "GS", "JPM", "CASH"]
      bench = fromList [0.12, 0.12, 0.11, 0.10, 0.09, 0.09, 0.08, 0.08, 0.11, 0.10, 0.015] -- Sum=1.0
  rawNonCash <- forM (filter (/= "CASH") syms) $ \s -> do
    xs <- fetchStooq s
    pure (s, xs)

  let alignedNC = alignByDateStrict rawNonCash
      commonDays = map fst alignedNC

  let commonDaysLimited = take 30 commonDays
  let cashRows = cashFromDays commonDaysLimited

  let raw = rawNonCash ++ [("CASH", cashRows)]

  let aligned = alignByDateStrict raw
      rets = logReturns aligned -- [(Day, [(sym, r, close, vol)])]
  putStrLn $ "Symbols loaded: " ++ show (length syms)
  putStrLn $ "Date range: " ++ (show . fst . head) rets ++ " - " ++ (show . fst . last) rets
  putStrLn $ "Symbols: " ++ (show $ map fst raw)
  putStrLn $ "Days loaded: " ++ show (length rets)

  -- Init EWMA
  -- Update F, D by EWMA
  -- Since K can have different value each day due to industry factors, use K of B on that day
  let lambdaEW = 0.97
  -- Main loop: summarize only last several dozen business days
  let loop (fCovPrev, dPrevM) (d, rows') = do
        let sy = [s | (s, (_, _, _)) <- rows']
            n = length sy
            -- Benchmark weights: align with today's order (missing symbols get 0)
            w = fromList [fromMaybe 0 (lookup s (zip syms (toList bench))) | s <- sy]
            w' = if sumElements w > 0 then w else konst (1 / fromIntegral n) n

            -- Style factors (ADV, MOM)
            letHist = takeWhile (\(dd, _) -> dd <= d) rets
            adv = adv20 letHist d
            mom = mom12m1 letHist d

            -- Exposure matrix B, and column names
            (b, _colNames) = buildB sy w' adv mom industryOf

            -- Objective variable r
            r = fromList [r' | (_, (r', _, _)) <- rows']

            -- WLS
            f_t = factorReturnCS b r w'

            -- Residuals
            eps = r - (b #> f_t)
            eps2 = cmap (\x -> x * x) eps

            -- F（K×K）EWMA、D（N）EWMA
            k = cols b
        fCovPrev' <- case fCovPrev of
          Nothing -> pure (scale 1e-8 (ident k)) -- Small ridge to start
          Just fOld ->
            if rows fOld == k then pure fOld else pure (scale 1e-8 (ident k))
        let fCov = ewmaUpdateCov lambdaEW fCovPrev' f_t

        dPrev <- case dPrevM of
          Nothing -> pure (konst 1e-4 n) -- Initial value around 1bp^2 daily
          Just dP -> if size dP == n then pure dP else pure (konst 1e-4 n)
        let dEW = cmap (max 1e-8) (ewmaUpdateVarVec lambdaEW dPrev eps2)

            -- Σ
            sigma = b <> fCov <> tr b + diag dEW

            -- Reference: Annualized volatility of equal-weighted (demo)
            xEq = konst (1 / fromIntegral n) n
            volDaily = sqrt (xEq <.> (sigma #> xEq))
            volAnnual = volDaily * sqrt 252

        -- Identifier for the day (YYYY-MM-DD)
        let dayStr = show d
        let outDir = "out" </> dayStr
        createDirectoryIfMissing True outDir

        -- Names
        let symOrder = sy -- Ordered by today's symbol order（[String]）
        let facNames = _colNames -- Column names returned by buildB（["SIZE","MOM","IND:Tech",…]）

        -- Save F: K×K (Daily, not annualized)
        saveMatrixWideCSV outDir "F_daily" facNames facNames fCov

        -- Save D: N (Daily, not annualized)
        saveVectorCSV outDir "D_daily" symOrder dEW

        -- Save B
        saveMatrixWideCSV outDir "B" symOrder facNames b

        putStrLn $
          show d
            ++ "  N="
            ++ show n
            ++ "  vol_d="
            ++ show volDaily
            ++ "  vol_ann="
            ++ show volAnnual

        pure (Just fCov, Just dEW)

  -- Run for only a few dozen days
  _ <- foldM loop (Nothing, Nothing) rets
  putStrLn "Done."

cashFromDays :: [Day] -> [(Day, Double, Double)]
cashFromDays ds = [(d, 1.0, 0.0) | d <- ds]
