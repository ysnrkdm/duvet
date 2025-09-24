{-# LANGUAGE OverloadedStrings #-}

module RiskModel.CsvSave where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Numeric.LinearAlgebra
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Save vector as "index,value" format
saveVectorCSV ::
  FilePath -> -- Output directory path
  String -> -- File name (without extension)
  [String] -> -- Row names (N elements)
  Vector Double -> -- Data (N)
  IO ()
saveVectorCSV dir base rowNames v = do
  let rows = zip rowNames (toList v) :: [(String, Double)]
      bs =
        Csv.encodeByName
          (Csv.header ["name", "value"])
          [ Csv.namedRecord
              [ "name" Csv..= nm,
                "value" Csv..= val
              ]
          | (nm, val) <- rows
          ]
  createDirectoryIfMissing True dir
  BL.writeFile (dir </> (base ++ ".csv")) bs

-- Save matrix as "1st column = row names, 1st row = column names" wide format
saveMatrixWideCSV ::
  FilePath -> -- Output directory path
  String -> -- File name (without extension)
  [String] -> -- Row names (R)
  [String] -> -- Column names (C)
  Matrix Double -> -- Matrix (R×C)
  IO ()
saveMatrixWideCSV dir base rnames cnames m = do
  let r = rows m
      c = cols m
      vals = toLists m -- [[Double]] Length r, Length for each c
      -- Top row: empty cell + column names
      headerRow = "" : cnames
      body = [rn : map show rowVals | (rn, rowVals) <- zip rnames vals]
      -- Rough CSV generation (not strictly using cassava)
      toCSV ls =
        BL.fromStrict . encodeUtf8 . T.intercalate "\n" $
          map (T.intercalate "," . map T.pack) ls
  -- ↑ If using Text/encodeUtf8, import Data.Text as T, Data.Text.Encoding
  createDirectoryIfMissing True dir
  BL.writeFile (dir </> (base ++ ".csv")) (toCSV (headerRow : body))
