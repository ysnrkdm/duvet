{-# LANGUAGE OverloadedStrings #-}

module RiskModel.CsvStore
  ( saveVectorCSV,
    saveMatrixWideCSV,
    loadVectorCSV,
    loadMatrixWideCSV,
  )
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
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
  let rows' = zip rowNames (toList v) :: [(String, Double)]
      bs =
        Csv.encodeByName
          (Csv.header ["name", "value"])
          [ Csv.namedRecord
              [ "name" Csv..= nm,
                "value" Csv..= val
              ]
          | (nm, val) <- rows'
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
  let vals = toLists m -- [[Double]] Length r, Length for each c
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

loadVectorCSV :: FilePath -> String -> IO ([String], Vector Double)
loadVectorCSV dir base = do
  bs <- BL.readFile (dir </> (base ++ ".csv"))
  case Csv.decodeByName bs of
    Left err -> error $ "loadVectorCSV: " ++ err
    Right (_, v) -> do
      let names = [n | r <- V.toList v, let Right n = Csv.runParser $ r Csv..: "name"]
          vals = [x | r <- V.toList v, let Right x = Csv.runParser (r Csv..: "value")]
      pure (names, fromList vals)

loadMatrixWideCSV :: FilePath -> String -> IO ([String], [String], Matrix Double)
loadMatrixWideCSV dir base = do
  let path = dir </> (base ++ ".csv")
  bs <- BL.readFile path
  let txt = TE.decodeUtf8 (BL.toStrict bs)
      rows' = map (T.splitOn ",") (T.lines txt)
  case rows' of
    [] -> error $ "loadMatrixWideCSV: empty " ++ path
    (hdr : body) -> do
      let colNames = map T.unpack (drop 1 hdr)
          parseRow ts =
            case ts of
              [] -> Nothing
              (r : xs) ->
                let name = T.unpack r
                    nums = map parseD xs
                 in Just (name, nums)
          parseD t =
            case TR.double t of
              Right (d, _) -> d
              Left _ -> error $ "parseD: bad number: " ++ T.unpack t
          parsed = mapMaybe parseRow body
          rowNames = map fst parsed
          matLists = map snd parsed -- [[Double]]
          m = fromLists matLists
      pure (rowNames, colNames, m)
