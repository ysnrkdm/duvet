{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module RiskModel.FetchData (readDay, fetchStooq) where

import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import qualified Data.Csv as Csv
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Time (Day, NominalDiffTime, defaultTimeLocale, diffUTCTime, getCurrentTime, parseTimeM)
import qualified Data.Vector as V
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Simple
import Network.TLS (ClientParams (..), EMSMode (..), Supported (..), defaultParamsClient, sharedCAStore)
import RiskModel.Types
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.FilePath ((</>))
import System.X509 (getSystemCertificateStore)

readDay :: String -> Day
readDay s =
  fromMaybe
    (error $ "bad day: " ++ s)
    (parseTimeM True defaultTimeLocale "%Y-%m-%d" s)

newtype CsvBar = CsvBar {unCsvBar :: Bar}

instance Csv.FromNamedRecord CsvBar where
  parseNamedRecord :: Csv.NamedRecord -> Csv.Parser CsvBar
  parseNamedRecord r =
    CsvBar
      <$> ( Bar
              <$> (readDay <$> r Csv..: "Date")
              <*> r Csv..: "Close"
              <*> r Csv..: "Volume"
          )

-- CSV: Date,Open,High,Low,Close,Volume
stooqURL :: String -> String
stooqURL sym = "https://stooq.com/q/d/l/?s=" ++ symQ ++ "&i=d"
  where
    symQ = toStooqSymbol sym

toStooqSymbol :: String -> String
toStooqSymbol s
  | s == "CASH" = s
  | otherwise =
      let base = map toLower s
          dash = [if c == '.' then '-' else c | c <- base]
       in if ".us" `isSuffixOf` dash then dash else dash ++ ".us"

mkEMSManager :: String -> IO Manager
mkEMSManager host' = do
  store <- getSystemCertificateStore
  let base = defaultParamsClient host' ""
      params =
        base
          { clientSupported =
              (clientSupported base) {supportedExtendedMainSecret = AllowEMS},
            clientShared =
              (clientShared base)
                { sharedCAStore = store
                }
          }
  newManager (mkManagerSettings (TLSSettings params) Nothing)

downloadStooq :: String -> IO (Either String BL.ByteString)
downloadStooq sym = do
  putStrLn $ "Downloading daily bars (Stooq) for [" ++ sym ++ "] …"
  let url = stooqURL sym
  initReq <- parseRequest url
  let req =
        setRequestHeader "User-Agent" ["riskpipe/0.1"] $
          setRequestResponseTimeout (responseTimeoutMicro (20 * 1000000)) $ -- 20s
            initReq
  mgr <- mkEMSManager "stooq.com"
  resp <- httpLBS (setRequestManager mgr req)
  let bs = getResponseBody resp
  if BL.length bs < 100
    then return $ Left $ "Downloaded data too short: " ++ show (BL.length bs) ++ " bytes"
    else return $ Right bs

isStale :: FilePath -> NominalDiffTime -> IO Bool
isStale filePath maxAge = do
  tNow <- getCurrentTime
  tMod <- getModificationTime filePath
  pure $ (tNow `diffUTCTime` tMod) > maxAge

fetchStooq :: String -> IO [(Day, Double, Double)] -- (date, close, volume)
fetchStooq sym = do
  let dataCachePath = cachePath sym
  createDirectoryIfMissing True cacheDir
  exists <- doesFileExist dataCachePath
  stale <- case (exists, maxAgeDays) of
    (True, Just d) -> isStale dataCachePath (fromIntegral d * 86400)
    _ -> pure False
  let needDownload = forceRefresh || (not exists) || stale

  if needDownload
    then do
      eres <- downloadStooq sym
      case eres of
        Left err -> do
          putStrLn $ "Error downloading " ++ sym ++ ": " ++ err
          if exists
            then do
              putStrLn $ "Using cached data for " ++ sym
              loadFromCache dataCachePath
            else fail $ "No cached data for " ++ sym ++ ", cannot proceed."
        Right bs -> do
          BL.writeFile dataCachePath bs
          parseCsv bs sym
    else do
      putStrLn $ "Using cached data for " ++ sym
      loadFromCache dataCachePath
  where
    maxAgeDays = Just 7 -- Re-download if cache older than this (days); Nothing = never re-download
    forceRefresh = False -- Set to True to always re-download
    loadFromCache p = do
      bs <- BL.readFile p
      parseCsv bs sym
    parseCsv bs s =
      case Csv.decodeByName bs of
        Left err -> fail $ "CSV parse error " ++ s ++ ": " ++ err
        Right (_, v) ->
          pure
            [ (bDate b, bClose b, bVolume b)
            | CsvBar b <-
                V.toList v
            ]

-- putStrLn $ "Downloading daily bars (Stooq) for [" ++ sym ++ "] …"
-- let url = stooqURL sym
-- initReq <- parseRequest url
-- let req =
--       setRequestHeader "User-Agent" ["riskpipe/0.1"] $
--         setRequestResponseTimeout (responseTimeoutMicro (20 * 1000000)) $ -- 20s
--           initReq
-- mgr <- mkEMSManager "stooq.com"
-- resp <- httpLBS (setRequestManager mgr req)
-- let bs = getResponseBody resp
-- case Csv.decodeByName bs of
--   Left err -> fail $ "CSV parse error " ++ sym ++ ": " ++ err
--   Right (_, v) ->
--     pure [(bDate b, bClose b, bVolume b) | CsvBar b <- V.toList v]
-- where
--   toLowerAscii c
--     | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
--     | otherwise = c

cacheDir :: FilePath
cacheDir = "tmp/cache"

cachePath :: String -> FilePath
cachePath sym = cacheDir </> (toStooqSymbol sym ++ ".csv")
