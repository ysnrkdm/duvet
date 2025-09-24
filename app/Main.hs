{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Lib
import RiskModel.Generate (generateRiskData)
import System.Environment (getArgs)

main :: IO ()
main = do
  args' <- getArgs
  case args' of
    ["build"] -> generateRiskData
    ["solve", dateStr] -> do
      runDemo SolverOptions {analysisDate = dateStr, annualized = False}
    ["solve", dateStr, "--annualized"] -> do
      runDemo SolverOptions {analysisDate = dateStr, annualized = True}
    _ -> do
      putStrLn "Usage:"
      putStrLn "  duvet build"
      putStrLn "  (to build risk model data)"
      putStrLn "  duvet solve <date>"
      putStrLn "  (to solve the QP problem for a specific date)"
      putStrLn ""
      putStrLn "Example:"
      putStrLn "  duvet solve 2025-08-15"
      putStrLn "  (date format: YYYY-MM-DD)"
