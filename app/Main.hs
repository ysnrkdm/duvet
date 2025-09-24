{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Lib
import RiskModel.Generate (generateRiskData)
import System.Console.CmdArgs

data CmdOptions = CmdOptions
  { demo :: Bool,
    generate :: Bool
  }
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  opts <- cmdArgs (CmdOptions {demo = False, generate = False} &= help "Run demo or generate risk data" &= summary "Duvet 0.1")
  if demo opts
    then runDemo
    else
      if generate opts
        then generateRiskData
        else putStrLn "Please specify --demo or --generate"
