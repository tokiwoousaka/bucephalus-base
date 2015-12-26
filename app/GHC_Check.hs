{-# LANGUAGE GADTs #-}
module Main where
import Control.Monad.Operational
import CheckLogic

-- if you need execute this program with "stack runghc" in the cabal file directory,
-- then you can type "cd app; stack runghc GHC_Check.hs"

main :: IO ()
main = do
  runCheck check

runCheck :: CheckLogic a -> IO a
runCheck f = interpret run f
  where
    run :: CheckLogicData a -> IO a
    run (Output s) = putStrLn s
