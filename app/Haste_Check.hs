{-# LANGUAGE GADTs #-}
module Main where
import Haste 
import Haste.DOM

import Control.Monad.Operational
import Control.Monad.Writer
import CheckLogic

main :: IO ()
main = do
  runCheck check
  return ()

----

runCheck :: CheckLogic a -> IO Success
runCheck f = setContents . execWriter $ interpret run f
  where
    run :: CheckLogicData a -> Writer String a
    run (Output s) = tell $ s ++ "<br/>"

----

data Success = Success | Failue deriving Show

setContents :: String -> IO Success
setContents s = do
  v <- elemById "contents"
  case v of
    Just e -> do 
      setProp e "innerHTML" s
      return Success
    Nothing -> return Failue

unsafeGetContents :: IO (Maybe JSString)
unsafeGetContents = do
  v <- elemById "contents"
  case v of
    Just e -> do 
      s <- getProp e "innerHTML" 
      return . Just $ toJSString s
    Nothing -> return Nothing
