module Game.Bucephalus 
  ( module Game.Bucephalus.Object
  , module Game.Bucephalus.Figure
  , BucephalusStateData(..)
  , BucephalusFrame
  , BucephalusOptions
  , makeInitData
  ----
  , toBucephalusFramefromReader
  , toBucephalusFrame
  ----
  , askObject 
  , getObject
  , insertObject 
  , getState 
  , putObject 
  , modifyObject 
  , deleteObject
  , moveObject 
  , modifyObjects 
  ) where
import Game.Bucephalus.Object
import Game.Bucephalus.Figure
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader.Class

data BucephalusStateData a i s = BucephalusStateData
  { buceObjects :: M.Map a (Object i)
  , buceState :: s
  }

makeInitData :: Ord a => s -> BucephalusStateData a i s
makeInitData d = BucephalusStateData 
  { buceObjects = M.fromList []
  , buceState = d  
  }

------

type BucephalusFrame a i s 
  = BucephalusStateData a i s -> (BucephalusStateData a i s, PaintInfo i)
data BucephalusOptions = BucephalusOptions
type BucephalusReader a i s = (->) (BucephalusStateData a i s)
type BucephalusState a i s =  
  StateT (BucephalusStateData a i s) (BucephalusReader a i s) 

toBucephalusFramefromReader :: BucephalusReader a i s (BucephalusStateData a i s) -> BucephalusFrame a i s
toBucephalusFramefromReader f s = 
  let st = f s in (st, getPaintInfo . map snd . M.toList $ buceObjects s) 

runBucephalusState 
  :: BucephalusState a i s x -> BucephalusStateData a i s -> BucephalusReader a i s (BucephalusStateData a i s)
runBucephalusState f s = do
  res <- execStateT f s
  return res

toBucephalusFrame :: BucephalusState a i s x -> BucephalusFrame a i s
toBucephalusFrame f s = toBucephalusFramefromReader (runBucephalusState f s) s

------

askObject :: Ord a => a -> BucephalusState a i s (Maybe (Object i))
askObject = acquisitionObject ask

getObject :: Ord a => a -> BucephalusState a i s (Maybe (Object i))
getObject = acquisitionObject get

insertObject :: Ord a => a -> Object i -> BucephalusState a i s ()
insertObject x o = modifyObjects (M.insert x o)

getState :: BucephalusState a i s s
getState = get >>= return . buceState

putObject :: Ord a => a -> Object i -> BucephalusState a i s ()
putObject x o = modifyObject x (const o)

modifyObject :: Ord a => a -> (Object i -> Object i) -> BucephalusState a i s ()
modifyObject x f = modifyObjects (M.update (Just . f) x)

deleteObject :: Ord a => a -> BucephalusState a i s ()
deleteObject x = modifyObjects (M.delete x)

moveObject :: Ord a => a -> Vector2D -> BucephalusState a i s ()
moveObject x v = modifyObjects (M.update (Just . move v) x)

modifyObjects :: (M.Map a (Object i) -> M.Map a (Object i)) -> BucephalusState a i s ()
modifyObjects f = modify $ \s -> let
    objs = buceObjects s
  in s { buceObjects = f objs }

------
-- help

acquisitionObject :: Ord a => 
  BucephalusState a i s (BucephalusStateData a i s) -> a -> BucephalusState a i s (Maybe (Object i))
acquisitionObject f x = do
  o <- f >>= return . buceObjects
  return $ M.lookup x o

