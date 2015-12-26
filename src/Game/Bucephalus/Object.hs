module Game.Bucephalus.Object where
import Game.Bucephalus.Figure
import Data.Bool

data ImageDrawType = DrawDefault Point | DrawScaled Rectangle
  deriving (Show, Read, Eq, Ord)

data DrawInfo i 
  = DrawFigure Figure DrawOption 
  | DrawImageInfo
  deriving (Show, Read, Eq, Ord)

data BuceColor = BuceRGB Int Int Int | BuceRGBA Int Int Int Double deriving (Show, Read, Eq, Ord)
data DrawOption = DrawOption
  { borderColor :: Maybe BuceColor
  , fillColor :: Maybe BuceColor
  , drawLineWidth :: Double
  } deriving (Show, Read, Eq, Ord)

drawOption :: DrawOption
drawOption = DrawOption
  { borderColor = Just $ BuceRGB 0 0 0
  , fillColor = Nothing
  , drawLineWidth = 1
  }

-----

data Object i = Object
  { objDrawInfos :: [DrawInfo i]
  , objCollisionFrame :: [Figure]
  , objPosition :: Point
  }

instance Collisional (Object i) where
  collision o0 o1 = or . concat $ map mapCollision (objCollisionFrame o0)
    where
      mapCollision :: Figure -> [Bool]
      mapCollision f = map (collision f) $ objCollisionFrame o1

instance Moveable (Object i) where
  move v o = o { objPosition = move v (objPosition o) }

instance Moveable (DrawInfo i) where
  move v (DrawFigure f o) = DrawFigure (move v f) o

-----

data PaintInfo i = PaintInfo [DrawInfo i]

getPaintInfo :: [Object i] -> PaintInfo i
getPaintInfo xs = PaintInfo $ concatMap adjustPos xs
  where
    adjustPos :: Object i -> [DrawInfo i]
    adjustPos o = map (move . pointToVector2D $ objPosition o) $ objDrawInfos o
