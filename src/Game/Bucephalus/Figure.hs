module Game.Bucephalus.Figure where

newtype Point = Point (Double, Double)
  deriving (Show, Read, Eq, Ord)
-- TODO : normalization for Rectangle
newtype Rectangle = Rectangle (Point, Point)
  deriving (Show, Read, Eq, Ord)
newtype Vector2D = Vector2D (Double, Double)
  deriving (Show, Read, Eq, Ord)

data Figure
  = FigurePoint Point
  | FigureRectangle Rectangle
  deriving (Show, Read, Eq, Ord)

pointToVector2D :: Point -> Vector2D
pointToVector2D (Point p) = Vector2D p

point :: (Double, Double) -> Figure
point = FigurePoint . Point

rectangle :: (Double, Double) -> (Double, Double) -> Figure
rectangle p1 p2 = FigureRectangle $ Rectangle (Point p1, Point p2)

----
-- Collision

class Collisional c where
  collision :: c -> c -> Bool

instance Collisional Point where
  collision = (==)

instance Collisional Rectangle where
  collision rect1 rect2 = let
      (Rectangle (point0, point1)) = rect1
      (Rectangle (point2, point3)) = rect2
      (Point (x0, y0)) = point0
      (Point (x1, y1)) = point1
      (Point (x2, y2)) = point2
      (Point (x3, y3)) = point3
    in (x0 < x3 && x2 < x1) && (y0 < y3 && y2 < y1)

----

instance Collisional Figure where
  collision (FigurePoint l) (FigurePoint r) = l `collision` r
  collision (FigureRectangle l) (FigureRectangle r) = l `collision` r
  collision (FigureRectangle r) (FigurePoint l) = r `collision_rect_point` l
  collision x y = y `collision` x

collision_rect_point :: Rectangle -> Point -> Bool
collision_rect_point rect point = let
    (Rectangle (point0, point1)) = rect
    (Point (x0, y0)) = point0
    (Point (x1, y1)) = point1
    (Point (x , y )) = point
  in (x0 < x && y0 < y) && (x < x1 && y < y1)

----
-- Moveable

class Moveable a where
  move :: Vector2D -> a -> a

instance Moveable Point where
  move (Vector2D (x0, y0)) (Point (x1, y1)) = Point (x0 + x1, y0 + y1)

instance Moveable Rectangle where
  move v (Rectangle (point0, point1)) = Rectangle (move v point0, move v point1)

instance Moveable Figure where
  move v (FigurePoint p) = FigurePoint $ move v p
  move v (FigureRectangle r) = FigureRectangle $ move v r
