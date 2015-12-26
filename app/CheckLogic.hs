{-# LANGUAGE GADTs #-}
module CheckLogic where
import Control.Monad.Operational
import Game.Bucephalus

check :: CheckLogic ()
check = do
  output $ "rect1 = " ++ show rect1
  output $ "rect2 = " ++ show rect2
  output $ "rect3 = " ++ show rect3
  output $ "rect1 `collision` rect2 = " ++ show (rect1 `collision` rect2)
  output $ "rect2 `collision` rect3 = " ++ show (rect2 `collision` rect3)
  output $ "rect1 `collision` rect3 = " ++ show (rect1 `collision` rect3)

----

rect1 :: Figure 
rect1 = FigureRectangle $ Rectangle (Point (100, 100), Point (200, 200))

rect2 :: Figure 
rect2 = FigureRectangle $ Rectangle (Point (150, 150), Point (250, 250))

rect3 :: Figure 
rect3 = FigureRectangle $ Rectangle (Point (225, 225), Point (325, 325))

----

data CheckLogicData a where
  Output :: String -> CheckLogicData ()

type CheckLogic = Program CheckLogicData

output :: String -> CheckLogic ()
output s = singleton $ Output s

puts :: Show a => a -> CheckLogic ()
puts = output . show
