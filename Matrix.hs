module Matrix  
( Coord  
, addCoord
, inverseCoord
, safeGetCoord
, walkWhile
, walkWhileSame
) where  

import Data.Matrix
import Data.Maybe

type Coord = (Int, Int)

addCoord :: Coord -> Coord -> Coord
addCoord (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

inverseCoord :: Coord -> Coord
inverseCoord (y, x) = (-y, -x)

safeGetCoord :: Coord -> Matrix a -> Maybe a
safeGetCoord (y, x) = safeGet y x

walkWhile :: (a -> Bool) -> Coord -> Coord -> Matrix a -> Int
walkWhile predicate startPoint direction matrix = 
    let nextCoord = addCoord startPoint direction
        maybeNext = safeGetCoord nextCoord matrix
    in if ((fromMaybe False) . (fmap predicate) $ maybeNext)
        then 1 + walkWhile predicate nextCoord direction matrix
        else 0

walkWhileSame :: (Eq a) => Coord -> Coord -> Matrix a -> Int
walkWhileSame startPoint direction matrix = 
    fromMaybe 0 maybeWalkWhile
    where
        maybeWalkWhile = do
            predicate <- fmap (\v -> (== v)) (safeGetCoord startPoint matrix)
            return $ walkWhile predicate startPoint direction matrix

