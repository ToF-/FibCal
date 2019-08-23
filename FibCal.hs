module FibCal
where
import Data.Char

data Color = Black | Purple | Green | Blue | Red | Orange
    deriving (Show)

data Direction = D_Right | D_Up | D_Left | D_Down
    deriving (Eq,Show)

type Coord = (Integer, Integer)


fibCal :: Show a => [a] -> [Color] -> [String]
fibCal xs cs = map showNode (zip3 xs cs [(0,0),(0,1)])
    where
    showNode (n,c,coords) = 
        "\\node[draw,circle,color="
        ++ (map toLower . show) c 
        ++ ",minimum size=0.9cm,inner sep=0pt] at "
        ++ show coords
        ++" {$"++ show n ++"$};"

square :: Coord -> Integer -> (Direction, Direction) -> [[Coord]]
square (x,y) 1 _ = [[(x,y)]]
square (x,y) n (d, e) = extract n (map (advance e)) (extract n (advance d) (x,y))
    where
    extract :: Integer -> (a -> a) -> a -> [a]
    extract n f t = take (fromIntegral n) (iterate f t)
     
    advance :: Direction -> Coord -> Coord 
    advance D_Right (x,y) = (x+1,y)
    advance D_Left  (x,y) = (x-1,y)
    advance D_Up    (x,y) = (x,y+1)
    advance D_Down  (x,y) = (x,y-1)

nextDirection :: (Direction, Direction) -> (Direction, Direction)
nextDirection (D_Right, D_Up)  = (D_Up, D_Left)
nextDirection (D_Up, D_Left)   = (D_Left, D_Down)
nextDirection (D_Left, D_Down) = (D_Down,D_Right)
nextDirection (D_Down,D_Right) = (D_Right, D_Up)



