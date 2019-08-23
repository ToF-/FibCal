module FibCal
where
import Data.Char

data Color = Black | Purple | Green | Blue | Red | Orange
    deriving (Show)

type Direction = (Integer, Integer)

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

squares :: [Coord]
squares = coords $ foldl nextSquare initial $ fibs

coords (c,d,cs) = cs

initial :: (Coord, Direction, [Coord]) 
initial = ((-1,-1),(1,1),[])

nextSquare :: (Coord, Direction, [Coord]) -> Integer -> (Coord, Direction, [Coord])
nextSquare ((x,y),(dx,dy),s) n = ((x+dx*n,y+dy*n), next (dx,dy), s ++ square n (x,y) (dx,dy))

square :: Integer -> Direction -> Coord -> [Coord]
square n (dx,dy) (x,y) = [(i,j) | j <- range y dy y, i <- range y dx x]

range a d n = map (\i-> a + i * d) [0..n-1]

next :: Direction -> Direction
next (1,1) = (-1,-1)
next (-1,-1) = (1,1)

fibs = [1,1,2,3,5,8,13]


-- [1,1,2,3,5,8,13,21,..]
-- [(right,up),(up,left),(left,down),(down,right),(right,up),..]
-- [[1],[2],[3,4,5,6],[7,8,9,10,11,12,13,14,15],..]
-- [(0,0),(0,1),(-1,1),(-2,-1),(0,-3)
