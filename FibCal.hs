module FibCal
where
import Data.Char

data Color = Black | Purple | Green | Blue | Red | Orange
    deriving (Show)

fibCal :: Show a => [a] -> [Color] -> [String]
fibCal xs cs = map showNode (zip3 xs cs [(0,0),(0,1)])
    where
    showNode (n,c,coords) = 
        "\\node[draw,circle,color="
        ++ (map toLower . show) c 
        ++ ",minimum size=0.9cm,inner sep=0pt] at "
        ++ show coords
        ++" {$"++ show n ++"$};"


-- [1,1,2,3,5,8,13,21,..]
-- [(right,up),(up,left),(left,down),(down,right),(right,up),..]
-- [[1],[2],[3,4,5,6],[7,8,9,10,11,12,13,14,15],..]
-- [(0,0),(0,1),(-1,1),(-2,-1),(0,-3)
