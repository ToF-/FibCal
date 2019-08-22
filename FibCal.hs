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


