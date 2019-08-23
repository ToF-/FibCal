module FibCal
where
import Data.Char

data Color = Black | Purple | Green | Blue | Red | Orange
    deriving (Show)

colors = cycle [Black, Purple, Green, Blue, Red, Orange]

data Direction = R | U | L | D
    deriving (Eq,Show)

type Coord = (Integer, Integer)


fibCal :: Show a => Integer -> [a] -> [Color] -> [String]
fibCal n xs cs = zipWith number (concatMap showNodes $ zip3 xs cs $ squares $ map fib [1..n]) [1..]
    where
    showNodes (n,c,sq) = map (showNode n c) sq
    showNode n c coords = 
        "\\node[draw,circle,color="
        ++ (map toLower . show) c 
        ++ ",minimum size=0.9cm,inner sep=0pt] at "
        ++ show coords

    number s n = s ++" {$"++ show n ++"$};"

square :: Coord -> Integer -> (Direction, Direction) -> [Coord]
square (x,y) 1 _ = [(x,y)]
square (x,y) n (d, e) = concat (extract n (map (advance e)) (extract n (advance d) (x,y)))
    where
    extract :: Integer -> (a -> a) -> a -> [a]
    extract n f t = take (fromIntegral n) (iterate f t)
     
advance :: Direction -> Coord -> Coord 
advance R (x,y) = (x+1,y)
advance L  (x,y) = (x-1,y)
advance U    (x,y) = (x,y+1)
advance D  (x,y) = (x,y-1)

nextDirection :: (Direction, Direction) -> (Direction, Direction)
nextDirection (R, U)  = (U, L)
nextDirection (U, L)   = (L, D)
nextDirection (L, D) = (D,R)
nextDirection (D,R) = (R, U)


squares :: [Integer] -> [[Coord]]
squares ns = snd $ foldl addSquare ((U,L), [[(0,0)]]) ns
    where
    addSquare :: ((Direction,Direction), [[Coord]]) -> Integer -> ((Direction,Direction),[[Coord]])
    addSquare ((d1,d2),acc) n = (nextDirection (d1,d2),acc ++ [square (advance d1 l) n (d1,d2)])
        where 
        l = last (last acc) 

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
