import FibCal
import System.Environment

main = do
    args <- getArgs
    let n = read (head args) :: Integer
    let s = read (head (tail args)) :: Double
    putStrLn $ unlines $ (header s) ++ fibCal n [1..] colors ++ footer


header scale = ["\\documentclass{article}"
         ,"\\usepackage{tikz}"
         ,"\\begin{document}"
         ,"\\begin{tikzpicture}[transform canvas={scale="++show scale++"}]"]

footer = ["\\end{tikzpicture}"
         ,"\\end{document}"]
