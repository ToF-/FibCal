import FibCal
import System.Environment

main = do
    args <- getArgs
    let x = read (head args) :: Integer
    let y = read (head (tail args)) :: Integer
    let n = read (head (tail (tail args))) :: Integer
    let s = read (head (tail (tail (tail args)))) :: Double
    putStrLn $ unlines $ (header s) ++ fibCal (x,y) n [1..] realColors ++ footer


header scale = ["\\documentclass[a4paper,landscape]{article}"
         ,"\\usepackage{tikz}"
         ,"\\begin{document}"
         ,"\\thispagestyle{empty}"
         ,"\\begin{tikzpicture}[transform canvas={scale="++show scale++"}]"]

footer = ["\\end{tikzpicture}"
         ,"\\end{document}"]
