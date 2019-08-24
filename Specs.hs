import Test.Hspec
import FibCal

main = hspec $ do
    describe "fibcal" $ do
        it "starts with a black circled number in position (0,0)" $ do
            head (fibCal (0,0) 3 [1..] colors) `shouldBe` 
                "\\node[draw,thick,circle,color=black,minimum size=0.9cm,inner sep=0pt] at (0,0) {$1$};"

        it "continues with a purple circled number in position (0,1)" $ do
            (head . tail) (fibCal (0,0) 3 [1..] colors) `shouldBe` 
                "\\node[draw,thick,circle,color=purple,minimum size=0.9cm,inner sep=0pt] at (0,1) {$2$};"

        it "then with four green circled numbers in positions (-1,1) to (-2,0)" $ do
            take 4 (drop 2 (fibCal (0,0) 5 [1..] colors))  `shouldBe` 
                ["\\node[draw,thick,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-1,1) {$3$};"
                ,"\\node[draw,thick,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-2,1) {$4$};"
                ,"\\node[draw,thick,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-1,0) {$5$};"
                ,"\\node[draw,thick,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-2,0) {$6$};"]

    describe "square" $ do
        it "given an origin, size and direction, yields coords" $ do
            square (0,0) 1 (R, U)  `shouldBe` [(0,0)]
            square (17,42) 1 (R, U)  `shouldBe` [(17,42)]
            square (0,0) 2 (R, U) `shouldBe` [(0,0),(1,0),(0,1),(1,1)]
            square (0,0) 2 (U,L) `shouldBe` [(0,0),(0,1),(-1,0),(-1,1)]


    describe "squares" $ do
        it "given a list of directions and a list of sizes, yields a list of list of coords" $ do
            squares [1,2] `shouldBe` 
                [[(0,0)],[(0,1)],[(-1,1),(-2,1),(-1,0),(-2,0)]] 
            squares [1,2,3] `shouldBe` 
                    [[(0,0)],[(0,1)],[(-1,1),(-2,1),(-1,0),(-2,0)],[(-2,-1),(-2,-2),(-2,-3),(-1,-1),(-1,-2),(-1,-3),(0,-1),(0,-2),(0,-3)]]

    describe "fib" $ do
        it "yields the fibonacci number of its argument" $ do
            fib 0  `shouldBe` 1
            fib 1  `shouldBe` 1
            fib 2  `shouldBe` 2
            fib 3  `shouldBe` 3
            fib 4  `shouldBe` 5

