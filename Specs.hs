import Test.Hspec
import FibCal

colors = cycle [Black, Purple, Green, Blue, Red, Orange]
main = hspec $ do
    describe "fibcal" $ do
        it "starts with a black circled number in position (0,0)" $ do
            head (fibCal [1..] colors) `shouldBe` 
                "\\node[draw,circle,color=black,minimum size=0.9cm,inner sep=0pt] at (0,0) {$1$};"

        it "continues with a purple circled number in position (0,1)" $ do
            (head . tail) (fibCal [1..] colors) `shouldBe` 
                "\\node[draw,circle,color=purple,minimum size=0.9cm,inner sep=0pt] at (0,1) {$2$};"

        it "then with four green circled numbers in positions (-1,1) to (-2,0)" $ do
            take 4 (drop 2 (fibCal [1..] colors))  `shouldBe` 
                ["\\node[draw,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-1,1) {$3$};"
                ,"\\node[draw,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-2,1) {$4$};"
                ,"\\node[draw,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-1,0) {$5$};"
                ,"\\node[draw,circle,color=green,minimum size=0.9cm,inner sep=0pt] at (-2,0) {$6$};"]
