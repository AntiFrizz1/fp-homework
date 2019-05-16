import Criterion.Main (defaultMain, bgroup, nf, bench)
import Task1 (simpleMultiply, multiply)
import Task2 (slowPerimeter, perimeter, doubleArea, simpleDoubleArea,
 Point(Point))

genMatrix' :: Int -> Int -> [[Int]]
genMatrix' 0 _ = []
genMatrix' n m = (genVector' m n):(genMatrix' (n - 1) m)

genVector' :: Int -> Int -> [Int]
genVector' 0 _ = []
genVector' a p = (a + p):(genVector' (a - 1) p)

generateRectangle :: Int -> Int -> [Point]
generateRectangle xSize ySize =
  (fmap (\s -> Point s 0) [0 .. xSize]) ++ [Point xSize ySize, Point 0 ySize]

main :: IO ()
main = defaultMain
  [ bgroup "simpleMultiply"
    [ bench "250x250" $ nf (\(x,y) -> simpleMultiply x y) (genMatrix' 250 250,
     genMatrix' 250 250)
    , bench "300x300" $ nf (\(x,y) -> simpleMultiply x y) (genMatrix' 300 300,
     genMatrix' 300 300)
    , bench "400x400" $ nf (\(x,y) -> simpleMultiply x y) (genMatrix' 400 400,
     genMatrix' 400 400)
    ]
  , bgroup "parallelMultiply"
    [ bench "250x250" $ nf (\(x,y) -> multiply x y) (genMatrix' 250 250,
     genMatrix' 250 250)
    , bench "300x300" $ nf (\(x,y) -> multiply x y) (genMatrix' 300 300,
     genMatrix' 300 300)
    , bench "400x400" $ nf (\(x,y) -> multiply x y) (genMatrix' 400 400,
     genMatrix' 400 400)
    ]
  , bgroup "slowPerimeterRectangle"
    [ bench "10^6" $ nf slowPerimeter (generateRectangle 1000000 1000000)
    , bench "10^7" $ nf slowPerimeter (generateRectangle 10000000 10000000)
    ]
  , bgroup "parallelPerimeterRectangle"
    [ bench "10^6" $ nf perimeter (generateRectangle 1000000 1000000)
    , bench "10^7" $ nf perimeter (generateRectangle 10000000 10000000)
    ]
  , bgroup "simpleDoubleAreaRectangle"
    [ bench "10^6" $ nf simpleDoubleArea (generateRectangle 1000000 1000000)
    , bench "10^7" $ nf simpleDoubleArea (generateRectangle 10000000 10000000)
    ]
  , bgroup "parallelDoubleAreaRecatenge"
    [ bench "10^6" $ nf doubleArea (generateRectangle 1000000 1000000)
    , bench "10^7" $ nf doubleArea (generateRectangle 10000000 10000000)
    ]
  ]
