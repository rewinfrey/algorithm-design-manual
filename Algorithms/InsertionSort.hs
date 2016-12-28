module InsertionSort where

run :: [Int] -> [Int] -> [Int]
run [] acc     = acc
run [x] acc    = acc' x acc
run (x:xs) acc = run xs (acc' x acc)

acc' :: Int -> [Int] -> [Int]
acc' num acc = mconcat [[ y | y <- acc, num > y ], [num], [ y | y <- acc, num < y ]]
