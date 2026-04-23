import Data.List (nub) -- Импортируем функцию для удаления дубликатов

findalldivs :: Int -> [Int]
findalldivs n = nub $ concat [[x, n `div` x] | x <- [1 .. limit], n `mod` x == 0] --- классическая прогонка всех множитель до корня
  where
    limit = floor (sqrt (fromIntegral n))

main :: IO ()
main = do
    let number = 100 -- Пример числа для проверки
    print $ findalldivs number
    let number2 = 0 -- Пример числа для проверки
    print $ findalldivs number2
    let number3 = 1 -- Пример числа для проверки
    print $ findalldivs number3
    return ()
---[1,100,2,50,4,25,5,20,10]
---[]
---[1]