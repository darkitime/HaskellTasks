---Задача 2А. Определить, является ли данное целое число произведением трёх трёхзначных целых чисел.---
isProductOfThreeThreeDigitNumbers :: Int -> Bool
isProductOfThreeThreeDigitNumbers n
    | n < 100*100*100 || n > 999*999*999 = False --- Проверяем, что число находится в диапазоне трёх трёхзначных целых чисел
    | otherwise = any (\x -> 
        n `mod` x == 0 &&
        let m = n `div` x in 
            any (\y -> 
                m `mod` y == 0 && 
                let z = m `div` y in 
                    z >= 100 && z <= 999)
                    [100..min 999 m])
                    [100..min 999 n] --- Проверяем все трёхзначные числа в качестве первого множителя, затем для каждого такого 
     ---множителя проверяем второй множитель, и если он подходит, проверяем третий множитель.

main = do
    let number = 123456789 -- Пример числа для проверки
    print $ isProductOfThreeThreeDigitNumbers number
    return ()
