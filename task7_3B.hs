---Задача 3Б. Бесконечный список элементов геометрической прогрессии с основанием initи показателем ratio---
geometricprogression :: Num a => a -> a -> [a]
geometricprogression init ratio = [init * ratio^n | n <- [0..]] -- Генерируем бесконечную геометрическую прогрессию, начиная с init и умножая на ratio 
---в степени n, где n - натуральное число.
main :: IO ()
main = do
    let init = 1
    let ratio = 2
    let result = geometricprogression init ratio
    print $ take 10 result -- Выводим первые 10 элементов геометрической прогрессии для проверки
    return ()
---[1,2,4,8,16,32,64,128,256,512]