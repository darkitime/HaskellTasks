---Задача 3Б. Определить, состоит ли данная строка из трёх слов (в начале строки, в конце и между словами может присутствовать любое количество пробелов).---
import Data.Char (isSpace)

isThreeWords :: String -> Bool
isThreeWords s = length (words s) == 3 --- Функция words автоматически удаляет лишние пробелы и разделяет строку на слова, поэтому нам просто нужно проверить,
    --- что количество слов равно 3.

main = do
    let sentence = "Hello, world!"
    print $ isThreeWords sentence
    let sentence2 = "   This is a test.   "
    print $ isThreeWords sentence2
    let sentence3 = "1 2 3"
    print $ isThreeWords sentence3
    return ()
