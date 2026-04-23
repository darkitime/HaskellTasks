data PrimeFactorization = PF [(Int, Int)] --- Тип данных для представления разложения на простые множители
    deriving (Show, Eq)
nodPF :: PrimeFactorization -> PrimeFactorization -> PrimeFactorization
nodPF (PF []) (PF []) = PF [] --- НОД для двух пустых разложений
nodPF (PF []) (PF ys) = PF [] --- НОД для пустого разложения и не пустого разложения
nodPF (PF xs) (PF []) = PF [] --- НОД для не пустого разложения и пустого разложения
nodPF (PF xs) (PF ys) = PF (nodHelper xs ys) --- НОД для двух не пустых разложений
    where
        nodHelper [] _ = [] --- Если первое разложение пустое, возвращаем пустой список
        nodHelper _ [] = [] --- Если второе разложение пустое, возвращаем пустой список
        nodHelper ((p1, e1):xs) ((p2, e2):ys)
            | p1 < p2 = nodHelper xs ((p2, e2):ys) --- Если первый простой множитель меньше второго, пропускаем его
            | p1 > p2 = nodHelper ((p1, e1):xs) ys --- Если первый простой множитель больше второго, пропускаем его
            | otherwise = (p1, min e1 e2) : nodHelper xs ys --- Если простые множители равны, добавляем их в результат с минимальной степенью и продолжаем поиск
nokPF :: PrimeFactorization -> PrimeFactorization -> PrimeFactorization
nokPF (PF []) (PF []) = PF [] --- НОК для двух пустых
nokPF (PF []) (PF ys) = PF ys --- НОК для пустого разложения и не пустого разложения
nokPF (PF xs) (PF []) = PF xs --- НОК для не пустого разложения и пустого разложения
nokPF (PF xs) (PF ys) = PF (nokHelper xs ys) --- НОК для двух не пустых разложений
    where
        nokHelper [] ys = ys --- Если первое разложение пустое, возвращаем второе разложение
        nokHelper xs [] = xs --- Если второе разложение пустое, возвращаем первое разложение
        nokHelper ((p1, e1):xs) ((p2, e2):ys)
            | p1 < p2 = (p1, e1) : nokHelper xs ((p2, e2):ys) --- Если первый простой множитель меньше второго, добавляем его в результат и продолжаем поиск
            | p1 > p2 = (p2, e2) : nokHelper ((p1, e1):xs) ys --- Если первый простой множитель больше второго, добавляем второй простой множитель в результат и продолжаем поиск
            | otherwise = (p1, max e1 e2) : nokHelper xs ys --- Если простые множители равны, добавляем их в результат с максимальной степенью и продолжаем поиск

main :: IO ()
main = do
    let pf1 = PF [(2, 2), (3, 2), (5, 1)]
    let pf2 = PF [(2, 1), (3, 1), (5, 1)]
    print $ nodPF pf1 pf2
    print $ nokPF pf1 pf2
    let pf3 = PF [(2, 3), (7, 1)]
    let pf4 = PF [(2, 1), (3, 2), (7, 2)]
    print $ nodPF pf3 pf4
    print $ nokPF pf3 pf4
    return ()
--- nod:
--- PF [(2,1),(3,1),(5,1)]
--- nok:
--- PF [(2,2),(3,2),(5,1)]
--- nod:
--- PF [(2,1),(7,1)]
--- nok:
--- PF [(2,3),(3,2),(7,2)]