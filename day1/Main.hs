import Data.List

getLists :: String -> ([Int],[Int])
getLists = (\(x,y) -> (map read x, map read y)) . (\(x:y:[]) -> (x,y)) . transpose . map words . lines

--tail recursive quick sort implementation
qsort s [] = s
qsort s (pivot:ps) = qsort (pivot: (qsort s upper)) lower
  where lower = [ x | x <- ps, x <= pivot]
        upper = [ x | x <- ps, x > pivot]

result l1 l2 = foldr (\(a, b) s -> distance a b + s) 0 $ zip (sort l1) (sort l2)
  where sort = qsort []
        distance x y = abs (x - y)

main = do
  inp <- readFile "input"
  let (a,b) = getLists inp
  putStrLn $ show $ result a b
