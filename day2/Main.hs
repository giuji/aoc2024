import Data.Monoid

isDistanceOk x y = d >= 1 && d <= 3
  where d = abs (x - y)

--i got this from stack overflow, (-> r) applicative is genius
respectsOrder rel = and . (zipWith rel <*> tail)

-- adding "getAny ." fails typecheck??
anyOrder :: [a -> a -> Bool] -> [a] -> Any
anyOrder = mconcat . map (Any .) . map respectsOrder

isSafe :: [Int] -> Bool
isSafe = getAny . anyOrder [a,b]
   where a x y = x <= y && isDistanceOk x y
         b x y = x >= y && isDistanceOk x y

result = length . filter (isSafe) . map (map read . words) . lines

main = do
  inp <- readFile "input"
  putStrLn $ show $ result inp
