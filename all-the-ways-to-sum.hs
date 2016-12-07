import qualified Data.Map.Lazy as M
import Data.List (sort)

insertEm :: Int -> Int -> Int -> [Int] -> [Int]
insertEm target n k v
    | n + sum (k : v) <= target = n : v
    | otherwise                 = v

mapEm :: Int -> Int -> M.Map Int [Int] -> M.Map Int [Int]
mapEm target n m =
    M.insert n [] $ M.mapWithKey (insertEm target n) m

joinEm :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
joinEm target k v acc
    | sum (k : v) == target = (sort (k : v)) : acc
    | otherwise             = acc

countTheNumber :: Int -> [Int] -> [[Int]]
countTheNumber target xs = sort $ M.foldrWithKey (joinEm target) [] allCombos
    where allCombos   = foldr (mapEm target) M.empty validValues
          validValues = filter (<target) xs
