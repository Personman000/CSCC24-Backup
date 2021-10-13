-- getDFS' :: (a -> [a]) -> [a] -> Int -> [a]
-- getDFS' _ _ 0 = []
-- getDFS' next xs depth = xs ++ getDFS' next (concat (map next xs)) (depth-1)

-- getDFS :: (a -> [a]) -> [a] -> [[a]]
-- getDFS _ [] = []
-- getDFS next xs = map next xs

-- _ iterDeepHelper :: (a -> [a]) -> [[a]] -> [a]
-- iterDeepHelper _ map getDFS (concat ([] = []))
-- iterDeepHelper next (x:xs) = concat (getDFS next x) ++ iterDeepHelper next xs

-- dfs :: (a -> [a]) -> [a] -> Int -> [a]
-- dfs _ _ 0 = []
-- dfs _ [] _ = []
-- dfs next (x:xs) depth = x : xs ++ dfs next (xs ++ (next x)) (depth-1)