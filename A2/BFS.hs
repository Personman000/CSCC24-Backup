module BFS where

-- Question 1

knightNext :: Int -> (Int, Int) -> [(Int, Int)]
knightNext n (x,y) = filter op list
    where
        op = \(a, b) -> a>0 && a<=n && b>0 && b<=n      -- filter moves that are within board
        list = [(op1 x xz, op2 y (3-xz)) |              -- get list of all possible moves
                                xz <- [1, 2],
                                op1 <- [(+), (-)],
                                op2 <- [(+), (-)]]


-- The quad function as in the handout
quad :: Int -> [Int]
quad i = [4*i + 1, 4*i + 2, 4*i + 3, 4*i + 4]


-- Question 2

-- bfs :: Eq a => (a -> [a]) -> a -> [a]
-- bfs next x0 = x0 : bfsHelper next (next x0)

-- bfsHelper :: Eq a => (a -> [a]) -> [a] -> [a]
-- bfsHelper :: (a -> [a]) -> [a] -> [a]
-- bfsHelper next [] = []
-- bfsHelper next (q:qs) = q : bfsHelper next (qs ++ (filter (\a -> not (a `elem` (q:qs))) (next q)))

bfs :: (a -> [a]) -> a -> [a]
bfs next x0 = x0 : bfsHelper next (next x0)

bfsHelper :: (a -> [a]) -> [a] -> [a]
bfsHelper next [] = []
bfsHelper next (q:qs) = q : bfsHelper next (qs ++ (next q))


-- Question 3

iterDeep :: (a -> [a]) -> a -> [a]
iterDeep next x0 = getDFS next [x0]

getDFS :: (a -> [a]) -> [a] -> [a]
getDFS _ [] = []
getDFS next xs = xs ++ getDFS next (concat (map next xs))