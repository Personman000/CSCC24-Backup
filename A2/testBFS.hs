-- How to use: runghc testBFS.hs
--
-- To compile optimized:
-- ghc -O testBFS.hs
--
-- To run just efficiency test A and find out time usage:
-- time ./testBFS 3
-- It is meant to take a while (20 seconds on mathlab), but it should stay below
-- 10MB of memory. (Use top or htop to watch during those 20 seconds.)
--
-- If you have your own Linux, /usr/bin/time reports both time and memory (maxresident).

import           Data.List (sort)
import           TestLib

import           BFS (knightNext)
import qualified BFS as C (bfs, iterDeep)

-- Re-assert polymorphic types.
bfs :: (a -> [a]) -> a -> [a]
bfs = C.bfs
iterDeep :: (a -> [a]) -> a -> [a]
iterDeep = C.iterDeep

quad :: Int -> [Int]
quad i = [4*i + 1, 4*i + 2, 4*i + 3, 4*i + 4]

tests =
    [ "knight handout"
      ~: sort (knightNext 60 (2,5)) ~?= [(1,3),(1,7),(3,3),(3,7),(4,4),(4,6)]
    , "take 50 (bfs quad 0)"
      ~: take 50 (bfs quad 0) ~?= [0..49]
    , "take 50 (iterdeep quad 0)"
      ~: take 50 (iterDeep quad 0) ~?= [0..49]
    , "efficiency test A"
      ~: iterDeep quad 0 !! 123456789 ~?= 123456789
    ]
-- more when marking

main = testlibMain tests
