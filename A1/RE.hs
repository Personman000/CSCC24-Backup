module RE where

import REDef

render :: R -> String
render Eps = renderHelper 0 Eps
render (Single r) = renderHelper 0 (Single r)
render (Cat r1 r2) = renderHelper 0 (Cat r1 r2)
render (Or r1 r2) = renderHelper 0 (Or r1 r2)
render (Star r) = renderHelper 0 (Star r)



renderHelper :: Integer -> R -> String

-- Case of Eps
renderHelper prec Eps = "e"

-- Case of Single
renderHelper prec (Single r) = [r]

-- Case of Concatenation
renderHelper paren_prec (Cat r1 r2) =
    paren
        (paren_prec > curr_prec)
        (renderHelper curr_prec r1 ++ renderHelper curr_prec r2)
            where curr_prec = 2

-- Case of Or
renderHelper paren_prec (Or r1 r2) =
    paren
        (paren_prec > curr_prec)
        (renderHelper curr_prec r1 ++ "|" ++ renderHelper curr_prec r2)
            where curr_prec = 1

-- Case of Star
renderHelper prec (Star r) =
    renderHelper curr_prec r ++ "*"
            where curr_prec = 3


-- Helper for conditionally adding parentheses. You supply the condition.
paren :: Bool -> String -> String
paren True xs = "(" ++ xs ++ ")"
paren False xs = xs


match :: R -> String -> [String]

-- Case of Epsilon
match Eps s = [s]

-- Case of Single
match (Single r) (s:sx)
    | r == s = [sx]
    | otherwise = []

-- Case of Cat
match (Cat r1 r2) s = matchHelper r2 (match r1 s)

-- Case of Or
match (Or r1 r2) s = match r1 s ++ match r2 s

-- Case of Star
match (Star r) [] = [""]
match (Star r) s = s : matchHelper (Star r) s_star
                    where s_star = match r s


matchHelper :: R -> [String] -> [String]
matchHelper r [] = []
matchHelper r (s:sx) = match r s ++ matchHelper r sx


isInRL :: R -> String -> Bool
isInRL r xs = "" `elem` match r xs
