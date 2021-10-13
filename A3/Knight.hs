module Knight where

import KnightDef

-- Argument order: board size n, initial position, destination
knightServer :: MonadKnight m => Int -> (Int, Int) -> (Int, Int) -> m Integer
knightServer n (cx, cy) (dx, dy) = knightServerHelper n (cx, cy) (dx, dy) 0


knightServerHelper :: MonadKnight m => Int -> (Int, Int) -> (Int, Int) -> Integer -> m Integer
knightServerHelper n (cx, cy) (dx, dy) count
    | cx == dx && cy == dy = return count
    | otherwise = tellPosAskNext (MkPos n (dx,dy) (cx,cy) choices) >>= \newpos ->
        if newpos `elem` choices
            then knightServerHelper n newpos (dx, dy) (count+1)
            else buzz >> knightServerHelper n (cx, cy) (dx, dy) count
        where
            choices = knightNext n (cx, cy)




instance Functor KnightTrace where
    fmap f ks = ks >>= \a -> return (f a)

instance Applicative KnightTrace where
    pure a = return a
    fs <*> xs = fs >>= \f -> xs >>= \x -> return (f x)

instance Monad KnightTrace where
    return a = Answer a
    Answer a >>= cb = cb a
    Buzz k >>= cb = Buzz (k >>= cb)
    Step pos f >>= cb = Step pos (\x -> f x >>= \y -> cb y)


instance MonadKnight KnightTrace where
    buzz = Buzz (Answer ())
    tellPosAskNext pos = Step pos return

simpleCheck :: KnightTrace Integer -> Bool
simpleCheck (Step (MkPos n dpos cpos choices) f) = (1,1) == cpos && simpleCheck2 (f (3,2))
simpleCheck _ = False

simpleCheck2 :: KnightTrace Integer -> Bool
simpleCheck2 (Answer a) = a == 1
simpleCheck2 _ = False