module Utility.State where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State os) = State $ \ns -> let (a, s) = os ns in (f a, s)

instance Applicative (State s) where
    -- pure x = State (x, )                    -- Equivalent to: State $ \s -> (x, s)
    pure x = State $ \s -> (x, s)

    (State sf) <*> (State os) = 
        State $ \ns -> let (fn, s') = sf ns
                           (a,  s ) = os s'
                       in (fn a, s)   

instance Monad (State s) where
    return = pure
    State os >>= f =
        State $ \ns -> let (a, s) = os ns
                       in runState (f a) s

    -- pA >>= f = State $ \s1 -> (v2, s3)
    --             where 
    --                 (v1, s2) = runState pA s1
    --                 pB = f v1
    --                 (v2, s3) = runState pB s2 
    
state :: (s -> (a, s)) -> State s a
state = State

put :: s -> State s ()
put newState = state $ \_ -> ((), newState)

execState :: State s a -> s -> s
execState m s = snd (runState m s)