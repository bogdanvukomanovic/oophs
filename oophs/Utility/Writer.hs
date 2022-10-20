module Utility.Writer where

newtype Writer w a = Writer {runWriter :: (a, w)} deriving (Show)

instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    Writer (f, w) <*> Writer (a, w') = Writer (f a, w <> w')

instance Monoid w => Monad (Writer w) where
    return = pure
    Writer (a, w) >>= f = let (b, w') = runWriter (f a)
                          in Writer (b, w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)