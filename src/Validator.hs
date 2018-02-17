module Validator where

data Validator m a =
  Validator m
            a

errorMessages :: Validator m a -> m
errorMessages (Validator m a) = m

isValid :: (Monoid m, Eq m) => Validator m a -> Bool
isValid (Validator m a) = m == mempty

instance Functor (Validator m) where
  fmap f (Validator m a) = Validator m $ f a

instance (Monoid m) => Applicative (Validator m) where
  pure a = Validator mempty a

instance (Monoid m) => Monad (Validator m) where
  (Validator m a) >>= f = do
    let (Validator m' a') = f a
    (Validator (mappend m m') a')

addError :: a -> String -> Validator [String] a
addError a msg = Validator [msg] a

validate :: (Monoid m) => [a -> Validator m a] -> a -> Validator m a
validate vs a = foldl (>>=) (pure a) vs

presence :: String -> (a -> Maybe b) -> a -> Validator [String] a
presence fieldName getter a =
  case getter a of
    (Just _) -> return a
    Nothing  -> addError a (fieldName ++ " cannot be blank")
