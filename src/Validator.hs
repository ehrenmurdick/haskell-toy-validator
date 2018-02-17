module Validator where

data Validated a =
  Validated [String]
            a

errorMessages :: Validated a -> [String]
errorMessages (Validated msgs a) = msgs

isValid :: Validated a -> Bool
isValid (Validated m a) = m == mempty

instance Functor Validated where
  fmap f (Validated m a) = Validated m $ f a

instance Applicative Validated where
  pure a = Validated mempty a
  -- <*> not implemented -- dont care this is a toy repo

instance Monad Validated where
  (Validated m a) >>= f = do
    let (Validated m' a') = f a
    (Validated (mappend m m') a')

addError :: a -> String -> Validated a
addError a msg = Validated [msg] a

validate :: [a -> Validated a] -> a -> Validated a
validate vs a = foldl (>>=) (pure a) vs

presence :: String -> (a -> Maybe b) -> a -> Validated a
presence fieldName getter a =
  case getter a of
    (Just _) -> return a
    Nothing  -> addError a (fieldName ++ " cannot be blank")

size ::
     (Foldable t)
  => String
  -> (a -> Maybe (t b))
  -> (Int -> Bool)
  -> a
  -> Validated a
size msg getter p a =
  case getter a of
    (Just v) ->
      if p (length v)
        then addError a msg
        else return a
    Nothing -> return a
