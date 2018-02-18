module Validator where

validate :: [a -> [String]] -> a -> [String]
validate vs a = concat $ map ($a) vs

presence :: String -> (a -> Maybe b) -> a -> [String]
presence fieldName getter a =
  case getter a of
    (Just _) -> []
    Nothing  -> [fieldName ++ " cannot be blank"]

size ::
     (Foldable t)
  => String
  -> (a -> Maybe (t b))
  -> (Int -> Bool)
  -> a
  -> [String]
size msg getter p a =
  case getter a of
    (Just v) ->
      if p (length v)
        then [msg]
        else []
    Nothing -> []
