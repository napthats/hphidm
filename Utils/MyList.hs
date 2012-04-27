module Utils.MyList
       (
         filterMaybe,
       ) where

filterMaybe :: [Maybe a] -> [a]
filterMaybe list =
  map (\just_x -> case just_x of Nothing -> undefined; Just x -> x) $
  filter (\maybe_x -> case maybe_x of Nothing -> False; Just _ -> True) list
  