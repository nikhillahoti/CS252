 data Tree v =
     Empty
   | Node v (Tree v) (Tree v)
   deriving (Show)

 findT :: Ord v => v -> Tree v -> Maybe v
 findT _ Empty = Nothing
 findT v (Node val left right) =
   if val == v then
     Just val
   else if v < val then
     findT v left
   else
     findT v right
