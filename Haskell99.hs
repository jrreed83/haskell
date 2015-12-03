
-- Problem 1: Find the last element of the list
lastElement :: [a] -> Maybe a
lastElement []    = Nothing
lastElement [h]   = Just h
lastElement (h:t) = lastElement t

-- Problem 2: Find the last but one element of list
lastButOneElement :: [a] -> Maybe a
lastButOneElement []    = Nothing
lastButOneElement [x,y] = Just x
lastButOneElement (h:t) = lastButOneElement (t)

--Problem 3: Find the k'th element of list
elementAt :: Int->[a]->Maybe a
elementAt k list
  | k < 0 = Nothing
  | otherwise = let loop (j) (k) (h:t)
                      | j == k = Just h
                      | j < k  = loop (j+1) (k) (t)
                    loop (_) (_) [] = Nothing
                in loop (0) (k) (list)

-- Problem 4: Find the number of elements in list
myLength :: [a] -> Int
myLength list =
  let loop accum [] = accum
      loop accum (h:t) = loop (accum+1) (t)
  in loop 0 list

-- Problem 5: Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList list =
  let loop (h:t) (accum) = loop (t) (h:accum)
      loop [] (accum) = accum
  in loop list []

-- Problem 6 : Check whether word in Palindrome
isPalindrome :: String -> Bool
isPalindrome str = str == reverseList str

-- Problem 7 :: Flatten a nested list structure (non tail-recursive)
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten nestedList =
  let
    loop (Elem x) (accum) = reverseList $ x : accum
    loop ((List ((Elem x) : [rest]))) (accum) = loop (rest) (x:accum)
  in
    loop (nestedList) ([])

-- Problem 8: Remove all duplicates from List
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress list =
  let
    loop [x] (accum) = reverseList $ x:accum
    loop (x:y:rest) (accum)
      | x == y = loop (y:rest) (accum)
      | x /= y = loop (y:rest) (x:accum)

  in
    loop (list) ([])

-- Problem 9: Pack consecutive repeats into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack list =
  let
    loop ([x]) (subList) (accumList) = reverseList $ (x:subList):accumList
    loop (x:y:rest) (subList) (accumList)
      | x == y = loop (y:rest) (x:subList) (accumList)
      | x /= y =  let finalSubList = reverseList $ x:subList
                  in  loop (y:rest) ([]) (finalSubList:accumList)
  in
    loop (list) ([]) ([])

-- Problem 10: Length encoding
encoding :: (Eq a) => [a] -> [(a,Int)]
encoding list =
  let
    loop ([x]) (count) (accumList) = reverseList $ (x,count):accumList
    loop (x:y:rest) (count) (accumList)
      | x == y = loop (y:rest) (count+1) (accumList)
      | x /= y = loop (y:rest) (1) ((x,count):accumList)
  in
    loop (list) (1) ([])

-- Problem 11 and Problem 13: Modified run length encoding
data Element a = Multiple {val::a,count::Int}
               | Single {val::a}
                 deriving Show
modEncoding :: (Eq a) => [a] -> [Element a]
modEncoding list =
  let
    loop ([x]) (count) (accumList)
      | count == 1 = reverseList $ (Single x):accumList
      | count > 1  = reverseList $ (Multiple x count):accumList
    loop (x:y:rest) (count) (accumList)
      | x == y = loop (y:rest) (count+1) (accumList)
      | x /= y && count == 1 = loop (y:rest) (1) (Single x : accumList)
      | x /= y && count > 1  = loop (y:rest) (1) ((Multiple x count): accumList)
  in
    loop (list) (1) ([])
