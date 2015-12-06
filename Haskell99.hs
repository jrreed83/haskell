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
(.@.) :: [a]->Int->Maybe a
(.@.) list k
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

-- Problem 12: Decode a run length encoded array
decoding :: [Element a] -> [a]
decoding list =
  let
    loop ([]) (accum) = accum
    loop (Single h : rest) (accum) = loop (rest) (h:accum)
    loop ((Multiple a cnt) : rest) (accum) = loop (rest) (accum ++ (replicate (cnt) (a)))
  in
    loop (list) ([])

--Problem 14 :: Duplicate the elements of a list
dupli :: [a] -> [a]
dupli list =
  let
    loop (h:t) (accum) = loop (t) (h:h:accum)
    loop ([]) (accum) = reverseList accum
  in
    loop (list) ([])

-- Problem 15 :: Duplicate the elements of list a specified number of times
repli :: [a]->Int->[a]
repli list n =
  let
    loop (h:t) (n) (accumList) = loop (t) (n) (accumList ++ (replicate (n) (h)))
    loop ([]) (_) (accumList)  = accumList
  in
    loop (list) (n) ([])

-- Problem 16 :: Drop the n'th element from a list
dropItem :: [a] -> Int -> [a]
dropItem list n =
  let
    loop ([]) (_) (_) (accumList) = reverseList accumList
    loop (h:t) (n) (j) (accumList)
      | j == n = loop (t) (n) (1) (accumList)
      | j < n  = loop (t) (n) (j+1) (h:accumList)
  in
    loop (list) (n) (1) ([])

-- Problem 17 : Split a list into two parts, the length of first is given
split :: [a]->Int->([a],[a])
split list n =
  let
    loop (h:t) (j) (accum)
      | j == 0 = (reverseList(h:accum),t)
      | j >  0  = loop (t) (j-1) (h:accum)
    loop [] _ accum = (reverseList accum,[])
  in
    loop (list) (n-1) ([])

-- Problem 18 : Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice (list) (start) (stop)
  | start > stop = error "The start index cannot be larger than stop index"
  | otherwise = let loop ([]) (_) (_) (_) (accumList) = reverseList accumList
                    loop (h:t) (start) (stop) (n) (accumList)
                      | n < start = loop (t) (start) (stop) (n+1) (accumList)
                      | start <= n && n <= stop = loop (t) (start) (stop) (n+1) (h:accumList)
                      | n > stop = reverseList accumList
                in loop (list) (start) (stop) (1) ([])

--Problem 19 : Rotate a list N places to the left
rotate :: Int->[a]->[a]
rotate _ [] = []
rotate 0 list = list
rotate n list
  | n > 0 = let len = length list
                (left,right) = split (list) (n `mod` len)
            in  (right ++ left)
  | n < 0 = let len = length list
                (left,right) = split (list) ((len+n) `mod` len)
            in  (right ++ left)
--Problem 20: Remove the kth element from list
remove :: Int->[a]->[a]
remove k list
  | k < 1 || k > length list = list
  | k == 1 = tail list
  | otherwise = let (left,h:t) = split list (k-1) in (left ++ t)


--Problem 21 : Insert into a list (Should use split)
insertAt :: a->[a]->Int->[a]
insertAt x list j
  | j < 1 = list
  | j > (length list) = list
  | otherwise = let loop (x) (h:t) (j) (k) (accumList)
                      | k == j = (accumList ++ (x:h:t))
                      | k < j = loop (x) (t) (j) (k+1) (accumList ++ [h])
                in loop (x) (list) (j) (1) ([])

-- Problem 22: Create a list containing integers within a certain range
range :: Int->Int->[Int]
range start stop
  | start > stop = []
  | otherwise = [x | x <- [start..stop]]

-- Problem 23:

-- Problem 31: Determine if a number is prime.  We perform tail recursion, starting
--             from the square-root of the supplied number and decrementing
isPrime :: (Integral a)=>a->Bool
isPrime x
  | x <= 0 = False
  | otherwise = let loop x j
                      | j == 1 = True
                      | j > 1 && (mod x j) == 0 = False
                      | j > 1 && (mod x j) /= 0 = loop (x) (j-1)
                in loop (x) (start)
                  -- There is probably a better way to take the square-root of an Integral
                  -- type and casting to Integral type
                  where start = round $ sqrt $ fromIntegral $ x

-- Problem 32 : Find common common divisor using Euclid's algorithm
mygcd :: (Integral a)=> a->a->a
mygcd x y
  | x == y = x
  | x == 0 && y /= 0 = y
  | x /= 0 && y == 0 = x
  | x /= 0 && y /= 0 && x > y && (mod x y) == 0 = y
  | x /= 0 && y /= 0 && x > y && (mod x y) /= 0 = mygcd (mod x y) (y)
  | x /= 0 && y /= 0 && x < y && (mod y x) == 0 = x
  | x /= 0 && y /= 0 && x < y && (mod y x) /= 0 = mygcd (mod y x) (x)

-- Problem 33 : Determine if two numbers are coprime
coprime :: (Integral a)=> a->a->Bool
coprime x y = (mygcd x y == 1)
