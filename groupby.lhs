This snippet deals with grouping data in lists.

> import Data.List


A HashTree describes a recursive tree where each branch has an associated
look-up key. Each branch can have any number of "limbs" (unlike a binary tree,
where each branch must have exactly two limbs). Each leaf is a list of values,
which all match the key of the branch in which the leaf resides.

> data HashTree k v = Leaf [v] | Branch [(k, HashTree k v)]
>      deriving (Show)


This will pretty-print a HashTree using a single space as indentation.

> printHashTree (Leaf xs) = show xs
> printHashTree (Branch xs) = subbranch 0 (Branch xs)
>     where
>         subbranch _ (Leaf []) = ""
>         subbranch ident (Leaf xs) = replicate ident ' ' ++ show xs
>         subbranch _ (Branch []) = ""
>         subbranch ident (Branch ((k,tree):xs)) =
>             replicate ident ' ' ++ show k ++ "\n" ++
>             subbranch (ident + 1) tree ++ remaining xs
>             where
>                 remaining [] = ""
>                 remaining xs = "\n" ++ printHashTree (Branch xs)


The groupby functions take one or many grouping functions and a list. The
grouping functions can create a look-up key from any item in the list.


groupby1 will group a list by one grouping function. Since it only takes one
grouping function, it doesn't really need the HashTree (since the output is not
recursive).

> groupby1 :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
> groupby1 _ [] = []
> groupby1 f xs = combine (map makeKeyValue xs)
>     where makeKeyValue v = (f v, v)


groupby will recursively group a list by any number of grouping functions and
return the output as a HashTree.

> groupby :: (Eq k) => [(v -> k)] -> [v] -> HashTree k v
> groupby _ [] = Leaf []
> groupby [] xs = Leaf xs
> groupby (f:fs) xs = Branch (map subgroup layer)
>     where layer = combine (map makeKeyValue xs)
>           subgroup (k, v) = (k, groupby fs v)
>           makeKeyValue v = (f v, v)


combine is just a helper function. It takes a list of key/value pairs and
returns a list of key/list-of-values pairs. The input list may have the same key
repeated multiple times. The output will collect all pairs that have the same
key and combine them into a single key/value pair, where each output value is a
list of all the input values with the same key.

> combine :: (Eq k) => [(k,v)] -> [(k,[v])]
> combine [] = []
> combine ((k,v):xs) = (k, v:values) : (combine nonmatching)
>     where
>         (matching, nonmatching) = partition ((k==) . fst) xs
>         values = map snd matching
