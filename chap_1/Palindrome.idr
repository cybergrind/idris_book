module Palindrome

export
palindrome: Nat -> String -> Bool
palindrome n st = let s = toLower st in
           length (s) > n && s == (reverse s)
           
           
counts: String -> (Nat, Nat)
counts s = (length (words s), length s)


top_ten: Ord a => List a -> List a
top_ten lst = take 10 (reverse $ sort lst)


over_length: Nat -> List String -> Nat
over_length n ls = length $ filter (\ s => (length s) > n) ls

|||some function
|||@s some variable
all_lengths: (s: List String) -> List Nat
all_lengths [] = []
all_lengths (word :: words) = length word :: all_lengths words


isEven: Nat -> Bool 
isEven Z = True
isEven (S k) = not (isEven k)
