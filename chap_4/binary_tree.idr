module binary_tree

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1


insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x original@(Node tree y tree1)
       = case compare x y of
              LT => Node (insert x tree) y tree1
              EQ => original
              GT => Node tree y (insert x tree1)


listToTree : Ord elem => List elem -> Tree elem
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)


treeToList : Ord elem => Tree elem -> List elem
treeToList Empty = []
treeToList (Node tree x tree1) = treeToList tree ++ [x] ++ treeToList tree1
