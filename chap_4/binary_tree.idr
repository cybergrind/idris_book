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
