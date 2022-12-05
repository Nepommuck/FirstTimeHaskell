data Tree a = EmptyTree a |
            Node a (Tree a) (Tree a) 
            deriving(Eq, Ord, Show, Read)


-- printNode :: Show a => Tree a -> String
printNode (EmptyTree a) = ""
printNode (Node a lt rt) =  show a


main = do
    let t = EmptyTree 0
    let t2 = EmptyTree 0
    let b = Node 5 t t2

    print $ printNode t
    print $ printNode b
