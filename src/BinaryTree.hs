module BinaryTree (
    addItem,
    addList,
    bstToList,
    deleteItem,
    lookupKey,
    newBSTFromItem,
    newBSTFromList,
    newEmptyBST,
    testTree,
    BST(..)
) where


data BST item = Leaf
                | Node Int item (BST item) (BST item)
                deriving (Show, Eq)

testTree :: BST String
testTree =
    Node 22 "Mary"
        (Node 0 "Harry"
            (Node 4 "Steve"
                (Node (-1) "Ed" Leaf Leaf)
                (Node 1 "Will" Leaf Leaf)
            )
            (Node 9 "Joseph"
                Leaf
                (Node 19 "Henry" Leaf Leaf)
            )
        )
        (Node 37 "Vicky"
            (Node 26 "Charley"
                (Node 24 "Jim"
                    (Node 23 "Liz" Leaf Leaf)
                    Leaf
                )
                (Node 31 "Anne" Leaf Leaf)
            )
            (Node 42 "John" Leaf Leaf)
        )

newEmptyBST :: BST item
newEmptyBST = Leaf

newBSTFromItem :: Int -> item -> BST item
newBSTFromItem key value = Node key value Leaf Leaf

newBSTFromList :: Eq item => [(Int, item)] -> BST item
newBSTFromList [] = newEmptyBST
newBSTFromList items = addList items newEmptyBST

addItem :: Int -> item -> BST item -> BST item
addItem key item Leaf = Node key item Leaf Leaf
addItem key item tree@(Node rootKey rootItem left right)
    | key == rootKey = Node key item left right
    | key < rootKey = Node rootKey rootItem (addItem key item left) right
    | key > rootKey = Node rootKey rootItem left (addItem key item right)

addList :: Eq item => [(Int, item)] -> BST item -> BST item
addList [] tree = tree
addList items Leaf = addList (tail items) (uncurry addItem (head items) newEmptyBST)
addList items tree = addList (tail items) (uncurry addItem (head items) tree)

lookupKey :: Int -> BST item -> Maybe item
lookupKey soughtKey Leaf = Nothing
lookupKey soughtKey node@(Node key item left right)
    | soughtKey < key = lookupKey soughtKey left
    | soughtKey > key = lookupKey soughtKey right
    | otherwise = Just item

deleteItem :: Int -> BST item -> BST item
deleteItem key Leaf = Leaf
deleteItem key node@(Node rootKey rootItem left right)
    | key == rootKey = removeNode node
    | key < rootKey = Node rootKey rootItem (deleteItem key left) right
    | key > rootKey = Node rootKey rootItem left (deleteItem key right)

removeNode :: BST item -> BST item
removeNode node@(Node key item Leaf Leaf) = Leaf
removeNode node@(Node key item Leaf right) = right
removeNode node@(Node key item left Leaf) = left
removeNode node@(Node key item left right) =
    Node newKey newItem left newRight where
        (newKey, newItem, newRight) = removeMinimumNode right

removeMinimumNode :: BST item -> (Int, item, BST item)
removeMinimumNode node@(Node key item Leaf right) = (key, item, right)
removeMinimumNode node@(Node key item left right) =
    let (minKey, minItem, newLeft) = removeMinimumNode left
    in (minKey, minItem, Node key item newLeft right)

bstToList :: Eq item => BST item -> [Int]
bstToList tree@(Node key _ left right) = bstToList left ++ [key] ++ bstToList right
bstToList Leaf = []


