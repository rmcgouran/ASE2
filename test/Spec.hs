{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, sort, nubBy)
import Test.Hspec
import BinaryTree
import Test.QuickCheck (Arbitrary(..), Gen, sized, oneof, resize, property)

instance Arbitrary a => Arbitrary (BST a) where
  arbitrary = sized arbitraryBST

arbitraryBST :: forall a. Arbitrary a => Int -> Gen (BST a)
arbitraryBST 0 = return Leaf
arbitraryBST n = do
  key <- arbitrary
  value <- arbitrary
  left <- resize (n `div` 2) arbitrary
  right <- resize (n `div` 2) arbitrary
  let tree = Node key value left right
  return $ makeUniqueKeys tree

makeUniqueKeys :: BST a -> BST a
makeUniqueKeys tree = foldr (\(k, v) acc -> addItem k v acc) Leaf keyValueList
  where
    keyValueList = nubBy (\(k1, _) (k2, _) -> k1 == k2) $ treeToList tree

treeToList :: BST a -> [(Int, a)]
treeToList Leaf = []
treeToList (Node k v l r) = treeToList l ++ [(k, v)] ++ treeToList r

main :: IO ()
main = hspec $ do
    describe "BinaryTree" $ do
        describe "newEmptyBST" $
            it "creates an empty binary search tree" $
                newEmptyBST `shouldBe` (Leaf :: BST Int)

        describe "newBSTFromItem" $
            it "creates a binary search tree from a single item" $
                newBSTFromItem 5 "Apple" `shouldBe` Node 5 "Apple" Leaf Leaf

        describe "newBSTFromList" $ do
            it "creates a binary search tree from a list of key-value pairs" $
                newBSTFromList [(5, "Apple"), (2, "Banana"), (8, "Cherry")] `shouldBe`
                    Node 5 "Apple"
                        (Node 2 "Banana" Leaf Leaf)
                        (Node 8 "Cherry" Leaf Leaf)

            it "creates an empty binary search tree from an empty list" $
                newBSTFromList [] `shouldBe` (Leaf :: BST String)

        describe "addItem" $
            it "adds an item to the binary search tree" $ do
                let tree = newBSTFromItem 5 "Apple"
                addItem 2 "Banana" tree `shouldBe`
                    Node 5 "Apple"
                        (Node 2 "Banana" Leaf Leaf)
                        Leaf

        describe "addList" $
            it "adds a list of key-value pairs to the binary search tree" $ do
                let tree = newBSTFromItem 5 "Apple"
                addList [(2, "Banana"), (8, "Cherry")] tree `shouldBe`
                    Node 5 "Apple"
                        (Node 2 "Banana" Leaf Leaf)
                        (Node 8 "Cherry" Leaf Leaf)

        describe "lookupKey" $
            it "finds the value associated with a given key" $ do
                let tree = newBSTFromItem 5 "Apple"
                lookupKey 5 tree `shouldBe` Just "Apple"

        describe "deleteItem" $
            it "removes an item with a given key" $ do
                let tree = newBSTFromItem 5 "Apple"
                deleteItem 5 tree `shouldBe` Leaf

        describe "bstToList" $
            it "returns a list of keys in the binary search tree" $ do
                let tree = newBSTFromList [(5, "Apple"), (2, "Banana"), (8, "Cherry")]
                bstToList tree `shouldBe` [2, 5, 8]

        describe "property-based tests" $ do
            it "addItem and lookupKey" $ property $
                \key value (tree :: BST String) ->
                    lookupKey key (addItem key value tree) == Just value

            it "bstToList is sorted" $ property $
                \list -> let tree =                 newBSTFromList (list :: [(Int, String)])
                         in bstToList tree == sort (nub (map fst list))

        describe "additional tests" $ do
            it "can't find items not in the dictionary" $ property $
                \key (tree :: BST String) ->
                    lookupKey key (deleteItem key tree) == Nothing

            it "can find every item individually" $ property $
                \list -> let tree = newBSTFromList (list :: [(Int, String)])
                             keys = map fst list
                         in all (\key -> lookupKey key tree /= Nothing) keys

            it "can delete an item when given a key and maintain order" $ property $
                \key (tree :: BST String) ->
                    let tree' = deleteItem key tree
                    in bstToList tree' == sort (nub (filter (/= key) (bstToList tree)))

            it "can delete all items in the dictionary in reverse order" $ property $
                \list -> let tree = newBSTFromList (list :: [(Int, String)])
                             keys = reverse . sort . nub . map fst $ list
                             emptyTree = foldr deleteItem tree keys
                         in emptyTree == (Leaf :: BST String)

            it "can delete all items in the dictionary in a random order" $ property $
                \list -> let tree = newBSTFromList (list :: [(Int, String)])
                             keys = nub . map fst $ list
                             emptyTree = foldr deleteItem tree keys
                         in emptyTree == (Leaf :: BST String)

