{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, sort)
import Test.Hspec
import BinaryTree
import Test.QuickCheck (Arbitrary(..), Gen, sized, oneof, resize, property)


instance Arbitrary a => Arbitrary (BST a) where
  arbitrary = sized arbitraryBST

arbitraryBST :: Arbitrary a => Int -> Gen (BST a)
arbitraryBST 0 = return Leaf
arbitraryBST n = do
  key <- arbitrary
  value <- arbitrary
  left <- resize (n `div` 2) arbitrary
  right <- resize (n `div` 2) arbitrary
  oneof [return Leaf, return (Node key value left right)]

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
                \list -> let tree = newBSTFromList (list :: [(Int, String)])
                         in bstToList tree == sort (nub (map fst list))
