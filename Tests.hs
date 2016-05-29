{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe (fromJust, isNothing)
import Test.QuickCheck

import Buffer

fromBuffer :: Buffer -> String
fromBuffer (bef, aft) = reverse bef ++ aft

newtype NoBefore = NoBefore Buffer deriving Show
newtype NoAfter = NoAfter Buffer deriving Show

instance Arbitrary NoBefore where
  arbitrary = do
    after <- arbitrary :: Gen String
    return $ NoBefore ("", after)

instance Arbitrary NoAfter where
  arbitrary = do
    before <- arbitrary :: Gen String
    return $ NoAfter (before, "")

-- EMPTY PROPS

prop_emptyBuffer :: Buffer -> Bool
prop_emptyBuffer b = (atLeft b && atRight b) == (b == empty)

-- CURSOR PROPS

prop_emptyBufferHasNothingOnCursor :: Bool
prop_emptyBufferHasNothingOnCursor = isNothing (cursor empty)

prop_cursorLeftGetsInsert :: Char -> Buffer ->Bool
prop_cursorLeftGetsInsert c b = (fromJust . cursor . left . insert c $ b) == c

-- INSERT PROPS

prop_insertIncreasesBuffer :: Char -> Buffer -> Bool
prop_insertIncreasesBuffer c buf =
  (length . fromBuffer . insert c $ buf) ==
    (length . fromBuffer $ buf) + 1

prop_insertDualDelete :: Char -> Buffer -> Bool
prop_insertDualDelete c b = (delete . insert c $ b) == b

prop_insertAndLeftDualRemove :: Char -> Buffer -> Bool
prop_insertAndLeftDualRemove c b = (remove . left . insert c) b == b

-- DELETE PROPS

prop_deleteIdempotentAtLeft :: NoBefore -> Bool
prop_deleteIdempotentAtLeft (NoBefore b) = delete b == b

prop_deleteDecreasesBuffer :: Buffer -> Property
prop_deleteDecreasesBuffer b = (not . atLeft $ b) ==>
  (length . fromBuffer . delete $ b) == (length . fromBuffer $ b) - 1

-- REMOVE PROPS

prop_removeIdempotentAtRight :: NoAfter -> Bool
prop_removeIdempotentAtRight (NoAfter b) = remove b == b

prop_removeDecreasesBuffer :: Buffer -> Property
prop_removeDecreasesBuffer b = (not . atRight $ b) ==>
  (length . fromBuffer . remove $ b) == (length . fromBuffer $ b) - 1

-- LEFT PROPS

prop_leftIdempotentAtLeft :: NoBefore -> Bool
prop_leftIdempotentAtLeft (NoBefore b) = left b == b

prop_leftDualRight :: Buffer -> Property
prop_leftDualRight b = (not . atLeft $ b) ==>
    (right . left $ b) == b

-- RIGHT PROPS

prop_rightIdempotentAtRight :: NoAfter -> Bool
prop_rightIdempotentAtRight (NoAfter b) = right b == b

prop_rightDualLeft :: Buffer -> Property
prop_rightDualLeft b = (not . atRight $ b) ==>
  (left . right $ b) == b

-- Quickcheck Setup

return []
runTests :: IO Bool
runTests = $quickCheckAll

-- Run all property tests

main :: IO Bool
main = runTests
