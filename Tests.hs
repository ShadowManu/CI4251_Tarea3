module Tests () where

import Test.QuickCheck

import Buffer

fromBuffer :: Buffer -> String
fromBuffer (bef, aft) = reverse bef ++ aft

prop_insertIncreasesBuffer :: Char -> Buffer -> Bool
prop_insertIncreasesBuffer c buf =
  (length . fromBuffer . insert c $ buf) ==
    (length . fromBuffer $ buf) + 1

prop_deleteDecreasesBuffer :: Buffer -> Property
prop_deleteDecreasesBuffer b = (not . atLeft $ b) ==>
  (length . fromBuffer . delete $ b) == (length . fromBuffer $ b) - 1

prop_removeDecreasesBuffer :: Buffer -> Property
prop_removeDecreasesBuffer b = (not . atRight $ b) ==>
  (length . fromBuffer . remove $ b) == (length . fromBuffer $ b) - 1

prop_insertDualDelete :: Char -> Buffer -> Bool
prop_insertDualDelete c b = (delete . insert c $ b) == b

prop_insertAndLeftDualRemove :: Char -> Buffer -> Bool
prop_insertAndLeftDualRemove c b = (remove . left . insert c) b == b

prop_leftDualRight :: Buffer -> Property
prop_leftDualRight b = (not . atLeft $ b) ==>
    (right . left $ b) == b

prop_rightDualLeft :: Buffer -> Property
prop_rightDualLeft b = (not . atRight $ b) ==>
  (left . right $ b) == b

prop_emptyBuffer :: Buffer -> Bool
prop_emptyBuffer b = (atLeft b && atRight b) == (fromBuffer b == "")
