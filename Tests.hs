import Data.Maybe (fromJust)
import Test.QuickCheck

import Buffer

fromBuffer :: Buffer -> String
fromBuffer (bef, aft) = reverse bef ++ aft

newtype NoBefore = NoBefore { fromNoBefore :: Buffer }
newtype NoAfter = NoAfter { fromNoAfter :: Buffer}

instance Arbitrary NoBefore where
  arbitrary = do
    after <- arbitrary :: Gen String
    return $ NoBefore ("", after)

-- EMPTY PROPS

prop_emptyBuffer :: Buffer -> Bool
prop_emptyBuffer b = (atLeft b && atRight b) == (b == empty)

-- CURSOR PROPS

prop_emptyBufferHasNothingOnCursor :: Bool
prop_emptyBufferHasNothingOnCursor = cursor empty == Nothing

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

prop_deleteDecreasesBuffer :: Buffer -> Property
prop_deleteDecreasesBuffer b = (not . atLeft $ b) ==>
  (length . fromBuffer . delete $ b) == (length . fromBuffer $ b) - 1

-- REMOVE PROPS

prop_removeDecreasesBuffer :: Buffer -> Property
prop_removeDecreasesBuffer b = (not . atRight $ b) ==>
  (length . fromBuffer . remove $ b) == (length . fromBuffer $ b) - 1

-- LEFT PROPS

prop_leftDualRight :: Buffer -> Property
prop_leftDualRight b = (not . atLeft $ b) ==>
    (right . left $ b) == b

-- RIGHT PROPS

prop_rightDualLeft :: Buffer -> Property
prop_rightDualLeft b = (not . atRight $ b) ==>
  (left . right $ b) == b


main :: IO ()
main = do
  -- EMPTY PROPS
  quickCheck prop_emptyBuffer
  -- CURSOR PROPS
  quickCheck prop_emptyBufferHasNothingOnCursor
  quickCheck prop_cursorLeftGetsInsert
  -- INSERT PROPS
  quickCheck prop_insertIncreasesBuffer
  quickCheck prop_insertDualDelete
  quickCheck prop_insertAndLeftDualRemove
  -- DELETE PROPS
  quickCheck prop_deleteDecreasesBuffer
  -- REMOVE PROPS
  quickCheck prop_removeDecreasesBuffer
  -- LEFT PROPS
  quickCheck prop_leftDualRight
  -- RIGHT PROPS
  quickCheck prop_rightDualLeft
