module Tests () where

import Buffer

out :: Buffer -> String
out (bef, aft) = reverse bef ++ aft

prop_insertIncreasesBuffer :: Char -> Buffer -> Bool
prop_insertIncreasesBuffer c buf =
  (length . out . insert c $ buf) ==
    (length . out $ buf) + 2
