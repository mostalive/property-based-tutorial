{-# LANGUAGE ScopedTypeVariables #-} -- We will explain why we need this later
module GettingStarted (runTests) where
import Test.QuickCheck

payDutch :: Int -> Int -> [Int]
payDutch amount people = take people $ repeat amount

prop_dutchLength :: NonNegative Int -> NonNegative Int -> Property
prop_dutchLength (NonNegative n) (NonNegative len) = (length $ payDutch n len) === len

runTests :: IO ()
runTests = do
  quickCheck prop_dutchLength

