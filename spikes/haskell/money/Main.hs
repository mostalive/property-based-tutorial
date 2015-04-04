-- We use the ScopedTypedVariables extension so we can describe types right inside our property descriptions
{-# LANGUAGE ScopedTypeVariables #-}
import           Test.Tasty

--import           Data.ByteString.Builder.Scientific
--import           Data.Scientific                    as Scientific
import           Data.Fixed
import           Test.Tasty.QuickCheck as QC
-- Money type
-- |We use a newtype here, so we have type safety, but can use the operations from the number inside amount
-- We want to add a currency, to save us some time during the tutorial we already call the field amount
--newtype Money = Money Float

--Tasty inspired by - http://documentup.com/feuerbach/tasty
--See also http://www.reddit.com/r/haskell/comments/25aagr/quickcheck2_vs_smallcheck_vs_smartcheck_etc
-- lazy smallcheck also worth a look, but no tasty plugin from the looks of it.

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

-- tests pass for rational, but representation is not going to be very nice
type Amount = Centi

multiplyAndDivide :: Amount -> Amount -> Amount
multiplyAndDivide a b = (a * b) / b

-- See the difference with the first step where we used a custom constraint with =>
propMultiplyAndDivide :: Amount -> NonZero Amount -> Bool
propMultiplyAndDivide v (NonZero n) = (multiplyAndDivide v n) == v

-- | If you do this a lot in your application, it might be worth making a ReadAndShow typeclass with a default implementation of ReadAndShow t, so you can reuse the readAndShow property over a number of implementations, because you can define the property against the typeclass
readAndShowAmount :: String -> String
readAndShowAmount s = show a
                  where a = read s :: Amount

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty  "Multiply and then divide Amount by N should yield Amount" $ propMultiplyAndDivide
   ,QC.testProperty "Convert to and from String the same String" $
      \s  -> readAndShowAmount s == s
  ]

