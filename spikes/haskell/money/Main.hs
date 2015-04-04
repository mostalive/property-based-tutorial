{-# LANGUAGE ScopedTypeVariables #-}
import           Test.Tasty

--import           Data.ByteString.Builder.Scientific
--import           Data.Scientific                    as Scientific
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
type Amount = Rational

multiplyAndDivide :: Amount -> Amount -> Amount
multiplyAndDivide a b = (a * b) / b

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty  "multiply an amount by N and dividing it by N should yield amount" $
      \(v , n ) -> n > 0.0 ==> (multiplyAndDivide v n) == v
  ]
