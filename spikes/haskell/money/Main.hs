-- We use the ScopedTypedVariables extension so we can describe types right inside our property descriptions
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
import           Test.Tasty

import           Control.Monad         (liftM)
--import           Data.ByteString.Builder.Scientific
--import           Data.Scientific                    as Scientific
import           Control.Applicative   ((<$>), (<*>))
import           Data.Fixed
import           Data.Monoid           ((<>))
import           Debug.Trace
import           GHC.Generics
import           Test.Tasty.QuickCheck as QC
-- Money type
-- |We use a newtype here, so we have type safety, but can use the operations from the number inside amount
-- We want to add a currency, to save us some time during the tutorial we already call the field amount
--newtype Money = Money Float

--Tasty inspired by - http://documentup.com/feuerbach/tasty
--See also http://www.reddit.com/r/haskell/comments/25aagr/quickcheck2_vs_smallcheck_vs_smartcheck_etc
-- lazy smallcheck also worth a look, but no tasty plugin from the looks of it.

data CoinValue = Cent | FiveCent | TenCent | TwentyCent | FiftyCent | OneEuro deriving (Eq, Show, Read, Generic)

data Coin = Coin CoinValue
             deriving (Eq, Show,Read,Generic)

value :: Coin -> Amount
value (Coin c) = case c of
  Cent -> 1/100
  FiveCent -> 5 / 100
  TenCent -> 10 / 100
  TwentyCent -> 20 / 100
  FiftyCent -> 50 / 100
  OneEuro -> 100

data CoinBox = CoinBox { inbox  :: [Coin]
                         ,vault :: [Coin]
                       } deriving (Show)

instance Arbitrary Coin where
  arbitrary = elements (map Coin [Cent, TenCent])

instance Arbitrary CoinBox where
  arbitrary = CoinBox <$> arbitrary <*> arbitrary

data Checkout = Checkout { coinBox' :: CoinBox } -- new coinbox and result, is checkout allowed, and how much money is returned

-- Note that we normally start with inserting coins and increasing balance, but the easier property to write is for checkout it seems
-- Also drives a Checkout data type, so we can indicate success and a value or failure, but be specific about it
-- but before we implement checkout and its properties, we need to state that the inbox and the vault always have a positive balance
checkout :: CoinBox -> Checkout
checkout (CoinBox i v) = Checkout $ CoinBox [] (v <> i)

values :: [Coin] -> Amount
values = sum . map value

moneyInserted :: CoinBox -> Amount
moneyInserted (CoinBox i _ ) = values i

balance :: CoinBox -> Amount
balance (CoinBox _ v ) = values v

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

-- tests pass for rational, but representation is not going to be very nice
type Amount = Rational

-- this will change when people don't pay with exact amount and we give change
propSumOfBalancesSameOnCheckout :: CoinBox -> Bool
propSumOfBalancesSameOnCheckout c = s' == s
  where
    s = (moneyInserted c) + (balance c)
    s' = (moneyInserted c') + (balance c')
    c' = coinBox' $ checkout c

propInboxZeroAfterCheckout :: CoinBox -> Bool
propInboxZeroAfterCheckout c = (moneyInserted c') == 0
  where
    c' = coinBox' $ checkout c


propPositiveBalanceInInbox :: CoinBox -> Bool
propPositiveBalanceInInbox c = (moneyInserted c) >= 0

propPositiveBalanceInVault :: CoinBox -> Bool
propPositiveBalanceInVault c = (balance c) >= 0

multiplyAndDivide :: Amount -> Amount -> Amount
multiplyAndDivide a b = (a * b) / b

-- Debug.Trace tip for QuickCheck : http://stackoverflow.com/questions/2517152/verbosecheck-in-quickcheck-2
-- for when you can't use verboseCheck (tasty hides it)
propMultiplyAndDivide :: Amount -> NonZero Amount -> Bool
propMultiplyAndDivide v (NonZero n) = ts $ result == v
                                        where result = (multiplyAndDivide v n)
                                              ts = traceShow $ "(" <> show v <> "," <> show result <> ")"

-- | If you do this a lot in your application, it might be worth making a ReadAndShow typeclass with a default implementation of ReadAndShow t, so you can reuse the readAndShow property over a number of implementations, because you can define the property against the typeclass
readAndShowAmount :: AmountDisplay -> AmountDisplay
readAndShowAmount = read . show

-- following "Tip: Using newtype" in http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
newtype AmountDisplay = AmountDisplay String deriving (Show, Read, Eq)

-- do notation for arbitrary - http://stackoverflow.com/questions/16440208/how-to-generate-arbitrary-instances-of-a-simple-type-for-quickcheck
-- handy because we can choose Int but need to generate an AmountDisplay through Show-ing String
instance Arbitrary AmountDisplay where
  arbitrary = do
    x :: Int <- choose (-10000,10000)
    decimals :: Int <- choose (0,99)
    return $ AmountDisplay ((show x) <> "." <> (show decimals))

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty  "Multiply and then divide Amount by N should yield Amount" $ propMultiplyAndDivide
   ,QC.testProperty "Convert to and from String the same String" $
      \s  -> readAndShowAmount s == s
   ,QC.testProperty "Positive Balance in Inbox " $ propPositiveBalanceInInbox
   ,QC.testProperty "Positive Balance in Vault" $ propPositiveBalanceInVault
   ,QC.testProperty "Balances in Inbox and Vault add up to same amount before and after checkout" $ propSumOfBalancesSameOnCheckout
   ,QC.testProperty "Inbox zero on checkout" $ propInboxZeroAfterCheckout
   ]

