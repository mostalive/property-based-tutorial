Property Based Testing Hands On - Haskell money exercise
(c) QWAN - Quality Without a Name - www.qwan.eu
May 2015

- Rob Westgeest - rob@qwan.eu
- Marc Evers - marc@qwan.eu
- Willem van den Ende - willem@qwan.eu

Modelling Money

In this exercise, we are going to build a class representing Money, step by step. 
Representing money in software might sound easy, but it is actually non trivial and is often done wrong. 
Doing it wrong can result in small or big rounding errors or just losing
a few millions on a bad day...

Getting started

Open the Money.hs file, and add the quickcheck boilerplate, and import fmap and sequential application from Control.Appliccative:

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for Arbitrary Amount and Currency, and to add Amounts
{-# LANGUAGE TemplateHaskell #-} -- to generate a test suite as explained in TODO REF
module Money where
import Test.QuickCheck
import Test.QuickCheck.Property ((===))
import Language.Haskell.TH
import Control.Applicative ((<$>), (<*>)) -- to generate Arbitrary instances
\end{code}

Create a GADT to represent money. It is constructed with an
amount and a currency. This is a good moment to think of the types of amount and currency. (TODO from quickCheck manual: use newtypes ??)

We learnt in the introduction that QuickCheck could easily force us to change our mind about the types that we use. Primitive obsession (using Strings, Ints and other primitive data types instead of types we define ourselves) is painful enough. Lets keep our options open and define an Amount as an Int, meaning a number in cents. For the purpose of this exercise we don't want to get into rounding errors again, we've been there already today. The Currency will be represented as a String.


\begin{code}
newtype Amount = Amount {amountValue :: Int} deriving (Arbitrary, Num, Eq, Show)
newtype Currency = Currency { currencyValue :: String} deriving (Eq,Show) 
\end{code}

For Amount we use GeneralizedNewTypeDeriving, so we can derive an Arbitrary instance to generate arbitrary amounts. The Eq and Show instance are needed so our properties can compare amounts, and quickcheck can show us the failures.

Currency has a string on the inside. We could of course do this with a proper datatype, but for this tutorial the strings make it a bit more interesting, because we don't want arbitrary strings, we want strings representing a currency.

\begin{code}
data Money = Money { currency :: Currency
                     ,amount :: Amount
                   } deriving (Eq, Show)

\end{code}

Choosing explicit data types also will help us generate very specific arbitrary data with QuickCheck.

Start with something simple: when we add two amounts of the same
currency, we get a new amount. Our property needs two money objects, so
we pass two arbitraries to forall and define a property function with two
arguments:

\begin{code} 
-- adding a generator for same pairs of money is probably easier, than filtering and demonstrates use of monad.

prop_moneyOfSameCurrencyAddsUp m1@(Money _ (Amount a1))
                               m2@(Money _ (Amount a2)) = (a1 + a2) === (total mtotal)
  where total (Right money) = amountValue $ amount $ money
        total (Left msg)    = -4242424242424242 -- TODO we should throw exception here, or find a less obtrusive way of getting the left
        mtotal = addMoney m1 m2
\end{code}

Now we have to generate arbitrary money. We'll do this in small steps, a bit smaller than we would do in production code, so you can see another example of a custom generators as well as Arbitrary instances for more complex data types.

QuickCheck can generate random amounts for us without our intervention. But we don't want random strings for currencies! We can generate the currency using the `elements` generator:

\begin{code}
currencies :: Gen Currency
currencies = elements(map Currency ["EUR", "USD", "GBP"]);
\end{code}

How do we generate Money instances? We define an instance of the Arbitrary typeclass and create an implementation for its' arbitrary generator. Arbitrary is what provides shrinking behaviour etc, but the only function we _must_ implement is arbitrary.

We use the Applicative instance for Arbitrary to construct Money, because there is no dependency between currency and arbitrary. We could have used the Monad instance here (as you will see in the original quickCheck paper), but that would have been overkill. The Appliccative instance makes it almost look like we are just constructing a record the regular way. This makes it quite easy to create Arbitrary instances for all our domain objects.

\begin{code}
instance Arbitrary Money where
  arbitrary = Money <$> currencies <*> arbitrary
\end{code}

We could also define an Arbitrary for currency like this,

\begin{code}
instance Arbitrary Currency where
  arbitrary = elements(map Currency ["EUR", "USD", "GBP"])
\end{code}

or simply reusing the 'currencies' generator we defined above.

We could have defined our Currency also as an Arbitrary, so we could have written 

Now we can generate Money objects!

Now add the behaviour that Money can only be added to money of the same
currency. Throw an exception when the currencies are different. Make
sure your property handles this correctly.

TODO: hide implementation with tex / lhs to make it painting by numbers

\begin{code} 

newtype ErrorMessage = ErrorMessage { errorMsg :: String }

addMoney :: Money -> Money -> Either ErrorMessage Money
addMoney (Money c1 a1) (Money c2 a2) = case sameCurrency of
                                         True -> Right (Money c1 (a1 + a2))
                                         _    -> Left $ ErrorMessage "incompatible currencies"
  where sameCurrency = c1 == c2
\end{code}

Add a new property that checks that adding is not possible whenever the
currencies are different.

== Money allocation

We want to be able to allocate money: divide it in equal
parts, without money getting lost in rounding. 
An example: dividing €100 in 3 should yield [€33, €33, €34] (assuming whole numbers for now).

Define a property that states that no money gets lost when dividing an
amount.

Note that we should make the integer assumption of the amounts more
explicit, otherwise Javascript will just use doubles.

\begin{code} 
{- function Money(amount, currency) {
  this.amount = Math.floor(amount);
  ...
-}
\end{code}

== Converting from string

We want to be able to create money objects from strings; we support EUR
and USD. Some legal values:
- 100 (default is EUR)
- EUR 10
- USD 300
- USD 40- (negative value)
- EUR 10.000 (. as thousand separator for euros)

And some illegal values:

- USD 4.999 (USD has , as thousand separator and . as cents separator)
- 1,000 (illegal because EUR is default and EUR uses . as thousand
  separator)
- HFL 50 (illegal currency)
- EUR 10bla

In our domain model, we work with valid objects. This part focuses on
processing and validation outside, 'untrusted' data that is transformed
into domain objects (e.g. data coming in from a web form). We want this transformation to be robust, so that it can
handle any data and only results in valid domain objects when it is able
to (and we don't need to do defensive programming in our domain).

There are actually two concerns we need to address:

- Given the input string is valid, will our code create a valid Money
  object with the correct contents? (correctness)
- Is the conversion code robust for every possible input string? The
  code should always either return a valid Money object or a rejection of the input. 
  It should not throw unexpected exceptions or create invalid Money
  objects (robustnesss)

Let's capture these in properties.

Define conversion function that returns a JSON object that is either { money: <valid money
object> } or { error: 'some error message' }

Start e.g. with something like:

\begin{code}
{- function moneyFromString(input) {
  var theMoney = new Money(...);
  return { money: theMoney };
-}
\end{code}

We can start with either correctness or robustness first. It would be
interesting to try both approaches and see whether and how it drives you
to different design decisions.

=== Correctness

Given correct input, the conversion should produce valid Money objects. 
How would you define this as property? 

Write an arbitrary that creates valid input strings. You can again use smap to create valid strings
from primitive values.

Hints and tips:
- If you let your arbitrary generate a tuple (array) of an input string together
  with the corresponding amount and currency values, it will be easier.
  for your property function to check correctness: __['EUR 100', 'EUR', 100]__
- Take baby steps; start e.g. with regular input with a currency; add support for the default currency; add
  support for thousand separators.
- Make sure your test fails for the right reason at every step. 

=== Robustness

Given any input, the conversion should always produce either a valid
Money object or a validation error: { money: ... } or { error: 'some message' }

Define a property that captures this.

Write an arbitrary that creates all kinds of input strings. Is the
'asciistring' arbitrary suited for this? Why (not)? 

Corner cases that almost look like valid input are particularly
interesting, like "EUR 12jsde", "HFL 30", and "300 EUR". Adapt your arbitrary so that 
it will generate cases like these.

Refine your conversion function and make sure the correctness property
is also satisfied.

Hints and tips:
- jsverify provides the 'oneof' function that combines two arbitraries
  into one that generates values taking values from both arbitraries.

=== Reflection

How did defining the properties and the arbitraries influence you in
thinking about the problem?

Where would you like to put the property logic? To what extent is it
test code, to what extent should/could it be part of production code?

What happens if you would start with robustness first and then correctness? 
Do the exercise again, but start with robustness and see if this leads
you to different design decisions.

We end with the usual boilerplate to generate tests with TemplateHaskell.
\begin{code}
return [] -- needed in GHC 7.8 series, not after
runTests = $quickCheckAll
\end{code}

