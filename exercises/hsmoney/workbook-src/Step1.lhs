
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1
    }

Create a new file Step1 with a module declaration.

\begin{code}
{-# LANGUAGE ScopedTypeVariables #-} -- We will explain why we need this later
module Step1 where
\end{code}

We need the Test.QuickCheck module to generate random data, and calculate properties.

\begin{code}
import Test.QuickCheck
\end{code}

Let's start with a simple example using some math functions, to introduce the basic concepts.
An invariant of the square root and square functions is:

a number's square is equal to that number: sqrt( x * x ) = x

In an example based test approach, we would write a small number of examples showing that this holds.
We now want to 'proof' that the mentioned invariant holds for all numbers.

Define the 'system under test'. We have a `squared` function, and we don't care that much yet about what type its input `x` is, so we leave it off for now.

\begin{code}
squared x = x * x
\end{code}

We want to show that for all numbers, the following holds:

sqrt (squared n) == n

So we define a property stating just that. We have to add a type signature here, so that QuickCheck can choose what kind of values to generate. At this stage it will compile just fine, compilation will fail once you use it with quickCheck, because there are several Floating instances to choose from. We choose Double (a large Real number).

\begin{code}
prop_SquareRootOfNSquaredEqualsN :: Double -> Bool
prop_SquareRootOfNSquaredEqualsN n = (sqrt (squared n)) == n
\end{code}

Starting it with 'prop_' is more than just a naming convention, we will use this later on to load all properties. You can load this into ghci and play around with it for some n.

And now comes the magic:

\begin{code}
main0 = quickCheck prop_SquareRootOfNSquaredEqualsN
\end{code}

Run it, In ghci, or call your function 'main' and use `cabal exec <filename>`. We number them here because we can only have one main, and we like to execute our documentation.

This will fail. After some number of steps. For me it failed after three:

*** Failed! Falsifiable (after 5 tests and 1080 shrinks):
5.0e-324

What happened? QuickCheck generates input test data and checks if the property holds
(returns true) for all the generated input values. If it finds a case
for which the property does not hold, it fails and returns the counter example.

How does it know to generate test data? QuickCheck will use the properties' type to generate data. Default generators are available for many built-in types such as numbers, strings and even functions. In this case it will generate Doubles, because that is what we specified in the type of the property.

QuickCheck tells us that the failing input is a very small number with mantissa 5 and exponent -324. What happens when we multiply and perform sqrt on such a small number?

Using QuickCheck for just a little bit, we realize that when we deal with floats, rounding is a problem. QuickCheck generates a really small number as value for `n`, very close to zero and then runs out of precision when doing (sqrt (squared n)).

This is annoying. Let's work around it by making our own `sqrti` function that rounds to the nearest integer, and write a new property for which QuickCheck will generate only integers. Otherwise we could round inside the property, but that would be very messy.

\begin{code}

sqrti :: Integral a => a -> a
sqrti = floor . sqrt . fromIntegral

prop_SqrtiOfSquaredNEqualsN :: Int -> Bool
prop_SqrtiOfSquaredNEqualsN n = (sqrti (squared n)) == n

\end{code}

Now it fails again: 

*** Failed! Falsifiable (after 3 tests and 1 shrink):
-1

You notice that the output mentions tests and shrinks. The number of tests and shrinks you see will be different. When it finds a counterexample, QuickCheck shrinks the input so that you get the `smallest` input that fails. In this case the smallest number it can find, for lists it is the shortest list etc.

The test fails because the invariant does not hold for negative numbers.
We should restrict the generated input to natural numbers only (>=0). We can do this by changing the type of our generator to NonNegative a, in our case NonNegative Int. NonNegative is a [Modifier](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Modifiers.html)

\begin{code}
prop_SqrPositive :: NonNegative Int -> Bool
prop_SqrPositive (NonNegative n) = (sqrti $ squared n) == n

main3 = quickCheck prop_SqrPositive

\end{code}

This way we can communicate our intention for the properties' input in its type.

We can also use generators explicitly inside the property to achieve a similar effect. You can find many available generators by browsing [the Test.QuickCheck.Gen module documentation](http://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Gen.html)

We could for instance say we are only interested in numbers between one and 100. For that we use the `choose` function together with forAll. Since we made the type of sqrti a bit general, we have to specify what Integral we want, so we choose (n :: Int).

\begin{code} 

smallPositiveInteger = choose (1,100)

prop_SqrSmallInt = forAll smallPositiveInteger $ \n -> (sqrti (squared n)) == (n :: Int)

main4 = quickCheck prop_SqrSmallInt
\end{code}

There is one subtlety we did not show, if you use forAll, the type of your property changes, it no longer results in a Bool, but a Property. We can declare it like this:

\begin{code} 
prop_SqrSmallInt :: Property
\end{code}


\begin{code}

prop_SqrPositiveForGreaterEqualsZero n = n >= 0 ==> (sqrti (squared n)) == (n :: Int) 

\end{code}

Here we show how to turn this into a small test suite by hand. This also acts as a nice summary for what we have just covered. We have written a deceptively simple function that, when it comes to testing, has a few edge cases we had to think about. Writing QuickCheck properties forced us to think about the type of input we can deal with, and what constrainst apply to that input. We have also played with a few ways to express constraints for a property.

\begin{code}
runTests = do
  quickCheck prop_SquareRootOfNSquaredEqualsN
  quickCheck prop_SqrSmallInt
  quickCheck prop_SqrPositive
  quickCheck prop_SqrtiOfSquaredNEqualsN
  quickCheck prop_SqrPositiveForGreaterEqualsZero
\end{code}

Writing a test suite in this way gets repetitive quickly. See PropertySuite (TODO: link) for how to generate a test suite automagically using Template Haskell.

