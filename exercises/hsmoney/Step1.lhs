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

We need the Test.QuickCheck module to generate random data, we will show how to use Debug.Trace to understand what values quickCheck generates.

\begin{code}
import Test.QuickCheck
import Debug.Trace (traceShow)
\end{code}

Let's start with a simple example using some math functions, to introduce the basic concepts.
An invariant of the square root and square functions is:

a number's square is equal to that number: sqrt( x * x ) = x

In an example based test approach, we would write a small number examples showing that this holds.
We now want to 'proof' that the mentioned invariant holds for all numbers.


Define the 'system under test'

\begin{code}
squared x = x * x
\end{code}

We want to show that for all numbers, the following holds:

sqrt (squared n) == n

So we define a property stating just that. We have to add a type signature here, so that QuickCheck can choose what kind of values to generate. At this stage it will compile just fine, compilation will fail once you use it with quickCheck, because there are several Floating instances to choose from.

\begin{code}
propSquareRootOfNSquaredEqualsN :: Double -> Bool 
propSquareRootOfNSquaredEqualsN n = sqrt (squared n) == n
\end{code}

Starting it with 'prop' is just a naming convention. You can load this into ghci and play around with it for some n.

And now comes the magic:

\begin{code}
main0 = quickCheck propSquareRootOfNSquaredEqualsN
\end{code}

Run it, In ghci, or call your function 'main' and use `cabal exec <filename>`. We number them here because we can only have one main, and we like to execute our documentation.

This will fail. After some number of steps. For me it failed after three:

*** Failed! Falsifiable (after 3 tests and 1 shrink):
-1.0

What happened? QuickCheck generates input test data and checks if the property holds
(returns true) for all the generated input values. If it finds a case
for which the property does not hold, it fails and returns the counter example.

By default it is configured to generate 100 test cases. You can specify a
different number like so:

\begin{code}
main1 = quickCheckWith stdArgs { maxSuccess = 10 } propSquareRootOfNSquaredEqualsN
\end{code}

On a failure, QuickCheck only shows the resulting value, not the input. If you want to see all the inputs and results,
use traceShow inside the property. This can be very useful when developing new properties, or understanding a regression.

\begin{code}
propTraceSqrNEqualsN :: Double -> Bool
propTraceSqrNEqualsN n = debugShow $ result == n
  where
    result = sqrt (squared n)
    debugShow = traceShow $ "input: " ++ show n ++ " result: " ++ show result

main2 = quickCheck propTraceSqrNEqualsN
\end{code}

How does it know to generate test data? QuickCheck will use the properties' type to generate data. Default generators are available for many built-in types such as numbers, strings and even functions.

We can also use generators explicitly. You can find many available generators by [searching hoogle on ::Gen a](https://www.haskell.org/hoogle/?hoogle=%3a%3a+Gen+a) or browsing [the Test.QuickCheck.Gen module documentation](http://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Gen.html)

We often use choose, to limit the range of numbers to  look for.

\begin{code}
propSqrPositive :: NonNegative Double -> Bool
propSqrPositive (NonNegative n) = (sqrt (squared n)) == n

main = quickCheck propSqrPositive

\end{code}

It is a _generator_ that can generate random integers. More about
generators later on.

The test fails because the invariant does not hold for negative numbers.
We should restrict the generated input to natural numbers only (>=0).
Replace p.integer() by p.nat() and run it again.


