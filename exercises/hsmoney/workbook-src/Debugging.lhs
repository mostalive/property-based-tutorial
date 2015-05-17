Let's talk about a bit more debugging for a bit. 

Out of the box QuickCheck does not give that much feedback about what fails. It will show the input values for the property that failed, but what happens inside that property is invisible.

We've seen in the introduction that we can use the `===` operator to at least see the expected and actual output inside a property. But what if we don't want to compare with equality, or if we want to see some other values in our property or the code under test.

We will import Haskell's Debug.Trace module, so we can print values to the console, even though we are otherwise operating without IO.

\begin{code}
module Debugging where
import Test.QuickCheck
import Debug.Trace
\end{code}

==== Using standard haskell debugging

If you are experienced in debugging haskell programs, you can skip to the next section, otherwise it is a good idea to follow the example below, so that you don't get stuck doing the more elaborate exercise.

On a failure, QuickCheck only shows the resulting value, not the input. If you want to see all the inputs and results,
use traceShow inside the property. This can be very useful when developing new properties, or understanding a regression.

\begin{code}
prop_TraceSqrNEqualsN :: Int -> Property
prop_TraceSqrNEqualsN n = debugShow $ result === n
  where
    result = sqrt $ fromIntegral $ squared n
    debugShow = traceShow $ "input: " ++ show n ++ " result: " ++ show result
\end{code}

=== Playing with the number of test cases
By default it is configured to generate 100 test cases. You can specify a
different number using quickCheckWith, here with a trivial property.

\begin{code}
gen10 = quickCheckWith stdArgs { maxSuccess = 10 } (\x -> x == x)
\end{code}

\begin{code}
runTests = do
  mapM_ quickCheck [prop_TraceSqrNEqualsN, gen10]
\end{code}


TODO explain use of sample to see what data gets generated.
