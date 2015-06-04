# Useful reading for QuickCheck

## Original papers

- [QuickCheck: A Lightweight Tool for Random Testing of
Haskell Programs, Koen Claessen and John
Hughes.](http://www.eecs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf) In Proc. of
International Conference on Functional Programming (ICFP), ACM SIGPLAN,
2000. Very readable, many examples and references, and still useful
      after 15 years.

- [Testing IO actions with
QuickCheck](www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps) by Koen
Claessen and John Hughes

- [Introduction to
QuickCheck](https://wiki.haskell.org/Introduction_to_QuickCheck#Testing_with_QuickCheck)
on the HaskellWiki has a useful list of references as well.

## Original tutorial and API Docs

- [QuickCheck: an Automated Testing Tool for
haskell](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) This
is the reason we are using NewType extensively in our tutorial.

- [The QuickCheck
package](https://hackage.haskell.org/package/QuickCheck) is on Hackage.
The documentation for
[Test.QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html)
contains some examples, and since Test.QuickCheck re-exports many of the
underlying modules, this is a good place to start exploring the API in
depth.

## Integratiion with other test frameworks

## Hspec

The [Hspec](http://hspec.github.io/quickcheck.html) github page has a
working example of a quickcheck property inside an hspec describe block.

## Tasty

Same for the [Tasty github page](https://github.com/feuerbach/tasty) .

## Up and downsides of both

Upsides:

- One test suite to rule them all
- Readable explanation / reports
- Fail fast behaviour - quickCheckAll runs all tests even when one
fails. E.g HSpec can stop after a testsuite has failed.

Downsides:

- May need a separate testsuite anyway, because a QuickCheck suite runs
a lot more tests than a regular one and may therefore be slow.
- Duplication. Name of property and the string in the testsuite
describing it may be earily similar. Hspec and Tasty both need a couple
of lines per property. It can be compressed by e.g. using mapM_, but
then the reporting advantage also disappears.

Integrating in a test suite probably works best if properties are
one-liners.  

It might be worth bundling a few quickCheck tests in a `runTests`
function, e.g. by using `quickCheckAll` and integrating that into a test
suite as opposed to individual properties.

# Things to explore

## Reusing properties across typeclasses

- [Verifying Typeclass Laws in Haskell with QuickCheck](http://austinrochford.com/posts/2014-05-27-quickcheck-laws.html) by [Austin Rochford](Austin Rochford). Shows how to write reusable
properties and generators for typeclasses.

- The [Checkers](https://hackage.haskell.org/package/checkers) package by [Conal Elliot](http://conal.net) , provides arbitrary instances and
generator combinators for common data types.

## Using Property based tests for stateful applications

### Redesign stateful code so we can test it as pure functions

- [Purifying code using Free
Monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)
by [Gabriel
Gonzalez](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)

Shows how to transform a program containing some input and output into a
Free Monad, and then use an interpreter in pure code together with
QuickCheck to validate some assumptions.

### Testing stateful code as is

- Testing a web application with hspec
(http://looprecur.com/blog/testing-a-web-application-with-hspec/) by
Adam Baker has a small example of using QuickCheck with monadic IO.
- [Testing IO Actions with monadic
quickcheck](http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck)
on StackOverflow has a few more.
- [QuickCheck and
WebDriver](https://www.fpcomplete.com/user/christianpbrink/quickcheck-and-webdriver)
by Christian Brink. Check that a property holds over a number of web
pages. In this case a form of decoration.


## Testing legacy code

- [QuickCheck as a test set
generator](https://wiki.haskell.org/QuickCheck_as_a_test_set_generator)
on the Haskell Wiki. You can use just the Arbitraries and generators to
generate data for just about anything. Lecacy code, or, we use it to
test migrations - generate a data structure for one version, save it,
migrate, and
check the results with a regular unit test.
 
# Essays including QuickCheck as part of a wider discussion

- [Software Testing From the Perspective of a Hardware
Engineer](http://danluu.com/testing/) by [Dan
Luu](http://danluu.com/about/) . About combining random tests with code
coverage, and some things you'll miss even then.



