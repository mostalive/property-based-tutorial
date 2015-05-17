#!/home/willem/.cabal/bin/cabal exec runhaskell PropertySuite.lhs

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

In Test.QuickCheck.All contains a few '*checkAll' functions to automatigically run all the properties in a module. This way we can quickly create a suite of properties. We can also include QuickCheck properties in a regular test suite, e.g. in HSpec, or tasty.

We'll demonstrate it with quickCheckAll, you can look up the others in the module documentation. To let quickCheckAll generate code, we need the TemplateHaskell language extension.

\begin{code}
{-# LANGUAGE TemplateHaskell #-} 
module PropertySuite where
\end{code}

We've seen Test.QuickCheck, and Test.QuickCheck.Property before. We need the Template Haskell (TH) module in addition to the language pragma.

\begin{code}
import Test.QuickCheck
import Language.Haskell.TH 
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Property ((===))
\end{code}

We'll introduce two small properties to show we can run them. Their names start with prop_ so quickCheckAll can find them.

\begin{code}

prop_reverse xs = (reverse (reverse xs)) === xs

prop_associative :: Int -> Int -> Int -> Property
prop_associative x y z = ((x + y) + z) === (x + (y + z))

\end{code}

The following code has to go at the bottom of your module, otherwise TemplateHaskell can not find all the functions.

\begin{code}
return [] -- needed in GHC 7.8 series, not after
runTests = $quickCheckAll

main = runTests
\end{code}

There are a few variants of *CheckAll that you may find useful, See the Test.QuickCheck.All documentation 

