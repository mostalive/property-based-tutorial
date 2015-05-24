\documentclass{article}
%include polycode.fmt
\begin{document}
Workbook is the main module, that we use to ensure all the code in the examples compiles.

\begin{code}
module Main where
import Step1 as S
--import Debugging as D
import PropertySuite as P
import Money as M
\end{code}

Manually aggregated property suite.

\begin{code}
main = do
 P.runTests
 --D.runTests
 S.runTests
 M.runTests
\end{code}
