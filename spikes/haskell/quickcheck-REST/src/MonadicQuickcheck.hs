module Main where

import Prelude as P
import Test.QuickCheck
import Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))
import System.Directory (removeFile)
import System.IO (hGetContents, hPutStr, hSeek, openBinaryTempFile, SeekMode (..))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Network.Wreq
import Control.Lens ((^.))
import Text.Groom

genEndpoint :: Gen Text
genEndpoint = elements $ P.map withHost ["en","nl","en/consulting"]
  where withHost p = "http://qwan.eu/" <> p

endPointsLessThanTwenty :: Text -> Bool
endPointsLessThanTwenty a = 50 > (T.length a)

-- Demonstrating pick and pre as well:
prop_writeThenRead :: Property
prop_writeThenRead =
  monadicIO $
    do writtenData <- pick genEndpoint
       pre $ not (T.null writtenData)
       readData <- run $ writeThenRead writtenData
       assert $ writtenData == readData

prop_pageOK :: Property
prop_pageOK =
  monadicIO $
    do endpoint <- pick genEndpoint
       r <- run $ get (T.unpack endpoint)
       assert $ 200 == (r ^. responseStatus . statusCode)

writeThenRead :: Text -> IO Text
writeThenRead outp =
  do (path, h) <- openBinaryTempFile "/tmp" "quickcheck.tmp"
     removeFile path
     hPutStr h (T.unpack outp)
     hSeek h AbsoluteSeek 0
     s <- hGetContents h
     return $ T.pack s

main :: IO ()
main = do
  endpoints <- sample' genEndpoint
  TIO.putStrLn "start"
  mapM_ TIO.putStrLn endpoints
  quickCheck (forAll genEndpoint endPointsLessThanTwenty)
  quickCheck prop_writeThenRead
  r <- get "http://qwan.eu"
  putStrLn $ groom r
  putStrLn $ show $ r ^. responseStatus . statusCode
  quickCheck prop_pageOK
  TIO.putStrLn "end"
  
