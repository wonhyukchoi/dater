import Data.Either (isRight)

import System.FilePath ((</>))

import System.Directory (listDirectory)

import Test.Hspec (hspec, describe, it, shouldSatisfy, shouldBe, Spec)

import Parser (parseDater)

buildExpectations :: (a -> Spec) -> [a] -> Spec
buildExpectations mkExpectation =
  foldr (\input acc -> acc >> mkExpectation input) (return ())

parseTests :: IO ()
parseTests = do
  fileNames    <- listDirectory base_dir
  fileContents <- mapM (readFile . (base_dir </>)) fileNames
  runParseTests fileContents
  where 
    base_dir :: FilePath
    base_dir = "test/files/parser"

    runParseTests :: [String] -> IO ()
    runParseTests =
      hspec . (describe "Parse Tests") . (buildExpectations parseSucceeds)
      where parseSucceeds input =
              it input $ (parseDater input) `shouldSatisfy` isRight

main :: IO ()
main = parseTests
