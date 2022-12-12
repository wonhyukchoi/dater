import Data.Either (isRight, isLeft)

import System.FilePath ((</>))

import System.Directory (listDirectory)

import Test.Hspec (hspec, describe, it, shouldSatisfy, shouldBe, Spec)

import Parser (parseDater)

buildExpectations :: (a -> Spec) -> [a] -> Spec
buildExpectations mkExpectation =
  foldr (\input acc -> acc >> mkExpectation input) (return ())

parseTests :: IO ()
parseTests = do
  getAbsolutePathsInDirectory success_dir >>= runParseTests True
  getAbsolutePathsInDirectory failure_dir >>= runParseTests False
  where 
    base_dir    = "test/files/parser"
    success_dir = base_dir </> "success"
    failure_dir = base_dir </> "failure"

    getAbsolutePathsInDirectory :: FilePath -> IO [FilePath]
    getAbsolutePathsInDirectory path = do
      files <- listDirectory path
      return $ map (path </>) files

    runParseTests :: Bool -> [FilePath] -> IO ()
    runParseTests shouldSucceed paths = do
      manyFileContents <- traverse readFile paths
      hspec $ describe ("Parse " ++ show shouldSucceed ++ " Tests")
            $ buildExpectations parseSucceeds manyFileContents
      where parseSucceeds input =
              it input $ parseDater input `shouldSatisfy` expectedResult
            expectedResult = if shouldSucceed then isRight else isLeft

main :: IO ()
main = parseTests
