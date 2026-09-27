{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module ParserTest where

import Control.Monad (filterM, when, unless)
import Data.Foldable (for_)
import Data.Void (Void)
import Squimble.Grammar (Module, module')
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

spec_passing :: Spec
spec_passing = do
  let root :: FilePath
      root = "tests" </> "parser"

  groups <- runIO $ directories (root </> "pass")

  for_ groups \group -> do
    let path :: FilePath
        path = root </> "pass" </> group

    describe group do
      runIO (directories path) >>= mapM_ \name -> do
        it name do
          errors <- stderr (path </> name)

          unless (null errors) do
            expectationFailure errors

spec_failing :: Spec
spec_failing = do
  let root :: FilePath
      root = "tests" </> "parser"

  groups <- runIO $ directories (root </> "fail")

  for_ groups \group -> do
    let path :: FilePath
        path = root </> "fail" </> group

    describe group do
      runIO (directories path) >>= mapM_ \name -> do
        it name do
          actual <- stderr (path </> name)

          when (null actual) do
            expectationFailure "expected a parse error"

          expected <- readFile (path </> name </> "stderr.log")
          actual `shouldBe` expected

directories :: FilePath -> IO [FilePath]
directories parent = listDirectory parent >>= filterM check
  where check entry = doesDirectoryExist (parent </> entry)

stderr :: FilePath -> IO String
stderr directory = do
  let path = directory </> "main.sq"
  source <- readFile path

  let parser :: Parsec Void String Module
      parser = module'

  case runParser parser path source of
    Left es -> pure (errorBundlePretty es)
    Right _ -> mempty
