{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.JUnit.Formatter
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

junitConfig :: JUnitConfig
junitConfig =
  setJUnitConfigOutputName "hsmonitor-junit.xml" $
    defaultJUnitConfig "hsmonitor"


main :: IO ()
main = hspec $ add junitConfig $ do
  pure ()