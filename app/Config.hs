{-# LANGUAGE OverloadedStrings #-}

module Config where

import Types

schedule :: MonitoringConfig
schedule = MonitoringConfig []

cfg :: Config
cfg = defaultConfig
