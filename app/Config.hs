{-# LANGUAGE OverloadedStrings #-}

module Config where

import Types
import Webpage

schedule :: MonitoringConfig
schedule =
  MonitoringConfig
    [ task
        "ag-website"
        ( webpage
            { url = "https://active-group.de"
            , toAppear = ["Active Group"]
            }
        )
    ]

cfg :: Config
cfg = Config Nothing Nothing
