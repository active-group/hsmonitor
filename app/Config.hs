module Config where

import Types

schedule :: MonitoringConfig
schedule = MonitoringConfig []

cfg :: Config
cfg = Config "localhost" 5555 True
