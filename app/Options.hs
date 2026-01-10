{-# LANGUAGE OverloadedRecordDot #-}

module Options where

import Data.Time
import Options.Applicative
import Types

config :: Config -> Parser Config
config cfg =
  Config
    <$> optional riemannConfig
    <*> startupDelay
    <*> debug
  where
    debug =
      flag
        False
        True
        ( long "debug"
            <> help "Enables debug printing of commands and outputs"
        )

    riemannHost =
      strOption
        ( long "riemann-host"
            <> short 'h'
            <> help "Riemann Host"
        )
    riemannPort =
      option
        auto
        ( long "riemann-port"
            <> short 'p'
            <> help "Riemann Port"
        )

    riemannConfig =
      RiemannConfig
        <$> riemannHost
        <*> riemannPort

    startupDelay =
      optional $
        option
          (maybeReader parseNominalDiffTime)
          ( long "max-startup-delay"
              <> short 'd'
              <> help "Specify maximum startup delay (in seconds) to use when first scheduling checks"
              <> showDefault
              <> maybe mempty value cfg.startupDelay
          )

parseNominalDiffTime :: String -> Maybe NominalDiffTime
parseNominalDiffTime inp =
  case reads (inp <> "s") of
    [(x, "")] -> Just x
    _ -> Nothing

opts :: Config -> ParserInfo Config
opts cfg =
  info
    (config cfg <**> helper)
    ( fullDesc
        <> progDesc "Montitoring tool for Active Group Services"
        <> header "HSMonitor"
    )

exec :: Config -> IO Config
exec = execParser . opts
