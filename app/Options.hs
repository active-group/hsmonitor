{-# LANGUAGE OverloadedRecordDot #-}

module Options where

import Options.Applicative
import Types

config :: Config -> Parser Config
config cfg =
  Config
    <$> riemannHost
    <*> riemannPort
    <*> startupDelay
  where
    riemannHost =
      option
        auto
        ( long "riemann-host"
            <> short 'h'
            <> help "Riemann Host"
            <> showDefault
            <> value cfg.riemannHost
        )
    riemannPort =
      option
        auto
        ( long "riemann-port"
            <> short 'p'
            <> help "Riemann Port"
            <> showDefault
            <> value cfg.riemannPort
        )

    startupDelay =
      option
        auto
        ( long "startup-delay"
            <> short 'd'
            <> help "Specify maximum startup delay to use when first scheduling checks"
            <> showDefault
        )

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
