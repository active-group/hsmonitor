{-# LANGUAGE OverloadedRecordDot #-}

module Riemann where

import Control.Monad
import System.Process
import Types

sendToRiemann :: Config -> RiemannEvent -> IO ()
sendToRiemann cfg re = do
  print re
  let cp =
        ( proc
            "riemann-client"
            ( [ "-H"
              , cfg.riemannHost
              , "-P"
              , show cfg.riemannPort
              , "-T"
              , "tcp"
              , "send"
              , "-s"
              , re.riemannService
              , "-m"
              , show re.metric
              , "-d"
              , re.description
              , "--ttl"
              , "120"
              ]
                <> case re.eventHost of
                  Nothing -> []
                  Just h -> ["-h", h]
            )
        )
          { std_err = NoStream
          }
  void $ readCreateProcess cp ""
