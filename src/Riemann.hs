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
            ( [ "send"
              , "-T"
              , "-s"
              , re.riemannService
              , "-f"
              , show re.metric
              , "-D"
              , re.description
              , "--ttl"
              , "120"
              , cfg.riemannHost
              , show cfg.riemannPort
              ]
                <> case re.eventHost of
                  Nothing -> []
                  Just h -> ["-h", h]
            )
        )
          { std_err = NoStream
          }
  void $ readCreateProcess cp ""
