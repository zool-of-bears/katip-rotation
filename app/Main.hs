{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Katip
import Katip.Scribes.Rotation
import Control.Exception
import Control.Monad.IO.Class

main = do
    rotationScribe <- mkRotationScribe (RotationSettings "." "writer.log" 10 1000) bracketFormat (permitItem InfoS) V2
    let makeLogEnv = registerScribe "rotation" rotationScribe defaultScribeSettings =<< initLogEnv "Writer" "test"
    -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
    bracket makeLogEnv closeScribes $ \le -> do
        let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
        let initialNamespace = "main"
        let loop = do
                str <- liftIO getLine
                $(logTM) InfoS (ls str)
                liftIO $ putStrLn "Logged"
                loop
        runKatipContextT le initialContext initialNamespace $ do
            liftIO $ putStrLn "Starting"
            $(logTM) InfoS "Starting"
            loop

