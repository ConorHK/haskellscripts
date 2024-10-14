#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.aeson pkgs.http-conduit ])"

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getEnv, getArgs, lookupEnv)
import System.Process (createProcess, waitForProcess, proc)
import System.Exit (ExitCode(..))
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Data.Maybe (fromMaybe)
import Data.Time (DiffTime, secondsToDiffTime)
import Control.Exception (catch)
import Network.HTTP.Simple (httpNoBody, setRequestBodyJSON, setRequestMethod, setRequestPath, defaultRequest, setRequestHost)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T  -- Import Data.Text
import qualified Data.Text.Encoding as TE  -- Import Data.Text.Encoding

-- Default priorities
defaultSuccessPriority :: Int
defaultSuccessPriority = 3

defaultFailurePriority :: Int
defaultFailurePriority = 4

-- Function to send notifications
sendNotification :: String -> Int -> Bool -> DiffTime -> IO ()
sendNotification command priority isError execTime = do
    let topic = "foo"
    let title = if isError then Aeson.String "Command failure" else Aeson.String "Command success"
        tags = if isError then [Aeson.String "warning"] else [Aeson.String "tada"]
        returnCode = if isError then "1" else "0"
        payload = Aeson.object 
            [ "topic" Aeson..= T.pack topic  -- Convert to Data.Text
            , "message" Aeson..= (command ++ " :: completed with return code " ++ show returnCode ++ " in " ++ show execTime)
            , "title" Aeson..= title
            , "priority" Aeson..= priority
            , "tags" Aeson..= tags
            ]
    
    let request = setRequestHost (BS.pack "ntfy.sh")
                  $ setRequestPath (BS.pack "/")
                  $ setRequestMethod (BS.pack "POST")
                  $ setRequestBodyJSON payload defaultRequest
    _ <- httpNoBody request
    return ()

-- Main function
main :: IO ()
main = do
    -- Get command-line arguments
    args <- getArgs
    let command = unwords args

    -- Get environment variables with fallback
    successPriority <- fmap (read . fromMaybe (show defaultSuccessPriority)) (lookupEnv "SUCCESS_PRIORITY")
    failurePriority <- fmap (read . fromMaybe (show defaultFailurePriority)) (lookupEnv "FAILURE_PRIORITY")
    alias <- lookupEnv "ALIAS"

    -- Time command execution
    start <- getTime Monotonic
    (_, _, _, handle) <- createProcess (proc (head args) (tail args))
    exitCode <- waitForProcess handle
    end <- getTime Monotonic

    let execTime = secondsToDiffTime $ fromIntegral $ (toNanoSecs end - toNanoSecs start) `div` 1000000000
        commandString = fromMaybe command alias
        isError = case exitCode of
            ExitSuccess -> False
            _           -> True
        priority = if isError then failurePriority else successPriority

    -- Send notification
    sendNotification commandString priority isError execTime
