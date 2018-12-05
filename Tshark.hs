module Main where

import System.IO
import System.Process
import Control.Exception
import System.Posix.Signals
import System.Exit
import System.Environment(getArgs)
import qualified Network.Info
import qualified System.Directory
import FlowLog

tSharkCommonParameters = ["-Tfields", "-eframe.time_relative", "-eip.src", "-eip.dst", "-ebgp.type", "bgp"]
-- tShark p = readProcessWithExitCode "tshark" ( p : tSharkCommonParameters ) ""
tShark p = let -- sigIntSet = addSignal sigINT emptySignalSet 
               proc' = ( proc "tshark" ( p : tSharkCommonParameters ) ) { delegate_ctlc = True }
    in do
      readCreateProcessWithExitCode proc' "" 
      -- mask_ ( readCreateProcessWithExitCode proc' "" )
      -- blockSignals sigIntSet
      -- installHandler sigINT (CatchOnce (putStrLn "bang bang!")) Nothing
{-
      catch ( readCreateProcessWithExitCode proc' "" )
            (\e -> do let err = show (e :: AsyncException)
                      hPutStr stderr ("AsyncException : " ++ err)
                      return (ExitFailure 0,"","AsyncException" )
            )
-}
        -- installHandler sigINT Ignore Nothing
        -- unblockSignals sigIntSet
        -- return r

main = do
    interfaces <- Network.Info.getNetworkInterfaces
    let interfaceNames = fmap Network.Info.name interfaces
    args <- getArgs
    let arg1 = head args
    if null args then
        putStrLn "interface or file name required"
    else if arg1 `elem` interfaceNames then
        readNetwork arg1
        else do
            p <- System.Directory.doesFileExist arg1
            if p
                then Main.readFile arg1
                else putStrLn $ arg1 ++ " is not a valid interface or file name"

readFile fname = mask_ $ do
    putStrLn $ "opening " ++ fname
    tsharkVersion <- readProcess "tshark" ["-v"] ""
    -- putStrLn tsharkVersion

    (ec,logs,errMsg) <- tShark ( "-r" ++ fname )

    case ec of
        ExitSuccess -> do putStrLn $ show (( length . lines ) logs) ++ " BGP messages read"
                          flowLog logs
        ExitFailure f -> putStrLn $ "tshark failed: " ++ show f ++ " - " ++ errMsg 
    
 
readNetwork ifname = do
    putStrLn $ "opening " ++ ifname
 
{-
    command = shell $ "sudo tshark -T fields -e frame.time_relative -eip.src -eip.dst -ebgp.type -i " ++ interface ++ " port 179"
    (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess command
    forkIO $ processTrace mb_stdout_hdl ( Data.Map.Strict.empty :: LogMap )
    ec <- waitForProcess ph
    putStrLn $ "done: " ++ ec
-}
