module Main where

import System.Process
import System.Exit
import System.Environment(getArgs)
import qualified Network.Info
import qualified System.Directory

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

readFile fname = do
    putStrLn $ "opening " ++ fname
    tsharkVersion <- readProcess "tshark" ["--version"] ""
    -- putStrLn tsharkVersion
    (ec,logs,errMsg) <- readProcessWithExitCode "tshark"
                                                [ "-r"++fname, "-Tfields", "-eframe.time_relative", "-eip.src", "-eip.dst", "-ebgp.type", "bgp"]
                                                ""
    case ec of
        ExitSuccess -> putStrLn $ show (( length . lines ) logs) ++ " BGP messages read"
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
