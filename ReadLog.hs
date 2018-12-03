module Main where

import System.Process
import System.Environment(getArgs)
import qualified Data.Map.Strict
import qualified Data.IP

data Flow = Flow { tOpen , tFirstUpdate, tLastUpdate, tNotify :: Maybe Double }
nullFlow = Flow Nothing Nothing Nothing Nothing
type LogMap = Data.Map.Strict Map (Data.IP.IPv4,Data.IP.IPv4) Flow

main = do
    args <- getArgs
    let interface = if null args then "eth0" else head args
        command = shell $ "sudo tshark -T fields -e frame.time_relative -eip.src -eip.dst -ebgp.type -i " ++ interface ++ " port 179"
    (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess command
    forkIO $ processTrace mb_stdout_hdl ( Data.Map.Strict.empty :: LogMap )
    ec <- waitForProcess ph
    putStrLn $ "done: " ++ ec

processTrace hdl flowMap = do 
    line <- hReadLn hdl
    let vals@(ts,src,dst,types) = readTraceLine line
    print vals 
    let flowMap' = Data.Map.Strict.update (f ts types) (src,dst) flowMap 
        f _ [] b = b
        f ts (a:ax) b = f ts ax (f' ts a b )

        -- OPEN processing - (BGP type code 1)
        -- (reset the UPDATE/NOTIFICATION timestamps if we see an OPEN)
        f' ts 1 ( Flow tOpen _ _ _ ) = Flow (Just ts) Nothing Nothing Nothing

        -- UPDATE processing - (BGP type code 2)
        f' ts 2 ( Flow tOpen Nothing Nothing tNotify ) = Flow tOpen (Just ts) (Just ts) tNotify
        f' ts 2 ( Flow tOpen tFirstUpdate tLastUpdate tNotify ) = Flow tOpen tFirstUpdate (Just ts) tNotify

        -- NOTIFICATION processing - (BGP type code 3)
        f' ts 3 ( Flow tOpen tFirstUpdate tLastUpdate tNotify ) = Flow tOpen tFirstUpdate tLastUpdate (Just ts)

        -- other, e.g. KEEPALIVE processing - (BGP type code 4+)
        f' _ _ x = x

readTraceLine s = (ts,src,dst,types) where
    [ts],s' = reads s :: [([Double], String)] 
    ([src,dst],s'') = reads s' :: [([Data.IP.IPv4], String)] 
    types = readList s'' :: [Int]
