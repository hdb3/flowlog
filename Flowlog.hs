module Main where

import qualified Data.Map.Strict
import qualified Data.IP

data Flow = Flow { tOpen , tFirstUpdate, tLastUpdate, tNotify :: Maybe Double } deriving Show
nullFlow = Flow Nothing Nothing Nothing Nothing
type LogMap = Data.Map.Strict.Map (Data.IP.IPv4,Data.IP.IPv4) Flow

main = do
    logs <- getContents
    let newMap = Data.Map.Strict.empty :: LogMap
        flowMap = foldl processTrace newMap (lines logs)
        flowList = map readTraceLine (lines logs)
    putStrLn $ unlines $ map show $ Data.Map.Strict.toList flowMap

processTrace flowMap logLine = Data.Map.Strict.alter (f ts types) (src,dst) flowMap where
    vals@(ts,src,dst,types) = readTraceLine logLine
    f :: Double -> [Int] -> Maybe Flow -> Maybe Flow
    f _ [] b = b
    f ts (a:ax) Nothing = f ts (a:ax) (Just nullFlow)
    f ts (a:ax) (Just b) = f ts ax (Just (f' ts a b ))

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

readTraceLine :: String -> (Double,Data.IP.IPv4,Data.IP.IPv4,[Int])
readTraceLine s = (ts,src,dst,types) where
    wx = words s
    ts = read (wx !! 0)
    src = read (wx !! 1)
    dst = read (wx !! 2)
    types = map read $ commas (wx !! 3)

commas :: String -> [String]
commas s = let isComma c = ',' == c
           in case dropWhile isComma s of "" -> []
                                          s' -> w : commas s''
                                                where (w, s'') = break isComma s'
