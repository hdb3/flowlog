module FlowLog where

import qualified Data.Map.Strict
import qualified Data.IP
import Data.List(nub,sort,group)
import Data.Maybe(isJust)
import Numeric(showFFloat)

data Flow = Flow { tOpen , tFirstUpdate, tLastUpdate, tNotify :: Maybe Double, updateCount, openCount :: Int } deriving Show
nullFlow = Flow Nothing Nothing Nothing Nothing 0 0

combineFlows 
    ( Flow (Just tOpen1) (Just tFirstUpdate1) (Just tLastUpdate1) (Just tNotify1) updateCount1 openCount1 )
    ( Flow (Just tOpen2) (Just tFirstUpdate2) (Just tLastUpdate2) (Just tNotify2) updateCount2 openCount2 )
    = Flow (Just $ min tOpen1 tOpen2)
           (Just $ min tFirstUpdate1 tFirstUpdate2)
           (Just $ max tLastUpdate1 tLastUpdate2)
           (Just $ max tNotify1 tNotify2)
           (updateCount1 + updateCount2)
           (openCount1 + openCount2 )

type LogMap = Data.Map.Strict.Map (Data.IP.IPv4,Data.IP.IPv4) Flow

flowLog logs = do
    let newMap = Data.Map.Strict.empty :: LogMap
        flowMap = foldl processTrace newMap (lines logs)
        getFlows = filter (isJust . tFirstUpdate . snd) . Data.Map.Strict.toList
        getBigFlows n = filter ((n <) . updateCount . snd) . getFlows
        bigFlows = getBigFlows 10 flowMap
        -- sources = map ( fst . fst ) bigFlows
        aggregates :: [Data.IP.IPv4] -> [Data.IP.IPv4] 
        aggregates = map head . filter ( (1 <) . length ) . group . sort
        sources = aggregates $ map ( fst . fst ) bigFlows
        sinks   = aggregates $ map ( snd . fst ) bigFlows
    print (sources,sinks)
        -- flowList = map readTraceLine (lines logs)
    -- putStrLn $ unlines $ displayFlows flowMap

    putStrLn $ unlines $ map displayFlow bigFlows


-- displayFlows = map displayFlow . getFlows 

displayFlow ((src,dst),( Flow tOpen (Just tFirstUpdate) (Just tLastUpdate) tNotify updateCount openCount )) =
    let deltaUpdates = tLastUpdate - tFirstUpdate
        show' f = showFFloat (Just 6) f ""
        show'' n = take 8 $ show n ++ "        " 
        deltaOpenUpdate = maybe 0
                                (\t -> tFirstUpdate - t)
                                tOpen
    in show src ++ "\t-> " ++ show dst
            ++ "\t" ++ show' deltaUpdates
            ++ "\t" ++ show'' updateCount
            ++ "\t" ++ show'' openCount
            ++ "\t" ++ show' deltaOpenUpdate

processTrace flowMap logLine = Data.Map.Strict.alter (f ts types) (src,dst) flowMap where
    vals@(ts,src,dst,types) = readTraceLine logLine
    f :: Double -> [Int] -> Maybe Flow -> Maybe Flow
    f _ [] b = b
    f ts (a:ax) Nothing = f ts (a:ax) (Just nullFlow)
    f ts (a:ax) (Just b) = f ts ax (Just (f' ts a b ))

    -- OPEN processing - (BGP type code 1)
    -- (reset the UPDATE/NOTIFICATION timestamps if we see an OPEN)
    f' ts 1 ( Flow tOpen _ _ _ openCount _ ) = Flow (Just ts) Nothing Nothing Nothing 0 (openCount+1)

    -- UPDATE processing - (BGP type code 2)
    f' ts 2 ( Flow tOpen Nothing Nothing tNotify updateCount openCount ) = Flow tOpen (Just ts) (Just ts) tNotify (updateCount+1) openCount 
    f' ts 2 ( Flow tOpen tFirstUpdate tLastUpdate tNotify updateCount openCount ) = Flow tOpen tFirstUpdate (Just ts) tNotify (updateCount+1) openCount 

    -- NOTIFICATION processing - (BGP type code 3)
    f' ts 3 ( Flow tOpen tFirstUpdate tLastUpdate tNotify updateCount openCount ) = Flow tOpen tFirstUpdate tLastUpdate (Just ts) updateCount openCount 

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
