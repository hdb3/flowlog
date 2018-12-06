{-# LANGUAGE OverloadedStrings #-}
module FlowLog where

import qualified Data.Map.Strict
import qualified Data.IP
import Data.List(nub,sort,group)
import Data.Maybe(isJust)
import Numeric(showFFloat)

data Flow = Flow { tOpen , tFirstUpdate, tLastUpdate, tNotify :: Maybe Double, updateCount, openCount :: Int } deriving Show
nullFlow = Flow Nothing Nothing Nothing Nothing 0 0

combineFlowLogs :: [((Data.IP.IPv4,Data.IP.IPv4) , Flow)] -> ((Data.IP.IPv4,Data.IP.IPv4) , Flow)
combineFlowLogs = foldl1 combine2FlowLogs
combine2FlowLogs ((src1,dst1),flow1) ((src2,dst2),flow2) = ((src,dst),flow) where
    flow = combine2Flows flow1 flow2
    src  = combine2ips src1 src2 
    dst  = combine2ips dst1 dst2 
    combine2ips a b | a == b = a
                    | otherwise = "0.0.0.0"

combineFlows :: [Flow] -> Flow
combineFlows = foldl1 combine2Flows 
combine2Flows 
    ( Flow  tOpen1  tFirstUpdate1  tLastUpdate1  tNotify1 updateCount1 openCount1 )
    ( Flow  tOpen2  tFirstUpdate2  tLastUpdate2  tNotify2 updateCount2 openCount2 )
    = Flow (fmap' min tOpen1 tOpen2)
           (fmap' min tFirstUpdate1 tFirstUpdate2)
           (fmap' max tLastUpdate1 tLastUpdate2)
           (fmap' max tNotify1 tNotify2)
           (updateCount1 + updateCount2)
           (openCount1 + openCount2 )
    where
        fmap' f Nothing Nothing = Nothing
        fmap' f (Just a) Nothing = Just a
        fmap' f Nothing (Just a) = Just a
        fmap' f (Just a) (Just b) = Just (f a b)

type LogMap = Data.Map.Strict.Map (Data.IP.IPv4,Data.IP.IPv4) Flow

flowLog logs = do
    let newMap = Data.Map.Strict.empty :: LogMap
        flowMap = foldl processTrace newMap (lines logs)
        getFlows = filter (isJust . tFirstUpdate . snd) . Data.Map.Strict.toList
        getBigFlows n = filter ((n <) . updateCount . snd) . getFlows
        bigFlows = getBigFlows 10 flowMap

    putStrLn $ unlines $ map displayFlow bigFlows
    putStrLn ""
    putStrLn $ unlines $ map displayFlow (buildAggregates bigFlows)

buildAggregates flows = sourceAggregates ++ sinkAggregates where
    aggregates :: [Data.IP.IPv4] -> [Data.IP.IPv4] 
    aggregates = map head . filter ( (1 <) . length ) . group . sort
    buildAggregate p = combineFlowLogs . filter p
    buildSourceAggregate ip = buildAggregate ( (ip ==) . flowSource )
    buildSinkAggregate ip   = buildAggregate ( (ip ==) . flowSink   )

    flowSource = fst . fst
    flowSink   = snd . fst
        
    sources = aggregates $ map flowSource flows
    sinks   = aggregates $ map flowSink flows

    sourceAggregates = map (\ip -> buildSourceAggregate ip flows) sources
    sinkAggregates = map (\ip -> buildSinkAggregate ip flows) sinks


-- displayFlows = map displayFlow . getFlows 

displayFlow ((src,dst),( Flow tOpen (Just tFirstUpdate) (Just tLastUpdate) tNotify updateCount openCount )) =
    let deltaUpdates = tLastUpdate - tFirstUpdate
        updateRate = (fromIntegral updateCount) / deltaUpdates
        showF n f = showFFloat (Just n) f ""
        pad n s = take n $ s ++ repeat ' '
        showN n = pad n . show
        showIP = pad 11 . show
        deltaOpenUpdate = maybe 0
                                (\t -> tFirstUpdate - t)
                                tOpen
    in showIP src ++ " -> " ++ showIP dst
            ++ "  " ++ showF 6 deltaUpdates
            ++ "  " ++ showN 9 updateCount
            ++ "  " ++ showF 1 updateRate
            ++ "  " ++ showN 2 openCount
            ++ "  " ++ showF 6 deltaOpenUpdate

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
