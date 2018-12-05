module Main where

import FlowLog

-- requires an input stream such as the following tshark command would produce:
-- -- tshark -rtrace -Tfields -eframe.time_relative -eip.src -eip.dst -ebgp.type bgp
main = do
    logs <- getContents
    flowLog logs
