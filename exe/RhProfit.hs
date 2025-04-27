module Main where

import RobinHood.Cmd
import RobinHood.Run


main :: IO ()
main = parseCmdArgs >>= runRhProfit
