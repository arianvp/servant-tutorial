module Main where

import qualified Api
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = run 8080 (logStdoutDev Api.app)
