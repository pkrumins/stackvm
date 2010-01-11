module Main where

import Beanstalk.Web
import Beanstalk.VM
import Network.RFB

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

main :: IO ()
main = do
    rfb <- connect' "localhost" $ PortNumber 5900
    vm <- newVM rfb
    forkIO $ updateThread vm
    putStrLn "http://localhost:3000/"
    run $ loli $ do
        public (Just "static") ["/css", "/js"]
        beanstalkRoutes vm

beanstalkRoutes :: VM -> UnitT ()
beanstalkRoutes vm = do
    get "/api/png/init" $ do
        with_type "image/png"
        draw <- io $ getScreen vm
        with_body $ drawPng draw
     
    get "/" $ do
        with_type "text/html"
        with_layout "layout.html"
            $ output $ text_template "console.html"
