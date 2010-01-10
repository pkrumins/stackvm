module Main where

import Beanstalk.Web
import Beanstalk.VM

import Network.RFB
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    rfb <- connect' "localhost" $ PortNumber 5900
    let vm = VM { vmRFB = rfb }
    
    forkIO $ updateThread vm
    run $ loli $ do
        public (Just "static") ["/css", "/js"]
        beanstalkRoutes vm

beanstalkRoutes :: VM -> UnitT ()
beanstalkRoutes vm = do
    get "/api/png/init" $ do
        with_type "image/png"
        with_body =<< (io $ pngData vm)
     
    get "/" $ do
        with_type "text/html"
        with_layout "layout.html"
            $ output $ text_template "console.html"
