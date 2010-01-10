module Main where

import qualified Network.RFB as RFB
import Network (PortID(..))

import Beanstalk.Web
import Beanstalk.VM

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)

main :: IO ()
main = do
    rfb <- RFB.connect' "localhost" $ PortNumber 5900
    chan <- atomically $ newTChan
    let vm = VM { vmRFB = rfb, vmChan = chan }
    
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
        with_layout "layout.html"
            $ output $ text_template "console.html"
        with_type "text/html"
