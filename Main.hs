module Main where

import Beanstalk.Web
import Beanstalk.VM
import Network.RFB

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 (pack)

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
    get "/api/console/get_update/:version/:update" $ do
        versionId <- read <$> fromJust <$> capture "version"
        updateId <- read <$> fromJust <$> capture "update"
        updates <- io $ getUpdates vm versionId
        
        let draw = updateData updates !! updateId
        with_type "image/png"
        with_header "Image-Position" $ show $ drawPos draw
        with_header "Image-Size" $ show $ drawSize draw
        with_body $ drawPng draw
    
    get "/api/console/get_update/:version" $ do
        versionId <- read <$> fromJust <$> capture "version"
        updates <- io $ getUpdates vm versionId
        let updateCount = length $ updateData updates
        let latest = updateVersion updates
        with_type "text/javascript"
        with_body $ pack $ show $ [updateCount,latest]
    
    get "/api/console/get_screen" $ do
        draw <- io $ getScreen vm
        with_type "image/png"
        with_header "Image-Position" $ show $ drawPos draw
        with_header "Image-Size" $ show $ drawSize draw
        with_body $ drawPng draw
    
    get "/" $ do
        with_type "text/html"
        with_layout "layout.html"
            $ output $ text_template "console.html"
