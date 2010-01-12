module Main where

import Beanstalk.Web
import Beanstalk.VM
import Network.RFB

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad (msum, mzero)

import Data.ByteString.Lazy.Char8 (pack)
import qualified Text.JSON as JS

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
        with_body $ drawPng draw
    
    get "/api/console/get_update_list/:version" $ do
        versionId <- read <$> fromJust <$> capture "version"
        -- loop until updates are available
        updates <- io $ msum $ repeat $ do
            u <- getUpdates vm versionId
            if null $ updateData u
                then mzero
                else return u
        
        let updateCount = length $ updateData updates
        let latestVersion = updateVersion updates
        io $ print updateCount
        
        with_type "text/javascript"
        with_body $ pack $
            if updateCount > 100
                then JS.encode [[latestVersion,-1]]
                else JS.encode $ [[latestVersion,updateCount]]
                    ++ [ [
                        fst $ drawPos u, snd $ drawPos u,
                        fst $ drawSize u, snd $ drawSize u
                    ] | u <- updateData updates ]
    
    get "/api/console/get_screen" $ do
        draw <- io $ getScreen vm
        with_type "image/png"
        with_body $ drawPng draw
    
    get "/" $ do
        with_type "text/html"
        with_layout "layout.html"
            $ output $ text_template "console.html"
