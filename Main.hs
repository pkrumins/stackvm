module Main where

import StackVM.Web
import StackVM.VM
import Network.RFB

import Hack.Handler.Happstack

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
    rfb <- connect' "127.0.0.1" $ PortNumber 5900
    vm <- newVM rfb 30
    forkIO $ updateThread vm
    putStrLn "http://localhost:9000/"
    runWithConfig (ServerConf 9000 "0.0.0.0") $ loli $ do
        public (Just "static") ["/css", "/js"]
        stackRoutes vm

stackRoutes :: VM -> UnitT ()
stackRoutes vm = do
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
        
        with_type "text/javascript"
        with_body $ pack $ JS.encode
            $ [[latestVersion]]
            ++ [ [
                fst $ drawPos u, snd $ drawPos u,
                fst $ drawSize u, snd $ drawSize u
            ] | u <- updateData updates ]
    
    get "/api/console/get_latest_version" $ do
        with_type "text/javascript"
        with_body =<< pack <$> show <$> updateVersion
            <$> io (getUpdates vm 0)
    
    get "/api/console/get_screen" $ do
        draw <- io $ getScreen vm
        with_type "image/png"
        with_body $ drawPng draw
    
    get "/api/console/send_key_down/:key" $ do
        key <- read <$> fromJust <$> capture "key"
        io $ sendKeyEvent (vmRFB vm) True key
        with_body $ pack "ok"
    
    get "/api/console/send_key_up/:key" $ do
        key <- read <$> fromJust <$> capture "key"
        io $ sendKeyEvent (vmRFB vm) False key
        with_body $ pack "ok"
    
    get "/" $ do
        with_type "text/html"
        with_layout "layout.html"
            $ output $ text_template "console.html"
