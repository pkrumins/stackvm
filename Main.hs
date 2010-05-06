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
import Data.ByteString.Lazy (unpack)
import Codec.Binary.Base64 (encode)
import qualified Text.JSON as JS

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let (vm_port, stackvm_port) = case args of
                                    [] -> (5900, 25900)
                                    _  -> (read $ args!!0, read $ args!!1)
    rfb <- connect' "127.0.0.1" $ PortNumber $ fromIntegral vm_port
    vm <- newVM rfb 30
    forkIO $ updateThread vm
    putStrLn $ "Running on http://localhost:" ++ show stackvm_port
    runWithConfig (ServerConf stackvm_port "0.0.0.0") $ loli $ do
        public (Just "static") ["/css", "/js"]
        stackRoutes vm

stackRoutes :: VM -> UnitT ()
stackRoutes vm = do
    get "/api/console/get_update_base64/:version/:update" $ do
        versionId <- read <$> fromJust <$> capture "version"
        updateId <- read <$> fromJust <$> capture "update"
        updates <- io $ getUpdates vm versionId
        
        let draw = updateData updates !! updateId
        with_type "text/plain"
        with_body $ pack $ encode $ unpack $ drawPng draw

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
        with_header "screen-width"  $ show $ fst $ drawSize draw
        with_header "screen-height" $ show $ snd $ drawSize draw
        with_body $ drawPng draw

    get "/api/console/get_screen_base64" $ do
        draw <- io $ getScreen vm
        with_type "text/plain"
        with_header "screen-width"  $ show $ fst $ drawSize draw
        with_header "screen-height" $ show $ snd $ drawSize draw
        with_body $ pack $ encode $ unpack $ drawPng draw
    
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
