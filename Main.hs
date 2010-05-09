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
    (vmPort,stackvmPort) <- (<$> getArgs)
        $ \argv -> case argv of
            [] -> (5900, 25900)
            [x,y] -> (read x, read y)
    
    rfb <- connect' "127.0.0.1" $ PortNumber $ fromIntegral vmPort
    vm <- newVM rfb 30
    forkIO $ updateThread vm
    putStrLn $ "Running on http://localhost:" ++ show stackvmPort
    runWithConfig (ServerConf stackvmPort "0.0.0.0") $ loli $ do
        public (Just "static") ["/css", "/js"]
        stackRoutes vm

stackRoutes :: VM -> UnitT ()
stackRoutes vm = do
    get "/api/console/get_update_base64/:version/:update" $ do
        versionId <- read <$> fromJust <$> capture "version"
        updateId <- read <$> fromJust <$> capture "update"
        updates <- io $ getUpdates vm versionId
        
        let draw = updateData updates !! updateId
        withType "text/plain"
        withBody $ pack $ encode $ unpack $ drawPng draw

    get "/api/console/get_update/:version/:update" $ do
        versionId <- read <$> fromJust <$> capture "version"
        updateId <- read <$> fromJust <$> capture "update"
        updates <- io $ getUpdates vm versionId
        
        let draw = updateData updates !! updateId
        withType "image/png"
        withBody $ drawPng draw
    
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
        
        withType "text/javascript"
        withBody $ pack $ JS.encode
            $ [[latestVersion]]
            ++ [ [
                fst $ drawPos u, snd $ drawPos u,
                fst $ drawSize u, snd $ drawSize u
            ] | u <- updateData updates ]
    
    get "/api/console/get_latest_version" $ do
        withType "text/javascript"
        withBody =<< pack <$> show <$> updateVersion
            <$> io (getUpdates vm 0)
    
    get "/api/console/get_screen" $ do
        draw <- io $ getScreen vm
        withType "image/png"
        withHeader "screen-width"  $ show $ fst $ drawSize draw
        withHeader "screen-height" $ show $ snd $ drawSize draw
        withBody $ drawPng draw

    get "/api/console/get_screen_base64" $ do
        draw <- io $ getScreen vm
        withType "text/plain"
        withHeader "screen-width"  $ show $ fst $ drawSize draw
        withHeader "screen-height" $ show $ snd $ drawSize draw
        withBody $ pack $ encode $ unpack $ drawPng draw
    
    get "/api/console/send_key_down/:key" $ do
        key <- read <$> fromJust <$> capture "key"
        io $ sendKeyEvent (vmRFB vm) True key
        withBody $ pack "ok"
    
    get "/api/console/send_key_up/:key" $ do
        key <- read <$> fromJust <$> capture "key"
        io $ sendKeyEvent (vmRFB vm) False key
        withBody $ pack "ok"
    
    get "/api/console/send_pointer/:x,:y,:mask" $ do
        x <- read <$> fromJust <$> capture "x"
        y <- read <$> fromJust <$> capture "y"
        mask <- read <$> fromJust <$> capture "mask"
        io $ sendPointer (vmRFB vm) mask x y
        withBody $ pack "ok"
    
    get "/" $ do
        withType "text/plain"
        withBody $ pack "meow"
