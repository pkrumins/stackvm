module Main where

import StackVM.Web
import StackVM.VM
import qualified Network.RFB as RFB
import Network (PortID(..))

import Hack.Handler.Happstack

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad (msum, mzero)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import Codec.Binary.Base64 (encode)
import qualified Text.JSON as JS

import System.Environment (getArgs)

withText = withBody . L8.pack

main :: IO ()
main = do
    (vmPort,stackvmPort) <- (<$> getArgs)
        $ \argv -> case argv of
            [] -> (5900, 25900)
            [x,y] -> (read x, read y)
    
    rfb <- RFB.connect' "127.0.0.1" $ PortNumber $ fromIntegral vmPort
    vm <- newVM rfb
    forkIO $ updateThread vm
    putStrLn $ "Running on http://localhost:" ++ show stackvmPort
    runWithConfig (ServerConf stackvmPort "0.0.0.0") $ loli $ do
        public (Just "static") ["/css", "/js"]
        stackRoutes vm

stackRoutes :: VM -> UnitT ()
stackRoutes vm = do
    let updateRoute :: AppUnitT LBS.ByteString
        updateRoute = do
            updateID <- read <$> fromJust <$> capture "updateID"
            update@UpdateFB{
                updateSize = size,
                updatePos = pos
            } <- io $ getUpdate vm updateID
            
            withHeader "screen-size" $ show size
            withHeader "screen-pos" $ show pos
            withHeader "update-id" $ show updateID
            io $ LBS.pack . BS.unpack <$> renderPng update
    
    get "/api/console/get_update_base64/:updateID" $ do
        withType "text/plain"
        withText . encode . LBS.unpack =<< updateRoute
    
    get "/api/console/get_update/:updateID" $ do
        withType "image/png"
        withBody =<< updateRoute
    
    get "/api/console/send_key_down/:key" $ do
        key <- read <$> fromJust <$> capture "key"
        io $ RFB.sendKeyEvent (vmRFB vm) True key
        withText "ok"
    
    get "/api/console/send_key_up/:key" $ do
        key <- read <$> fromJust <$> capture "key"
        io $ RFB.sendKeyEvent (vmRFB vm) False key
        withText "ok"
    
    get "/api/console/send_pointer/:pointer" $ do
        [x,y,mask] <- read <$> fromJust <$> capture "pointer"
        io $ RFB.sendPointer (vmRFB vm) (fromIntegral mask) x y
        withText "ok"
