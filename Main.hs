module Main where

import qualified Network.RFB as RFB
import qualified Graphics.GD as GD
import Network (PortID(PortNumber))
import Control.Monad (forever)
import Control.Applicative ((<$>))

import Hack.Handler.Happstack (run)
import Hack.Contrib.Response (set_content_type, set_body)

import Network.Loli
import Network.Loli.Type (UnitT)
import Network.Loli.Utils (update)
import Network.Loli.Template.TextTemplate (text_template)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
repack = LB.pack . B.unpack

type RChan = TChan [RFB.Rectangle]

data VM = VM {
    vmRFB :: RFB.RFB,
    vmChan :: RChan
}

pngData :: VM -> IO LB.ByteString
pngData vm = repack <$> GD.savePngByteString im where
    im = RFB.fbImage $ RFB.rfbFB $ vmRFB vm

updateThread :: VM -> IO ()
updateThread vm = forever $ do
    let rfb = vmRFB vm
    update <- RFB.getUpdate rfb
    mapM_ (RFB.render rfb) $ RFB.fbuRectangles update

main :: IO ()
main = do
    rfb <- RFB.connect' "localhost" $ PortNumber 5900
    chan <- atomically $ newTChan
    let vm = VM { vmRFB = rfb, vmChan = chan }
    
    forkIO $ updateThread vm
    run $ loli $ do
        beanstalkRoutes vm
        public (Just "static") ["/css", "/js"]

beanstalkRoutes :: VM -> UnitT ()
beanstalkRoutes vm = do
    get "/api/png/init" $ do
        with_type "image/png"
        with_body =<< (io $ pngData vm)
        
    get "/" $ do
        with_layout "layout.html"
            $ output $ text_template "console.html"
        with_type "text/html"

with_type = update . set_content_type
with_body = update . set_body
