module Main where

import qualified Network.RFB as RFB
import Control.Monad
import qualified Graphics.GD as GD
import Network (PortID(PortNumber))
import Control.Applicative ((<$>))

import Hack
import Hack.Handler.Happstack
import Network.Loli
import Network.Loli.Type
import Network.Loli.Template.TextTemplate

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
-- convert a strict bytestring to a lazy one
repack = LB.pack . B.unpack

rectangle :: AppUnit
rectangle = app $ \env -> do
    rfb <- RFB.connect' "localhost" $ PortNumber 5900
    update <- RFB.getUpdate rfb
    mapM_ (RFB.render rfb) $ RFB.fbuRectangles update
    pngData <- repack <$> (GD.savePngByteString $ RFB.fbImage $ RFB.rfbFB rfb)
    
    return $ Response {
        status = 200,
        headers = [ ("Content-Type", "image/png") ],
        body = pngData
    }

main = run $ loli $ do
    get "/" rectangle
