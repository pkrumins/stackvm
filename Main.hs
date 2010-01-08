module Main where

import Network.RFB
import Control.Monad
import qualified Graphics.GD as GD
import Network (PortID(PortNumber))
import Control.Applicative ((<$>))

import Hack
import Hack.Handler.Happstack
import Data.ByteString (unpack)
import Data.ByteString.Lazy (pack)
import Data.Word

app :: Application
app = \env -> do
    rfb <- connect' "localhost" $ PortNumber 5900
    update <- getUpdate rfb
    mapM_ (render rfb) $ fbuRectangles update
    pngData <- pack . unpack
        <$> (GD.savePngByteString $ fbImage $ rfbFB rfb)
    
    return $ Response {
        status = 200,
        headers = [ ("Content-Type", "image/png") ],
        body = pngData
    }

main = run app
