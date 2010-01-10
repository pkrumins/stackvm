module Beanstalk.VM (
    RChan, VM(..), updateThread, pngData
) where

import qualified Network.RFB as RFB
import qualified Graphics.GD as GD

import Control.Monad (forever, mapM_)
import Control.Applicative ((<$>))

import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)

import qualified Data.ByteString as B
import Data.ByteString.Lazy
repack = pack . B.unpack

type RChan = TChan [RFB.Rectangle]

data VM = VM {
    vmRFB :: RFB.RFB,
    vmChan :: RChan
}

pngData :: VM -> IO ByteString
pngData vm = repack <$> GD.savePngByteString im where
    im = RFB.fbImage $ RFB.rfbFB $ vmRFB vm

updateThread :: VM -> IO ()
updateThread vm = forever $ do
    let rfb = vmRFB vm
    update <- RFB.getUpdate rfb
    mapM_ (RFB.render rfb) $ RFB.fbuRectangles update
