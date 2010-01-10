module Beanstalk.VM (
    VM(..), updateThread, pngData
) where

import qualified Graphics.GD as GD
import Network.RFB

import Control.Monad (forever, mapM_)
import Control.Applicative ((<$>))

import qualified Data.ByteString as B
import Data.ByteString.Lazy
repack = pack . B.unpack

data VM = VM {
    vmRFB :: RFB
}

pngData :: VM -> IO ByteString
pngData vm = do
    im <- getImage $ vmRFB vm
    repack <$> GD.savePngByteString im

updateThread :: VM -> IO ()
updateThread vm = forever $ do
    let rfb = vmRFB vm
    renderImage' rfb =<< rectangles <$> getUpdate rfb
