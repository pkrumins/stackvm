module Beanstalk.VM (
    VM(..), updateThread
) where

import qualified Graphics.GD as GD
import Network.RFB

import Control.Monad (forever, mapM_, when)
import Control.Arrow ((&&&))
import Control.Applicative ((<$>))

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
repack = BL.pack . BS.unpack -- convert strict to lazy

data VM = VM {
    vmRFB :: RFB,
    vmUpdates :: TMVar [UpdateData],
    vmScreen :: TMVar UpdateData,
    vmLatestUpdate :: TMVar Int
}

data UpdateData = UpdateData {
    updateBytes :: Int,
    updateVersion :: Int,
    updateData :: [DrawData]
}

data DrawData = DrawData {
    drawPng :: BL.ByteString,
    drawPos :: (Int,Int),
    drawSize :: (Int,Int),
    drawBytes :: Int
}

getScreenPng :: VM -> IO BL.ByteString
getScreenPng vm = do
    updateScreenPng vm
    drawPng . head . updateData <$> (atomically $ readTMVar $ vmScreen vm)

-- update the screen png data if it needs updating
updateScreenPng :: VM -> IO ()
updateScreenPng vm = do
    screen <- atomically $ readTMVar $ vmScreen vm
    let screenV = updateVersion screen
    updateV <- atomically $ readTMVar $ vmLatestUpdate vm
    
    when (screenV /= updateV) $ do
        let tm = vmScreen vm
        atomically $ takeTMVar tm
        im <- getImage $ vmRFB vm
        png <- repack <$> GD.savePngByteString im
        let bytes = fromIntegral $ BL.length png
        atomically $ putTMVar tm $ UpdateData {
            updateBytes = bytes,
            updateVersion = updateV,
            updateData = (:[]) $ DrawData {
                drawPng = png,
                drawPos = (0,0),
                drawSize = fbWidth &&& fbHeight $ rfbFB $ vmRFB vm,
                drawBytes = bytes
            }
        }

updateThread :: VM -> IO ()
updateThread vm = forever $ do
    let tm = vmLatestUpdate vm
    updateV <- atomically $ takeTMVar tm
    updateV' <- atomically $ swapTMVar tm (succ updateV)
    
    let
        drawRect :: Rectangle -> IO DrawData
        drawRect rect = do
            let im = rawImage $ rectEncoding rect
            png <- repack <$> GD.savePngByteString im
            return $ DrawData {
                drawPng = png,
                drawPos = rectPos rect,
                drawSize = rectSize rect,
                drawBytes = fromIntegral $ BL.length png
            }
        
        rfb = vmRFB vm
    
    dx <- mapM drawRect =<< rectangles <$> getUpdate rfb
    
    let
        tm = vmUpdates vm
        update = UpdateData {
            updateBytes = sum $ map drawBytes dx,
            updateVersion = updateV',
            updateData = dx
        }
    updates <- atomically $ takeTMVar tm
    atomically $ putTMVar tm $ update : updates
