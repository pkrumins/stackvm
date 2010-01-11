module Beanstalk.VM (
    VM(..), UpdateData(..), DrawData(..),
    updateThread, getScreenPng, newVM
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

newVM :: RFB -> IO VM
newVM rfb = do
    updates <- atomically $ newTMVar []
    screen <- atomically . newTMVar =<< getScreenUpdate rfb 0
    latest <- atomically $ newTMVar 0
    
    return $ VM {
        vmRFB = rfb,
        vmUpdates = updates,
        vmScreen = screen,
        vmLatestUpdate = latest
    }

-- get the screen png if it needs to be gotten
getScreenPng :: VM -> IO BL.ByteString
getScreenPng vm = do
    screen <- atomically $ readTMVar $ vmScreen vm
    let screenV = updateVersion screen
    updateV <- atomically $ readTMVar $ vmLatestUpdate vm
    
    if screenV == updateV
        then return $ drawPng $ head $ updateData screen
        else do
            let tm = vmScreen vm
            update <- getScreenUpdate (vmRFB vm) updateV
            atomically $ swapTMVar tm update
            return $ drawPng $ head $ updateData update

-- get the screen as an UpdateData
getScreenUpdate :: RFB -> Int -> IO UpdateData
getScreenUpdate rfb version = do
    im <- getImage rfb
    png <- repack <$> GD.savePngByteString im
    let bytes = fromIntegral $ BL.length png
    return $ UpdateData {
        updateBytes = bytes,
        updateVersion = version,
        updateData = (:[]) $ DrawData {
            drawPng = png,
            drawPos = (0,0),
            drawSize = fbWidth &&& fbHeight $ rfbFB rfb,
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
