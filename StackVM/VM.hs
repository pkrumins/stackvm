module StackVM.VM (
    VM(..), UpdateData(..), DrawData(..),
    updateThread, getScreen, getUpdates, newVM
) where

import qualified Graphics.GD as GD
import Network.RFB

import Control.Monad (forever, when, forM)
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
    vmUpdateMax :: Int,
    vmScreen :: TMVar DrawData
}

type Version = Int
type ByteSize = Int

data UpdateData = UpdateData {
    updateBytes :: ByteSize,
    updateVersion :: Version,
    updateData :: [DrawData]
}

data DrawData = DrawData {
    drawPng :: BL.ByteString,
    drawPos :: (Int,Int),
    drawSize :: (Int,Int),
    drawBytes :: ByteSize
}

newVM :: RFB -> Int -> IO VM
newVM rfb uMax = do
    updates <- atomically $ newTMVar []
    screen <- atomically . newTMVar =<< getScreen' rfb
    
    return $ VM {
        vmRFB = rfb,
        vmUpdates = updates,
        vmUpdateMax = uMax,
        vmScreen = screen
    }

getScreen :: VM -> IO DrawData
getScreen = getScreen' . vmRFB

getScreen' :: RFB -> IO DrawData
getScreen' rfb = do
    im <- getImage rfb
    png <- repack <$> GD.savePngByteString im
    let bytes = fromIntegral $ BL.length png
    return $ DrawData {
        drawPng = png,
        drawPos = (0,0),
        drawSize = fbWidth &&& fbHeight $ rfbFB rfb,
        drawBytes = bytes
    }

getUpdates :: VM -> Version -> IO UpdateData
getUpdates vm version = do
    updates <- reverse
        <$> takeWhile ((> version) . updateVersion)
        <$> (atomically $ readTMVar $ vmUpdates vm)
    return $ UpdateData {
        updateBytes = sum $ map updateBytes updates,
        updateVersion =
            if null updates
                then version
                else updateVersion $ last updates,
        updateData = concatMap updateData updates
    }

updateThread :: VM -> IO ()
updateThread vm = forever $ do
    let rfb = vmRFB vm
    rx <- rectangles <$> getUpdate rfb
    renderImage' rfb rx
    dx <- if length rx > vmUpdateMax vm
        then (:[]) <$> getScreen vm
        else forM rx $ \rect -> do
            im <- fromByteString (rectSize rect) (rawImage $ rectEncoding rect)
            png <- repack <$> GD.savePngByteString im
            return $ DrawData {
                drawPng = png,
                drawPos = rectPos rect,
                drawSize = rectSize rect,
                drawBytes = fromIntegral $ BL.length png
            }
     
    let tm = vmUpdates vm
    updates <- atomically $ takeTMVar tm
    let
        ver = if null updates then 0 else updateVersion $ head updates
        update = UpdateData {
            updateBytes = sum $ map drawBytes dx,
            updateVersion = ver + 1,
            updateData = dx
        }
    if length rx > vmUpdateMax vm
        then atomically $ putTMVar tm [update]
        else atomically $ putTMVar tm $ update : updates
