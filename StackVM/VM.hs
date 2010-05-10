module StackVM.VM (
    VM(..), UpdateFB(..),
    updateThread, getUpdate, newVM
) where

import qualified Graphics.GD.State as GD
import qualified Network.RFB as RFB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Monad (forever,forM_,join,liftM2)
import Control.Arrow ((&&&),(***))
import Control.Applicative ((<$>))

import Data.List (find)
import Data.List.Split (splitEvery)
import Data.Word (Word8,Word32)

import Control.Concurrent.MVar

type Version = Int

data VM = VM {
    vmRFB :: RFB.RFB,
    vmUpdates :: MVar [UpdateFB],
    vmScreen :: MVar UpdateFB,
    vmLatest :: MVar Version
}

data UpdateFB = UpdateFB {
    updateImage :: GD.Image,
    updatePng :: BS.ByteString,
    updatePos :: (Int,Int),
    updateSize :: (Int,Int),
    updateBytes :: Int,
    updateVersion :: Version
}

newVM :: RFB.RFB -> IO VM
newVM rfb = do
    updates <- newMVar []
    let screenSize = RFB.fbWidth &&& RFB.fbHeight $ RFB.rfbFB rfb
    screen <- newMVar
        =<< updateFromImage (0,0) screenSize
        =<< RFB.getImage rfb
    version <- newMVar 0
    
    return $ VM {
        vmRFB = rfb,
        vmUpdates = updates,
        vmScreen = screen,
        vmLatest = version
    }

updateFromImage :: GD.Point -> GD.Size -> GD.Image -> IO UpdateFB
updateFromImage pos size im = do
    png <- GD.savePngByteString im
    return $ UpdateFB {
        updatePng = png,
        updateImage = im,
        updatePos = pos,
        updateSize = size,
        updateBytes = BS.length png,
        updateVersion = 0
    }

newUpdate :: UpdateFB -> [RFB.Rectangle] -> IO UpdateFB
newUpdate UpdateFB{ updateImage = screenIm } rects = do
    let
        -- compute the bounds of the new synthesis image
        nw = join (***) minimum $ unzip $ map (RFB.rectPos) rects
        se = join (***) minimum $ unzip -- rectSE = rectNW + size
            $ map (join (***) (uncurry (+)) . (RFB.rectPos &&& RFB.rectSize))
            rects
        size = join (***) (uncurry subtract) (nw,se)
        (imX,imY) = nw
    
    im <- GD.newImage size $ forM_ rects
        $ \rect -> do
            let RFB.Rectangle{ RFB.rectPos = rPos@(rx,ry) } = rect
                RFB.Rectangle{ RFB.rectSize = rSize@(sx,sy) } = rect
                points = liftM2 (,)
                    [ rx - imX .. rx + sx - imX ]
                    [ ry - imY .. ry + sy - imY ]
            case (RFB.rectEncoding rect) of
                RFB.RawEncoding rawData -> do
                    mapM_ (uncurry GD.setPixel)
                        $ zip points
                        $ map rgba
                        $ splitEvery 4
                        $ LBS.unpack rawData
                    where
                        rgba :: [Word8] -> GD.Color
                        rgba cs = sum $ zipWith (*)
                            (map fromIntegral cs) (iterate (*256) 1)
                RFB.CopyRectEncoding pos ->
                    GD.copyRegion pos rSize screenIm rPos
    png <- GD.savePngByteString im
    
    return $ UpdateFB {
        updatePng = png,
        updateImage = im,
        updatePos = nw,
        updateSize = size,
        updateBytes = BS.length png,
        updateVersion = 0
    }
 
getUpdate :: VM -> Version -> IO UpdateFB
getUpdate VM{ vmUpdates = uVar, vmScreen = sVar } version = do
    mUpdate <- find ((version ==) . updateVersion) <$> readMVar uVar
    case mUpdate of
        Just update -> return update
        Nothing -> readMVar sVar

-- add rectangles to an update
mergeUpdate :: UpdateFB -> UpdateFB -> IO UpdateFB
mergeUpdate u1 u2 = undefined

updateThread :: VM -> IO ()
updateThread vm@VM{ vmRFB = rfb, vmLatest = versionVar } = forever $ do
    let VM{ vmUpdates = updateVar, vmScreen = screenVar } = vm
    screen <- takeMVar screenVar
    
    update <- newUpdate screen . RFB.rectangles =<< RFB.getUpdate rfb
    
    version <- takeMVar versionVar
    putMVar versionVar (version + 1)
    
    -- updates are tied to the latest version upon creation
    let newestUpdate = update { updateVersion = version }
    
    putMVar updateVar . take 50 . (newestUpdate :)
        =<< mapM (mergeUpdate update)
        =<< takeMVar updateVar
    putMVar screenVar =<< mergeUpdate update screen
