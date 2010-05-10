module StackVM.VM (
    VM(..), UpdateFB(..),
    updateThread, getUpdate, newVM, renderPng
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
import Debug.Trace (trace)

type UpdateID = Int

data VM = VM {
    vmRFB :: RFB.RFB,
    vmUpdates :: MVar [UpdateFB],
    vmScreen :: MVar UpdateFB,
    vmLatest :: MVar UpdateID
}

data UpdateFB = UpdateFB {
    updateImage :: GD.Image,
    updatePos :: (Int,Int),
    updateSize :: (Int,Int),
    updateID :: UpdateID
}

renderPng :: UpdateFB -> IO BS.ByteString
renderPng UpdateFB{ updateImage = im } = GD.savePngByteString im

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
    return $ UpdateFB {
        updateImage = im,
        updatePos = pos,
        updateSize = size,
        updateID = 0
    }

newUpdate :: UpdateFB -> [RFB.Rectangle] -> IO UpdateFB
newUpdate UpdateFB{ updateImage = screenIm } rects = do
    let
        -- compute the bounds of the new synthesis image
        n = minimum $ map (snd . RFB.rectPos) rects
        w = minimum $ map (fst . RFB.rectPos) rects
        
        s = maximum ss
        ss = [ (snd $ RFB.rectPos r) + (snd $ RFB.rectSize r) | r <- rects ]
        e = maximum es
        es = [ (fst $ RFB.rectPos r) + (fst $ RFB.rectSize r) | r <- rects ]
        
        size = (e - w, s - n)
    
    im <- GD.newImage size $ forM_ rects
        $ \rect -> do
            let RFB.Rectangle{ RFB.rectPos = rPos@(rx,ry) } = rect
                RFB.Rectangle{ RFB.rectSize = rSize@(sx,sy) } = rect
                points = liftM2 (,)
                    [ rx - w .. rx + sx - w ]
                    [ ry - n .. ry + sy - n ]
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
                RFB.CopyRectEncoding pos -> do
                    GD.copyRegion pos rSize screenIm rPos
    
    return $ UpdateFB {
        updateImage = im,
        updatePos = (n,w),
        updateSize = size,
        updateID = 0
    }
 
getUpdate :: VM -> UpdateID -> IO UpdateFB
getUpdate VM{ vmScreen = sVar } 0 = readMVar sVar
getUpdate VM{ vmUpdates = uVar, vmScreen = sVar } version = do
    mUpdate <- find ((version ==) . updateID) <$> readMVar uVar
    case mUpdate of
        Just update -> return update
        Nothing -> readMVar sVar

-- Overlay the imagery from u1 onto the data and image in u2.
mergeUpdate :: UpdateFB -> UpdateFB -> IO UpdateFB
mergeUpdate u1 u2 = do
    let
        uu = [u1,u2]
        n = minimum $ map (snd . updatePos) uu
        w = minimum $ map (fst . updatePos) uu
        s = maximum [ (snd $ updatePos u) + (snd $ updateSize u) | u <- uu ]
        e = maximum [ (fst $ updatePos u) + (fst $ updateSize u) | u <- uu ]
        
        size = (e - w, s - n)
    
    im <- GD.withImage (updateImage u1) $ do
        uncurry GD.resize size
        GD.getImage
    
    return $ UpdateFB {
        updateImage = im,
        updatePos = (n,w),
        updateSize = size,
        updateID = 0
    }

updateThread :: VM -> IO ()
updateThread vm@VM{ vmRFB = rfb, vmLatest = idVar } = forever $ do
    let VM{ vmUpdates = updateVar, vmScreen = screenVar } = vm
    
    rects <- RFB.rectangles <$> RFB.getUpdate rfb
    
    screen <- takeMVar screenVar
    update <- newUpdate screen rects
    
    uID <- takeMVar idVar
    putMVar idVar (uID + 1)
    
    -- updates are tied to the latest version upon creation
    let newestUpdate = update { updateID = uID }
    
    putMVar updateVar . take 5 . (newestUpdate :)
        =<< mapM (mergeUpdate update)
        =<< takeMVar updateVar
    putMVar screenVar =<< mergeUpdate update screen
