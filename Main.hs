module Main where

import StackVM.Web
import StackVM.VM
import qualified Network.RFB as RFB
import Network (PortID(..))

import Hack.Handler.Happstack

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad (msum, mzero)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (unpack)
import Codec.Binary.Base64 (encode)
import qualified Text.JSON as JS

import System.Environment (getArgs)

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

main :: IO ()
main = do
    (vmPort, stackvmPort) <- (<$> getArgs)
        $ \argv -> case argv of
            [] -> (5900, 25900)
            [x,y] -> (read x, read y)

    rfb <- RFB.connect' "127.0.0.1" $ PortNumber $ fromIntegral vmPort
    vm <- newVM rfb
    forkIO $ updateThread vm
    putStrLn $ "Starting a TCP server on localhost:" ++ show stackvmPort
    withSocketsDo $
        do addrInfo <- getAddrInfo
                        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                        Nothing (Just $ show stackvmPort)
           let serverAddr = head addrInfo
           sock <- socket (addrFamily serverAddr) Stream defaultProtocol
           setSocketOption sock ReuseAddr 1
           bindSocket sock (addrAddress serverAddr)
           listen sock 100
           procRequests sock vm
        
procRequests :: Socket -> VM -> IO ()
procRequests serverSock vm = do
    (clientSock, clientAddr) <- accept serverSock
    putStrLn $ "Client connected from " ++ show clientSock
    forkIO $ procVM clientSock clientAddr vm
    procRequests serverSock vm

procVM :: Socket -> SockAddr -> VM -> IO ()
procVM clientSock clientAddr vm = do
    clientHandle <- socketToHandle clientSock ReadWriteMode
    hSetBuffering clientHandle LineBuffering
    messages <- hGetContents clientHandle
    mapM_ (handleMsg clientHandle vm) (lines messages)
    hClose clientHandle
    putStrLn $ "Client from " ++ show clientSock ++ " disconnected"
    
handleMsg :: Handle -> VM -> String -> IO ()
handleMsg clientHandle vm msg = do
    let splitMsg = words msg
    let action = splitMsg !! 0
    case lookup action messageHandlers of
        Just f  -> f clientHandle vm splitMsg
        Nothing -> putStrLn $ "unknown action " ++ action

messageHandlers :: [(String, Handle -> VM -> [String] -> IO ())]
messageHandlers = [("update",   handleUpdate),
                   ("key_down", handleKeyDown),
                   ("key_up",   handleKeyUp),
                   ("pointer",  handlePointer)]

handleUpdate :: Handle -> VM -> [String] -> IO ()
handleUpdate clientHandle vm update_msg = do
    {- update_msg is "update <vm id> <update id>" -}
    let updateID = read $ update_msg !! 2
    update@UpdateFB{
        updateSize = size,
        updatePos = pos
    } <- io $ getUpdate vm updateID

    png <- renderPng update
    let jsonData = JS.encode . JS.toJSObject $
          [("vm_id", update_msg !! 1), {- todo: make this client_id later -}
           ("action", "update_screen"),
           ("width", show $ fst size),
           ("height", show $ snd size),
           ("x", show $ fst pos),
           ("y", show $ snd pos),
           ("png", encode $ BS.unpack png)]
    hPutStrLn clientHandle $ (show $ length jsonData) ++ " update " ++ (show updateID)
    hPutStrLn clientHandle jsonData

handleKeyDown :: Handle -> VM -> [String] -> IO ()
handleKeyDown clientHandle vm key_msg = do
    {- key_msg is "key_down <vm id> key" -}
    let key = read $ key_msg !! 2
    io $ RFB.sendKeyEvent (vmRFB vm) True key

handleKeyUp :: Handle -> VM -> [String] -> IO ()
handleKeyUp clientHandle vm key_msg = do
    {- key_msg is "key_up <vm id> key" -}
    let key = read $ key_msg !! 2
    io $ RFB.sendKeyEvent (vmRFB vm) False key

handlePointer :: Handle -> VM -> [String] -> IO ()
handlePointer clientHandle vm pointer_msg = do
    {- pointer_msg is "pointer <vm id> x y mask" -}
    let x = read $ pointer_msg !! 2
    let y = read $ pointer_msg !! 3
    let mask = read $ pointer_msg !! 4
    io $ RFB.sendPointer (vmRFB vm) (fromIntegral mask) x y

