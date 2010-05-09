module StackVM.Web (
    module Hack,
    module Hack.Contrib.Response,
    module Network.Loli,
    module Network.Loli.Type,
    module Network.Loli.Utils,
    capture, withType, withBody, withHeader
) where

import Hack
import Hack.Contrib.Response

import Network.Loli
import Network.Loli.Type hiding (router)
import Network.Loli.Utils 

import Data.List (find)

import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy (StateT)

import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))

type HackT = ReaderT AppReader (StateT AppState IO) ()

withType :: String -> HackT
withType = update . set_content_type

withBody :: ByteString -> HackT
withBody = update . set_body

withHeader :: String -> String -> HackT
withHeader = \field -> update . set_header field

capture :: String -> AppUnitT (Maybe String)
capture key = lookup key <$> captures
