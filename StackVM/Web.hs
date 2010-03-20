module StackVM.Web (
    module Hack,
    module Hack.Contrib.Response,
    module Network.Loli,
    module Network.Loli.Type,
    module Network.Loli.Utils,
    module Network.Loli.Template.TextTemplate,
    run, capture,
    with_type, with_body, with_header
) where

import Hack
import Hack.Handler.Happstack (run)
import Hack.Contrib.Response

import Network.Loli
import Network.Loli.Type hiding (router)
import Network.Loli.Utils 
import Network.Loli.Template.TextTemplate

import Data.List (find)

import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy (StateT)

import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))

type HackT = ReaderT AppReader (StateT AppState IO) ()

with_type :: String -> HackT
with_type = update . set_content_type

with_body :: ByteString -> HackT
with_body = update . set_body

with_header :: String -> String -> HackT
with_header = \field -> update . set_header field

capture :: String -> AppUnitT (Maybe String)
capture key = lookup key <$> captures
