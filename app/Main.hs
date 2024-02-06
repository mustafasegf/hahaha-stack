-- ConstraintKinds needed only for 7.8.4
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Data.Maybe
  ( fromMaybe,
  )
import Data.Text
import Lucid
import Lucid.Base
import Lucid.Servant
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Lucid
import System.Environment
  ( lookupEnv,
  )
import Text.Read
  ( readMaybe,
  )

type API =
  Get '[HTML] (Html ())
    :<|> "data" :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

apiLink_ ::
  (IsElem endpoint API, HasLink endpoint) =>
  Proxy endpoint ->
  MkLink endpoint Attribute
apiLink_ = safeAbsHref_ (Proxy :: Proxy API)

server :: Server API
server = return root :<|> return html_data
  where
    root :: Html ()
    root =
      html_ $ do
        head_ $ do
          title_ "hello haskell"
          script_ [src_ "https://unpkg.com/htmx.org@1.9.10", type_ "text/javascript"] (mempty :: Html ())
          script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12", type_ "text/javascript"] (mempty :: Html ())
        body_ $ do
          h1_ "Hello, Haskell!"
          p_ [hxget_ "/data", hxswap_ "innerHTML", hyperscript_ "on click set *background-color to `red`"] "Click me !"

    html_data :: Html ()
    html_data = p_ "This is the data"

hxget_ :: Text -> Attribute
hxget_ = makeAttribute "hx-get"

hyperscript_ :: Text -> Attribute
hyperscript_ = makeAttribute "_"

hxswap_ :: Text -> Attribute
hxswap_ = makeAttribute "hx-swap"

app :: Application
app = serve api server

main :: IO ()
main = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  Warp.run port app
